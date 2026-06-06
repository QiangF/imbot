;;; fcitx-dbus-backend.el

(require 'dbus)

;; https://github.com/fcitx/fcitx5/discussions/350
;; dbus 接口的定义：
;; https://github.com/fcitx/fcitx5-qt/blob/master/qt5/dbusaddons/interfaces/org.fcitx.Fcitx.InputContext1.xml
;; https://github.com/fcitx/fcitx5-qt/blob/master/qt5/dbusaddons/interfaces/org.fcitx.Fcitx.InputMethod1.xml

;; 输入法状态是和每个 input context 绑定的
;; 首先 CreateInputContext，可以理解为创建了一个会话/连接，然后返回一个 dbus object path
;; 之后继续用这个 object 和 fcitx 互相通信，发送按键，通过dbus signal获取预编辑和候选词列表
;; 可以使用以下命令查看发送的事件：
;; dbus-monitor --session "type='signal',sender='org.fcitx.Fcitx5'"
;; dbus-monitor "interface='org.fcitx.Fcitx.InputContext1'"

(defvar fcitx-service "org.fcitx.Fcitx5")
(defvar fcitx-ic-path nil)
(defvar fcitx-ic-interface "org.fcitx.Fcitx.InputContext1")
(defvar fcitx-im-name "rime")

(defun fcitx-alive ()
  "Check if theres a running fcitx."
  (dbus-ping :session fcitx-service 100))

(defun imbot-toggle ()
  "Function used to toggle input method outside emacs, used in Exwm."
  (interactive)
  ;; 1st engine is english input method
  (if (equal major-mode 'exwm-mode)
      (if (equal (fcitx-controller-call "State") 1)
          (fcitx-controller-call "Activate")
        (fcitx-controller-call "Deactivate"))
    (toggle-input-method)))

(defun fcitx-find-correct-service ()
  "List all registered D-Bus services containing 'Fcitx'."
  (interactive)
  (let ((services (dbus-call-method :session "org.freedesktop.DBus"
                                    "/org/freedesktop/DBus"
                                    "org.freedesktop.DBus"
                                    "ListNames")))
    (message "Found Fcitx-related services: %s"
             (seq-filter (lambda (s) (string-match-p "Fcitx" s)) services))))

(defun fcitx-list-all-im ()
  "Get a list of all available input methods and their unique names."
  (interactive)
  (let ((im-list (dbus-call-method :session "org.fcitx.Fcitx5"
                                   "/controller"
                                   "org.fcitx.Fcitx.Controller1"
                                   "AvailableInputMethods")))
    (with-current-buffer (get-buffer-create "*fcitx-engines*")
      (erase-buffer)
      (dolist (im im-list)
        ;; im is a list: (name native-name icon-name unique-name)
        (insert (format "Name: %s | ID: %s\n" (car im) (nth 3 im))))
      (display-buffer (current-buffer)))
    (message "Listed %d engines in *fcitx-engines*" (length im-list))))

(defun fcitx-get-current-im ()
  "Get the unique name of the currently active Input Method."
  (interactive)
  (let ((im (dbus-call-method :session "org.fcitx.Fcitx5"
                              "/controller"
                              "org.fcitx.Fcitx.Controller1"
                              "CurrentInputMethod")))
    (message "Current IM: %s" im)
    im))

(defun fcitx-ic-call (method &rest args)
  (apply 'dbus-call-method `(:session ,fcitx-service ,fcitx-ic-path ,fcitx-ic-interface
                                      ,method ,@args)))

(defun fcitx-controller-call (method &rest args)
  (apply 'dbus-call-method `(:session ,fcitx-service "/controller" "org.fcitx.Fcitx.Controller1"
                                      ,method ,@args)))

;; If the CreateInputContext method requires input arguments (as D-Bus methods often do),
;; you would append them as additional arguments to the function call.
;; You can use dbus-introspect-get-signature to determine the exact arguments required for the method.
(defun fcitx-create-input-context (client-name)
  "Input argument: A single string (DBus type s) named client_name.
   Return type: A single object path (DBus type o).

The object path returned points to the newly created input context object,
 which implements the org.fcitx.Fcitx.InputContext1 interface (or similar).
You then interact with this new object path for input method operations. "
  (let ((ic (dbus-call-method :session fcitx-service
                              "/org/freedesktop/portal/inputmethod"
                              "org.fcitx.Fcitx.InputMethod1"
                              "CreateInputContext"
                              `((:struct "program" ,client-name)
                                (:struct "display" "emacs")))))
    (setq fcitx-ic-path (car ic))
    ;; set capability CapabilityFlag::ClientSideInputPanel = (1ULL << 39)
    (fcitx-ic-call "SetCapability" :uint64
                          (logior
                           (ash 1 39)
                           ;; ClientSideControlState
                           (ash 1 2)))
    (dbus-register-signal :session fcitx-service
                          fcitx-ic-path fcitx-ic-interface "CommitString"
                          'fcitx-handler-for-commit-string)
    (dbus-register-signal :session fcitx-service
                          fcitx-ic-path fcitx-ic-interface "UpdateClientSideUI"
                          'fcitx-handler-for-client-ui)))

;; (s str)
(defun fcitx-handler-for-commit-string (s)
  "use return to update region in iedit-mode"
  ;; (imbot--map-unset)
  ;; (insert s)
  ;; (set-buffer-modified-p t)
  ;; (run-hooks 'post-self-insert-hook)
  ;; (when (equal major-mode 'mistty-mode)
  ;;   (mistty--post-command))
  ;; (redisplay)
  (setq imbot--commit s))

(defun fcitx-handler-for-client-ui (&rest tooltip)
  (setq imbot--tooltip tooltip))

(when (bound-and-true-p exwm-enable)
  (defvar exwm-inside-input-field nil)
  (defun exwm-input-field-entry-handler (&rest args)
    (setq exwm-inside-input-field t))
  (defun exwm-input-field-exit-handler ()
    (setq exwm-inside-input-field nil))

  (dbus-register-signal
   :session fcitx-service
   nil                                  ; PATH: Wildcard, listen on all object paths
   fcitx-ic-interface "CurrentIM"
   #'exwm-input-field-entry-handler)

  (dbus-register-signal
   :session fcitx-service
   nil
   fcitx-ic-interface "NotifyFocusOut"
   #'exwm-input-field-exit-handler))

;; backend interface functions
(defun imbot-backend-activate ()
  (unless fcitx-ic-path
    (fcitx-create-input-context (number-to-string (round (time-to-seconds)))))
  (fcitx-ic-call "FocusIn")
  ;; im is a string, such as pinyin, rime
  (fcitx-controller-call "SetCurrentIM" :string fcitx-im-name))

;; keycode can be looked up in keyboard.py
;; keyval can be looked up in keysyms.py
;; or use xev for keysym and keycode
;; ProcessKeyEvent(u keyval, u keycode, u state, b type, u time) = (b ret)
;; bool processKeyEvent
;; (uint32_t keyval, uint32_t keycode, uint32_t state, bool isRelease, uint32_t time)
;; state representing the state of modifier keys (like Shift, Ctrl, Alt) at the time of the event. nil suggests no modifiers were active or the state is not specified. (shift: state 1
;; The last argument, which likely provides a timestamp for the event, probably in milliseconds since a certain epoch, for timing purposes.
;; ProcessKeyEvent(code, 0, mask, false, 0)
;; nil (False) for the type parameter usually means Key Release in some DBus specs, or Key Press depending on the specific implementation. For Fcitx, usually 0 is press and 1 is release. Ensure you are sending a "Press" event to trigger a response.

;; key event states
;; Modifier	X11 Bitmask Value
;; Shift	(ash 1 0) → 1
;; Lock	(ash 1 1) → 2
;; Control	(ash 1 2) → 4
;; Alt/Meta	(ash 1 3) → 8

(defun fcitx-process-key (keysym state)
  (fcitx-ic-call "ProcessKeyEvent" keysym 0 state nil 0))

(defun fcitx-translate-emacs-key (event)
  "Translate output of `read-key-sequence` to (keysym . mask) for Fcitx5."
  (let* (;; If event is a string " ", convert to character ?\s
         (clean-event (if (stringp event) (string-to-char event) event))
         (base (event-basic-type clean-event))
         ;; base for captial letter might be out of bound, so test clean-event
         (capital-p (and (integerp clean-event)
                         (>= clean-event ?A)
                         (<= clean-event ?Z)))
         (mods (event-modifiers event))
         (mask 0) keysym)
    ;; (message "base %s event %s mods %s" base event mods)

    ;; 1. Calculate the Mask (Fcitx5/X11 standard)
    (when (memq 'shift mods) (setq mask (logior mask (ash 1 0))))   ; ShiftMask
    (when (memq 'control mods) (setq mask (logior mask (ash 1 2)))) ; ControlMask
    (when (memq 'meta mods) (setq mask (logior mask (ash 1 3))))    ; Mod1Mask (Alt)
    (when (memq 'super mods) (setq mask (logior mask (ash 1 6))))   ; Mod4Mask (Super/Win)

    ;; 2. Determine the Keysym
    ;; fcitx5 doesn't accept upper case letter as first char
    ;; capital letter in other places is accepted, eg. /Phi
    (cond
     ;; Case A: It's a character or control integer (Common in Terminals)
     ((integerp base)
      (cond
       ((= base 127) (setq keysym 65288)) ; ASCII DEL -> X11 Backspace
       ((= base 8) (setq keysym 65288))   ; ASCII C-h -> X11 Backspace
       ;; return event is 13, base is 109, mods is (control)
       ((= event 13) (progn (setq keysym 65293)
                            (setq mask 0)))
       ((= base 13) (setq keysym 65293)) ; ASCII CR  -> X11 Return
       ((= base 10) (setq keysym 65293)) ; ASCII LF  -> X11 Return
       ((= base 27) (setq keysym 65307)) ; ASCII ESC -> X11 Escape
       ;; Handle control characters (e.g., C-a is ASCII 1)
       ;; Fcitx5 usually wants the 'raw' keysym + mask
       ((and (memq 'control mods) (< base 32))
        (setq keysym (+ base 96)))      ; Map ASCII 1 (C-a) to 'a' (97)
       (t
        (if capital-p
            ;; captial letter event does not have shift mod
            (setq keysym clean-event)
          (setq keysym base)))))

     ;; Case B: It's a symbol (Common in GUI)
     ((symbolp base)
      (setq keysym
            (let ((name (symbol-name base)))
              (cond
               ((string= name "return") 65293)
               ;; Depending on your environment (terminal vs. GUI, OS, keyboard settings), backspace key typically yields:
               ;; "\C-h" (ASCII 8, with control modifier) — treated as C-h
               ;; "\d" (ASCII 127, no modifier) — the DEL character
               ((string= name "backspace") 65288)
               ;; ((string= name "tab") 65289)
               ((string= name "escape") 65307)
               ((string= name "deletechar") 65535)
               ((string= name "home") 65360)
               ((string= name "left") 65361)
               ((string= name "up") 65362)
               ((string= name "right") 65363)
               ((string= name "down") 65364)
               ((string= name "prior") 65365) ;; PageUp
               ((string= name "next") 65366)  ;; PageDown
               ;; ((string-match "f\\([0-9]+\\)" name)
               ;;  (+ 65470 (- (string-to-number (match-string 1 name)) 1)))
               (t nil))))))
    (cons keysym mask)))

(defvar fcitx-dbus-max-timeout 0.1
  "Maximum second to wait for Fcitx5 signals before giving up.")

(defun imbot-backend-process-key (keysym &optional mask)
  "Send key to D-Bus and reactively pump the event loop until signals fire."
  (let ((old-tooltip imbot--tooltip)
        (old-commit imbot--commit)
        (start-time (float-time))
        handled)
    ;; 1. Capture whether Fcitx5 actually swallowed/filtered the key
    (setq handled (fcitx-process-key keysym mask))

    ;; 2. Only wait for async D-Bus signals if Fcitx is actively processing it.
    ;;    If handled is nil, no UI updates or commits are coming anyway.
    (when handled
      (while (and (equal old-tooltip imbot--tooltip)
                  (equal old-commit imbot--commit)
                  (< (- (float-time) start-time) fcitx-dbus-max-timeout))
        (accept-process-output nil 0.001)))

    ;; 3. Return the actual boolean back to imbot-translate
    handled))

;; (a(si) preedit, i cursorpos, a(si) auxUp, a(si) auxDown, a(ss) candidates,
;; i candidateIndex, i layoutHint, b hasPrev, b hasNext)
;; preedit	String	The current composition string (e.g., "nihao").
;; cursorpos	Int32	Position of the cursor within the preedit string.
;; auxUp	String	Auxiliary text above the input (often empty).
;; auxDown	String	Auxiliary text below the input (often empty).
;; candidates	List	A list of structs containing (String, Label).
;; candidateIndex	Int32	The currently highlighted candidate index.
;; layoutHint	Int32	UI layout suggestion (0 for horizontal, 1 for vertical).
;; hasPrev	Boolean	Whether there is a previous page of candidates.
;; hasNext	Boolean	Whether there is a next page of candidates.
;; eg.
;; ((("ni" 0)) 2 nil nil
;;  (("1 " "你") ("2 " "拟") ("3 " "泥") ("4 " "霓") ("5 " "尼"))
;;  0 0 nil t)
(defun imbot-backend-format-tooltip ()
  "Build candidate menu tooltip from imbot context."
  (destructuring-bind (preedit cursorpos auxUp auxDown candidates candidateIndex layoutHint hasPrev hasNext)
      imbot--tooltip
    (let* (;; Helper function to extract and concatenate strings from a(si) dbus arrays
           (extract-strings (lambda (arr)
                              (mapconcat (lambda (item) (car item)) arr "")))

           ;; Extract the actual strings
           (preedit-str (funcall extract-strings preedit))
           ;; (aux-up-str (funcall extract-strings auxUp))
           (aux-down-str (funcall extract-strings auxDown)) 
           prompt-str page-str candidate-str)
      (setq prompt-str (with-temp-buffer
                         ;; (insert (caar preedit))
                         (insert preedit-str)
                         (goto-char (1+ cursorpos))
                         (insert "˰")
                         (insert aux-down-str)
                         (buffer-string)))
      (when candidates
        (setq page-str (mapconcat (lambda (c)
                                    (if (car c) (cadr c) ""))
                                  (list (list hasPrev "<") (list hasNext ">"))))
        (setq candidate-str
              (mapconcat (lambda (c)
                           (let ((idx (string-trim (car c)))
                                 (word (cadr c)))
                             (if (= (1- (string-to-number idx)) candidateIndex)
                                 (format "[%s%s]" idx word)
                               (format "%s%s" idx word)))) candidates " ")))
      (concat prompt-str page-str "\n" candidate-str))))

(defun imbot-backend-clear-composition ()
  (fcitx-ic-call "Reset"))

(defun imbot-backend-cleanup ()
  (fcitx-ic-call "DestroyIC"))

(defun imbot-backend-send-escape ()
  "Clear the composition."
  (interactive)
  (fcitx-process-key 65307 0))

(provide 'backend-fcitx-dbus)
