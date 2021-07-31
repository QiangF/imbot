local fcitx = require("fcitx")

fcitx.watchEvent(fcitx.EventType.KeyEvent, "switch_key_event")

local fcitx_enable = false

function switch_key_event(sym, state, release)
  if state == fcitx.KeyState.Ctrl and sym == 92 and not release then
    -- Ctrl + \
    -- for cdda
    -- io.popen("notify-send 'Toggle input method!'")
    -- io.popen("emacsclient --eval '(imbot--pre-command-check)'")
    io.popen("update_im_state.sh")
    return true
  end
  return false
end
