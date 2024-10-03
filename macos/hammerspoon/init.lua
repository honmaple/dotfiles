local yabai = require "yabai"
local bind = hs.hotkey.bind

hs.window.animationDuration = 0

yabai.init()

bind({"cmd"}, "H", function() yabai.exec("window --focus west") end)
bind({"cmd"}, "L", function() yabai.exec("window --focus east") end)
bind({"cmd"}, "J", function() yabai.exec("window --focus south") end)
bind({"cmd"}, "K", function() yabai.exec("window --focus north") end)

bind({"cmd", "shift"}, "H", function() yabai.exec("window --swap west") end)
bind({"cmd", "shift"}, "L", function() yabai.exec("window --swap east") end)
bind({"cmd", "shift"}, "J", function() yabai.exec("window --swap south") end)
bind({"cmd", "shift"}, "K", function() yabai.exec("window --swap north") end)

bind({"cmd"}, "E", function() yabai.exec("window --toggle split") end)
bind({"cmd"}, "B", function() yabai.exec("window --toggle border") end)
bind({"cmd"}, "W", function() yabai.exec("window --toggle zoom-parent") end)

bind({"cmd", "shift"}, "F", function() yabai.exec("window --toggle zoom-fullscreen") end)
bind({"cmd", "shift"}, "SPACE", function() yabai.exec("window --toggle float --grid 4:4:1:1:2:2") end)

-- bind({"cmd"}, "-", function() yabai.show_scratchpad() end)
-- bind({"cmd", "shift"}, "-", function() yabai.move_to_scratchpad() end)

bind({"cmd"}, "-", function() yabai.hide_or_show("终端", true) end)
bind({"cmd"}, "G", function() yabai.hide_or_show("Google Chrome", true) end)
bind({"cmd"}, "M", function() yabai.hide_or_show("launchpad", true) end)

bind({"cmd", "shift"}, "X", function() hs.caffeinate.lockScreen() end)
bind({"cmd", "shift"}, "R", function() hs.reload() end)

function mode(name, mod, key)
    local modal = hs.hotkey.modal.new(mod, key)
    local alert = {}

    function modal:entered()
        alert = hs.alert.show(string.format("Entered %s mode", name), "always")
    end

    function modal:exited()
        hs.alert.closeSpecific(alert)
        hs.alert.show(string.format('Exited %s mode', name))
    end

    modal:bind('', 'escape', function() modal:exit() end)
    return modal
end

local window_resize = mode("resize", {"cmd"}, "R")
window_resize:bind("", "H", function() yabai.exec({"window --resize left:-20:0",   "window --resize right:-20:0"}, "OR") end)
window_resize:bind("", "L", function() yabai.exec({"window --resize left:20:0",    "window --resize right:20:0"}, "OR") end)
window_resize:bind("", "K", function() yabai.exec({"window --resize bottom:0:-20", "window --resize top:0:-20"}, "OR") end)
window_resize:bind("", "J", function() yabai.exec({"window --resize bottom:0:20",  "window --resize top:0:20"}, "OR") end)

hs.notify.new({title="Hammerspoon", informativeText="配置已成功加载"}):send()
