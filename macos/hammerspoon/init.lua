local window = require "window"
local layout = require "layout"
local launch = require "launch"
local screen = require "screen"
local bind = hs.hotkey.bind

local modkey =  {"cmd"}
local modkey1 = {"cmd", "shift"}
local screens = {"main", "brower", "terminal", "global"}
local layouts = {"vertical", "horizontal", "fullscreen"}

hs.window.animationDuration = 0
layout.init(layouts)

bind(modkey, "H", window.focus_left)
bind(modkey, "L", window.focus_right)
bind(modkey, "K", window.focus_up)
bind(modkey, "J", window.focus_down)

bind(modkey, "E", layout.cycle)
bind(modkey, "-", function() launch.hide_or_show("终端", true) end)
bind(modkey, "G", function() launch.hide_or_show("Google Chrome", true) end)
bind(modkey, "M", function() launch.hide_or_show("launchpad", true) end)

bind(modkey1, "H", window.move_left)
bind(modkey1, "L", window.move_right)
bind(modkey1, "K", screen.move_up)
bind(modkey1, "J", screen.move_down)

bind(modkey1, "F", window.fullscreen)
bind(modkey1, "SPACE", window.toggle_float)

bind(modkey1, "X", function() hs.caffeinate.lockScreen() end)
bind(modkey1, "R", function() hs.reload() end)

function mode(name, mod, key)
    local modal = hs.hotkey.modal.new(mod, key)

    function modal:entered()
        hs.alert(string.format("Entered %s mode", name))
    end

    function modal:exited()
        hs.alert(string.format('Exited %s mode', name))
    end

    modal:bind('', 'escape', function() modal:exit() end)
    return modal
end

local window_resize = mode("resize", modkey, "R")
window_resize:bind("", "H", window.resize_left)
window_resize:bind("", "L", window.resize_right)
window_resize:bind("", "K", window.resize_up)
window_resize:bind("", "J", window.resize_down)

-- local window_move = mode("move", modkey, "M")
-- window_move:bind("", "H", window.move_left)
-- window_move:bind("", "L", window.move_right)
-- window_move:bind("", "K", window.move_up)
-- window_move:bind("", "J", window.move_down)

hs.notify.new({title="Hammerspoon", informativeText="配置已成功加载"}):send()
