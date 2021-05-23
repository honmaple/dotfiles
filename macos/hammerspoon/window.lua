local _M = {
    step = 30,
    history = {}
}

local function get_history(window, remove)
    local id = window:id()
    for index, value in ipairs(_M.history) do
        if value[1] == id then
            if remove then table.remove(_M.history, index) end
            return value
        end
    end
    return nil
end

local function set_history(window)
    if #_M.history > 100 then table.remove(_M.history) end
    local id = window:id()
    local fr = window:frame()
    table.insert(_M.history, 1, {id, fr})
end

local function get_window(window)
    local window = window or hs.window.focusedWindow()
    if window then set_history(window) end
    return window
end

local function is_float(window)
    local f = window:screen():frame()
    local wf = window:frame()
    return wf.w ~= f.w and wf.h ~= f.h
end

_M.is_float = is_float

_M.resize = function (direction, window, otherWindow)
    local window = get_window(window)
    if not window then return end

    local frame = window:screen():fullFrame()
    local w = frame.w/_M.step
    local h = frame.h/_M.step
    local f = window:frame()
    if direction == "left" then
        if f.x > 0 then
            f.x = f.x - w
            f.w = f.w + w
        else
            f.w = f.w - w
        end
    elseif direction == "right" then
        if f.x > 0 then
            f.x = f.x + w
            f.w = f.w - w
        else
            f.w = f.w + w
        end
    elseif direction == "up" then
        if f.y > 25 then
            f.y = f.y - h
            f.h = f.h + h
        else
            f.h = f.h - h
        end
    elseif direction == "down" then
        if f.y > 25 then
            f.y = f.y + h
            f.h = f.h - h
        else
            f.h = f.h + h
        end
    end
    window:setFrame(f)

    if not otherWindow then
        local windows = window:otherWindowsSameScreen()
        hs.fnutils.each(windows, function(win) _M.resize(direction, win, true) end)
    end
end

_M.resize_left = function(window)
    return _M.resize("left", window)
end

_M.resize_right = function(window)
    return _M.resize("right", window)
end

_M.resize_up = function(window)
    return _M.resize("up", window)
end

_M.resize_down = function(window)
    return _M.resize("down", window)
end

_M.move = function (direction, window)
    local window = get_window(window)
    if not window then return end

    local screen = window:screen()
    local frame = screen:fullFrame()
    local stepw = frame.w/_M.step
    local steph = frame.h/_M.step
    local topleft = window:topLeft()
    if direction == "left" then
        window:setTopLeft({x=topleft.x-stepw, y=topleft.y})
    elseif direction == "right" then
        window:setTopLeft({x=topleft.x+stepw, y=topleft.y})
    elseif direction == "up" then
        window:setTopLeft({x=topleft.x, y=topleft.y-steph})
    elseif direction == "down" then
        window:setTopLeft({x=topleft.x, y=topleft.y+steph})
    end

end

_M.move_left = function(window)
    return _M.move("left", window)
end

_M.move_right = function(window)
    return _M.move("right", window)
end

_M.move_up = function(window)
    return _M.move("up", window)
end

_M.move_down = function(window)
    return _M.move("down", window)
end

_M.focus = function(direction, window)
    local window = get_window(window)
    if not window then return end

    if direction == "left" then
        window:focusWindowWest()
    elseif direction == "right" then
        window:focusWindowEast()
    elseif direction == "up" then
        window:focusWindowNorth()
    elseif direction == "down" then
        window:focusWindowSouth()
    end
end

_M.focus_left=function(window)
    return _M.focus("left", window)
end

_M.focus_right= function(window)
    return _M.focus("right", window)
end

_M.focus_up = function(window)
    return _M.focus("up", window)
end

_M.focus_down = function(window)
    return _M.focus("down", window)
end

_M.switch = function(direction)
    if direction == "left" then
        hs.window.switcher.previousWindow()
    elseif direction == "right" then
        hs.window.switcher.nextWindow()
    end
end

_M.switch_left = function()
    return _M.switch("left")
end

_M.switch_right = function()
    return _M.switch("right")
end

_M.move_and_resize = function(direction, window)
    local window = get_window(window)
    if not window then return end

    local newrect
    if direction == "left" then
        newrect = hs.layout.left50
    elseif direction == "right" then
        newrect = hs.layout.right50
    elseif direction == "up" then
        newrect = {0,0,1,0.5}
    elseif direction == "down" then
        newrect = {0,0.5,1,0.5}
    elseif direction == "max" then
        newrect = hs.layout.maximized
    elseif direction == "float" then
        newrect = {0.25,0.25,0.5,0.5}
    end
    window:move(newrect)
end

_M.move_left_and_resize = function(window)
    return _M.move_and_resize("left", window)
end

_M.move_right_and_resize = function(window)
    return _M.move_and_resize("right", window)
end

_M.move_up_and_resize = function(window)
    return _M.move_and_resize("up", window)
end

_M.move_down_and_resize = function(window)
    return _M.move_and_resize("down", window)
end

_M.fullscreen = function(window)
    return _M.move_and_resize("max", window)
end

_M.float = function(window)
    return _M.move_and_resize("float", window)
end

_M.toggle = function(fn)
    local window = hs.window.focusedWindow()
    local hs = get_history(window, true)

    if hs then
        return window:setFrame(hs[2])
    else
        return fn(window)
    end
end

_M.toggle_float = function()
    return _M.toggle(_M.float)
end

_M.toggle_fullscreen = function()
    return _M.toggle(_M.fullscreen)
end

return _M
