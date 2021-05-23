local _M = {}

_M.focus = function(direction, screen)
    local win = hs.window.focusedWindow()
    local screen = screen or win:screen()
    if direction == "left" then
        screen:toWest()
    elseif direction == "right" then
        screen:toEast()
    elseif direction == "up" then
        screen:toNorth()
    elseif direction == "down" then
        screen:toSouth()
    end
end

_M.move = function(direction, window, screen)
    local window = window or hs.window.focusedWindow()
    local screen = screen or window:screen()
    if window:screen() == screen then return end

    if direction == "up" then
        window:moveOneScreenNorth()
    elseif direction == "down" then
        window:moveOneScreenSouth()
    elseif direction == "left" then
        window:moveOneScreenWest()
    elseif direction == "right" then
        window:moveOneScreenEast()
    end
end

_M.move_up = function(window)
    return _M.move("up", window)
end

_M.move_down = function(window)
    return _M.move("up", window)
end

_M.move_left = function(window)
    return _M.move("left", window)
end

_M.move_right = function(window)
    return _M.move("right", window)
end

_M.next = function()
    local window = hs.window.focusedWindow()
    local screen = window:screen():next()
end

_M.previous = function()
    local window = hs.window.focusedWindow()
    local screen = window:screen():previous()
end

return _M
