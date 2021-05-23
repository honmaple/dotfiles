local window = require "window"

local _M = {
    layouts = {},
    spaces= {}
}

local function current_windows()
    local windows = hs.fnutils.filter(
        hs.window.visibleWindows(),
        function(win)
            local fw = hs.window.focusedWindow()
            return (
                win:isStandard() and win:isVisible() and win:screen() == fw:screen()
            )
    end)
    return windows
end

local function current_space()
    local screen = hs.window.focusedWindow():screen()
    local spaces = hs.fnutils.filter(
        _M.spaces,
        function(space) return space.screen == screen end)

    if #spaces > 0 then
        return spaces[1]
    end

    local space = {
        screen = screen,
        layout = _M.layouts[1],
        layoutCycle = hs.fnutils.cycle(_M.layouts)
    }
    table.insert(_M.spaces, space)
    return space
end


local function set_layout(name, windows)
    local layouts = {
        fullscreen = _M.fullscreen,
        horizontal = _M.horizontal,
        vertical = _M.vertical,
        columns = _M.columns,
        rows = _M.rows
    }
    --  不允许浮动窗口加入布局
    local windows = hs.fnutils.filter(
        windows or current_windows(),
        function(win)
            return not window.is_float(win)
    end)
    return layouts[name](windows)
end

_M.fullscreen = function(windows)
    hs.fnutils.each(windows, window.fullscreen)
end

_M.vertical = function(windows)
    local win_count = #windows

    if win_count == 1 then
        return _M.fullscreen(windows)
    end

    for index, win in pairs(windows) do
        local frame = win:screen():frame()

        if index == 1 then
            frame.w = frame.w / 2
        else
            frame.x = frame.x + frame.w / 2
            frame.w = frame.w / 2
            frame.h = frame.h / (win_count - 1)
            frame.y = frame.y + frame.h * (index - 2)
        end

        win:setFrame(frame)
    end
end

_M.horizontal = function(windows)
    local win_count = #windows

    if win_count == 1 then
        return _M.fullscreen(windows)
    end

    for index, win in pairs(windows) do
        local frame = win:screen():frame()

        if index == 1 then
            frame.h = frame.h / 2
        else
            frame.y = frame.y + frame.h / 2
            frame.h = frame.h / 2
            frame.w = frame.w / (win_count - 1)
            frame.x = frame.x + frame.w * (index - 2)
        end

        win:setFrame(frame)
    end
end

_M.columns = function(windows)
    local win_count = #windows

    if win_count == 1 then
        return _M.fullscreen(windows)
    end

    for index, win in pairs(windows) do
        local frame = win:screen():frame()

        frame.w = frame.w / win_count
        frame.x = frame.x + (index - 1) * frame.w
        frame.y = 0

        win:setFrame(frame)
    end
end

_M.rows = function(windows)
    local win_count = #windows

    if win_count == 1 then
        return _M.fullscreen(windows)
    end

    for index, win in pairs(windows) do
        local frame = win:screen():frame()

        frame.h = frame.h / win_count
        frame.y = frame.y + (index - 1) * frame.h
        frame.x = 0

        win:setFrame(frame)
    end
end

_M.cycle = function(windows)
    local space = current_space()
    space.layout = space.layoutCycle()
    return set_layout(space.layout, windows)
end

_M.set_fullscreen = function(windows)
    return set_layout("fullscreen", windows)
end

_M.set_vertical = function(windows)
    return set_layout("vertical", windows)
end

_M.set_horizontal = function(windows)
    return set_layout("horizontal", windows)
end

_M.set_rows = function(windows)
    return set_layout("rows", windows)
end

_M.set_columns = function(windows)
    return set_layout("columns", windows)
end

_M.init = function(layouts)
    -- if #layoutsconf > 0 then
    --     set_layout(layoutsconf[1])
    -- end
    _M.layouts = layouts
end

return _M
