local _M = {
    autostart = true,
    lastwindow = {}
}

-- hs.application.enableSpotlightForNameSearches(true)

local function app_window(app)
    local app_window = app:focusedWindow()
    if app_window then
        return app_window
    end

    local app_windows = app:allWindows()
    if  #app_windows > 0 then
        return app_windows[1]
    end
end

local function application(name, notice)
    local app = hs.application.find(name)
    if app then return app end

    if notice then
        hs.notify.new({title=name, informativeText="应用未启动"}):send()
    end
    return
end

_M.hide_or_show = function(name, start)
    local app = application(name, not start)

    if not app and start then
        return hs.application.launchOrFocus(name)
    end

    local app_window = app_window(app)

    if not app_window then
        hs.notify.new({title=name, informativeText="当前屏幕未发现该应用"}):send()
    end

    if app_window:isMinimized() then
        app_window:unminimize()
        app_window:focus()
    elseif app:isFrontmost() then
        -- app:isFrontmost() mean app window is focused
        -- app_window:minimize()
        app:hide()
    elseif app:isHidden() then
        app:activate(true)
        app:unhide()
        app_window:focus()
    else
        app:hide()
    end
end

_M.start = function(name)
    local app = application(name)
    if not app then
        hs.application.launchOrFocus(name)
    end
end

_M.stop = function(name)
    local app = application(name, true)
    if app then app:kill() end
end

return _M
