local _M = {
    scratchpad = {}
}

local yabai = string.gsub(hs.execute("which yabai", true), "%s+", "")

local function application(name, autostart, notify)
    local app = hs.application.find(name)
    if app then return app end

    if autostart then
        app = hs.application.launchOrFocus(name)
    end

    if not app and notify then
        hs.notify.new({title=name, informativeText="应用不存在或者未启动"}):send()
    end
    return app
end

_M.init = function()
    local output, status = _M.exec("query --windows")
    if not status then return end

    local windows = hs.json.decode(output)
    for _, window in pairs(windows) do
        if window["is-hidden"] then
            table.insert(_M.scratchpad, window)
        end
    end
end

_M.exec = function(args, operator)
    if (type(args) == "table") then
        local o,s,t,r
        for _, arg in pairs(args) do
            o,s,t,r = _M.exec(arg)
            if (operator == "OR" == s) then
                break
            end
        end
        return o,s,t,r
    end

    local command = string.format("%s -m %s", yabai, args)
    print(command)
    return hs.execute(command)
end

_M.hide_or_show = function(name, autostart)
    local app = application(name, autostart, true)

    if not app then
        return
    end

    if app:isFrontmost() then
        app:hide()
    elseif app:isHidden() then
        app:unhide()
        app:activate(true)
    else
        app:hide()
    end
end

_M.show_scratchpad = function(name)
    for index, window in pairs(_M.scratchpad) do
        if not name or name == window["app"] then
            table.remove(_M.scratchpad, index)

            local app = hs.application.find(window["pid"])
            app:unhide()
            app:activate(true)
            -- 先移动，再切换浮动
            -- _M.exec(string.format("window %d --move abs:%d:%d", window["id"], window["frame"]["x"], window["frame"]["y"]))
            if not window["is-floating"] then
                _M.exec(string.format("window %d --toggle float", window["id"]))
            end
            return
        end
    end

    if name then
        hs.notify.new({informativeText="应用未找到"}):send()
        return
    end

    local output, status = _M.exec("query --windows --window")
    if not status then return end

    local window = hs.json.decode(output)
    if window and window["is-floating"] then
        table.insert(_M.scratchpad, window)

        local app = hs.application.find(window["pid"])
        return app:hide()
        -- return _M.exec("window --move abs:10000:10000")
    end
    hs.notify.new({informativeText="没有隐藏应用"}):send()
end

_M.move_to_scratchpad = function()
    local output, status = _M.exec("query --windows --window")
    if not status then return end

    local window = hs.json.decode(output)
    if window and window["has-focus"] then
        table.insert(_M.scratchpad, window)

        if not window["is-floating"] then
            _M.exec("window --toggle float")
        end

        local app = hs.application.find(window["pid"])
        return app:hide()
        -- return _M.exec("window --move abs:10000:10000")
    end

    hs.notify.new({informativeText="应用未找到"}):send()
end

return _M