local _M = {
    labels = {
        "main",
        "emacs",
        "chrome",
    }
}

_M.moveTo = function(index)
    local screen = hs.screen.mainScreen()
    local spaces = hs.spaces.spacesForScreen(screen)
    if not spaces then
        return
    end

    local space = spaces[index]
    if not space then
        hs.notify.new({informativeText="未找到space"}):send()
        return
    end

    local focusedWindow = hs.window.focusedWindow()
    if focusedWindow then
        hs.spaces.moveWindowToSpace(focusedWindow:id(), space)
    end
end

_M.switchTo = function(index)
    local screen = hs.screen.mainScreen()
    local spaces = hs.spaces.spacesForScreen(screen)
    if not spaces then
        return
    end

    local space = spaces[index]
    if not space then
        hs.notify.new({informativeText="未找到space"}):send()
        return
    end

    hs.spaces.gotoSpace(space)
end

_M.create = function()
    hs.spaces.addSpaceToScreen(hs.screen.mainScreen())
end

_M.destory = function()
    hs.spaces.removeSpace(hs.spaces.activeSpaceOnScreen())
end

_M.show = function()
    hs.spaces.toggleMissionControl()
end

return _M