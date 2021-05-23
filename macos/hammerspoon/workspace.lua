local _M = {}

_M.workspaces = {
    {
        name = "space1"
    },
    {
        name = "space2"
    }
}

local hotkey = require "hs.hotkey"
local window = require "hs.window"
local spaces = require "hs._asm.undocumented.spaces"

_M.switch = function(index)
    return _M.resize("down", window)
end

_M.move = function(num)
    return _M.resize("down", window)
end

function getGoodFocusedWindow(nofull)
    local win = window.focusedWindow()
    if not win or not win:isStandard() then return end
    if nofull and win:isFullScreen() then return end
    return win
end

function flashScreen(screen)
    local flash=hs.canvas.new(screen:fullFrame()):appendElements({
            action = "fill",
            fillColor = { alpha = 0.25, red=1},
            type = "rectangle"})
    flash:show()
    hs.timer.doAfter(.15,function () flash:delete() end)
end

function switchSpace(skip,dir)
    for i=1,skip do
        hs.eventtap.keyStroke({"ctrl"},dir)
    end
end

function moveWindowOneSpace(dir,switch)
    local win = getGoodFocusedWindow(true)
    if not win then return end
    local screen=win:screen()
    local uuid=screen:spacesUUID()
    local userSpaces=spaces.layout()[uuid]
    local thisSpace=win:spaces() -- first space win appears on
    if not thisSpace then return else thisSpace=thisSpace[1] end
    local last=nil
    local skipSpaces=0
    for _, spc in ipairs(userSpaces) do
        if spaces.spaceType(spc)~=spaces.types.user then -- skippable space
            skipSpaces=skipSpaces+1
        else          -- A good user space, check it
            if last and
                (dir=="left"  and spc==thisSpace) or
                (dir=="right" and last==thisSpace)
            then
                win:spacesMoveTo(dir=="left" and last or spc)
                if switch then
                    switchSpace(skipSpaces+1,dir)
                    win:focus()
                end
                return
            end
            last=spc	 -- Haven't found it yet...
            skipSpaces=0
        end
    end
    flashScreen(screen)   -- Shouldn't get here, so no space found
end

-- mash =      {"ctrl", "cmd"}
-- mashshift = {"ctrl", "cmd","shift"}

-- hotkey.bind(mash, "s",nil,
--             function() moveWindowOneSpace("right",true) end)
-- hotkey.bind(mash, "a",nil,
--             function() moveWindowOneSpace("left",true) end)
-- hotkey.bind(mashshift, "s",nil,
--             function() moveWindowOneSpace("right",false) end)
-- hotkey.bind(mashshift, "a",nil,
--             function() moveWindowOneSpace("left",false) end)
return _M
