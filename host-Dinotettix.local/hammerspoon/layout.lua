hs.alert.show("Hammerspoon Reloaded!")
hs.ipc.cliInstall()

local function assign(tbl, ...)
    for _, tblToCopy in ipairs({...}) do
        for k, v in pairs(tblToCopy) do
            tbl[k] = v
        end
    end
    return tbl
end

local function merge(...)
    return assign({}, ...)
end

local function layoutMainLeft(layoutTree, frame, children)
    local windowWidth = frame.w / 2
    local mainFrame = merge(frame, {w = windowWidth})
    for i, child in pairs(children) do
        if i == 1 then
            layoutTree(mainFrame, child)
        else
            layoutTree(merge(mainFrame, {x = frame.x + windowWidth}), child)
        end
    end
end

local function layoutHorizontal(layoutTree, frame, children)
    local nbWindows = #children
    local windowWidth = frame.w / nbWindows
    local start = frame.x - windowWidth

    local sizedFrame = merge(frame, {w = windowWidth})
    for i, child in pairs(children) do
        layoutTree(
            merge(
                sizedFrame,
                {
                    x = start + (i * windowWidth)
                }
            ),
            child
        )
    end
end

local function layoutVertical(layoutTree, frame, children)
    local nbWindows = #children
    local windowHeight = frame.h / nbWindows
    local start = frame.y - windowHeight

    local sizedFrame = merge(frame, {h = windowHeight})
    for i, child in pairs(children) do
        layoutTree(
            merge(
                sizedFrame,
                {
                    y = start + (i * windowHeight)
                }
            ),
            child
        )
    end
end

local function withPadding(layoutTree, frame, child)
    local padding = {
        bottom = 0,
        left = 0,
        right = 0,
        top = 20
    }

    return layoutTree(
        {
            x = frame.x + padding.left,
            y = frame.y + padding.top,
            w = frame.w - (padding.left + padding.right),
            h = frame.h - (padding.top + padding.bottom)
        },
        child
    )
end

local function layoutTree(frame, treeNode)
    if treeNode[1] == "padding" then
        return withPadding(layoutTree, frame, treeNode[2])
    elseif treeNode[1] == "horizontal" then
        return layoutHorizontal(layoutTree, frame, treeNode[2])
    elseif treeNode[1] == "vertical" then
        return layoutVertical(layoutTree, frame, treeNode[2])
    elseif treeNode[1] == "mainLeft" then
        return layoutMainLeft(layoutTree, frame, treeNode[2])
    elseif treeNode[1] == "window" then
        treeNode[2]:setFrame(frame)
    else
        error("Unknown node type: " .. treeNode[1])
    end
end

local function test(mainLayout)
    hs.alert.show("testing")
    local screen = hs.screen.mainScreen()
    local frame = screen:frame()

    local function window(id)
        local w = hs.window.get(id)
        return {"window", w}
    end

    local tree = {
        "padding",
        {
            mainLayout,
            {window(28585), window(28580), window(28590)}
        }
    }
    layoutTree(frame, tree)
end

local function callback(_, stdout)
    local windows = nil
    local function usefulWindow(yabaiWindow)
        return ((yabaiWindow.minimized == 0) and (yabaiWindow.subrole == "AXStandardWindow"))
    end
    local function toHammerspoonWindow(yabaiWindow)
        return hs.window.get(yabaiWindow.id)
    end
    windows = hs.fnutils.map(hs.fnutils.filter(hs.json.decode(stdout), usefulWindow), toHammerspoonWindow)

    local frame = windows[1]:screen():frame()
    return withPadding(
        {
            frame = frame,
            windows = windows
        },
        layoutMainLeft,
        {bottom = 0, left = 0, right = 0, top = 20}
    )
end

local function layoutOnStart()
    local yabai = "/usr/local/bin/yabai"
    local args = {"-m", "query", "--windows", "--space"}
    local task = hs.task.new(yabai, callback, args)
    return task:start()
end

local function bind(tbl, key)
    return function(...)
        tbl[key](tbl, ...)
    end
end

local function setupFilter()
    local filter = hs.window.filter.new()
    filter:subscribe(
        {
            [hs.window.filter.windowCreated] = function(window)
                hs.alert.show(window:id())
            end
        }
    )
    return filter
end

setupFilter()

do
    local k = hs.hotkey.modal.new({}, nil)
    k:bind({}, "escape", bind(k, "exit"))
    k:bind({}, "r", hs.reload)
    k:bind({}, "l", layoutOnStart)
    k:bind(
        {},
        "h",
        function()
            test("horizontal")
        end
    )
    k:bind(
        {},
        "v",
        function()
            test("vertical")
        end
    )

    local function onPress()
        return k:enter()
    end
    local function onRelease()
        return k:exit()
    end
    hotkey = hs.hotkey.bind({}, "f18", onPress, onRelease)
end
