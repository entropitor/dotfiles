import XMonad
import XMonad.Actions.Volume
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Window
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Dzen (dzenConfig, center, (>=>), onCurr, addArgs, font)
import System.IO

-- general config
altMask = mod1Mask
myModMask = mod4Mask
myTerminal = "gnome-terminal"


-- My workspaces
myWorkspace1 = "1:WF"
myWorkspace2 = "2:Pinned"
myWorkspace3 = "3:Chrome"
myWorkspace4 = "4:Term"
myWorkspace5 = "5:Main"
myWorkspace6 = "6:Extra"
myWorkspace7 = "7:Mail"
myWorkspace8 = "8:Hang"
myWorkspace9 = "9:Slack"
myWorkspaces =
  [
    myWorkspace1, myWorkspace2, myWorkspace3,
    myWorkspace4, myWorkspace5, myWorkspace6,
    myWorkspace7, myWorkspace8, myWorkspace9
  ]
startupWorkspace = myWorkspace5

-- My Layouts
defaultLayouts = showWName (tiled ||| simpleTabbedBottom ||| noBorders Full ||| Grid ||| Mirror tiled)
  where tiled = Tall 1 (3/100) (1/2)
--defaultLayouts = Tall 1 (3/100) (1/2) ||| Full
--myLayouts = onWorkspace myWorkspace1 Full $ defaultLayouts
myLayouts = defaultLayouts

-- My key bindings
myKeyBindings =
  [ ((0, xK_Print), spawn "sleep 0.2; gnome-screenshot -c -a") -- sleep to get keyboard!
    , ((controlMask, xK_Print), spawn "gnome-screenshot -c -w")
    , ((controlMask .|. shiftMask, xK_Print), spawn "gnome-screenshot -i -a")
    -- http://superuser.com/questions/389737/how-do-you-make-volume-keys-and-mute-key-work-in-xmonad
    -- http://dmwit.com/volume/
    , ((0, 0x1008FF11), lowerVolume 5 >>= alertDouble)
    , ((0, 0x1008FF13), raiseVolume 5 >>= alertDouble)
    , ((0, 0x1008FF12), toggleMuteChannels ["Master", "Speaker", "Headphone", "Headphone 1"] >>= \muted -> alert (if muted then "off" else "on"))
    -- old terminal shortcut
    , ((controlMask .|. altMask, xK_t), spawn myTerminal)
    , ((myModMask, xK_e), spawn "nautilus --new-window")
    , ((myModMask .|. shiftMask, xK_b), windowPromptBring defaultXPConfig)
    , ((myModMask .|. shiftMask, xK_g), windowPromptGoto defaultXPConfig)
  ]

{-
  Main configuration
-}
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    manageHook = manageDocks <+> manageHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "#abc" "" . shorten 70
        , ppCurrent = xmobarColor "#e6744c" "" . wrap "[" "]"
        , ppVisible = xmobarColor "#c185a7" "" . wrap "(" ")"
        , ppUrgent = xmobarColor "#c00" "" . wrap "{" "}"
        , ppHidden = xmobarColor "#ccc" ""
        , ppHiddenNoWindows = xmobarColor "#333" ""
      }
    , modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , layoutHook = avoidStruts $ myLayouts
    , startupHook = do
        setWMName "LG3D"
        spawn "~/.xmonad/startup-hook"
  } `additionalKeys` myKeyBindings

-- Alerts for sound settings
alertDouble = alert . show . bound . round
  where bound x | x < 0     = 0
                | x > 100   = 100
                | otherwise = x
alert = dzenConfig centered
  where centered = onCurr (center 150 66)
                    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
                    >=> addArgs ["-fg", "#ffffff"]
                    >=> addArgs ["-bg", "#000040"]
