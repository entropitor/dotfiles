import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
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
import qualified XMonad.StackSet as W
import System.IO

-- general config
altMask = mod1Mask
myModMask = mod4Mask
myTerminal = "gnome-terminal"


-- My workspaces
myWorkspaces = clickable [
  "WF", "Pin", "Chr",
  "Trm", "Dev", "Xtr",
  "Hng", "Ml", "Slk"
  ]
  where clickable l = ["<action=xdotool key super+" ++ show i ++ ">" ++ show i ++ ":" ++ ws ++ "</action>" | (i,ws) <- zip [1..] l]
[ wsWorkflowy, wsPinned, wsChrome,
  wsTerminal, wsMain, wsExtra,
  wsHangouts, wsMail, wsSlack ] = myWorkspaces
startupWorkspace = wsMain

-- My Layouts
defaultLayouts = tiled ||| simpleTabbedBottom ||| noBorders Full ||| Grid ||| Mirror tiled
  where tiled = Tall 1 (3/100) (1/2)
myLayouts =
  onWorkspace wsMain (Full ||| defaultLayouts)
  $ onWorkspaces [wsMail, wsSlack] (simpleTabbedBottom ||| defaultLayouts)
  $ defaultLayouts
-- myLayouts = defaultLayouts

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
    , ((myModMask, xK_x), spawn "nautilus --new-window")
    , ((myModMask, xK_c), kill)
    , ((myModMask .|. shiftMask, xK_b), windowPromptBring defaultXPConfig)
    , ((myModMask .|. shiftMask, xK_g), windowPromptGoto defaultXPConfig)
    -- CycleWS keys
    , ((myModMask,               xK_Right), nextWS)
    , ((myModMask,               xK_Left),  prevWS)
    , ((myModMask .|. shiftMask, xK_Right), shiftToNext)
    , ((myModMask .|. shiftMask, xK_Left),  shiftToPrev)
    , ((myModMask,               xK_Up),    nextScreen)
    , ((myModMask,               xK_Down),  prevScreen)
    , ((myModMask .|. shiftMask, xK_Up),    shiftNextScreen)
    , ((myModMask .|. shiftMask, xK_Down),  shiftPrevScreen)
    , ((myModMask,               xK_z),     toggleWS)
    , ((myModMask,               xK_e),     moveTo Next EmptyWS)
    , ((myModMask .|. shiftMask, xK_e),     moveTo Prev EmptyWS)
    , ((myModMask,               xK_s),     swapNextScreen)
  ]

-- My Management hooks
myManagementHooks = [
  className =? "Slack" --> doF (W.shift wsSlack)
  , className =? "Nylas N1" --> doF (W.shift wsMail)
  , className =? "Thunderbird" --> doF (W.shift wsMail)
  , resource =? "crx_knipolnnllmklapflnccelgolnpehhpl" --> doF (W.shift wsHangouts) -- Hangouts
  , resource =? "crx_koegeopamaoljbmhnfjbclbocehhgmkm" --> doF (W.shift wsHangouts) -- Workflowy
  , className =? "jetbrains-rubymine" --> doF (W.shift wsMain)
  , className =? "jetbrains-webstorm" --> doF (W.shift wsMain)
  , className =? "jetbrains-studio" --> doF (W.shift wsMain)
  , resource =? "vstudio" --> doF (W.shift wsExtra)
  , title =? "SuperGenPass for Google Chrome™ by Denis" --> doFloat
  ]

-- Main configuration
main = do
  xmproc <- spawnPipe "LANG=en_US.UTF-8 xmobar ~/.xmonad/xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    logHook = dynamicLogWithPP xmobarPP {
        ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "#90b7bb" "" . shorten 100
        , ppCurrent = xmobarColor "#e6744c" "" . wrap "[" "]"
        , ppVisible = xmobarColor "#c185a7" "" . wrap "(" ")"
        , ppUrgent = xmobarColor "#c00" "" . wrap "{" "}"
        , ppHidden = xmobarColor "#ccc" ""
        , ppHiddenNoWindows = xmobarColor "#555" ""
      }
    , modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , layoutHook = (avoidStruts {-. showWName-}) myLayouts
    , manageHook = manageDocks
        <+> manageHook defaultConfig
        <+> composeAll myManagementHooks
    , startupHook = do
        setWMName "LG3D"
        -- windows $ W.greedyView startupWorkspace
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