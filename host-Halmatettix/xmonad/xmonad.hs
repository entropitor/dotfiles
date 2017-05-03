import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion
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

import XMonad.Layout.Spiral

-- general config
altMask = mod1Mask
myModMask = mod4Mask
myTerminal = "gnome-terminal"


-- My workspaces
myWorkspaces = clickable [
  "Pin",
  "Chr", "Main", "Trm",
  "Xtr", "Xtr", "Xtr",
  "Hng", "Mail", "Slk"
  ]
  -- where clickable l = ["<action=xdotool key super+" ++ show i ++ ">" ++ show i ++ ":" ++ ws ++ "</action>" | (i,ws) <- zip [1..] l]
  where clickable l = [show i ++ ":" ++ ws | (i,ws) <- zip [0..] l]
[ wsPinned,
  wsChrome, wsMain, wsTerminal,
  wsExtra1, wsExtra2, wsExtra3,
  wsHangouts, wsMail, wsSlack ] = myWorkspaces
myWorkspacesCorrectOrder = tail myWorkspaces ++ [head myWorkspaces]

-- My Layouts
defaultLayouts = tiled ||| simpleTabbedBottom ||| noBorders Full ||| Grid ||| Mirror tiled ||| Accordion ||| spiral (6/7)
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
    , ((myModMask, xK_o), spawn myTerminal)
    , ((myModMask, xK_x), spawn "nautilus --new-window")
    , ((myModMask, xK_i), spawn "google-chrome")
    , ((myModMask, xK_c), kill)
    , ((myModMask .|. shiftMask, xK_b), windowPromptBring defaultXPConfig)
    , ((myModMask .|. shiftMask, xK_g), windowPromptGoto defaultXPConfig)
    -- CycleWS keys
    , ((myModMask,               xK_Right), nextWS)
    , ((myModMask,               xK_Left),  prevWS)
    , ((myModMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
    , ((myModMask .|. shiftMask, xK_Left),  shiftToPrev >> prevWS)
    , ((myModMask,               xK_Up),    nextWS >> nextWS >> nextWS)
    , ((myModMask,               xK_Down),  prevWS >> prevWS >> prevWS)
    , ((myModMask .|. altMask,   xK_Up),    nextScreen)
    , ((myModMask .|. altMask,   xK_Down),  prevScreen)
    , ((myModMask .|. shiftMask, xK_Up),    shiftNextScreen)
    , ((myModMask .|. shiftMask, xK_Down),  shiftPrevScreen)
    , ((myModMask,               xK_z),     toggleWS)
    , ((myModMask,               xK_e),     moveTo Next EmptyWS)
    , ((myModMask .|. shiftMask, xK_e),     moveTo Prev EmptyWS)
    , ((myModMask,               xK_s),     swapNextScreen)
  ]
  ++
  [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_0 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (\i -> (W.greedyView i) . (W.shift i), shiftMask)]]

-- My Management hooks
myManagementHooks = [
  className =? "Slack" --> doF (W.shift wsSlack)
  , className =? "Nylas Mail" --> doF (W.shift wsMail)
  , className =? "Thunderbird" --> doF (W.shift wsMail)
  , resource =? "crx_knipolnnllmklapflnccelgolnpehhpl" --> doF (W.shift wsHangouts) -- Hangouts
  {-, resource =? "crx_koegeopamaoljbmhnfjbclbocehhgmkm" --> dof (w.shift wsworkflowy) -- workflowy-}
  , className =? "jetbrains-rubymine" --> doF (W.shift wsMain)
  , className =? "jetbrains-webstorm" --> doF (W.shift wsMain)
  , className =? "jetbrains-studio" --> doF (W.shift wsMain)
  , resource =? "emacs" --> doF (W.shift wsMain)
  , resource =? "vstudio" --> doF (W.shift wsExtra1)
  , resource =? "mendeleydesktop" --> doF (W.shift wsExtra1)
  -- floats
  , title =? "SuperGenPass for Google Chrome™ by Denis" --> doFloat
  , className =? "gitify" --> doFloat
  ]

myShowWNameConfig = defaultSWNConfig {
  swn_font = "-misc-fixed-*-*-*-*-40-*-*-*-*-*-*-*"
  , swn_bgcolor = "red"
  , swn_color   = "white"
  , swn_fade    = 1
}

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
    , workspaces = myWorkspacesCorrectOrder
    , layoutHook = (avoidStruts . (showWName' myShowWNameConfig)) myLayouts
    , manageHook = manageDocks
        <+> manageHook defaultConfig
        <+> composeAll myManagementHooks
    , startupHook = do
        setWMName "LG3D"
        -- windows $ W.greedyView wsMain
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
