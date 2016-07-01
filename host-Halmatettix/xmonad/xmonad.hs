import XMonad
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Dzen
import System.IO

altMask = mod1Mask

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  let config = defaultConfig {
      manageHook = manageDocks <+> manageHook defaultConfig
      , layoutHook = avoidStruts  $  layoutHook defaultConfig
      , logHook = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "green" "" . shorten 50
        }
      , modMask = mod4Mask   -- Rebind Mod to the Windows key
      , terminal = "gnome-terminal"
      , startupHook = do
          setWMName "LG3D"
          spawn "~/.xmonad/startup-hook"
    }

  xmonad $ config `additionalKeys`
    [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
      , ((0, xK_Print), spawn "scrot")
      -- http://superuser.com/questions/389737/how-do-you-make-volume-keys-and-mute-key-work-in-xmonad
      -- http://dmwit.com/volume/
      , ((0, 0x1008FF11), lowerVolume 5 >>= alertDouble)
      , ((0, 0x1008FF13), raiseVolume 5 >>= alertDouble)
      , ((0, 0x1008FF12), toggleMuteChannels ["Master", "Speaker", "Headphone", "Headphone 1"] >>= \muted -> alert (if muted then "off" else "on"))
      -- old terminal shortcut
      , ((controlMask .|. altMask, xK_t), spawn $ terminal config)
    ]

alertDouble = alert . show . bound . round
  where bound x | x < 0     = 0
                | x > 100   = 100
                | otherwise = x
alert = dzenConfig centered
  where centered = onCurr (center 150 66)
                    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
                    >=> addArgs ["-fg", "#ffffff"]
                    >=> addArgs ["-bg", "#000040"]

