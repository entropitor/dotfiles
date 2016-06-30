import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"

  xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
    , modMask = mod4Mask   -- Rebind Mod to the Windows key
    } `additionalKeys`
    [
      ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s"),
      ((0, xK_Print), spawn "scrot")
      ,
      -- http://superuser.com/questions/389737/how-do-you-make-volume-keys-and-mute-key-work-in-xmonad
      ((0, 0x1008FF11), spawn "amixer set Master 2-"),
      ((0, 0x1008FF13), spawn "amixer set Master 2+")
      -- ,
      -- ((0, 0x1008FF12), spawn "amixer set Master toggle")
    ]
