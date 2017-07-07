import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-------------------------------------------------
-- Configuration

myTerminal = "termite"
-- Sleep for 1 sec so that xset can really shut down the screen
myScreensaver = "xscreensaver-command -lock; sleep 1; xset dpms force off"
mySelectScreenshot = "sleep 0.2; " ++ myScreenshot ++ " -s"
myScreenshot = "scrot -e 'mv $f ~/Screenshots/'"
myLauncher = "$(yeganesh -x -- -fn 'Source Code Pro-11')"
myModMask = mod4Mask

-------------------------------------------------

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ desktopConfig
        { manageHook = manageDocks <+> manageHook desktopConfig
        , terminal = myTerminal
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 48
                        }
        , modMask = myModMask   
        } `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawn myScreensaver)
        , ((controlMask, xK_Print), spawn mySelectScreenshot)
        , ((myModMask, xK_p), spawn myLauncher)
        , ((0, xK_Print), spawn myScreenshot)
        ]
