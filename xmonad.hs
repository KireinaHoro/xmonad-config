import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Data.List(isInfixOf)

-------------------------------------------------
-- Configuration

myTerminal = "termite"
-- Sleep for 1 sec so that xset can really shut down the screen
myScreensaver = "xscreensaver-command -lock; sleep 1; xset dpms force off"
mySelectScreenshot = "sleep 0.2; " ++ myScreenshot ++ " -s"
myScreenshot = "scrot -e 'mv $f ~/Screenshots/'"
myLauncher = "$(yeganesh -x -- -fn 'Source Code Pro-11')"
myModMask = mod4Mask

myWorkspaces = ["T", "W", "I", "M", "5", "6", "7", "8", "9", "0", "-", "="]

myManageHook = composeAll . concat $
     [ [ className =? "Firefox"              --> doShift "W" ]
     , [ className =? "Termite"              --> doShift "T" ]
     , [ className =? "TelegramDesktop"      --> doShift "I" ]
     , [ className =? "Vlc"                  --> doShift "M" ]
     , [ className =? "netease-cloud-music"  --> doShift "M" ]
     , [ fmap ( "Directory index of /Anime/" `isInfixOf` ) title --> doShift "M" ] -- doesn't work for now as Firefox changes the title *after* the window was created
     , [(className =? "Firefox" <&&> resource =? "Places") --> doFloat]
     , [(className =? "Firefox" <&&> resource =? "Browser") --> doFloat]

       -- using list comprehensions and partial matches
     , [ className =? c --> doFloat | c <- myFloatsC ]
     , [ title     =? t --> doFloat | t <- myFloatsT ]
     , [ fmap ( c `isInfixOf` ) className   --> doFloat | c <- myMatchAnywhereFloatsC ]
     , [ fmap ( c `isInfixOf` ) title       --> doFloat | c <- myMatchAnywhereFloatsT ] 

     , [ manageDocks ]
     ]
    where myFloatsC = ["Pavucontrol", "feh", "Lxappearance", "Xscreensaver-demo"]
          myFloatsT = ["Cloud Music"]
          myMatchAnywhereFloatsC = ["Google"]
          myMatchAnywhereFloatsT = ["Directory index of /Anime/"] -- same as above

-------------------------------------------------

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ ewmh desktopConfig
        { manageHook = myManageHook <+> manageHook desktopConfig
        , layoutHook = smartBorders $ layoutHook desktopConfig
        , workspaces = myWorkspaces
        , terminal = myTerminal
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 45
                        }
        , modMask = myModMask   
        } `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawn myScreensaver)
        , ((controlMask, xK_Print), spawn mySelectScreenshot)
        , ((myModMask, xK_p), spawn myLauncher)
        , ((0, xK_Print), spawn myScreenshot)
        ]
