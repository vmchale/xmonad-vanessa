module XMonad.Config.Vanessa (vConfig) where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spiral
import XMonad.Layout.Reflect
import Control.Monad hiding (guard)
import Control.Monad.IO.Class
import XMonad.Util.Run
import qualified Data.Map as M
import XMonad.Hooks.ManageDocks

vConfig :: IO ()
vConfig = xmonad . config =<< spawnPipe "xmobar"
    where config = myConfig

myConfig xmproc = def { terminal = "gnome-terminal" , keys = newKeys , layoutHook = myLayout , logHook = (vLogHook xmproc) }

-- | Provides custom hooks to xmonad. This disables printing the window title.
-- (Consider putting this in its own module)
vLogHook xmproc = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc
                                             , ppTitle = const ""
                                             , ppLayout = const ""
                                             , ppHiddenNoWindows = id
                                             , ppHidden = (xmobarColor "darkorange" "black")
                                             }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
             [ ((modm, xK_Up), raiseVolume 5)
             , ((modm, xK_Down), lowerVolume 5)
             , ((modm, xK_Left), brighten (-100))
             , ((modm, xK_Right), brighten 100)
             , ((modm, xK_F8), toggleMute)
             , ((modm .|. shiftMask, xK_End), spawn "shutdown now")
             , ((modm, xK_F11), spawn "xmessage 'test succesful!'")
             , ((modm, xK_F12), spawn "cd ~/.screenshots && scrot")
             ]
             --alt + h and alt+l should go over by one?
             --overambition:vim-like window management

--fix for xmobar
myLayout = avoidStruts $ normalPanes ||| reflectHoriz normalPanes ||| Full ||| spiral (16/9)
    where normalPanes       = Tall 1 (3/100) (3/7)

--split off into its own module
toggleMute = vol $ "toggle"
raiseVolume n = vol $ (show n) ++ "%+"
lowerVolume n = vol $ (show n) ++ "%-"

brighten :: Int -> X ()
brighten n = liftIO $ do
    currentBacklight <- readFile "/sys/class/backlight/intel_backlight/brightness"
    length currentBacklight `seq` (writeFile "/sys/class/backlight/intel_backlight/brightness" (show . guard $ (read currentBacklight :: Int) + n))

guard n = if n > 2000 then 2000 else n

-- use createProcess + liftIO?
vol = spawn . ((++) "amixer -D pulse sset Master ")

newKeys x = myKeys x `M.union` keys def x
