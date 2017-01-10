module Config (vConfig) where

import XMonad
import XMonad.Hooks.DynamicLog
import Control.Monad
import qualified Data.Map as M

vConfig = xmonad config -- =<< xmobar config
    where config = myConfig

myConfig = def { terminal = "gnome-terminal" , keys = newKeys }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
             [ ((modm, xK_Up), raiseVolume 5)
             , ((modm, xK_Down), lowerVolume 5)
             , ((modm, xK_F8), toggleMute)
             , ((modm, xK_F11), spawn "xmessage 'test succesful!'")
             , ((modm, xK_F12), spawn "cd ~/.screenshots && scrot")
             ]
toggleMute = vol $ "toggle"
raiseVolume n = vol $ (show n) ++ "%+"
lowerVolume n = vol $ (show n) ++ "%-"

vol = spawn . ((++) "amixer -D pulse sset Master ")

newKeys x = myKeys x `M.union` keys def x
