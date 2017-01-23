-- | Configuration with defaults I like and brightness adjustable for my computer
module XMonad.Config.Vanessa (vConfig) where

--XMonad modules
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spiral
import XMonad.Layout.Reflect
import XMonad.Util.Run
import XMonad.Util.Volume
import XMonad.Util.Brightness
import XMonad.Util.Keyboard
import XMonad.StackSet
--Monads etc.
import qualified Data.Map as M
import Data.Monoid
import Control.Monad hiding (guard)
import Control.Monad.IO.Class

-- | IO action of the whole thing
vConfig :: IO ()
vConfig = xmonad . config =<< spawnPipe "xmobar"
    where config = myConfig

-- | Custom configuration taking in one pipe to xmobar
myConfig xmproc = def { terminal   = "gnome-terminal"
                      , keys       = newKeys
                      , layoutHook = myLayout
                      , logHook    = (vLogHook xmproc)
                      , manageHook = myManageHook }

-- | Provides custom hooks to xmonad. This disables printing the window title/connects xmobar and xmonad.
vLogHook xmproc = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc
                                            , ppTitle = const ""
                                            , ppLayout = const ""
                                            , ppHiddenNoWindows = id
                                            , ppHidden = (xmobarColor "darkorange" "black")
                                            , ppVisible = (xmobarColor "yellow" "black")
                                            }
 
-- | Doesn't work but I'm trying so hey. 
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll [ resource =? "gimp"          --> doFloat
                          , resource =? "spotify"       --> doF (shift "5")
                          , resource =? "google-chrome" --> doF (shift "2")
                          ]

-- | Custom keymaps to adjust volume, brightness, and 
myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
             [ ((modm, xK_Up), raiseVolume 5)
             , ((modm, xK_Down), lowerVolume 5)
             , ((modm, xK_Left), brighten (-100))
             , ((modm, xK_Right), brighten 100)
             , ((modm, xK_F8), toggleMute)
             , ((modm, xK_p), spawn "yeganesh -x")
             , ((modm .|. shiftMask, xK_End), spawn "shutdown now")
             , ((modm, xK_F12), spawn "cd ~/.screenshots && scrot")
             , ((modm, xK_F1), setLang def)
             , ((modm, xK_F2), setLang tibetan)
             , ((modm, xK_F3), setLang accented)
             ]
             --alt + h and alt+l should go over by one?
             --idea: "browse" workspaces but multiple of them/autospawn in a new one?
             --overambition:vim-like window management


-- | Function giving keybindings to undo
keysToRemove :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keysToRemove x@(XConfig {XMonad.modMask = modm}) = M.fromList
        [ ((modm , xK_p ), return ())
        ]

-- | Gives a better ratio for the master pane and lets us spiral windows
myLayout = avoidStruts $ normalPanes ||| reflectHoriz normalPanes ||| Full ||| spiral (16/9)
    where normalPanes = Tall 1 (3/100) (3/7)

-- | Make new key layout from a given keyboard layout
newKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
newKeys x = myKeys x `M.union` (keys def x `M.difference` keysToRemove x)
