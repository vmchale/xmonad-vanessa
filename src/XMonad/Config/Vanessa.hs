-- | Configuration with defaults I like and brightness adjustable for my computer and appropriate 
module XMonad.Config.Vanessa (vConfig) where

--XMonad modules
import XMonad hiding (workspaces)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spiral
import XMonad.Layout.Reflect
import XMonad.Util.Run
import XMonad.Util.Volume
import XMonad.Util.Brightness
import XMonad.Util.Keyboard
import XMonad.Util.MediaKeys
import XMonad.Util.NamedWindows
import XMonad.StackSet hiding (filter)
--Monads etc.
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Control.Monad hiding (guard)
import Control.Monad.IO.Class
--
import Control.Concurrent

-- | IO action of the whole thing
vConfig :: IO ()
vConfig = xmonad . config =<< spawnPipe "xmobar"
    where config = myConfig

-- | Custom configuration taking in one pipe to xmobar
myConfig xmproc = def { terminal   = "alacritty"
                      , keys       = newKeys
                      , layoutHook = myLayout
                      , logHook    = (vLogHook xmproc)
                      , manageHook = myManageHook }

-- | get the current music playing (assumed to be in number 5)
musicString :: X String
musicString = do
    winset <- gets windowset
    let p = (== "5") . fst
    wt <- maybe (pure "") (fmap show . getName) . (fmap snd) . listToMaybe . filter p $ zip (((map tag . workspaces)) winset) (allWindows winset) 
    pure . (xmobarColor "green" "black") . take 40 $ wt

-- | Provides custom hooks to xmonad. This disables printing the window title/connects xmobar and xmonad.
vLogHook xmproc = musicString >>= \m ->  dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc
                                                                   , ppTitle = const m
                                                                   , ppLayout = const ""
                                                                   , ppHiddenNoWindows = id
                                                                   , ppHidden = (xmobarColor "darkorange" "black") -- . (\x -> if x == "5" then "Spotify" else x)
                                                                   , ppVisible = (xmobarColor "yellow" "black")
                                                                   }
 
-- | Doesn't work but I'm trying so hey. 
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll [ className =? "Gimp"                                --> doFloat
                          , resource =? "spotify"                              --> doF (shift "5")
                          , resource =? "google-chrome"                        --> doF (shift "2")
                          , resource =? "crx_bikioccmkafdpakkkcpdbppfkghcmihk" --> doF (shift "7")
                          , resource =? "crx_bgkodfmeijboinjdegggmkbkjfiagaan" --> doF (shift "7")
                          , className =? "libreoffice-writer" --> doFloat
                          ]

-- | Custom keymaps to adjust volume, brightness, and 
myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = mediaKeys . M.fromList $
             [ --volume control
               ((modm, xK_Up), raiseVolume 5)
             , ((modm, xK_Down), lowerVolume 5)
             , ((modm, xK_F8), toggleMute)
             --personal (extra) media keys
             , ((modm, xK_End), audioNext)
             , ((modm, xK_Page_Down), audioPrev)
             , ((modm, xK_Home), audioPlayPause)
             --brightness
             , ((modm, xK_Left), brighten (-100))
             , ((modm, xK_Right), brighten 100)
             --program shortcuts
             , ((modm, xK_q), spawn "spotify") 
             , ((modm .|. shiftMask, xK_n), spawn "google-chrome --profile-directory=Default --app-id=bikioccmkafdpakkkcpdbppfkghcmihk") --open signal
             --launch bar
             , ((modm, xK_p), spawn "$(yeganesh -x)")
             --screenshots
             , ((0, xK_Print), spawn "cd ~/.screenshots && scrot")
             --shutdown etc.
             , ((modm .|. shiftMask, xK_End), spawn "shutdown now")
             --switch keyboards
             , ((modm, xK_F1), setLang def)
             , ((modm, xK_F2), setLang tibetan)
             , ((modm, xK_F3), setLang accented)
             , ((shiftMask, xK_F11), spawn "cargo install --git https://github.com/jwilm/alacritty.git --force")
             ]
             --alt + h and alt+l should go over by one?
             --idea: "browse" workspaces but multiple of them/autospawn in a new one?
             --overambition:vim-like window management

-- | Function giving keybindings to undo
keysToRemove :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keysToRemove x@(XConfig {XMonad.modMask = modm}) = M.fromList
        [ ((modm, xK_p), pure ()) ]

-- | Gives a better ratio for the master pane and lets us spiral windows
myLayout = avoidStruts $ normalPanes ||| reflectHoriz normalPanes ||| Full ||| spiral (16/9)
    where normalPanes = Tall 1 (3/100) (3/7)

-- | Make new key layout from a given keyboard layout
newKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
newKeys x = myKeys x `M.union` (keys def x `M.difference` keysToRemove x)
