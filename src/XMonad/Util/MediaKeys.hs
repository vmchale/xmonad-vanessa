-- | Bind media keys using dbus
-- Requires amixer to control volume. 
module XMonad.Util.MediaKeys (-- * default keybindings
                             mediaKeys
                             -- * media control in the 'X' monad
                             , audioPrev
                             , audioNext
                             , audioPlayPause) where

--gives us media keys
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Types
--XMonad stuff
import XMonad.Core
import XMonad.Util.Volume
--monads and all that
import qualified Data.Map as M
import Data.Monoid

-- | Given your keymaps, add the media keybindings
mediaKeys :: M.Map (KeyMask, KeySym) (X ()) -> M.Map (KeyMask, KeySym) (X ())
mediaKeys = M.fromList . ((<>) mediaKeyList) . M.toList

mediaKeyList :: [((KeyMask, KeySym), X ())]
mediaKeyList = [ ((0, xF86XK_AudioNext), audioNext)
               , ((0, xF86XK_AudioPrev), audioPrev)
               , ((0, xF86XK_AudioPlay), audioPlayPause)
               , ((0, xF86XK_AudioMute), toggleMute)
               , ((0, xF86XK_AudioLowerVolume), lowerVolume 5) --module elsewhere? 
               , ((0, xF86XK_AudioRaiseVolume), raiseVolume 5) 
               ]

-- | Helper function for use with dbus
sp :: String -> X ()
sp = spawn . ((<>) "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.")

-- | Action in the 'X' monad to go to next
audioNext      = sp "Next"

-- | Action in the 'X' monad to go the previous
audioPrev      = sp "Previous"

-- | Action in the 'X' monad to play/pause
audioPlayPause = sp "PlayPause"
