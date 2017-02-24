-- | Utils to use to control volume via amixer
module XMonad.Util.Volume where

import XMonad

-- | Generate action in the X monad to 
vol :: String -> X ()
vol = spawn . ((++) "amixer -D pulse sset Master ")

-- | Mute/unmute within the `X` Monad.
toggleMute :: X ()
toggleMute = vol "toggle"

-- | Raise volume
raiseVolume :: (Integral a, Show a) => a -> X ()
raiseVolume n = vol $ (show n) ++ "%+"

-- | Lower volume
lowerVolume :: (Integral a, Show a) => a -> X ()
lowerVolume n = vol $ (show n) ++ "%-"
