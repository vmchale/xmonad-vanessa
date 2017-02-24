-- | Module to control screen brightness within the `X` Monad.
module XMonad.Util.Brightness (brighten) where

import XMonad

-- | Brighten screen
-- also consider splitting off once this has been generalized
brighten :: Int -> X ()
brighten n = liftIO $ do
    currentBacklight <- readFile "/sys/class/backlight/intel_backlight/brightness"
    length currentBacklight `seq` (writeFile "/sys/class/backlight/intel_backlight/brightness" (show . guard $ (read currentBacklight :: Int) + n))

-- | Don't let the brightness be obnoxious
guard :: Int -> Int
guard = (gu 2000) . (gl 0)
    where gu m n = if n > m then m else n
          gl m n = if n < m then m else n
