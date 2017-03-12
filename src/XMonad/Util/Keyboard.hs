-- | Allows to switch keyboard layouts 
-- Uses the command-line utility setxkbmap. 
module XMonad.Util.Keyboard ( KbLayout (..)
                            , showKBLayout
                            , parseKB
                            , setLang
                            , tibetan
                            , accented
                            , français
                            , deutsch
                            , dansk
                            , anglisc
                            , dzongkha
                            ) where

import XMonad
import XMonad.Hooks.DynamicLog
import System.Process hiding (spawn)

-- | Datatype for a keyboard layout.
data KbLayout = Simple String | Regional String String

instance Show KbLayout where
    show (Simple "us") = "US"
    show (Simple "layout") = "US"
    show (Regional "cn" "tib") = xmobarColor "yellow" "black" "Tibetan"
    show (Simple "bt") = xmobarColor "yellow" "black" "Dzongkha"
    show (Regional "us" "altgr-intl") = "US Extended"
    show (Simple "fr") = "Fr"
    show (Simple "is") = "Ænglisc"
    show (Simple "de") = "De"
    show (Simple "dk") = "Dk"

-- | Set default keyboard layout to vanilla "us"
instance Default KbLayout where
    def = Simple "us"

-- | executable to yield current layout
showKBLayout :: IO ()
showKBLayout = putStrLn =<< (show <$> parseKB)

-- | Get current keyboard layout
parseKB :: IO KbLayout
parseKB = do
    out <- lines <$> readCreateProcess (shell "setxkbmap -query") ""
    let strip i = (dropWhile (==' ')) . (drop 1) . (dropWhile (/=':')) . (!!i)
    let line = flip strip out
    if length out == 3 then
        pure (Simple (line 2))
    else
        pure (Regional (line 2) (line 3))

-- | keyboard layout for typing Tibetan text
tibetan :: KbLayout
tibetan  = (Regional "cn" "tib")

-- | AZERTY French keyboard
français :: KbLayout
français = Simple "fr"

-- | QWERTZ German keyboard
deutsch :: KbLayout
deutsch = Simple "de"

-- | Danish keyboard
dansk :: KbLayout
dansk = Simple "dk"

-- | Icelandic keyboard; also good for typesetting Old English. 
anglisc :: KbLayout
anglisc = Simple "is"

-- | Dzongkha keyboard
dzongkha :: KbLayout
dzongkha = Simple "bt"

-- | Alr-gr keyboard providing common accents on a US keyboard
accented :: KbLayout
accented = (Regional "us" "altgr-intl")

-- | Set keyboard layout
setLang :: KbLayout -> X ()
setLang (Simple lc) = spawn $ "setxkbmap " ++ lc
setLang (Regional lc r) = spawn $ "setxkbmap -layout " ++ lc ++ " -variant " ++ r
