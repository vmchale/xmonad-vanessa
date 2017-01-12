-- | Configuration with defaults I like and brightness adjustable for my computer
module XMonad.Config.Vanessa where --(vConfig) where

--XMonad modules
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spiral
import XMonad.Layout.Reflect
import XMonad.Util.Run
import XMonad.StackSet
import XMonad.Hooks.ManageDocks
--Monads etc. 
import qualified Data.Map as M
import Control.Monad hiding (guard)
import Control.Monad.IO.Class
--
import System.Process hiding (spawn)
--Parsers
--import Text.Megaparsec

-- | Datatype for a keyboard layout.
-- Consider spinning this off into its own module
data KbLayout = Simple String | Regional String String

-- | Set default keyboard layout to vanilla "us"
instance Default KbLayout where
    def = Simple "us"

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
-- (Consider putting this in its own module)
vLogHook xmproc = dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc
                                            , ppTitle = const ""
                                            , ppLayout = const ""
                                            , ppHiddenNoWindows = id
                                            , ppHidden = (xmobarColor "darkorange" "black")
                                            }
 
-- | Doesn't work by I'm trying so hey. 
myManageHook = composeAll [ resource =? "gimp"          --> doFloat
                          , resource =? "spotify"       --> doF (shift "5")
                          , resource =? "google-chrome" --> doF (shift "2")
                          ]

-- | Custom keymaps to adjust volume, brightness, and 
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
             [ ((modm, xK_Up), raiseVolume 5)
             , ((modm, xK_Down), lowerVolume 5)
             , ((modm, xK_Left), brighten (-100))
             , ((modm, xK_Right), brighten 100)
             , ((modm, xK_F8), toggleMute)
             , ((modm .|. shiftMask, xK_End), spawn "shutdown now")
             , ((modm, xK_F12), spawn "cd ~/.screenshots && scrot")
             , ((modm, xK_F1), setLang def)
             , ((modm, xK_F2), setLang tibetan)
             , ((modm, xK_F3), setLang accented)
             --, ((shiftMask, xK_F5), parseKB >>= spawn . ((++) "xmessage "))
             ]
             --alt + h and alt+l should go over by one?
             --idea: "browse" workspaces but multiple of them/autospawn in a new one?
             --overambition:vim-like window management

parseKB :: X String --KbLayout
parseKB = liftIO $ do
    out <- readCreateProcess (shell "setxkbmap -query") ""
    pure . (dropWhile (==' ')) . (takeWhile (/=':')) . (!!3) . lines $ out

tibetan  = (Regional "cn" "tib")
accented = (Regional "us" "altgr-intl")

--make this appropriately typed. 
setLang :: KbLayout -> X ()
setLang (Simple lc) = spawn $ "setxkbmap " ++ lc
setLang (Regional lc r) = spawn $ "setxkbmap -layout " ++ lc ++ " -variant " ++ r

-- | Gives a better ratio for the master pane and lets us spiral windows
myLayout = avoidStruts $ normalPanes ||| reflectHoriz normalPanes ||| Full ||| spiral (16/9)
    where normalPanes = Tall 1 (3/100) (3/7)

-- | split off into its own module? 
toggleMute = vol "toggle"
raiseVolume n = vol $ (show n) ++ "%+"
lowerVolume n = vol $ (show n) ++ "%-"

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

-- | Generate action in the X monad to 
-- use createProcess + liftIO?
vol :: String -> X ()
vol = spawn . ((++) "amixer -D pulse sset Master ")

newKeys x = myKeys x `M.union` keys def x
