import Test.Hspec
import XMonad.Util.Keyboard
import XMonad hiding (liftX)
import Control.Monad

main = hspec $ do
    --describe "setLang" $ do
    --    it "sets current keyboard layout" $ do
    --        (liftX $ setLang (Simple "us")) >>= ((`shouldBe` ()))
    describe "parseKB" $ do
        it "gets current keyboard" $ do
            (show <$> parseKB) >>= (`shouldBe` "US")
