-- | Piles of cards that we can drag and drop onto.
module Cards.UI.Pile where

import           Control.Monad      (void)

import           Data.Default.Class (def)
import           Data.Map           (Map)
import           Data.Text          (Text)

import           UI.Attributes      (with)
import           UI.Element         (Dom, Html, elClass')
import           UI.Main            (Runnable (..), withCss)
import           UI.SVG             (Rectangle (..), defs, path, pattern_, rect,
                                     svg')
import           UI.SVG.Attributes  (fill, paintWith, stroke)
import           UI.SVG.Haskell     (HaskellPaths (..), haskellPaths)

demo :: forall m t. Dom t m => m ()
demo = void pile

-- | A potentially empty pile that we can drop cards onto.
pile :: forall m t. Dom t m => m (Html t)
pile = fst <$> elClass' "div" "pile" do
  svg' "svg" do
    defs [("background", background)]
    rect (pure full) (pure $ stroke "none" <> fill (paintWith "background"))
  where full = Rectangle { x = "0", y = "0", width = "100%", height = "100%" }

        background attributes = pattern_ (with base <$> attributes) do
          let HaskellPaths { lambda } = haskellPaths def
          path (pure lambda) (pure $ fill "#aaa" <> stroke "none")
          where base :: Map Text Text
                base = [("viewBox", "4 0 8 14"), ("width", "3.5%"), ("height", "3.7%")]
main :: IO ()
main = withCss "css/card-demo.css" (Runnable demo)
