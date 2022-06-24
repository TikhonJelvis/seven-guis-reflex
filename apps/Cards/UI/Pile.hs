-- | Piles of cards that we can drag and drop onto.
module Cards.UI.Pile where

import           Control.Monad                     (void)

import           Data.Default.Class                (def)

import           Linear                            (V2 (..))

import           Prelude                           hiding (div)

import           UI.Attributes                     (class_)
import           UI.Attributes.AttributeSet.Reflex (AttributeSet, (=:))
import           UI.Element                        (Dom)
import           UI.Html                           (Html, div_)
import           UI.Main                           (Runnable (..), withCss)
import           UI.Svg                            (Def (..), ViewBox (..),
                                                    defs, path, pattern_, rect,
                                                    svg)
import           UI.Svg.Attributes                 (d, fill, height, paintWith,
                                                    viewBox, width, x, y)
import           UI.Svg.Haskell                    (HaskellPaths (..),
                                                    haskellPaths)

demo :: forall m t. Dom t m => m ()
demo = void pile

-- | A potentially empty pile that we can drop cards onto.
pile :: forall m t. Dom t m => m (Html t)
pile = fst <$> div_ [class_ =: ["pile"]] do
  svg [] do
    defs [Def "background" background
           [ viewBox =: ViewBox (V2 4 0) (V2 8 14)
           , width   =: "3.5%"
           , height  =: "3.7%"
           ]
         ]
    rect [ x      =: "0"
         , y      =: "0"
         , width  =: "100"
         , height =: "100"
         , fill   =: paintWith "background"
         ]
      -- TODO: 100 ⇒ 100%
      -- TODO: stroke =: "none"
  where background :: AttributeSet t "pattern" "SVG" -> m ()
        background attributes = void $ pattern_ attributes do
          let HaskellPaths { lambda } = haskellPaths def
          path [ d =: lambda, fill =: "#aaa" ]
            -- TODO: stroke =: "none"

main :: IO ()
main = withCss "css/card-demo.css" (Runnable demo)
