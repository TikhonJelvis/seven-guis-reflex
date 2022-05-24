{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- | Rough/experimental bindings for reactive-banana to jsaddle-dom,
-- inspired by reflex-dom
module RB.UI.Dom where

import           Control.Lens                (iforM_)
import           Control.Monad               (forM_)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader (..), ReaderT)

import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Text                   (Text)

import qualified GHCJS.DOM.Document          as Dom
import qualified GHCJS.DOM.Element           as Dom
import qualified GHCJS.DOM.Node              as Dom

import           Language.Javascript.JSaddle (MonadJSM)

-- * DOM Builder

-- | A monad transformer that can interact with the DOM on a page.
newtype Dom m a = Dom { runDom :: ReaderT Dom.Document m a }
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadReader Dom.Document, MonadJSM, MonadIO)

-- | The global @document@ object for the page.
document :: Monad m => Dom m Dom.Document
document = ask
{-# INLINE document #-}

-- * Elements

-- | Create an element with the given tag, attributes and children.
--
-- Does /not/ insert the element into the page.
element :: MonadJSM m => Text -> Attributes -> [Dom.Element] -> Dom m Dom.Element
element tag attributes children = do
  d <- document
  e <- Dom.createElement d tag
  iforM_ attributes $ Dom.setAttribute e
  forM_ children $ Dom.appendChild_ e
  pure e
{-# INLINE element #-}

-- ** Attributes

type Attributes = HashMap Text Text
