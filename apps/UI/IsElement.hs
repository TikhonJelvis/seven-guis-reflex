{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Values that can be viewed as a raw JavaScript @Element@ object.
module UI.IsElement where

import           Data.Kind       (Type)

import qualified GHCJS.DOM.Types as GHCJS

import qualified Reflex.Dom      as Dom

-- | A value that can be viewed as a raw JavaScript @Element@ object.
class IsElement e where
  rawElement :: e -> GHCJS.Element

instance {-# OVERLAPPABLE #-} GHCJS.IsElement e => IsElement e where
  rawElement = GHCJS.toElement

-- | A value that can be viewed as a raw JavaScript @HTMLElement@ object.
class IsHtml e where
  rawHtml :: e -> GHCJS.HTMLElement

instance {-# OVERLAPPABLE #-} GHCJS.IsHTMLElement e => IsHtml e where
  rawHtml = GHCJS.toHTMLElement

-- | Values that can be created from an underlying 'Dom.Element'
class FromElement e where
  type EventResult e :: Dom.EventTag -> Type
  fromElement :: Dom.Element (EventResult e) Dom.GhcjsDomSpace t -> e t

-- | A value that can be viewed as a raw JavaScript @HTMLInputElement@ object.
class IsHtmlInput e where
  rawHtmlInput :: e -> GHCJS.HTMLInputElement

instance IsHtmlInput GHCJS.HTMLInputElement where
  rawHtmlInput = id
