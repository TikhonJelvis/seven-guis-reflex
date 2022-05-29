{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Values that can be viewed as a raw JavaScript @Element@ object.
module UI.IsElement where

import qualified GHCJS.DOM.Types as GHCJS

-- | A value that can be viewed as a raw JavaScript @Element@ object.
class IsElement e where
  rawElement :: e -> GHCJS.Element

instance {-# OVERLAPPABLE #-} GHCJS.IsElement e => IsElement e where
  rawElement = GHCJS.toElement
