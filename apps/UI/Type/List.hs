{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Basic functionality for working with type-level lists.
module UI.Type.List where

import           Data.Data          (Proxy (..))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Type.Bool
import           Data.Type.Equality

import           GHC.TypeLits       (AppendSymbol, KnownSymbol, Symbol,
                                     symbolVal)

-- * Known Symbols

-- | A list version of 'KnownSymbol'.
--
-- 'knownSymbols' has an ambiguous type variable, so you need to use
-- it with type applications:
--
-- >>> knownSymbols @["a", "b", "c"]
-- ["a","b","c"]
class KnownSymbols (symbols :: [Symbol]) where
  -- | Given a type-level list of symbols, return the corresponding
  -- value-level strings.
  --
  -- >>> knownSymbols @["a", "b", "c"]
  -- ["a","b","c"]
  knownSymbols :: [Text]

instance KnownSymbols '[] where
  knownSymbols = []

instance (KnownSymbol element, KnownSymbols elements) =>
         KnownSymbols (element ': elements) where
  knownSymbols = Text.pack (symbolVal (Proxy @element)) : knownSymbols @elements

-- * List Operations

-- | Is a type contained in a type-level list?
--
-- Type-level version of 'elem'.
type family Elem (element :: Symbol) (supports :: [Symbol]) :: Bool where
  Elem _ '[]       = 'False
  Elem e (x ': xs) = e == x || Elem e xs

-- | Turn a type-level list of symbols (@["a", "b", "c"]@) into a
-- single symbol (@"[a, b, c]"@).
type family ShowList (xs :: [Symbol]) :: Symbol where
  ShowList xs = AppendSymbol "[" (AppendSymbol (ShowElements xs) "]")

-- | Turn a type-level list of symbols (@["a", "b", "c"]@) into a
-- single symbol (@"a, b, c"@).
type family ShowElements (xs :: [Symbol]) :: Symbol where
  ShowElements '[] = ""
  ShowElements '[x] = x
  ShowElements (x ': xs) = AppendSymbol (AppendSymbol x ", ") (ShowElements xs)
