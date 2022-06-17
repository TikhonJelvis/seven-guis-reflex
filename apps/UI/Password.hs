-- | A wrapper type for passwords that has safe 'Show' and 'Display'
-- instance.
module UI.Password where

import           Data.Hashable           (Hashable)
import           Data.Text               (Text)
import           Data.Text.Display       (Display, ShowInstance (..))

import           GHC.Generics            (Generic)

import           UI.Attributes.Attribute (AsAttributeValue)

-- | A password.
--
-- The 'Show' and 'Display' instances are designed not to leak
-- information about the password itself.
--
-- >>> show (Password "hunter1")
-- "[**password**]"
--
-- >>> import Data.Text.Display (display)
-- >>> display (Password "hunter1")
-- "[**password**]"
newtype Password = Password Text
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving newtype (AsAttributeValue)
  deriving Display via ShowInstance Password

instance Show Password where
  show _ = "[**password**]"
