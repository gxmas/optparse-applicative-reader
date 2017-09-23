module Options.Applicative.Builder.Reader
  (
    -- | This module contains utility functions to create parsers
    -- for named value of the form 'name=value'.
    --

    NamedValue

    -- * Readers
  , namedValue
  ) where

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8 (pack)
import Options.Applicative.Builder (eitherReader)
import Options.Applicative.Builder.Reader.Internal (NamedValue, parseNamedValue)
import Options.Applicative.Types (ReadM)

-- |
namedValue :: ReadM NamedValue
namedValue = eitherReader (parseOnly parseNamedValue . pack)
{-# INLINABLE namedValue #-}
