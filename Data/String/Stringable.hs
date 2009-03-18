{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.String.Stringable
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   :  Stable
-- Portability :  Requires TypeSynonymInstances
--
-----------------------------------------------------------------------------

module Data.String.Stringable (Stringable(..)) where


import Data.String

import qualified Data.ByteString.Char8 as B
import qualified Data.DString          as DS
import qualified Data.Text             as T
import qualified Text.PrettyPrint      as PP


-- | Class of types that can be converted to and from a String.
class IsString s => Stringable s where
    toString :: s -> String


instance Stringable String where
    toString = id

instance Stringable B.ByteString where
    toString = B.unpack

instance Stringable PP.Doc where
    toString = PP.render

instance Stringable ShowS where
    toString s = s []

instance Stringable DS.DString where
    toString = DS.toString

instance Stringable T.Text where
    toString = T.unpack


instance IsString ShowS where
    fromString = showString

instance IsString PP.Doc where
    fromString = PP.text
