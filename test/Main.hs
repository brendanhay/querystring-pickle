{-# LANGUAGE DeriveGeneric             #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Control.Applicative
import Data.ByteString                      (ByteString)
import Data.List                            (stripPrefix)
import Data.Maybe
import Data.String
import GHC.Generics
import Network.HTTP.QueryString.Pickle
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

main :: IO ()
main = defaultMain
    [ testGroup "Isomorphisms"
        [ testProperty "Flat"    (query :: Iso Foo)
        , testProperty "Nested"  (query :: Iso Bar)
        , testProperty "Nullary" (query :: Iso Qux)
        , testProperty "Maybe"   (query :: Iso Baz)
        , testProperty "Complex" (query :: Iso Waldo)
        , testProperty "List"    (query :: Iso Wibble)
        ]
    , testGroup "Generic Option Modifiers"
        [ testProperty "Constructors" (query :: Iso Fred)
        , testProperty "Fields"       (query :: Iso Plugh)
        ]
    ]

instance IsQuery a => IsQuery (Maybe a) where
    queryPickler = qpOption queryPickler

instance (IsQuery a, IsQuery b) => IsQuery (Either a b) where
    queryPickler = queryPickler `qpEither` queryPickler

instance IsQuery a => IsQuery [a] where
    queryPickler = qpDefault [] $ qpList queryPickler

data Foo = Foo
    { fooInt        :: Int
    , fooByteString :: ByteString
    } deriving (Eq, Show, Generic)

instance IsQuery Foo

instance Arbitrary Foo where
    arbitrary = Foo <$> arbitrary <*> arbitrary

data Bar = Bar
    { barInt     :: Int
    , barInteger :: Integer
    , barFoo     :: Foo
    } deriving (Eq, Show, Generic)

instance IsQuery Bar

instance Arbitrary Bar where
    arbitrary = Bar <$> arbitrary <*> arbitrary <*> arbitrary

data Qux = Quux | Corge | Grault
    deriving (Eq, Read, Show)

instance IsQuery Qux where
    queryPickler = qpPrim

instance Arbitrary Qux where
    arbitrary = elements [Quux, Corge, Grault]

data Baz = Baz
    { bazFoo :: Maybe Foo
    , bazInt :: Int
    } deriving (Eq, Show, Generic)

instance IsQuery Baz

instance Arbitrary Baz where
    arbitrary = Baz <$> arbitrary <*> arbitrary

data Waldo = Waldo
    { waldoBaz   :: Baz
    , waldoMaybe :: Maybe Foo
    } deriving (Eq, Show, Generic)

instance IsQuery Waldo

instance Arbitrary Waldo where
    arbitrary = Waldo
        <$> arbitrary
        <*> arbitrary

data Wibble = Wibble
    { wibList :: [Int]
    } deriving (Eq, Show, Generic)

instance IsQuery Wibble

instance Arbitrary Wibble where
    arbitrary = Wibble <$> arbitrary

data Fred = PrefixXyzzy | PrefixThud
    deriving (Eq, Show, Generic)

instance IsQuery Fred where
    queryPickler = genericQueryPickler $ defaultOptions
        { constructorTagModifier = \s -> fromMaybe s $ stripPrefix "Prefix" s
        }

instance Arbitrary Fred where
    arbitrary = elements [PrefixXyzzy, PrefixThud]

data Plugh = Plugh
    { thisPrefixInt :: Int
    , thisPrefixFoo :: Foo
    } deriving (Eq, Show, Generic)

instance IsQuery Plugh where
    queryPickler = genericQueryPickler $ defaultOptions
        { fieldLabelModifier = reverse
        }

instance Arbitrary Plugh where
    arbitrary = Plugh <$> arbitrary <*> arbitrary

instance Arbitrary ByteString where
    arbitrary = do
        NonEmpty s <- arbitrary
        return $ fromString s

type Iso a = Isomorphism a -> Bool

data Isomorphism a = Iso
    { domain   :: a
    , codomain :: [(ByteString, ByteString)]
    , identity :: Either String a
    } deriving (Show)

instance (Eq a, Arbitrary a, IsQuery a) => Arbitrary (Isomorphism a) where
    arbitrary = do
        i <- arbitrary
        return $ Iso i (toQuery i) (fromQuery $ toQuery i)

query :: (Eq a, Arbitrary a, IsQuery a) => Isomorphism a -> Bool
query (Iso d _ i) = either (const False) (== d) i
