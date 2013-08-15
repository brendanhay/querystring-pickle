{-# LANGUAGE DeriveGeneric #-}

-- |
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
import Data.String
import GHC.Generics
import Network.HTTP.QueryString.Pickle
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = defaultMain
    [ testProperty "Record" (query :: Qry Record)
    ]

data Record = Record
    { recordField1 :: Int
    , recordField2 :: ByteString
    } deriving (Eq, Show, Generic)

instance IsQuery Record

instance Arbitrary Record where
    arbitrary = Record <$> arbitrary <*> arbitrary

instance Arbitrary ByteString where
    arbitrary = do
        NonEmpty s <- arbitrary
        return $ fromString s

type Qry a = TestQuery a -> Bool

data TestQuery a = TestQuery
    { input   :: a
    , interim :: [(ByteString, ByteString)]
    , output  :: Either String a
    } deriving (Show)

instance (Eq a, Arbitrary a, IsQuery a) => Arbitrary (TestQuery a) where
    arbitrary = do
        i <- arbitrary
        return $ TestQuery i (toQueryString i) (fromQueryString $ toQueryString i)

query :: (Eq a, Arbitrary a, IsQuery a) => TestQuery a -> Bool
query (TestQuery i m o) = either (const False) (== i) o
