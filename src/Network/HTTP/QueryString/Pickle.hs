{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE DeriveGeneric                   #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE FunctionalDependencies          #-}
{-# LANGUAGE KindSignatures                  #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE OverlappingInstances            #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeOperators                   #-}
{-# LANGUAGE UndecidableInstances            #-}
{-# LANGUAGE ViewPatterns                    #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- Module      : Network.HTTP.QueryString.Pickle
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.HTTP.QueryString.Pickle
    (
    -- * Class
      IsQuery (..)

    -- * Functions
    , toQuery
    , fromQuery
    , encodeQuery
    , decodeQuery

    -- * Data Types
    , Query   (..)
    , PU      (..)

    -- * Options
    , Options (..)
    , defaultOptions
    , loweredOptions

    -- * Generics
    , genericQueryPickler

    -- * Combinators
    , qpWrap
    , qpElem
    , qpPair
    , qpLift
    , qpPrim
    , qpOption
    , qpDefault
    , qpSum
    , qpEither
    , qpOrdinalList
    , qpList
    ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isLower, toLower)
import           Data.Either
import           Data.Foldable         (foldl')
import           Data.List             (sort)
import           Data.Monoid
import           GHC.Generics

--
-- Types
--

-- | A type that has a pairing of pickler + unpickler.
--
-- Using the @DeriveGeneric@ language extension, this class specifies a
-- default generic implementation using 'genericQueryPickler'.
--
-- For example:
--
-- @{-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics
--
-- data Foo { fooIntX :: Int, fooIntY :: Int } deriving (Generic)
--
-- instance IsQuery Foo
-- @
--
-- Note that you can parameterise some of the options to 'genericQueryPickler'
-- by specifying an implementation instead of using @DefaultSignatures@.
--
-- The previous example:
--
-- @
-- instance IsQuery Foo where
--     queryPickler = 'genericQueryPickler' 'defaultOptions'
-- @
--
-- More examples of creating 'queryPickler' implementations can be found in the
-- @README@ or in the @tests@.
class IsQuery a where
    queryPickler :: PU a

    default queryPickler :: (Generic a, GIsQuery (Rep a)) => PU a
    queryPickler = genericQueryPickler defaultOptions

-- | Internal tree representation for queries.
data Query
    = List [Query]
    | Pair ByteString Query
    | Value ByteString
      deriving (Eq, Show)

instance Ord Query where
    compare (List  ls)  (List  rs)  = ls `compare` rs
    compare (Pair k1 _) (Pair k2 _) = k1 `compare` k2
    compare (Value v1)  (Value v2)  = v1 `compare` v2

    compare (List _)   (Pair _ _) = GT
    compare (List _)   (Value _)  = GT
    compare (Pair _ _) (Value _)  = GT

    compare _ _ = LT

instance Monoid Query where
    mempty                    = List []
    mappend (List l) (List r) = List $ l ++ r
    mappend (List l) r        = List $ r : l
    mappend l        (List r) = List $ l : r
    mappend l        r        = List [l, r]

-- | Pairing of pickler to unpickler.
data PU a = PU
    { pickle   :: a -> Query
    , unpickle :: Query -> Either String a
    }

-- | Options for 'genericQueryPickler' to parameterise how constructor and record
-- field labels are un/pickled.
--
-- For example:
--
-- @import GHC.Generics
--
-- data Bar { barThisIsAByteString :: ByteString } deriving (Generic)
--
-- instance IsQuery Foo where
--     queryPickler = 'genericQueryPickler' $ Options
--         { constructorTagModifier = id
--         , fieldLabelModifier     = dropWhile isLower
--         }
-- @
--
-- Would remove @bar@ from the record field @barThisIsAByteString@ so the resulting
-- pair for that field in the association list would be @(ThisIsAByteString, n :: Int)@.
--
-- The above example is how 'defaultOptions' behaves.
data Options = Options
    { constructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags.
    , fieldLabelModifier     :: String -> String
      -- ^ Function applied to record field labels.
    }

-- | Strips lowercase prefixes from record fields.
defaultOptions :: Options
defaultOptions = Options id (dropWhile isLower)

-- | Strips lowercase prefixes from record fields and subsequently lowercases
-- the remaining identifier.
loweredOptions :: Options
loweredOptions = defaultOptions
    { fieldLabelModifier = map toLower . dropWhile isLower
    }

--
-- Functions
--

-- | Pickle a data type with an 'IsQuery' instance to an association list.
toQuery :: IsQuery a => a -> [(ByteString, ByteString)]
toQuery = enc "" . pickle queryPickler
  where
    enc k (List qs) = concatMap (enc k) qs
    enc k (Value v) = [(k, v)]
    enc k (Pair k' q)
        | BS.null k = enc k' q
        | otherwise = enc (k <> "." <> k') q

-- | Unpickle an association list to an 'IsQuery' type, returning an error
-- message when unpickling fails.
fromQuery :: IsQuery a => [(ByteString, ByteString)] -> Either String a
fromQuery = unpickle queryPickler . foldl' (\a b -> reify b <> a) mempty
  where
    reify (k, v)
        | BS.null k       = Value v
        | '.' `BS.elem` k = let ks     = BS.split '.' k
                                f k' q = Pair k' q
                             in foldr f (Pair (last ks) $ Value v) $ init ks
        | otherwise       = Pair k $ Value v

-- | Helper to encode an association list as a single canonical query string.
encodeQuery :: (ByteString -> ByteString) -- ^ URL Value Encoder
            -> [(ByteString, ByteString)] -- ^ Key/Value Pairs
            -> ByteString
encodeQuery f = BS.intercalate "&" . map (\(k, v) -> mconcat [k, "=", f v]) . sort

-- | Helper to decode a query string to an association list.
decodeQuery :: (ByteString -> ByteString) -- ^ URL Value Decoder
            -> ByteString                 -- ^ Input Query String
            -> [(ByteString, ByteString)]
decodeQuery f = map (pair . BS.split '=')
    . BS.split '&'
    . BS.dropWhile (\c -> c == '/' || c == '?')
  where
    pair (k:vs) = (k, f $ BS.intercalate "=" vs)
    pair []     = ("", "")

--
-- Generics
--

genericQueryPickler opts =
    (to, from) `qpWrap` (gQueryPickler opts) (genericQueryPickler opts)

class GIsQuery f where
    gQueryPickler :: Options -> PU a -> PU (f a)

instance IsQuery a => GIsQuery (K1 i a) where
    -- Constants
    gQueryPickler _ _ = (K1, unK1) `qpWrap` queryPickler

instance GIsQuery U1 where
    -- Empty Constructors Parameters
    gQueryPickler _ _ = (const U1, const ()) `qpWrap` qpLift ()

instance GIsQuery a => GIsQuery (M1 i d a) where
    -- Discard Metadata
     gQueryPickler opts = qpWrap (M1, unM1) . gQueryPickler opts

instance CtorIsQuery a => GIsQuery (C1 c a) where
    -- Constructor Encoding
    gQueryPickler opts = qpWrap (M1, unM1) . ctorQueryPickler opts

instance ( AllNullary  (a :+: b) allNullary
         , NullIsQuery (a :+: b) allNullary
         ) => GIsQuery (a :+: b) where
    -- Nullary Constructors
    gQueryPickler opts =
        (unTagged :: Tagged allNullary (PU ((a :+: b) d)) -> (PU ((a :+: b) d)))
            . nullQueryPickler opts

--
-- Nullary
--

class NullIsQuery f allNullary where
    nullQueryPickler :: Options -> PU a -> Tagged allNullary (PU (f a))

instance SumIsQuery (a :+: b) => NullIsQuery (a :+: b) True where
    nullQueryPickler opts _ = Tagged $ sumQueryPickler opts

class SumIsQuery f where
    sumQueryPickler :: Options -> PU (f a)

instance (SumIsQuery a, SumIsQuery b) => SumIsQuery (a :+: b) where
    sumQueryPickler opts = sumQueryPickler opts `qpSum` sumQueryPickler opts

instance Constructor c => SumIsQuery (C1 c U1) where
    sumQueryPickler opts = PU
        { pickle   = const $ Value name
        , unpickle = valueExists
        }
      where
        name = BS.pack . constructorTagModifier opts $ conName (undefined :: t c U1 p)

        valueExists qry
            | (List [Value v]) <- qry, v == name = Right $ M1 U1
            | (Value v)        <- qry, v == name = Right $ M1 U1
            | otherwise = Left . BS.unpack $ "valueExists: failure - " <> name

--
-- Records
--

class CtorIsQuery f where
    ctorQueryPickler :: Options -> PU a -> PU (f a)

class CtorIsQuery' f isRecord where
    ctorQueryPickler' :: Options -> PU a -> Tagged isRecord (PU (f a))

instance (IsRecord f isRecord, CtorIsQuery' f isRecord) => CtorIsQuery f where
    ctorQueryPickler opts = (unTagged :: Tagged isRecord (PU (f a)) -> PU (f a))
        . ctorQueryPickler' opts

instance RecIsQuery f => CtorIsQuery' f True where
    ctorQueryPickler' opts = Tagged . recQueryPickler opts

class RecIsQuery f where
    recQueryPickler :: Options -> PU a -> PU (f a)

instance (RecIsQuery a, RecIsQuery b) => RecIsQuery (a :*: b) where
    recQueryPickler opts f = qpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (recQueryPickler opts f `qpPair` recQueryPickler opts f)

instance (Selector s, GIsQuery a) => RecIsQuery (S1 s a) where
    recQueryPickler opts f = qpElem
        (BS.pack . fieldLabelModifier opts $ selName (undefined :: S1 s f r))
        ((M1, unM1) `qpWrap` gQueryPickler opts f)

instance (Selector s, IsQuery a) => RecIsQuery (S1 s (K1 i (Maybe a))) where
    recQueryPickler opts _ =
        (M1 . K1, unK1 . unM1) `qpWrap` qpOption (qpElem name queryPickler)
      where
        name = BS.pack
            . fieldLabelModifier opts
            $ selName (undefined :: t s (K1 i (Maybe a)) p)

--
-- Tagging
--

class IsRecord (f :: * -> *) isRecord | f -> isRecord

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord U1 False

class AllNullary (f :: * -> *) allNullary | f -> allNullary

instance ( AllNullary a allNullaryL
         , AllNullary b allNullaryR
         , And allNullaryL allNullaryR allNullary
         ) => AllNullary (a :+: b) allNullary
instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
instance AllNullary (a :*: b) False
instance AllNullary (K1 i c) False
instance AllNullary U1 True

data True
data False

class And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And True  True  True
instance And False False False
instance And False True  False
instance And True  False False

newtype Tagged s b = Tagged { unTagged :: b }

--
-- Combinators
--

qpWrap :: (a -> b, b -> a) -> PU a -> PU b
qpWrap (f, g) pua = PU
    { pickle   = pickle pua . g
    , unpickle = fmap f . unpickle pua
    }

qpElem :: ByteString -> PU a -> PU a
qpElem name pu = PU
    { pickle   = Pair name . pickle pu
    , unpickle = \qry -> (unpickle pu =<<) . note qry $ findPair name qry
    }
  where
    note _ = maybe (Right $ List []) Right

    findPair k qry
        | List qs <- qry            = mconcat $ map (findPair k) qs
        | Pair k' q <- qry, k == k' = Just q
        | otherwise                 = Nothing

qpPair :: PU a -> PU b -> PU (a, b)
qpPair pua pub = PU
    { pickle   = \(a, b) -> pickle pua a <> pickle pub b
    , unpickle = \qry -> case (unpickle pua qry, unpickle pub qry) of
          (Right a, Right b) -> Right (a, b)
          (Left ea, _)       -> failure qry $ "left - " ++ ea
          (_,       Left eb) -> failure qry $ "right - " ++ eb
    }
  where
    failure qry s = Left ("qpPair: " ++ s ++ ", qry: " ++ show qry)

qpLift :: a -> PU a
qpLift x = PU
    { pickle   = const $ List []
    , unpickle = const $ Right x
    }

qpPrim :: (Read a, Show a) => PU a
qpPrim = PU
    { pickle   = Value . BS.pack . show
    , unpickle = (eitherRead =<<) . findValue
    }
  where
    eitherRead (BS.unpack -> s) = case reads s of
        [(x, "")] -> Right x
        _         -> Left $ "qpPrim: failed to read value - " ++ s

    findValue qry
        | List [Value v] <- qry = Right v
        | (Value v)      <- qry = Right v
        | otherwise = Left $ "qpPrim: unexpected non-value - " ++ show qry

qpOption :: PU a -> PU (Maybe a)
qpOption pu = PU
    { pickle   = maybe (List []) (pickle pu)
    , unpickle = either (const $ Right Nothing) (Right . Just) . unpickle pu
    }

qpDefault :: a -> PU a -> PU a
qpDefault x pu = PU
    { pickle    = pickle pu
    , unpickle  = either (const $ Right x) Right . unpickle pu
    }

qpSum :: PU (f r) -> PU (g r) -> PU ((f :+: g) r)
qpSum left right = (inp, out) `qpWrap` qpEither left right
  where
    inp (Left  x) = L1 x
    inp (Right x) = R1 x

    out (L1 x) = Left x
    out (R1 x) = Right x

qpEither :: PU a -> PU b -> PU (Either a b)
qpEither pua pub = PU pickleEither unpickleEither
  where
    unpickleEither qry = either
        (handleFailure qry)
        (Right . Left) $ unpickle pua qry

    handleFailure qry err1 = either
        (\err2 -> Left $ "qpEither: both failed - " ++ err1 ++ " - " ++ err2)
        (Right . Right) $ unpickle pub qry

    pickleEither (Left  x) = pickle pua x
    pickleEither (Right y) = pickle pub y

qpOrdinalList :: PU a -> PU [a]
qpOrdinalList pu = PU
    { pickle   = List . zipWith pickler ([1..] :: [Integer])
    , unpickle = \qry -> case qry of
          (List qs) -> concatEithers $ map (unpickle pu) [v | Pair _ v <- sort qs]
          _         -> Left $ "qpOrdinalList: unexpected non-list - " ++ show qry
    }
  where
    pickler (BS.pack . show -> k) x =
        case pickle pu x of
            (Pair k' v) -> Pair k' (Pair k v)
            qry         -> (Pair k qry)

qpList :: PU a -> PU [a]
qpList pu = PU
    { pickle   = mconcat . map (pickle pu)
    , unpickle = \qry -> case qry of
          v@(Value _) -> fmap (:[]) $ unpickle pu v
          (List [])   -> Right []
          (List qs)   -> fmap reverse . concatEithers $ map (unpickle pu) qs
          _           -> Left $ "qpList: unexpected non-list - " ++ show qry
    }

concatEithers :: [Either b c] -> Either b [c]
concatEithers xs = case partitionEithers xs of
    (l:_, _) -> Left l
    ([], rs) -> Right rs

--
-- Instances
--

instance IsQuery Int where
    queryPickler = qpPrim

instance IsQuery Integer where
    queryPickler = qpPrim

instance IsQuery ByteString where
    queryPickler = PU
        { pickle   = Value
        , unpickle = \qry -> case qry of
              (Value v) -> Right v
              _         -> Left $ "qpByteString: unexpected non-value - " ++ show qry
        }
