{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}

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
    , toQueryString
    , fromQueryString
    , encode
    , decode

    -- * Data Types
    , Query (..)
    , PU (..)

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
    , qpConst
    , qpPrim
    , qpOption
    , qpSum
    , qpEither
    , qpOrdinalList
    ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isLower, toLower)
import           Data.List             (sort)
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics

--
-- Types
--

class IsQuery a where
    queryPickler :: PU a

    default queryPickler :: (Generic a, GIsQuery (Rep a)) => PU a
    queryPickler = genericQueryPickler defaultOptions

data Query
    = List [Query]
    | Pair ByteString Query
    | Value ByteString
      deriving (Eq, Show)

instance Monoid Query where
    mempty      = List []
    mappend l r = List [l, r]

data PU a = PU
    { pickle   :: a -> Query
    , unpickle :: Query -> Either String a
    }

data Options = Options
    { constructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags.
    , fieldLabelModifier     :: String -> String
      -- ^ Function applied to record field labels.
    }

defaultOptions :: Options
defaultOptions = Options id (dropWhile isLower)

loweredOptions :: Options
loweredOptions = defaultOptions
    { fieldLabelModifier = map toLower . dropWhile isLower
    }

--
-- Functions
--

toQueryString :: IsQuery a => a -> [(ByteString, ByteString)]
toQueryString = enc "" . pickle queryPickler
  where
    enc k (List qs) = concatMap (enc k) qs
    enc k (Value v) = [(k, v)]
    enc k (Pair k' q)
        | BS.null k = enc k' q
        | otherwise = enc (k <> "." <> k') q

fromQueryString :: IsQuery a => [(ByteString, ByteString)] -> Either String a
fromQueryString = unpickle queryPickler . List . map reify
  where
    reify :: (ByteString, ByteString) -> Query
    reify (k, v)
        | '.' `BS.elem` k = let ks = BS.split '.' k
                                f k' qry = Pair k' qry
                             -- foldr :: (a -> b -> b) -> b -> [a] -> b
                             in foldr f (Pair (last ks) $ Value v) $ init ks
        | otherwise    = Pair k $ Value v

encode :: (ByteString -> ByteString)  -- ^ URL Value Encoder
       -> [(ByteString, ByteString)] -- ^ Key/Value Pairs
       -> ByteString
encode f = BS.intercalate "&"
    . map (\(k, v) -> mconcat [k, "=", f v])
    . sort

decode :: (ByteString -> ByteString) -- ^ URL Value Decoder
       -> ByteString                -- ^ Input Query String
       -> [(ByteString, ByteString)]
decode f = map (pair . BS.split '=')
    . BS.split '&'
    . BS.dropWhile (\c -> c == '/' || c == '?')
  where
    pair (k:vs) = (k, f $ BS.intercalate "=" vs)

--
-- Generics
--

genericQueryPickler opts =
    (to, from) `qpWrap` (gQueryPickler opts) (genericQueryPickler opts)

class GIsQuery f where
    gQueryPickler :: Options -> PU a -> PU (f a)

instance IsQuery a => GIsQuery (K1 i a) where
    gQueryPickler _ _ = (K1, unK1) `qpWrap` queryPickler

instance GIsQuery U1 where
    gQueryPickler _ _ = (const U1, const ()) `qpWrap` qpLift ()

instance (GIsQuery f, GIsQuery g) => GIsQuery (f :+: g) where
    gQueryPickler opts f = gQueryPickler opts f `qpSum` gQueryPickler opts f

instance (GIsQuery f, GIsQuery g) => GIsQuery (f :*: g) where
    gQueryPickler opts f = qpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (gQueryPickler opts f `qpPair` gQueryPickler opts f)

instance (Datatype d, GIsQuery f) => GIsQuery (D1 d f) where
    gQueryPickler opts = qpWrap (M1, unM1) . gQueryPickler opts

instance (Constructor c, GIsQuery f) => GIsQuery (C1 c f) where
    gQueryPickler opts f = qpElem
        (BS.pack . constructorTagModifier opts $ conName (undefined :: C1 c f r))
        ((M1, unM1) `qpWrap` gQueryPickler opts f)

instance (Selector s, GIsQuery f) => GIsQuery (S1 s f) where
    gQueryPickler opts f = qpElem
        (BS.pack . fieldLabelModifier opts $ selName (undefined :: S1 s f r))
        ((M1, unM1) `qpWrap` gQueryPickler opts f)

instance Constructor c => GIsQuery (C1 c U1) where
    gQueryPickler opts = qpConst x . qpWrap (M1, unM1) . gQueryPickler opts
     where
       x = BS.pack $ conName (undefined :: t c U1 p)

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
    , unpickle = (unpickle pu =<<) . annotate . findPair name
    }
  where
    findPair k qry
        | List qs <- qry           = listToMaybe . catMaybes $ map (findPair k) qs
        | Pair k' q <- qry, k == k' = Just q
        | otherwise               = Nothing

    annotate = note ("qpElem: non-locatable - " <> BS.unpack name)

qpPair :: PU a -> PU b -> PU (a, b)
qpPair pua pub = PU
    { pickle   = \(a, b) -> pickle pua a <> pickle pub b
    , unpickle = \qry -> case (unpickle pua qry, unpickle pub qry) of
          (Right a, Right b) -> Right (a, b)
          (Left ea, Right _) -> Left $ "qpPair: left - " ++ ea
          (Right _, Left eb) -> Left $ "qpPair: right - " ++ eb
          (Left ea, Left eb) -> Left $ "qpPair: both - " ++ show (ea, eb)
    }

qpLift :: a -> PU a
qpLift x = PU
    { pickle   = const $ List []
    , unpickle = const $ Right x
    }

qpConst :: ByteString -> PU a -> PU a
qpConst name pu = PU
    { pickle   = const $ Value name
    , unpickle = const . unpickle pu $ Value name
    }

qpPrim :: (Read a, Show a) => PU a
qpPrim = PU
    { pickle   = Value . BS.pack . show
    , unpickle = \qry -> case qry of
          (Value x) -> let v = BS.unpack x
                      in note ("qpPrim: failed reading - " ++ v) $ maybeRead v
          _         -> Left $ "qpPrim: unexpected non-value - " ++ show qry
    }

qpOption :: PU a -> PU (Maybe a)
qpOption pu = PU
    { pickle   = maybe (List []) $ pickle pu
    , unpickle = fmap Just . unpickle pu
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

qpOrdinalList :: IsQuery a => PU [a]
qpOrdinalList = PU
    { pickle = List . zipWith pick ([1..] :: [Integer])
    , unpickle = undefined
    }
  where
    pick n x = case pickle queryPickler x of
        (Pair k v) -> Pair k (Pair k' v)
        other      -> (Pair k' other)
      where
        k' = BS.pack $ show n

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

--
-- Instances
--

instance IsQuery a => IsQuery (Maybe a) where
    queryPickler = qpOption queryPickler

instance (IsQuery a, IsQuery b) => IsQuery (Either a b) where
    queryPickler = qpEither queryPickler queryPickler

instance IsQuery () where
    queryPickler = qpLift ()

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
