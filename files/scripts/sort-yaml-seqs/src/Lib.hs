module Lib
    ( process
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import qualified Streamly.Internal.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import Text.Libyaml

data Scalar = Scalar !Text !Tag !Style !Anchor

instance Eq Scalar where
    Scalar s1 tag1 _ _ == Scalar s2 tag2 _ _ = s1 == s2 && tag1 == tag2

instance Ord Tag where
    compare NoTag NoTag = EQ
    compare NoTag _ = LT
    compare _ NoTag = GT
    compare StrTag StrTag = EQ
    compare StrTag _ = LT
    compare _ StrTag = GT
    compare (UriTag s1) (UriTag s2) = compare s1 s2
    compare (UriTag _) _ = LT
    compare _ (UriTag _) = GT
    compare FloatTag FloatTag = EQ
    compare FloatTag _ = LT
    compare _ FloatTag = GT
    compare NullTag NullTag = EQ
    compare NullTag _ = LT
    compare _ NullTag = GT
    compare BoolTag BoolTag = EQ
    compare BoolTag _ = LT
    compare _ BoolTag = GT
    compare SetTag SetTag = EQ
    compare SetTag _ = LT
    compare _ SetTag = GT
    compare IntTag IntTag = EQ
    compare IntTag _ = LT
    compare _ IntTag = GT
    compare SeqTag SeqTag = EQ
    compare SeqTag _ = LT
    compare _ SeqTag = GT
    compare MapTag MapTag = EQ

instance Ord Scalar where
    compare (Scalar s1 tag1 _ _) (Scalar s2 tag2 _ _) = 
        case compare s1 s2 of
            EQ -> compare tag1 tag2
            result -> result

fromScalar :: Scalar -> Event
fromScalar (Scalar t tag s a) = EventScalar (encodeUtf8 t) tag s a

toScalar :: Event -> Scalar
toScalar (EventScalar bs tag s a) = Scalar (decodeUtf8Lenient bs) tag s a

myFold :: Monad m => Fold.Fold m Event [Event]
myFold = snd <$> Fold.foldl' myStep mempty
    where
        myStep :: (Maybe (Set Scalar), [Event]) -> Event -> (Maybe (Set Scalar), [Event])
        myStep (Nothing, _) st@(EventSequenceStart _ _ _) = (Just Set.empty                  , [st])
        myStep (Nothing, _) x                             = (Nothing                         , [x])
        myStep (Just s , _) EventSequenceEnd              = (Nothing                         , fmap fromScalar (Set.toList s) ++ [EventSequenceEnd])
        myStep (Just s , _) ms@(EventMappingStart _ _ _)  = (Nothing                         , fmap fromScalar (Set.toList s) ++ [ms])
        myStep (Just s , _) e                             = (Just $ Set.insert (toScalar e) s, [])

process :: Monad m => Stream m Event -> Stream m Event
process = Stream.concatMap Stream.fromList . Stream.scan myFold
