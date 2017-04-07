module State where

import Degree (Degree(Degree))

import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as Fold
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Traversable (traverse)
import Data.Csv ((.=), (.:))
import Data.Vector (Vector)
import Data.Maybe (fromMaybe)

import Control.Monad (when, join)
import Control.Applicative (pure, liftA2, (<$>), (<$), (<*>), empty)

import qualified System.IO as IO


newtype File = File FilePath

data Angle = Angle FilePath (Degree Float)

data Overlap = Overlap FilePath (Degree Float) [Maybe Relation]

data Position = Position FilePath (Degree Float) (Float, Float)

data Displacement = Displacement FilePath FilePath Relation (Maybe (Float, Float))


imageId, angleId, dAngleId, xId, yId :: B.ByteString
imageId = B.pack "Image"
angleId = B.pack "Angle"
dAngleId = B.pack "DAngle"
xId = B.pack "X"
yId = B.pack "Y"

imageAId, imageBId, relationId, dxId, dyId :: B.ByteString
imageAId = B.pack "ImageA"
imageBId = B.pack "ImageB"
relationId = B.pack "Rel"
dxId = B.pack "DX"
dyId = B.pack "DY"

overId :: Int -> B.ByteString
overId k = B.pack $ "Over" ++ show k

instance Csv.ToNamedRecord File where
   toNamedRecord (File path) = Csv.namedRecord [imageId .= path]
instance Csv.DefaultOrdered File where
   headerOrder _ = Csv.header [imageId]

instance Csv.ToNamedRecord Angle where
   toNamedRecord (Angle path (Degree angle)) =
      Csv.namedRecord [imageId .= path, angleId .= angle]
instance Csv.DefaultOrdered Angle where
   headerOrder _ = Csv.header [imageId, angleId]

instance Csv.ToNamedRecord Overlap where
   toNamedRecord (Overlap path (Degree angle) xs) =
      Csv.namedRecord $
         [imageId .= path, angleId .= angle] ++
         zipWith (\k mx -> overId k .= mx) [0..] xs

data Relation = NonOverlapping | Overlapping
   deriving (Eq, Ord, Enum)

instance Csv.ToField Relation where
   toField NonOverlapping = B.singleton '-'
   toField Overlapping = B.singleton 'X'

instance Csv.FromField Relation where
   parseField bstr =
      case B.unpack bstr of
         "-" -> pure NonOverlapping
         "X" -> pure Overlapping
         _ -> empty

overlapHeader :: Int -> Csv.Header
overlapHeader n =
   Vector.fromList $ [State.imageId, State.angleId] ++ map overId (take n [0..])

instance Csv.ToNamedRecord Position where
   toNamedRecord (Position path (Degree angle) (x,y)) =
      Csv.namedRecord [imageId .= path, angleId .= angle, xId .= x, yId .= y]
instance Csv.DefaultOrdered Position where
   headerOrder _ = Csv.header [imageId, angleId, xId, yId]

instance Csv.ToNamedRecord Displacement where
   toNamedRecord (Displacement pathA pathB rel disp) =
      Csv.namedRecord
         [imageAId .= pathA, imageBId .= pathB,
          relationId .= rel, dxId .= fmap fst disp, dyId .= fmap snd disp]
instance Csv.DefaultOrdered Displacement where
   headerOrder _ = Csv.header [imageAId, imageBId, relationId, dxId, dyId]


write :: (Csv.ToNamedRecord a, Csv.DefaultOrdered a) => FilePath -> [a] -> IO ()
write path = BL.writeFile path . Csv.encodeDefaultOrderedByName

writeWithHeader ::
   (Csv.ToNamedRecord a) => FilePath -> Csv.Header -> [a] -> IO ()
writeWithHeader path header = BL.writeFile path . Csv.encodeByName header



{-
(.:) accepts missing fields and (.:?) instead accepts missing columns.

Warning: This implementation would also accept ill-formated cells:
(.:?) m field = (m.:field) <|> pure Nothing
-}
(.:?) ::
   Csv.FromField a => Csv.NamedRecord -> B.ByteString -> Csv.Parser (Maybe a)
(.:?) m field = traverse Csv.parseField $ HashMap.lookup field m

parseAngle ::
   (Csv.FromField a) =>
   a -> Csv.NamedRecord -> Csv.Parser (Maybe a, Maybe a)
parseAngle zero m =
   liftA2
      (\a mda -> (a, fromMaybe (zero <$ a) mda))
      (join <$> m .:? angleId) (m .:? dAngleId)

parseRelations :: (Csv.FromField a) => Csv.NamedRecord -> Csv.Parser [a]
parseRelations m =
   let go k =
         case HashMap.lookup (overId k) m of
            Nothing -> pure []
            Just str -> liftA2 (:) (Csv.parseField str) (go (k+1))
   in  go 0


data
   Proposed =
      Proposed FilePath
         (Maybe (Degree Float), Maybe (Degree Float))
         (Maybe Float, Maybe Float)
         [Maybe Relation]

instance Csv.FromNamedRecord Proposed where
   parseNamedRecord m =
      Proposed <$> m .: imageId
         <*> parseAngle (Degree 0) m
         <*> liftA2 (,) (join <$> m .:? xId) (join <$> m .:? yId)
         <*> parseRelations m

read :: FilePath -> IO (Vector Proposed)
read path = do
   (headers, body) <-
      either (ioError . userError) return . Csv.decodeByName
         =<< BL.readFile path
   let deleteOver k set =
         if Set.member (overId k) set
           then deleteOver (k+1) $! Set.delete (overId k) set
           else set
   let ignored =
         Set.toList $
         deleteOver 0 $
         Set.difference
            (Set.fromList $ Fold.toList headers)
            (Set.fromList [imageId, angleId, dAngleId, xId, yId])
   when (not $ null ignored) $ IO.hPutStrLn IO.stderr $
      "ignore unknown columns: " ++ List.intercalate ", " (map B.unpack ignored)
   return body
