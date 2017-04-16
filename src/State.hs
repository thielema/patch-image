module State where

import Degree (Degree(Degree))

import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.Vector as Vector
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Traversable (traverse)
import Data.Csv ((.=), (.:))
import Data.Vector (Vector)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Tuple.HT (mapSnd)
import Data.Map (Map)

import qualified Control.Monad.Exception.Synchronous as ME
import qualified Control.Monad.Trans.Writer as MW
import Control.Monad (when, join)
import Control.Applicative (pure, liftA2, (<$>), (<$), (<*>), empty)

import Text.Printf (printf)

import qualified System.IO as IO


newtype File = File FilePath

data Angle = Angle FilePath (Degree Float)

data Position = Position FilePath (Degree Float) (Float, Float)

data Displacement =
      Displacement FilePath FilePath (Maybe Relation) (Maybe (Float, Float))

data Rotated =
      Rotated
         (Maybe (FilePath, FilePath)) (Maybe Relation)
         (Maybe ((Float, Float), (Float, Float)))


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

xaId, yaId, xbId, ybId :: B.ByteString
xaId = B.pack "XA"
yaId = B.pack "YA"
xbId = B.pack "XB"
ybId = B.pack "YB"


instance Csv.ToNamedRecord File where
   toNamedRecord (File path) = Csv.namedRecord [imageId .= path]
instance Csv.DefaultOrdered File where
   headerOrder _ = Csv.header [imageId]

instance Csv.ToNamedRecord Angle where
   toNamedRecord (Angle path (Degree angle)) =
      Csv.namedRecord [imageId .= path, angleId .= angle]
instance Csv.DefaultOrdered Angle where
   headerOrder _ = Csv.header [imageId, angleId]

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

unrelated :: Bool -> Relation
unrelated b =
   if b
     then State.NonOverlapping
     else State.Overlapping


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

instance Csv.FromNamedRecord Displacement where
   parseNamedRecord m =
      Displacement
         <$> m .: imageAId
         <*> m .: imageBId
         <*> m .: relationId
         <*> runCombinedMaybe (parseMaybePair (m .:?? dxId) (m .:?? dyId))


instance Csv.ToNamedRecord Rotated where
   toNamedRecord (Rotated paths rel rot) =
      Csv.namedRecord $
         [imageAId .= fmap fst paths, imageBId .= fmap snd paths,
          relationId .= rel,
          xaId .= fmap (fst.fst) rot, yaId .= fmap (snd.fst) rot,
          xbId .= fmap (fst.snd) rot, ybId .= fmap (snd.snd) rot]
instance Csv.DefaultOrdered Rotated where
   headerOrder _ =
      Csv.header [imageAId, imageBId, relationId, xaId, yaId, xbId, ybId]


type
   CombinedMaybe =
      Compose (MW.Writer [B.ByteString]) (Compose Csv.Parser Maybe)

(.:??) ::
   Csv.FromField a => Csv.NamedRecord -> B.ByteString -> CombinedMaybe a
(.:??) m name = Compose $ MW.writer (Compose (m .: name), [name])

parseMaybePair :: CombinedMaybe a -> CombinedMaybe b -> CombinedMaybe (a, b)
parseMaybePair = liftA2 (,)

runCombinedMaybe :: CombinedMaybe a -> Csv.Parser (Maybe a)
runCombinedMaybe = getCompose . fst . MW.runWriter . getCompose


instance Csv.FromNamedRecord Rotated where
   parseNamedRecord m =
      Rotated
         <$>
            runCombinedMaybe
               (parseMaybePair (m .:?? imageAId) (m .:?? imageBId))
         <*> m .: relationId
         <*>
            (runCombinedMaybe $
             parseMaybePair
               (parseMaybePair (m .:?? xaId) (m .:?? yaId))
               (parseMaybePair (m .:?? xbId) (m .:?? ybId)))

segmentRotated ::
   [Rotated] ->
   ME.Exceptional String
      [((FilePath, FilePath),
        (Maybe Relation, [((Float, Float), (Float, Float))]))]
segmentRotated =
   (\(prefix, blocks) ->
      ME.assert "leading relations without image header" (null prefix)
      >>
      (ME.Success $
       map
         (\((mrel,mrot0,paths), rots) ->
            (paths,
             (mrel,
              catMaybes $ mrot0 : map (\(Rotated _ _ mrot) -> mrot) rots)))
         blocks))
   .
   ListHT.segmentBeforeMaybe
      (\(Rotated mpath mrel mrot) -> (,,) mrel mrot <$> mpath)

imagePairMap ::
   [((FilePath, FilePath), a)] ->
   ME.Exceptional String (Map (FilePath, FilePath) a)
imagePairMap =
   Trav.sequence .
   Map.fromListWithKey
      (\(pathA,pathB) _ _ ->
         ME.Exception $ printf "duplicate image pair: %s, %s" pathA pathB) .
   map (mapSnd ME.Success)

unmatchedImages ::
   [FilePath] -> Map (FilePath, FilePath) a -> [FilePath]
unmatchedImages paths pairs =
   Set.toList $
   Set.difference
      (Set.fromList $ concatMap (\(pathA,pathB) -> [pathA,pathB]) $
         Map.keys pairs)
      (Set.fromList paths)

warnUnmatchedImages :: [FilePath] -> Map (FilePath, FilePath) a -> IO ()
warnUnmatchedImages paths relations = do
   let unmatched = unmatchedImages paths relations
   when (not $ null unmatched) $ IO.hPutStrLn IO.stderr $
      "image in relations but not in the image list: " ++
      List.intercalate ", " unmatched



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


data
   Proposed =
      Proposed FilePath
         (Maybe (Degree Float), Maybe (Degree Float))
         (Maybe Float, Maybe Float)

propPath :: Proposed -> FilePath
propPath (Proposed path _ _) = path

instance Csv.DefaultOrdered Proposed where
   headerOrder _ = Csv.header [imageId, angleId, dAngleId, xId, yId]

instance Csv.FromNamedRecord Proposed where
   parseNamedRecord m =
      Proposed <$> m .: imageId
         <*> parseAngle (Degree 0) m
         <*> liftA2 (,) (join <$> m .:? xId) (join <$> m .:? yId)

read ::
   (Csv.FromNamedRecord a, Csv.DefaultOrdered a) =>
   FilePath -> IO (Vector a)
read path = do
   (headers, body) <-
      either (ioError . userError) return . Csv.decodeByName
         =<< BL.readFile path
   let ignored =
         Set.toList $
         Set.difference
            (Set.fromList $ Fold.toList headers)
            (Set.fromList $ Fold.toList $
             Csv.headerOrder $ Vector.head body)
   when (not $ null ignored) $ IO.hPutStrLn IO.stderr $
      "ignore unknown columns: " ++ List.intercalate ", " (map B.unpack ignored)
   return body
