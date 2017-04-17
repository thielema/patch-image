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
import Data.Traversable (traverse)
import Data.Foldable (foldMap)
import Data.Csv ((.=), (.:))
import Data.Vector (Vector)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, catMaybes)
import Data.Tuple.HT (mapFst, mapSnd, mapPair, swap)
import Data.Map (Map)

import qualified Control.Monad.Exception.Synchronous as ME
import qualified Control.Functor.HT as FuncHT
import Control.Monad (when, join)
import Control.Applicative (pure, liftA2, (<$>), (<$), (<*>), empty)

import Text.Printf (printf)

import qualified System.IO as IO
import Prelude hiding (read)


newtype File = File FilePath

data Angle = Angle FilePath (Degree Float)

data Position = Position FilePath (Degree Float) (Float, Float)

data Displacement =
      Displacement FilePath FilePath (Maybe Relation) (Maybe (Float, Float))

data Rotated =
      Rotated
         (Maybe ((FilePath, FilePath), Maybe Relation))
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
         <*> runCombinedParser (parseMaybePair (m .:# dxId) (m .:# dyId))


instance Csv.ToNamedRecord Rotated where
   toNamedRecord (Rotated pathsRel rot) =
      let ((pathA, pathB), rel) = mapFst FuncHT.unzip $ FuncHT.unzip pathsRel
          ((xa,ya),(xb,yb)) =
            mapPair (FuncHT.unzip, FuncHT.unzip) $ FuncHT.unzip rot
      in  Csv.namedRecord $
            [imageAId .= pathA, imageBId .= pathB, relationId .= join rel,
             xaId .= xa, yaId .= ya, xbId .= xb, ybId .= yb]
instance Csv.DefaultOrdered Rotated where
   headerOrder _ =
      Csv.header [imageAId, imageBId, relationId, xaId, yaId, xbId, ybId]


{- |
We use it like a @Compose Writer Csv.Parser@,
we do not have much use for the 'Applicative' combinators.
-}
data NamedParser a = NamedParser [B.ByteString] (Csv.Parser a)

(.:#) :: Csv.FromField a => Csv.NamedRecord -> B.ByteString -> NamedParser a
(.:#) m name = NamedParser [name] (m .: name)

enumerateNames :: [B.ByteString] -> String
enumerateNames = B.unpack . B.intercalate (B.pack ", ")

parseMaybePair ::
   NamedParser (Maybe a) -> NamedParser (Maybe b) ->
   NamedParser (Maybe (a, b))
parseMaybePair (NamedParser na pa) (NamedParser nb pb) =
   let n = na++nb
   in  NamedParser n $ do
         m <- liftA2 (,) pa pb
         case m of
            (Nothing, Nothing) -> return Nothing
            (Just a, Just b) -> return $ Just (a,b)
            _ ->
               fail $
               printf "The columns %s must be all set or all empty." $
               enumerateNames n

parseImpliedMaybe ::
   NamedParser (Maybe a) -> NamedParser (Maybe b) ->
   NamedParser (Maybe (a, Maybe b))
parseImpliedMaybe (NamedParser na pa) (NamedParser nb pb) =
   NamedParser (na++nb) $ do
      m <- liftA2 (,) pa pb
      case m of
         (Nothing, Just _) ->
            fail $
            printf "If the columns %s are set, then %s must be set as well."
               (enumerateNames nb) (enumerateNames na)
         (ma,mb) -> return $ flip (,) mb <$> ma

runCombinedParser :: NamedParser a -> Csv.Parser a
runCombinedParser (NamedParser _ p) = p


instance Csv.FromNamedRecord Rotated where
   parseNamedRecord m =
      Rotated
         <$>
            (runCombinedParser $
             parseImpliedMaybe
               (parseMaybePair (m .:# imageAId) (m .:# imageBId))
               (m .:# relationId))
         <*>
            (runCombinedParser $
             parseMaybePair
               (parseMaybePair (m .:# xaId) (m .:# yaId))
               (parseMaybePair (m .:# xbId) (m .:# ybId)))

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
         (\((mrot0,(paths,mrel)), rots) ->
            (paths,
             (mrel,
              catMaybes $ mrot0 : map (\(Rotated _ mrot) -> mrot) rots)))
         blocks))
   .
   ListHT.segmentBeforeMaybe
      (\(Rotated mPathRel mrot) -> (,) mrot <$> mPathRel)

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

maybeCorrection ::
   (Csv.FromField a) => a -> Maybe a -> Maybe (Maybe a) -> Maybe a
maybeCorrection zero a mda = fromMaybe (zero <$ a) mda

parseAngle :: Csv.FromField a => Csv.NamedRecord -> Csv.Parser (Maybe a)
parseAngle m = join <$> m .:? angleId

class Csv.DefaultOrdered angleCorr => AngleCorrected angleCorr where
   autoAngleCorrection :: angleCorr
   parseAngles ::
      Csv.NamedRecord -> Csv.Parser (Maybe (Degree Float), angleCorr)

newtype
   AngleCorrection =
      AngleCorrection {getAngleCorrection :: Maybe (Degree Float)}

instance AngleCorrected AngleCorrection where
   autoAngleCorrection = AngleCorrection Nothing
   parseAngles m =
      liftA2
         (\a mda -> (a, AngleCorrection $ maybeCorrection (Degree 0) a mda))
         (parseAngle m) (m .:? dAngleId)

data NoAngleCorrection = NoAngleCorrection

instance AngleCorrected NoAngleCorrection where
   autoAngleCorrection = NoAngleCorrection
   parseAngles m = liftA2 (,) (parseAngle m) (pure NoAngleCorrection)


data
   Proposed angleCorr =
      Proposed FilePath
         (Maybe (Degree Float), angleCorr)
         (Maybe Float, Maybe Float)

propPath :: Proposed angleCorr -> FilePath
propPath (Proposed path _ _) = path

propAngleCorr :: Proposed angleCorr -> angleCorr
propAngleCorr (Proposed _ (_,angleCorr) _) = angleCorr

instance Csv.DefaultOrdered AngleCorrection where
   headerOrder _ = Csv.header [dAngleId]

instance Csv.DefaultOrdered NoAngleCorrection where
   headerOrder _ = Csv.header []

instance
   Csv.DefaultOrdered angleCorr =>
      Csv.DefaultOrdered (Proposed angleCorr) where
   headerOrder p =
      Csv.header [imageId, angleId] <>
      Csv.headerOrder (propAngleCorr p) <>
      Csv.header [xId, yId]

instance
   (AngleCorrected angleCorr) =>
      Csv.FromNamedRecord (Proposed angleCorr) where
   parseNamedRecord m =
      Proposed <$> m .: imageId
         <*> parseAngles m
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


readDisplacement ::
   [FilePath] -> FilePath ->
   IO (Map (FilePath, FilePath) (Maybe Relation, Maybe (Float, Float)))
readDisplacement imagePaths path = do
   relations <-
      ME.switch (ioError . userError) return .
      imagePairMap .
      foldMap
         (\(Displacement pathA pathB rel d) ->
            ((pathA,pathB), (rel,d)) :
            ((pathB,pathA), (rel, mapPair (negate,negate) <$> d)) :
            [])
      =<< read path
   warnUnmatchedImages imagePaths relations
   return relations

readRotated ::
   [FilePath] -> FilePath ->
   IO (Map (FilePath, FilePath)
         (Maybe Relation, [((Float, Float), (Float, Float))]))
readRotated imagePaths path = do
   relationsPlain <- read path
   relations <-
      ME.switch (ioError . userError) return $
      imagePairMap .
      concatMap
         (\((pathA,pathB), (rel,ds)) ->
            ((pathA,pathB), (rel,ds)) :
            ((pathB,pathA), (rel, map swap ds)) :
            [])
      =<<
      segmentRotated (Vector.toList relationsPlain)
   warnUnmatchedImages imagePaths relations
   return relations
