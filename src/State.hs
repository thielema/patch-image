module State where

import Degree (Degree(Degree))

import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Csv ((.=), (.:))
import Data.Vector (Vector)

import Control.Applicative (pure, liftA2, (<$>), (<*>))


newtype File = File FilePath

data Angle = Angle FilePath (Degree Float)

data Position = Position FilePath (Degree Float) (Float, Float)


imageId, angleId, xId, yId :: B.ByteString
imageId = B.pack "Image"
angleId = B.pack "Angle"
xId = B.pack "X"
yId = B.pack "Y"

instance Csv.ToNamedRecord File where
   toNamedRecord (File path) = Csv.namedRecord [imageId .= path]
instance Csv.DefaultOrdered File where
   headerOrder _ = Csv.header [imageId]

instance Csv.ToNamedRecord Angle where
   toNamedRecord (Angle path (Degree angle)) =
      Csv.namedRecord [imageId .= path, angleId .= angle]
instance Csv.DefaultOrdered Angle where
   headerOrder _ = Csv.header [imageId, angleId]

instance Csv.ToNamedRecord Position where
   toNamedRecord (Position path (Degree angle) (x,y)) =
      Csv.namedRecord [imageId .= path, angleId .= angle, xId .= x, yId .= y]
instance Csv.DefaultOrdered Position where
   headerOrder _ = Csv.header [imageId, angleId, xId, yId]


write :: (Csv.ToNamedRecord a, Csv.DefaultOrdered a) => FilePath -> [a] -> IO ()
write path = BL.writeFile path . Csv.encodeDefaultOrderedByName



{-
(.:) accepts missing fields and (.:?) accepts even missing columns.

Warning: This implementation would also accept ill-formated cells:
(.:?) m field = (m.:field) <|> pure Nothing
-}
(.:?) ::
   Csv.FromField a => Csv.NamedRecord -> B.ByteString -> Csv.Parser (Maybe a)
(.:?) m field =
   maybe (pure Nothing) Csv.parseField $ HashMap.lookup field m


data
   Proposed =
      Proposed FilePath (Maybe (Degree Float)) (Maybe Float, Maybe Float)

instance Csv.FromNamedRecord Proposed where
   parseNamedRecord m =
      Proposed <$> m .: imageId
         <*> (fmap Degree <$> m .:? angleId)
         <*> liftA2 (,) (m .:? xId) (m .:? yId)

read :: FilePath -> IO (Vector Proposed)
read path =
   either (ioError . userError) (return . snd) . Csv.decodeByName
      =<< BL.readFile path
