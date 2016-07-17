{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.SWAPI.Types where

  import           Data.Aeson.Encode.Pretty   (encodePretty)
  import           GHC.Generics
  import           Data.Aeson.Types
  import           Data.Text                  (Text, unpack)
  import           Data.Time.Clock            (UTCTime)
  import           Servant.API                (ToHttpApiData, toUrlPiece)


  data BirthYear = ABY Int | BBY Int
    deriving (Show, Generic)
  instance FromJSON BirthYear where
    parseJSON (String s) = go . reverse . unpack $ s
      where
        go :: String -> Parser BirthYear
        go ('Y':'B':'A':xs) = return $ ABY (read xs::Int)
        go ('Y':'B':'B':xs) = return $ BBY (read xs::Int)
        go _                = fail "Can't even."


  data Gender = Male | Female
    deriving (Show, Generic)
  instance FromJSON Gender where
    parseJSON (String "male") = return Male
    parseJSON (String s)      = return Female

  data Color = Fair | Blue | Hazel | Green | Blond | Red | Brown | Unknown | NA | Missed Text
    deriving (Show, Generic)
  instance FromJSON Color where
    parseJSON (String "fair")    = return Fair
    parseJSON (String "blue")    = return Blue
    parseJSON (String "hazel")   = return Hazel
    parseJSON (String "green")   = return Green
    parseJSON (String "blond")   = return Blond
    parseJSON (String "red")     = return Red
    parseJSON (String "brown")   = return Brown
    parseJSON (String "unknown") = return Unknown
    parseJSON (String "n/a")     = return NA
    parseJSON (String s)         = return $ Missed s

  newtype Height = Height Int deriving (Show, Eq)
  instance FromJSON Height where
    parseJSON (String s) = return $ Height (read $ unpack s::Int)

  data Person =
    Person
    { birth_year  :: BirthYear
    , eye_color   :: Color
    , films      :: [Text]
    , gender     :: Gender
    , skin_color :: Color
    , hair_color  :: Color
    , height     :: Height
    , homeworld  :: Text
    , mass       :: Text
    , created  :: UTCTime
    , edited   :: UTCTime
    , species    :: [Text]
    , starships  :: Maybe [Text]
    , url        :: Text
    , vehicles   :: Maybe [Text]
    } deriving (Show, Generic)

  instance FromJSON Person
