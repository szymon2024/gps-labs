-- 2025-11-28

{-
  Program for matching GPS ephemerides (RINEX 3.04 NAV) to satellite
  observations (RINEX 3.04 OBS).

  - Parses navigation (.nav) and observation (.obs) files in RINEX 3.04 format.
  - Builds a navigation map of GPS satellite ephemerides.
  - For each observation epoch and each satellite PRN, attempts to select the valid navigation record.

  Input:
    - RINEX 3.04 navigation file name                       fnNav
    - RINEX 3.04 observation file name                      fnObs

  Output:
    - list of observations with matched
      navigation recrods                                    matches
    
  Print of run:
  Total observations: 7161
  Without matched ephemeris: 0
  
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MatchEphToObs where

import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict            as MS
import           Data.IntMap.Strict                (IntMap)
import qualified Data.IntMap.Strict         as IMS
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Calendar                (Day, fromGregorian, diffDays, addDays)
import           Data.Time.LocalTime               (LocalTime (..), TimeOfDay(..), diffLocalTime)
import           Data.Time.Format                  (formatTime, defaultTimeLocale)
import           Data.Fixed                        (Pico)
import           Data.Int                          (Int64)
import           Control.Monad                     (guard)
import           Data.Char                         (isSpace)
import           Data.Maybe                        (catMaybes, mapMaybe)    

-- For readDouble
import qualified Data.ByteString.Unsafe     as BSU (unsafeUseAsCString)    
import           Foreign                           (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types                   (CChar, CDouble(CDouble)) 
import           Foreign.C.String                  (CString)                 
import           System.IO.Unsafe                  (unsafePerformIO)         


type GpsTime    = LocalTime
type GpsWeekTow = (Integer, Pico)
type NavMap     = IntMap (Map GpsWeekTow NavRecord)         -- ^ key1: prn, key2: (week r, toe r)    
type ObsEpoch   = (GpsTime,[Int])

-- | GPS navigation data record from RINEX 3.04 navigation file.
data NavRecord = NavRecord
  { prn      :: Int               -- ^ satellite number
  , toc      :: GpsTime           -- ^ clock data reference time
  , af0      :: Double            -- ^ SV clock bias correction coefficient [s]
  , af1      :: Double            -- ^ SV clock drift correction coefficient [s/s]
  , af2      :: Double            -- ^ SV clock drift rate correction coefficient [s/s^2]
  , iode     :: Int               -- ^ issue-of-data, ephemeris; ephemeris data issue number,
  , crs      :: Double            -- ^ orbital radius correction [m]
  , deltaN   :: Double            -- ^ mean motion difference [rad/s]
  , m0       :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc      :: Double            -- ^ latitude argument correction [rad]
  , e        :: Double            -- ^ eccentricity []
  , cus      :: Double            -- ^ latitude argument correction [rad]
  , sqrtA    :: Double            -- ^ sqare root of semi-major axis [m^0.5]
  , toe      :: Pico              -- ^ time of ephemeris in GPS week (time-of-week of ephemeris) [s]
  , cic      :: Double            -- ^ inclination correction [rad]
  , omega0   :: Double            -- ^ longitude of ascending node at toe epoch [rad]
  , cis      :: Double            -- ^ inclination correction [rad]
  , i0       :: Double            -- ^ inclination at reference epoch [rad]
  , crc      :: Double            -- ^ orbital radius corrcetion [m]
  , omega    :: Double            -- ^ argument of perigee [rad]
  , omegaDot :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot     :: Double            -- ^ rate of inclination angle [rad/s]
  , week     :: Integer           -- ^ number of GPS week for toe and toc
  , svHealth :: Int               -- ^ SV health, 0 means ok
  , iodc     :: Int               -- ^ issue-of-data, clock; clock data issue number
  , ttom     :: Double            -- ^ transmission time of message - time stamp given by receiver [s]
  , fitIntv  :: Int               -- ^ fit interval, ephemeris validity interval related to toe [h]
  } deriving (Eq, Show)

data Match = Match
  { tro           :: GpsTime                        -- epoch time
  , obsPrn        :: Int
  , navRecord     :: Maybe NavRecord
  } deriving Show
               
main :: IO ()
main = do
  let fnNav = "source.nav"                                  -- Input: RINEX 3.04 navigation file name
      fnObs = "source.obs"                                  -- Input: RINEX 3.04 observation file name
  bsNav <- L8.readFile fnNav
  bsObs <- L8.readFile fnObs

  let navMap    = readNavRinex304 bsNav
      obsEpochs = readObsRinex304 bsObs
      matches   = matchAll obsEpochs navMap                 -- Output: list of observations with matched navigation recrods

  putStrLn $ "Total observations: " ++ show (length matches)
  putStrLn $ "Without matched ephemeris: " ++ show (length (filter (\m -> navRecord m == Nothing) matches))

-- | Matches all observation epochs with corresponding navigation records.
matchAll
    :: [ObsEpoch]                                           -- ^ list of observation epochs, each containing the
                                                            --   observation time (GpsCalendarTime) and a list of satellite PRNs
    -> NavMap                                               -- ^ GPS navigation data map, organized by PRN and ephemeris time
    -> [Match]
matchAll epochs navMap =
  [ Match { tro       = t
          , obsPrn    = p
          , navRecord = selectGpsEphemeris t p navMap
          }
  | (t, prns) <- epochs
  , p <- prns
  ]      

-- | Selects the appropriate GPS ephemeris record for a given observation.
--   Converts the observation time into GPS week and time-of-week
--   using gpsTimeToWeekTow.
--   Looks up the satellite PRN in the navigation map.
--   If no navigation data exists for that PRN, returns 'Nothing'.
--   Otherwise, searches the satellite's ephemeris records with
--   findValidEphemeris to find the one valid at the observation time.
selectGpsEphemeris tro prn navMap =
    let wto = gpsTimeToWeekTow tro
    in case IMS.lookup prn navMap of
         Nothing -> Nothing
         Just m  -> findValidEphemeris wto m

-- | Parses a RINEX 3.04 navigation file into a NavMap.
--   Validates the file header.
--   Splits the navigation data into 8-line chunks.
--   Converts each chunk into a NavRecord.
--   Builds a map.
readNavRinex304 :: L8.ByteString -> NavMap
readNavRinex304 bs
    | L8.null bs         = error "Empty file"
    | rinexVer /= "3.04" = error "Not RINEX 3.04 file"
    | fileType /= "N"    = error "Not navigation file"
    | otherwise =
        let ls = L8.lines bs
            rest = dropWhile (not . isEndOfHeader) ls
        in parseNavData (drop 1 rest)
      where
        rinexVer = trim $ getField  0 9 bs 
        fileType = trim $ getField 20 1 bs
        isEndOfHeader line =
            trim (L8.drop 60 line) == "END OF HEADER"

-- | Parses raw navigation file lines into a NavMap.
--   Each GPS navigation record spans 8 lines.
--   The function splits the input into 8-line chunks,
--   parses each chunk with readGpsNavRec,
--   discards invalid records (Nothing),
--   and builds a NavMap from the valid ones.
--   Throws an error if the input list is empty.                 
parseNavData [] = error "No navigation data"
parseNavData ls = buildNavMap
                  . catMaybes
                  . map readGpsNavRec
                  $ chunks8 ls

-- | Splits a list into chunks of 8 elements. Used to group navigation records.           
chunks8 :: [a] -> [[a]]
chunks8 [] = []
chunks8 l  = chunk : chunks8 rest
    where
      (chunk, rest) = splitAt 8 l                      
    
-- | Constructs a NavMap from a list of NavRecord values for healthy satellites.
buildNavMap :: [NavRecord] -> NavMap
buildNavMap rs =
  IMS.fromListWith MS.union
    [ (prn r, MS.singleton (week r, toe r) r)
    | r <- rs, svHealth r == 0]

-- |Finds the nearest valid ephemeris record for observation (GPS week, tow).
findValidEphemeris 
  :: GpsWeekTow                                             -- observation (GPS w, tow)
  -> Map GpsWeekTow NavRecord                                  -- nav records of one GPS satellite
  -> Maybe NavRecord
findValidEphemeris wto m = do
    r <- nearestNavRecord wto m
    if isEphemerisValid wto r
    then Just r
    else Nothing

-- | Finds the nearest ephemeris record for observation time.         
nearestNavRecord
    :: GpsWeekTow
    -> Map GpsWeekTow NavRecord                                -- nav records of one GPS satellite
    -> Maybe NavRecord
nearestNavRecord wto m =
    let mLE = MS.lookupLE wto m
        mGE = MS.lookupGE wto m

        choose (Just (_,r1)) (Just (_,r2)) =
            if    abs (diffGpsWeekTow (week r1, toe r1) wto)
               <= abs (diffGpsWeekTow (week r2, toe r2) wto)
            then Just r1
            else Just r2
        choose (Just (_,r1))  Nothing      = Just r1
        choose Nothing       (Just (_,r2)) = Just r2
        choose _ _ = Nothing
                     
    in choose mLE mGE

readGpsNavRec :: [L8.ByteString] -> Maybe NavRecord
readGpsNavRec ch@(l1:_) = do
  (sys, _) <- L8.uncons l1                                
  guard (sys == 'G')
  case readFields ch of
    Nothing -> error "Parse navigation record error"
    r       -> r

-- | Parses a single GPS navigation record from eight consecutive lines of a RINEX 3.04 navigation file.
--   Expects exactly 8 lines (l1–l8).
--   Converts numeric fields using readDoubleField.
--   Returns Nothing if the input does not match the expected format or if the satellite system is not GPS (sys == 'G').
--   On success, constructs and returns a NavRecord
readFields :: [L8.ByteString] -> Maybe NavRecord
readFields [l1,l2,l3,l4,l5,l6,l7,l8] = do
  (sys, _) <- L8.uncons l1                                
  guard (sys == 'G')
  (prn, _)  <- L8.readInt $ getField  1 2 l1     
  (y  , _)  <- L8.readInt $ getField  4 4 l1
  (mon, _)  <- L8.readInt $ getField  9 2 l1
  (d  , _)  <- L8.readInt $ getField 12 2 l1
  (h  , _)  <- L8.readInt $ getField 15 2 l1
  (m  , _)  <- L8.readInt $ getField 18 2 l1
  (s  , _)  <- L8.readInt $ getField 21 2 l1
  
  let toc = mkGpsTime (toInteger y) mon d h m (fromIntegral s)

  af0       <- readDoubleField $ getField 23 19 l1
  af1       <- readDoubleField $ getField 42 19 l1
  af2       <- readDoubleField $ getField 61 19 l1

  iodeD     <- readDoubleField $ getField  4 19 l2
  crs       <- readDoubleField $ getField 23 19 l2
  deltaN    <- readDoubleField $ getField 42 19 l2
  m0        <- readDoubleField $ getField 61 19 l2

  cuc       <- readDoubleField $ getField  4 19 l3
  e         <- readDoubleField $ getField 23 19 l3
  cus       <- readDoubleField $ getField 42 19 l3
  sqrtA     <- readDoubleField $ getField 61 19 l3

  toeD      <- readDoubleField $ getField  4 19 l4
  cic       <- readDoubleField $ getField 23 19 l4
  omega0    <- readDoubleField $ getField 42 19 l4
  cis       <- readDoubleField $ getField 61 19 l4

  i0        <- readDoubleField $ getField  4 19 l5
  crc       <- readDoubleField $ getField 23 19 l5
  omega     <- readDoubleField $ getField 42 19 l5
  omegaDot  <- readDoubleField $ getField 61 19 l5                 
                                                               
  iDot      <- readDoubleField $ getField  4 19 l6
  weekD     <- readDoubleField $ getField 42 19 l6

  svHealthD <- readDoubleField $ getField 23 19 l7
  iodcD     <- readDoubleField $ getField 61 19 l7                 
                     
  ttom      <- readDoubleField $ getField  4 19 l8
  fitIntvD  <- readDoubleField $ getField 23 19 l8

  let iode     = round      iodeD
      toe      = realToFrac toeD
      week     = round      weekD                           -- conversion is needed for equality comparisons
      svHealth = round      svHealthD
      iodc     = round      iodcD
      fitIntv  = round      fitIntvD
              
  return NavRecord {..}
readFields _ = Nothing       

-- | Calculates the number of seconds between two (GPS week, tow).
diffGpsWeekTow
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week [s]
    -> GpsWeekTow                                           -- ^ GPS week, time-of-week [s]
    -> Pico                                                 -- ^ time difference [s]
diffGpsWeekTow (w2,tow2) (w1,tow1) =
    fromInteger (dw * 604800) + dtow
    where
      dw   = w2   - w1
      dtow = tow2 - tow1

-- | Ephemeris validity check based on fitInterval ephemeris field for
--   a given observation time
isEphemerisValid
  :: GpsWeekTow                                             -- GPS week, time-of-week
  -> NavRecord
  -> Bool
isEphemerisValid (w, tow) eph =
    abs diffTime <= halfFitIntv
    where
      diffTime = diffGpsWeekTow  (w, tow) (week eph, toe eph)
      halfFitIntv = realToFrac ((fitIntv eph) `div` 2 * 3600)                  
                   
readObsRinex304 :: L8.ByteString -> [ObsEpoch]
readObsRinex304 bs
  | L8.null bs         = error "Empty file"
  | rinexVer /= "3.04" = error "Not RINEX 3.04 file"
  | fileType /= "O"    = error "Not an observation file"
  | otherwise =
      let ls   = L8.lines bs                        
          rest = dropWhile (not . isEndOfHeader) ls
      in parseEpochs (drop 1 rest)
  where
    rinexVer = trim $ getField  0 9 bs
    fileType = trim $ getField 20 1 bs
    isEndOfHeader line = trim (L8.drop 60 line) == L8.pack "END OF HEADER"           

parseEpochs :: [L8.ByteString] -> [ObsEpoch]
parseEpochs [] = []
parseEpochs (l:ls)
  | L8.isPrefixOf ">" l  =
      case parseEpoch (l:ls) of
        Just (epoch, rest) -> epoch : parseEpochs rest
        Nothing            -> error "Cannot parse observation data"
  | otherwise = error "Unexpected line: expected '>' at start of epoch"

parseEpoch :: [L8.ByteString] -> Maybe (ObsEpoch, [L8.ByteString])
parseEpoch (l:ls) = do
  (y  , _)  <- L8.readInt      $ getField  2  4 l
  (mon, _)  <- L8.readInt      $ getField  7  2 l
  (d  , _)  <- L8.readInt      $ getField 10  2 l
  (h  , _)  <- L8.readInt      $ getField 13  2 l
  (m  , _)  <- L8.readInt      $ getField 16  2 l
  s         <- readDoubleField $ getField 19 11 l

  let obsGpsTime = mkGpsTime (toInteger y) mon d h m (realToFrac s)               

  (n  , _)  <- L8.readInt $ getField 33  3 l
               
  let prns = mapMaybe (\line -> fmap fst (L8.readInt (getField 1 2 line)))
                      (take n ls)
             
  return ((obsGpsTime, prns), drop n ls)
parseEpoch [] = Nothing
               
-- | Makes GpsTime from numbers.
mkGpsTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsTime
mkGpsTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

-- | Function import: double strtod(const char *nptr, char **endptr)
foreign import ccall unsafe "stdlib.h strtod"
    c_strtod :: CString -> Ptr (Ptr CChar) -> IO CDouble

-- | 2025-11-01 Data.ByteString.Char8 does not have a readDouble function.
--   Reads Double value from Data.ByteString.Lazy.Char8 ByteString.
--   unsafeUseAsCString function needs strict argument and L8.drop
--   needs Int64 argument.
readDouble :: L8.ByteString -> Maybe (Double, L8.ByteString)
readDouble bs = unsafePerformIO $
    BSU.unsafeUseAsCString (L8.toStrict bs) $ \cstr -> 
      alloca $ \endPtr -> do
        val <- c_strtod cstr endPtr
        end <- peek endPtr
        if end == cstr
          then return Nothing
          else do
            let offset = end `minusPtr` cstr
            let rest   = L8.drop (fromIntegral offset) bs
            return (Just (realToFrac val, rest))

-- | Reads Double value from ByteString field.
--   Its purpose is to stop reading if it cannot read the entire field.
--   After reading, the rest may be empty or consist only of spaces.
readDoubleField :: L8.ByteString -> Maybe Double
readDoubleField bs = do
  (val, rest) <- readDouble bs
  case L8.uncons rest of
    Just (c, _) | c=='D' || c=='d' ->
      error $ "Unsupported number format with 'D': " ++ L8.unpack bs
    _ -> guard (L8.all (== ' ') rest) >> return val   

-- | Extract a substring from a line:
--   starting at position 'start' (0-based),
--   with length 'len'.
--   Used to read fixed-width fields.       
getField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
getField start len = L8.take len . L8.drop start
                                  
-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

-- | Converts a GPS time into GPS week and time-of-week (tow).
--   GPS time starts from January 6, 1980.
--   Calculates the number of weeks and seconds since that epoch.
gpsTimeToWeekTow
    :: GpsTime
    -> GpsWeekTow                                           -- ^ GPS week, time-of-week
gpsTimeToWeekTow (LocalTime date (TimeOfDay h m s)) =
    let gpsStartDate = fromGregorian 1980 1 6               -- the date from which the GPS time is counted
        days         = diffDays date gpsStartDate           -- number of days since GPS start date
        w            = days `div` 7                         -- GPS week
        dow          = days `mod` 7                         -- GPS day-of-week
        tow          = fromIntegral ( dow * 86400
                                    + toInteger (h * 3600 + m * 60)
                                    )
                     + s
    in (w, tow)

-- | Converts GPS week and time-of-week (tow) into GPS time
weekTowToGpsTime
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> GpsTime                                              -- ^ GPS time
weekTowToGpsTime (w, tow) =
    let gpsStartDate = fromGregorian 1980 1 6                       -- the date from which the GPS time is counted
        days         = w * 7 + (floor (tow / 86400) :: Integer)     -- number of days since GPS start date
        date         = addDays days gpsStartDate
        sod          = tow
                     - fromIntegral (floor (tow / 86400) * 86400)   -- GPS second-of-day
        h            = floor (sod / 3600)
        m            = floor ((sod - fromIntegral (h*3600)) / 60)
        s            = sod - fromIntegral (h*3600 + m*60)              
    in LocalTime date (TimeOfDay h m s)        
