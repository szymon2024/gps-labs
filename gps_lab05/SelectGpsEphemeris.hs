-- 2025-11-26

{- | The program selects the ephemeris (orbital parameters) from a RINEX
     3.04 navigation file for a given observation time and a GPS
     satellite.

     Main steps of the algorithm:
     
     1. The navigation file (RINEX 3.04) is read, excluding the
     header, and records with svHealth == 0 are stored in a map.
     
     2. The submap containing records for the given PRN is
     selected. If the submap does not exists program terminates.
     
     3. The navigation record with the nearest (week, toe) to the
     specified observation time is selected. If no record can be
     selected, the program terminates.
     
     4. The program then checks whether the selected record lies
     within the fitInterval relative to toe.  If the record satisfies
     this condition, it is printed. Otherwise the program terminates.

     Input:
       - RINEX 3.04 navigation file name                    fn
       - receiver time of signal reception
         (observation time)                                 obsGpsCalTime
       - satellite number                                   prn

     Output:
       - navigation record (ephemeris)                      r

     Print of run:
     Observation time: 2025 08 02 01 00 01.5
               calToe: 2025 08 02 02 00 00
     PRN: 6    calToc: 2025 08 02 02 00 00
     af0:      -4.722196608782e-4         
     af1:      -1.432454155292e-11        
     af2:      0.0                        
     iode:     69                         
     crs:      -23.6875                   
     deltaN:   3.821944913632e-9          
     m0:       -2.959416369262            
     cuc:      -1.28336250782e-6          
     e:        3.335768124089e-3          
     cus:      4.727393388748e-6          
     sqrtA:    5153.617370605             
     toe:      525600.0                   
     cic:      8.940696716309e-8          
     omega0:   0.681838248157             
     cis:      4.470348358154e-8          
     i0:       0.9884960063693            
     crc:      305.40625                  
     omega:    -0.669483880108            
     omegaDot: -7.833540584123e-9         
     iDot:     -1.753644474903e-10        
     week:     2377                       
     svHealth: 0                          
     iodc:     69                         
     ttom:     518400.0                   
     fitIntv:  4                          
     ----------------------------------

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SelectGpsEphemeris where

import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict    as MS
import           Data.IntMap.Strict                (IntMap)
import qualified Data.IntMap.Strict as IMS
import           Data.Char                         (isSpace)
import           Data.Int                          (Int64)
import           Control.Monad                     (guard, when)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Calendar                (Day, fromGregorian, diffDays, addDays)
import           Data.Time.LocalTime               (LocalTime (..), TimeOfDay(..), diffLocalTime)
import           Data.Time.Format                  (formatTime, defaultTimeLocale)
import           Data.Fixed                        (Pico)
import           Text.Printf                       (printf)
import           Data.List                         (unfoldr)

-- For readDouble
import qualified Data.ByteString.Unsafe     as BSU (unsafeUseAsCString)    
import           Foreign                           (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types                   (CChar, CDouble(CDouble)) 
import           Foreign.C.String                  (CString)                 
import           System.IO.Unsafe                  (unsafePerformIO)         
import           Data.Maybe                        (catMaybes)

-- For NavMap printing
import           Data.ByteString.Builder
import           System.IO                         (stdout)
    
type GpsCalendarTime  = LocalTime
type GpsTime          = (Integer, Pico)                     -- ^ GPS week, time-of-week
type NavMap           = IntMap (Map GpsTime NavRecord)      -- ^ key1: prn, key2: (week r, toe r)


-- | GPS navigation data record from RINEX 3.04 navigation file.
data NavRecord = NavRecord
  { prn      :: Int               -- ^ satellite number
  , calToc   :: GpsCalendarTime   -- ^ toc as calendar date - clock data reference time
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
  , toe      :: Pico              -- ^ time of ephemeris in GPS week [s]
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
  } deriving (Show)

-- Entry point of the program.
main = do
  let fn = "source.nav"                                        -- Input: RINEX 3.04 navigation file name
  bs <- L8.readFile fn               
  let navMap        = readNavRinex304 bs
      prn           = 6                                        -- Input: satellite number
      obsGpsCalTime = mkGpsCalendarTime 2025 08 02 01 00 01.5  -- Input: receiver time of signal reception
  case selectGpsEphemeris obsGpsCalTime prn navMap of
    Nothing -> putStrLn "Cannot find valid ephemeris \
                        \for given prn and observation time"
    Just r  -> do                                        -- Output: navigation record 
         putStrLn $ "Observation time: " ++ formatTime defaultTimeLocale "%Y %m %d %H %M %S" obsGpsCalTime
         L8.hPut stdout $ toLazyByteString $ buildEntry r

-- | Selects the appropriate GPS ephemeris record for a given observation.
--   Converts the observation time into GPS week and time-of-week
--   using gpsCalTimeToWeekTow.
--   Looks up the satellite PRN in the navigation map.
--   If no navigation data exists for that PRN, returns 'Nothing'.
--   Otherwise, searches the satellite's ephemeris records with
--   findValidEphemeris to find the one valid at the observation time.
selectGpsEphemeris
  :: GpsCalendarTime -> IMS.Key -> NavMap -> Maybe NavRecord
selectGpsEphemeris obsGpsCalTime prn navMap =
    let obsGpsTime = gpsCalTimeToWeekTow obsGpsCalTime
    in case IMS.lookup prn navMap of
         Nothing -> Nothing
         Just m  -> findValidEphemeris obsGpsTime m

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
--   parses each chunk with parseGpsNavRec,
--   discards invalid records (Nothing),
--   and builds a NavMap from the valid ones.
--   Throws an error if the input list is empty.
parseNavData
    :: [L8.ByteString]                                      -- ^ lines from a RINEX navigation data (body) 
    -> NavMap
parseNavData [] = error "No navigation data"
parseNavData ls = buildNavMap
                  . catMaybes
                  . map parseGpsNavRec
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

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

-- |Finds the nearest valid ephemeris record for observation time.
findValidEphemeris 
  :: GpsTime                                                -- observation time
  -> Map GpsTime NavRecord                                  -- nav records of one GPS satellite
  -> Maybe NavRecord
findValidEphemeris obsGpsTime m = do
    r <- nearestNavRecord obsGpsTime m
    if isEphemerisValid obsGpsTime r
    then Just r
    else Nothing

-- | Finds the nearest ephemeris record for observation time.         
nearestNavRecord
    :: GpsTime                                              -- observation time
    -> Map GpsTime NavRecord                                -- nav records of one GPS satellite
    -> Maybe NavRecord
nearestNavRecord obsGpsTime m =
    let mLE = MS.lookupLE obsGpsTime m
        mGE = MS.lookupGE obsGpsTime m

        choose (Just (_,r1)) (Just (_,r2)) =
            if    abs (diffGpsTime (week r1, toe r1) obsGpsTime)
               <= abs (diffGpsTime (week r2, toe r2) obsGpsTime)
            then Just r1
            else Just r2
        choose (Just (_,r1))  Nothing      = Just r1
        choose Nothing       (Just (_,r2)) = Just r2
        choose _ _ = Nothing
                     
    in choose mLE mGE


-- | Calculates the number of seconds between two GPS times.
diffGpsTime
    :: GpsTime                                               -- ^ GPS week, time-of-week [s]
    -> GpsTime                                               -- ^ GPS week, time-of-week [s]
    -> Pico                                                  -- ^ time difference [s]
diffGpsTime (w2,tow2) (w1,tow1) =
    fromInteger (dw * 604800) + dtow
    where
      dw   = w2   - w1
      dtow = tow2 - tow1

-- | Converts a GPS calendar time into GPS week number and time-of-week (TOW).
--   GPS time starts from January 6, 1980.
--   Calculates the number of weeks and seconds since that epoch.
gpsCalTimeToWeekTow
    :: GpsCalendarTime                                       -- ^ GPS calendar time
    -> GpsTime                                               -- ^ GPS week, time-of-week
gpsCalTimeToWeekTow (LocalTime date (TimeOfDay h m s)) =
    let gpsStartDate = fromGregorian 1980 1 6                -- the date from which the GPS time is counted
        days         = diffDays date gpsStartDate            -- number of days since GPS start date
        w            = days `div` 7                          -- GPS week
        dow          = days `mod` 7                          -- GPS day-of-week
        tow          = fromIntegral ( dow * 86400
                                    + toInteger (h * 3600 + m * 60)
                                    )
                     + s
    in (w, tow)

-- | Converts GPS week number and time-of-week (TOW) into GPS calendar time
weekTowToGpsCalTime
    :: GpsTime                                              -- ^ GPS week, time-of-week
    -> GpsCalendarTime                                      -- ^ GPS calendar time
weekTowToGpsCalTime (w, tow) =
    let gpsStartDate = fromGregorian 1980 1 6                       -- the date from which the GPS time is counted
        days         = w * 7 + (floor (tow / 86400) :: Integer)     -- number of days since GPS start date
        date         = addDays days gpsStartDate
        sod          = tow
                     - fromIntegral (floor (tow / 86400) * 86400)   -- GPS second-of-day
        h            = floor (sod / 3600)
        m            = floor ((sod - fromIntegral (h*3600)) / 60)
        s            = sod - fromIntegral (h*3600 + m*60)              
    in LocalTime date (TimeOfDay h m s)     

-- | Checks whether an ephemeris record is valid for a given observation time.
--   Compares GPS week and time-of-week with the record’s toe and fitIntv.
isEphemerisValid
  :: GpsTime                                                -- GPS week number, time-of-week
  -> NavRecord
  -> Bool
isEphemerisValid (w, tow) eph 
    |     dw == 0  = abs dtow <= halfFitIntv                -- condition for the same week
    | abs dw == 1  = abs dtow >  halfFitIntv                -- condition for adjacent weeks
    | otherwise    = False
    where
      dw   =   w - week eph
      dtow = tow - toe  eph
      halfFitIntv = realToFrac ((fitIntv eph) `div` 2 * 3600)       
               
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

-- | Parses a single GPS navigation record from eight consecutive lines of a RINEX 3.04 navigation file.
--   Expects exactly 8 lines (l1–l8).
--   Converts numeric fields using readDoubleField.
--   Returns Nothing if the input does not match the expected format or if the satellite system is not GPS (sys == 'G').
--   On success, constructs and returns a NavRecord
parseGpsNavRec :: [L8.ByteString] -> Maybe NavRecord
parseGpsNavRec [l1,l2,l3,l4,l5,l6,l7,l8] = do
  (sys, _) <- L8.uncons l1                                
  guard (sys == 'G')
  (prn, _)  <- L8.readInt $ getField  1 2 l1     
  (y  , _)  <- L8.readInt $ getField  4 4 l1
  (mon, _)  <- L8.readInt $ getField  9 2 l1
  (d  , _)  <- L8.readInt $ getField 12 2 l1
  (h  , _)  <- L8.readInt $ getField 15 2 l1
  (m  , _)  <- L8.readInt $ getField 18 2 l1
  (s  , _)  <- L8.readInt $ getField 21 2 l1
  
  let calToc = mkGpsCalendarTime (toInteger y) mon d h m (fromIntegral s)

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
parseGpsNavRec _ = Nothing

-- | Makes GpsCalendarTime from numbers.
mkGpsCalendarTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsCalendarTime
mkGpsCalendarTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

-- | Builds a textual representation of a navigation record for printing.
--   Produces a Builder that can be efficiently converted to a ByteString.
--   Each field is printed on a separate line with its label and value.
--   Ends with a separator line (----------------------------------).                                  
buildEntry :: NavRecord -> Builder
buildEntry NavRecord{..} =
    string8 "          calToe: "  <> string8 (formatTime defaultTimeLocale "%Y %m %d %H %M %S" calToe) <> char8 '\n'
    <> string8 "PRN: "     <> intDec prn <> string8 "   "
    <> string8 " calToc: "  <> string8 (formatTime defaultTimeLocale "%Y %m %d %H %M %S" calToc) <> char8 '\n'
    <> string8 "af0:      " <> doubleDec  af0             <> char8 '\n'
    <> string8 "af1:      " <> doubleDec  af1             <> char8 '\n'
    <> string8 "af2:      " <> doubleDec  af2             <> char8 '\n'
    <> string8 "iode:     " <> intDec     iode            <> char8 '\n'
    <> string8 "crs:      " <> doubleDec  crs             <> char8 '\n'
    <> string8 "deltaN:   " <> doubleDec  deltaN          <> char8 '\n'
    <> string8 "m0:       " <> doubleDec  m0              <> char8 '\n'
    <> string8 "cuc:      " <> doubleDec  cuc             <> char8 '\n'
    <> string8 "e:        " <> doubleDec  e               <> char8 '\n'
    <> string8 "cus:      " <> doubleDec  cus             <> char8 '\n'
    <> string8 "sqrtA:    " <> doubleDec  sqrtA           <> char8 '\n'
    <> string8 "toe:      " <> doubleDec (realToFrac toe) <> char8 '\n'
    <> string8 "cic:      " <> doubleDec  cic             <> char8 '\n'
    <> string8 "omega0:   " <> doubleDec  omega0          <> char8 '\n'
    <> string8 "cis:      " <> doubleDec  cis             <> char8 '\n'
    <> string8 "i0:       " <> doubleDec  i0              <> char8 '\n'
    <> string8 "crc:      " <> doubleDec  crc             <> char8 '\n'
    <> string8 "omega:    " <> doubleDec  omega           <> char8 '\n'
    <> string8 "omegaDot: " <> doubleDec  omegaDot        <> char8 '\n'
    <> string8 "iDot:     " <> doubleDec  iDot            <> char8 '\n'
    <> string8 "week:     " <> integerDec week            <> char8 '\n'
    <> string8 "svHealth: " <> intDec     svHealth        <> char8 '\n'
    <> string8 "iodc:     " <> intDec     iodc            <> char8 '\n'
    <> string8 "ttom:     " <> doubleDec  ttom            <> char8 '\n'
    <> string8 "fitIntv:  " <> intDec     fitIntv         <> char8 '\n'
    <> string8 "----------------------------------\n"
    where
      calToe = weekTowToGpsCalTime (week, toe)          -- ephemeris reference time in calendar format

               
printPrnNavRecords :: Int -> NavMap -> IO ()
printPrnNavRecords prn navMap =
  case IMS.lookup prn navMap of
    Nothing -> putStrLn $ "No records for PRN " ++ show prn
    Just m  ->
      L8.hPut stdout $ toLazyByteString $
        MS.foldr (\r acc -> buildEntry r <> acc) mempty m
