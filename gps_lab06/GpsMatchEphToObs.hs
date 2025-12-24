-- 2025-12-24

{-| The program matches navigation records from a RINEX 3.04 NAV file
    containing epehemerides to satellite observations from a RINEX
    3.04 OBS file. Filtered navigation records containing ephemerides
    are attached to the corresponding observations.

    Main steps of the algorithm:

    1. Build a map of healthy navigation records from the body of a
    RINEX 3.04 navigation file, keeping only the record with the
    maximum IODE for each (week, toe).
     
    2. Build a list of GPS observation records from RINEX 3.04
    observation file.
    
    3. Attach navigation records with GPS ephemerides from built map
    to GPS observations using navSelectEphemeris function.

    Input:
      - RINEX 3.04 navigation file name                navFn     defined in the code
      - RINEX 3.04 observation file name               obsFn     defined in the code

    Output:
    - observation records with attached
      ephemerides to observations                      obsNavRs
        
    
    Print of run:
    Total observations: 7161
    Without matched ephemeris: 0
  
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GpsMatchEphToObs where

import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict            as MS
import           Data.IntMap.Strict                (IntMap)
import qualified Data.IntMap.Strict         as IMS
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Calendar                (fromGregorian, diffDays)
import           Data.Time.LocalTime               (LocalTime (..), TimeOfDay(..))
import           Data.Fixed                        (Pico)
import           Data.Int                          (Int64)
import           Control.Monad                     (guard)
import           Data.Char                         (isSpace)
import           Text.Printf                       (printf)

-- For readDouble
import qualified Data.ByteString.Unsafe     as BSU (unsafeUseAsCString)    
import           Foreign                           (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types                   (CChar, CDouble(CDouble)) 
import           Foreign.C.String                  (CString)                 
import           System.IO.Unsafe                  (unsafePerformIO)         

-- | GPS navigation data record from RINEX 3.04 navigation file.
data NavRecord = NavRecord
  { navPrn      :: Int               -- ^ satellite number
  , toc         :: GpsTime           -- ^ clock data reference time
  , af0         :: Double            -- ^ SV clock bias correction coefficient [s]
  , af1         :: Double            -- ^ SV clock drift correction coefficient [s/s]
  , af2         :: Double            -- ^ SV clock drift rate correction coefficient [s/s^2]
  , iode        :: Int               -- ^ issue-of-data, ephemeris; ephemeris data issue number,
  , crs         :: Double            -- ^ orbital radius correction [m]
  , deltaN      :: Double            -- ^ mean motion difference [rad/s]
  , m0          :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc         :: Double            -- ^ latitude argument correction [rad]
  , e           :: Double            -- ^ eccentricity []
  , cus         :: Double            -- ^ latitude argument correction [rad]
  , sqrtA       :: Double            -- ^ sqare root of semi-major axis [m^0.5]
  , toe         :: Pico              -- ^ time of ephemeris in GPS week (time-of-week of ephemeris) [s]
  , cic         :: Double            -- ^ inclination correction [rad]
  , omega0      :: Double            -- ^ longitude of ascending node at toe epoch [rad]
  , cis         :: Double            -- ^ inclination correction [rad]
  , i0          :: Double            -- ^ inclination at reference epoch [rad]
  , crc         :: Double            -- ^ orbital radius corrcetion [m]
  , omega       :: Double            -- ^ argument of perigee [rad]
  , omegaDot    :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot        :: Double            -- ^ rate of inclination angle [rad/s]
  , week        :: Integer           -- ^ number of GPS week for toe and toc
  , svHealth    :: Int               -- ^ SV health, 0 means ok
  , iodc        :: Int               -- ^ issue-of-data, clock; clock data issue number
  , ttom        :: Double            -- ^ transmission time of message - time stamp given by receiver [s]
  , fitInterval :: Int               -- ^ fit interval, ephemeris validity interval related to toe [h]
  } deriving (Eq, Show)    

type GpsTime    = LocalTime
type GpsWeekTow = (Integer, Pico)
type EphWeekTow = GpsWeekTow
type NavMap     = IntMap (Map EphWeekTow NavRecord)       -- ^ key1:  prn (satellite identifie)
                                                          --   key2:  (week, toe)
                                                          --   value2: navigation record for a healthy satellite
                                                          --           and with max iode for (week, toe)
type ObsTime   = GpsTime                                  -- ^ observation time (epoch)
                                                          --   (don't confuse with observing time)
data Observation = Obs
    { obsPrn  :: Int
    }
type ObsRecord         = (ObsTime, [Observation])
type ObsRecordWithNavs = (ObsTime, [(Observation, Maybe NavRecord)])

main :: IO ()
main = do
  let navFn = "source.nav"                                  -- Input: RINEX 3.04 navigation file name
      obsFn = "source.obs"                                  -- Input: RINEX 3.04 observation file name
  navBs <- L8.readFile navFn
  obsBs <- L8.readFile obsFn

  let navMap   = navMapFromRinex navBs
      obsRs    = obsRecordsFromRinex obsBs
      obsNavRs = obsAttachNavs navMap obsRs                  -- Output: observation records
                                                            -- with attached ephemerides to observations
      numObs =
        foldl' (\acc (_, obss) -> acc + length obss
               ) 0 obsRs
               
      numObsWithoutEphemerides =
        foldl' (\acc (_, xs) -> acc + foldl' count 0 xs
               ) (0::Integer) obsNavRs
          where
            count n (_, Nothing) = n + 1
            count n _            = n
                              
  printf "Total observations: %d\n" numObs
  printf "Without matched ephemerides: %d\n" numObsWithoutEphemerides

-- | Build a navigation map from GPS navigation records of navigation
--   RINEX 3.04 body for healthy satellites and with max iode for
--   (week, toe).
navMapFromRinex :: L8.ByteString -> NavMap  
navMapFromRinex bs0
    | L8.null bs0        = error "Empty file"
    | rnxVer /= "3.04"   = error "Not RINEX 3.04 file"
    | rnxFileType /= "N" = error "Not navigation file"
    | otherwise = let rnxBody = rnxSkipHeader bs0
                  in if L8.null rnxBody
                     then error "Cannot find navigation data in the file."
                     else go IMS.empty rnxBody
      where
        rnxVer      = trim $ takeField  0 9 bs0 
        rnxFileType = trim $ takeField 20 1 bs0
                   
        go :: NavMap -> L8.ByteString -> NavMap
        go m bs
          | L8.null bs = m
          | L8.take 1 bs == "G" =
              let (r, bs') = navReadRecord bs
                  m' = if svHealth r == 0
                       then navInsertRecord r m
                       else m
              in go m' bs' 
          | otherwise =
              go m (navSkipRecord bs)

-- | Skips header of RINEX 3.04 file. Uses information about the label
--   position and the fixed length of the header line content.
rnxSkipHeader :: L8.ByteString -> L8.ByteString
rnxSkipHeader bs0 = loop bs0
  where
    loop bs
      | L8.null bs                  = error "Cannot find header"
      | label bs == "END OF HEADER" = dropLastLine bs
      | otherwise                   = loop (dropLine bs)
      where
        label    = trim . L8.takeWhile (not . (`L8.elem` "\r\n")) . L8.drop 60
        dropLine =        L8.dropWhile        (`L8.elem` "\r\n")  . L8.drop 80
        -- The last line is very often not completed until 80 characters
        dropLastLine = L8.dropWhile        (`L8.elem` "\r\n")
                     . L8.dropWhile (not . (`L8.elem` "\r\n"))
                     . L8.drop 72                           -- don't check before 60 + length "END OF HEADER"


-- | Read GPS satellite navigation record eight lines.  It is based on
--   the knowledge that the content of a line should be 80 characters,
--   but last line often breaks this rule.
navReadRecordLines :: L8.ByteString -> ([L8.ByteString], L8.ByteString)
navReadRecordLines bs0 =
    let (l1, bs1) = readLine bs0
        (l2, bs2) = readLine bs1
        (l3, bs3) = readLine bs2
        (l4, bs4) = readLine bs3
        (l5, bs5) = readLine bs4
        (l6, bs6) = readLine bs5
        (l7, bs7) = readLine bs6
        (l8, bs8) = readLastLine bs7
    in ([l1,l2,l3,l4,l5,l6,l7,l8], bs8)
    where
      -- Read a line of 80 characters and a line separator.
      readLine :: L8.ByteString -> (L8.ByteString, L8.ByteString)
      readLine bs =
          let (line, bs') = L8.splitAt 80 bs
              dropLineSep = L8.dropWhile (`L8.elem` "\r\n")
          in (line, dropLineSep bs')

      --   Last line can have two, three or four fields
      --   and sometimes it is not completed to 80 characters.
      readLastLine :: L8.ByteString -> (L8.ByteString, L8.ByteString)
      readLastLine bs =
          let (part1, bs1) = L8.splitAt 42 bs                         -- read the part containing two fields
              (part2, bs2) = L8.break     (`L8.elem` "\r\n") bs1
              dropLineSep  = L8.dropWhile (`L8.elem` "\r\n")
          in (part1 <> part2, dropLineSep bs2)

-- | Read GPS satellite navigation record
navReadRecord :: L8.ByteString -> (NavRecord, L8.ByteString)
navReadRecord bs =
    let (ls, bs') = navReadRecordLines bs
    in case navGetRecord ls of
         Just r  -> (r, bs')
         Nothing ->
             error $ L8.unpack $ "Unable to get GPS navigation record from \n:"
                               <> L8.unlines ls

-- | Get GPS navigation record from GPS record lines.  Expects 8 lines
--   as input. It does not read fields one by one, as parsers do, but
--   by position in the line.
navGetRecord :: [L8.ByteString] -> Maybe NavRecord
navGetRecord ls =
  case ls of
    [l1,l2,l3,l4,l5,l6,l7,l8] -> do
            (navPrn, _)  <- L8.readInt $ trim $ takeField  1 2 l1               -- trim is needed by readInt
            (y     , _)  <- L8.readInt $ trim $ takeField  4 4 l1
            (mon   , _)  <- L8.readInt $ trim $ takeField  9 2 l1
            (d     , _)  <- L8.readInt $ trim $ takeField 12 2 l1
            (h     , _)  <- L8.readInt $ trim $ takeField 15 2 l1
            (m     , _)  <- L8.readInt $ trim $ takeField 18 2 l1
            (s     , _)  <- L8.readInt $ trim $ takeField 21 2 l1
  
            let toc = mkGpsTime (toInteger y) mon d h m (fromIntegral s)

            af0       <- readDoubleField $ takeField 23 19 l1
            af1       <- readDoubleField $ takeField 42 19 l1
            af2       <- readDoubleField $ takeField 61 19 l1
                 
            iodeD     <- readDoubleField $ takeField  4 19 l2
            crs       <- readDoubleField $ takeField 23 19 l2
            deltaN    <- readDoubleField $ takeField 42 19 l2
            m0        <- readDoubleField $ takeField 61 19 l2
                 
            cuc       <- readDoubleField $ takeField  4 19 l3
            e         <- readDoubleField $ takeField 23 19 l3
            cus       <- readDoubleField $ takeField 42 19 l3
            sqrtA     <- readDoubleField $ takeField 61 19 l3

            toeD      <- readDoubleField $ takeField  4 19 l4
            cic       <- readDoubleField $ takeField 23 19 l4
            omega0    <- readDoubleField $ takeField 42 19 l4
            cis       <- readDoubleField $ takeField 61 19 l4

            i0        <- readDoubleField $ takeField  4 19 l5
            crc       <- readDoubleField $ takeField 23 19 l5
            omega     <- readDoubleField $ takeField 42 19 l5
            omegaDot  <- readDoubleField $ takeField 61 19 l5
                                                               
            iDot      <- readDoubleField $ takeField  4 19 l6
            weekD     <- readDoubleField $ takeField 42 19 l6

            svHealthD <- readDoubleField $ takeField 23 19 l7
            iodcD     <- readDoubleField $ takeField 61 19 l7
                     
            ttom      <- readDoubleField $ takeField  4 19 l8
            fitIntervalD  <- readDoubleField $ takeField 23 19 l8

            let iode         = round      iodeD
                toe          = realToFrac toeD
                week         = round      weekD             -- conversion is needed for equality comparisons
                svHealth     = round      svHealthD
                iodc         = round      iodcD
                fitInterval  = round      fitIntervalD
            return NavRecord {..}
    _ -> Nothing

-- | Skip record reading lines to begining of other record.  Used to
--   skip records of constellations other than GPS.
navSkipRecord :: L8.ByteString -> L8.ByteString
navSkipRecord bs =
  let (_, rest) = L8.break (== '\n') bs
      rest' = L8.drop 1 rest
      in if navIsNewRecordLine rest'
           then rest'
           else navSkipRecord rest'

-- | Returns True if the bs starts with other sign than ' '                
navIsNewRecordLine :: L8.ByteString -> Bool
navIsNewRecordLine bs = L8.take 1 bs /= " "

-- | Insert a navigation record into a 'NavMap'. If there is no entry
--   for the given PRN or epoch, the record is inserted. If an entry
--   already exists, the record is replaced only if the new record has
--   a greater IODE than the existing one.  This ensures that for each
--   @(week, toe)@ only the navigation record with the maximum IODE is
--   kept.
navInsertRecord :: NavRecord -> NavMap -> NavMap
navInsertRecord r =
  IMS.alter updatePrn key1
  where
    key1 = navPrn r
    key2 = (week r, toe r)
    updatePrn Nothing =
        Just (MS.singleton key2 r)
    updatePrn (Just subMap) =
        Just (MS.alter (chooseNewer r) key2 subMap)

    chooseNewer :: NavRecord -> Maybe NavRecord -> Maybe NavRecord
    chooseNewer new Nothing    = Just new
    chooseNewer new (Just old) =
        if iode new > iode old
        then Just new
        else Just old

                       
-- | Selects a navigation record for a given observation time and
--   satellite PRN from NavMap. The navigation record with the nearest
--   (week, toe) to the specified observation time is selected.
navSelectEphemeris
    :: GpsTime
    -> Int
    -> NavMap
    -> Maybe NavRecord
navSelectEphemeris tobs prn navMap = do
    subMap <- IMS.lookup prn navMap
    let t  = gpsTimeToWeekTow tobs
        past   = MS.lookupLE t subMap
        future = MS.lookupGE t subMap
        closest = case (past, future) of
          (Just (wtoeP, rP), Just (wtoeF, rF)) ->
              if abs (diffGpsWeekTow t wtoeP) <= abs (diffGpsWeekTow wtoeF t)
              then Just (wtoeP, rP)
              else Just (wtoeF, rF)
          (Just p, Nothing)  -> Just p
          (Nothing, Just f)  -> Just f
          (Nothing, Nothing) -> Nothing                              
    (_, r) <- closest
    if isEphemerisValid t r
      then Just r
      else Nothing

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

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

-- | Ephemeris validity check based on fit interval ephemeris field for
--   a given observation time
isEphemerisValid
  :: GpsWeekTow                                             -- GPS week, time-of-week
  -> NavRecord
  -> Bool
isEphemerisValid (w, tow) eph =
    abs diffTime <= halfFitInterval
    where
      diffTime        = diffGpsWeekTow  (w, tow) (week eph, toe eph)
      halfFitInterval = realToFrac ((fitInterval eph) `div` 2 * 3600)                  

-- | Build an observation record list from GPS observation records of
--   observation RINEX 3.04 body.
obsRecordsFromRinex :: L8.ByteString -> [ObsRecord]
obsRecordsFromRinex bs
  | L8.null bs            = error "Empty file"
  | rnxVer      /= "3.04" = error "Not RINEX 3.04 file"
  | rnxFileType /= "O"    = error "Not an observation file"
  | otherwise = let rnxBody = rnxSkipHeader $ bs
                in if L8.null rnxBody
                   then error "Cannot find navigation data in the file."
                   else go [] (L8.lines rnxBody)
  where
    rnxVer      = trim $ takeField  0 9 bs
    rnxFileType = trim $ takeField 20 1 bs

    go obss [] = obss
    go obss (l:ls)
        | L8.isPrefixOf ">" l  =
            case  obsReadRecord (l:ls) of
              Just (r, rest) -> go (r:obss) rest
              Nothing        ->
                  error $ L8.unpack $ "Unable to read observation record \
                                      \from text starting with:\n" <> l
        | otherwise = error "Unexpected line: expected '>' at start of record"

-- | Read observation time (epoch) with the observations of a GPS satellite
obsReadRecord :: [L8.ByteString] -> Maybe (ObsRecord, [L8.ByteString])
obsReadRecord [] = Nothing                         
obsReadRecord (l:ls) = do
  (y  , _) <- L8.readInt      $ takeField  2  4 l
  (mon, _) <- L8.readInt      $ takeField  7  2 l
  (d  , _) <- L8.readInt      $ takeField 10  2 l
  (h  , _) <- L8.readInt      $ takeField 13  2 l
  (m  , _) <- L8.readInt      $ takeField 16  2 l
  s        <- readDoubleField $ takeField 19 11 l
  let !tobs = mkGpsTime (toInteger y) mon d h m (realToFrac s)               
  (n  , _) <- L8.readInt $ takeField 33  3 l                          -- number of satellites observed in current epoch
                                                                      -- (observation time)
  let (obsLines, rest) = splitAt n ls
      gpsLines = filter (\line -> L8.take 1 line == "G") obsLines
  prns <- mapM (\line -> do
                  (n, _) <- L8.readInt (takeField 1 2 line)
                  return n
               ) gpsLines
          
  let obss = map Obs prns
            
  return ((tobs, obss), rest)
               
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
takeField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start
                                  
-- | Conversion of GPS time to GPS week and time-of-week
gpsTimeToWeekTow
    :: GpsTime
    -> GpsWeekTow
gpsTimeToWeekTow (LocalTime date (TimeOfDay h m s)) =
    let gpsStartDate = fromGregorian 1980 1 6               -- the date from which the GPS time is counted
        days         = diffDays date gpsStartDate           -- number of days since GPS start date
        (w, dow)     = days `divMod` 7                      -- GPS week, GPS day-of-week
        tow          =
            fromIntegral ( dow * 86400
                         + toInteger (h * 3600 + m * 60)
                         )
            + s
    in (w, tow)

-- | Attach navigation records with ephemerides to observation records
obsAttachNavs :: NavMap -> [ObsRecord] -> [ObsRecordWithNavs]
obsAttachNavs navMap obsRs =
 [ (tobs, [ (obs, r)
          | obs <- obss
          , let r = navSelectEphemeris tobs (obsPrn obs) navMap
          ]
   )
 | (tobs, obss) <- obsRs
 ]




