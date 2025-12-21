-- 2025-12-21

{-| The program matches GPS ephemerides from a RINEX 3.04 NAV file to
    satellite observations from a RINEX 3.04 OBS file. Filtered
    navigation records with ephemerides are attached to the
    corresponding observations.

    Main steps of the algorithm:

    1. Build a map of healthy navigation records, keeping only the
     record with the maximum IODE for each (week, toe) from the
     body of a rinex 3.04 navigation file.
     
    2. Build an observation map of GPS observations.
    
    3. Attach navigation records with GPS ephemerides from built map
    to GPS observations using navGpsSelectEphemeris function.

    Input:
      - RINEX 3.04 navigation file name                fnNav     defined in the code
      - RINEX 3.04 observation file name               fnObs     defined in the code

    Output:
      - GPS observations with attached navigation
        records containing ephemerides                 obsMap'
        
    
    Print of run:
    Total observations: 7161
    Without matched ephemeris: 0
  
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
  { prn         :: Int               -- ^ satellite number
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
type ObsTime    = GpsTime                                 -- ^ observation time (epoch)
                                                          --   (don't confuse with observing time)
type Prn        = Int
type ObsMap     = Map ObsTime (IntMap (Maybe NavRecord))  -- key2: PRN

main :: IO ()
main = do
  let fnNav = "source.nav"                                  -- Input: RINEX 3.04 navigation file name
      fnObs = "source.obs"                                  -- Input: RINEX 3.04 observation file name
  bsNav <- L8.readFile fnNav
  bsObs <- L8.readFile fnObs

  let navMap  = navGpsMapFromRinex bsNav
      obsMap  = obsGpsMapFromRinex bsObs
      obsMap' = obsGpsAttachEphemerides navMap obsMap             -- Output: observations with attached ephemerides
      totalObservations  = sum . MS.map  IMS.size
      withoutEphemerides = sum . MS.map (IMS.size . IMS.filter (==Nothing))
  printf "Total observations: %d\n" (totalObservations obsMap')
  printf "Without matched ephemerides: %d\n" (withoutEphemerides obsMap')

-- | Build a navigation map from healthy GPS navigation records of
-- RINEX 3.04 navigation body.
navGpsMapFromRinex :: L8.ByteString -> NavMap  
navGpsMapFromRinex bs
    | L8.null bs         = error "Empty file"
    | rinexVer /= "3.04" = error "Not RINEX 3.04 file"
    | fileType /= "N"    = error "Not navigation file"
    | otherwise = navReadFilteredGpsRecords (skipHeader bs)
      where
        rinexVer = trim $ getField  0 9 bs 
        fileType = trim $ getField 20 1 bs

-- | Skips header of RINEX 3.04 navigation file.  Uses information
-- about the label position and the fixed length of the header line
-- content.
skipHeader :: L8.ByteString -> L8.ByteString
skipHeader bs0 = loop bs0
  where
    loop bs
      | L8.null bs                  = error "Cannot find header"
      | label bs == "END OF HEADER" = dropLastLine bs
      | otherwise                   = loop (dropLine bs)
      where
        label        = trim . L8.takeWhile (not . (`L8.elem` "\r\n")) . L8.drop 60
        dropLine     =        L8.dropWhile        (`L8.elem` "\r\n")  . L8.drop 80
        -- The last line very ofthen is not completed to 80 characters
        dropLastLine = L8.dropWhile        (`L8.elem` "\r\n")
                     . L8.dropWhile (not . (`L8.elem` "\r\n"))
                     . L8.drop 72                           -- don't check before 60 + length "END OF HEADER"

-- | Extracts GPS navigation records of healthy satellites and with
-- max iode for (week, toe) from RINEX 3.04 navigation body into a
-- NavMap.
navReadFilteredGpsRecords
    :: L8.ByteString                                        -- ^ body of RINEX 3.04 navigation file
    -> NavMap
navReadFilteredGpsRecords bs0
    | L8.null bs0 = error "Cannot find navigation data in the file"
    | otherwise   = loop IMS.empty bs0
    where
      loop :: NavMap -> L8.ByteString -> NavMap
      loop m bs
          | L8.null bs = m
          | L8.take 1 bs == "G" =
              let (ls, rest) = navGpsRecordLines bs
              in case navGpsReadRecord ls of
                Just r | svHealth r == 0 -> loop (navGpsInsertRecord r m) rest
                       | otherwise       -> loop m rest
                Nothing  -> error $ "Cannot read GPS navigation record"
                                    ++ L8.unpack (L8.unlines ls)
          | otherwise =
              let rest = navSkipUnknownRecord bs
              in loop m rest        

-- | Consumes GPS navigation record eight lines.  It is based on the
--   knowledge that the content of a line should be 80 characters, but
--   last line often breaks this rule.
navGpsRecordLines :: L8.ByteString -> ([L8.ByteString], L8.ByteString)
navGpsRecordLines body =
    let (l1, r1) = line body
        (l2, r2) = line r1
        (l3, r3) = line r2
        (l4, r4) = line r3
        (l5, r5) = line r4
        (l6, r6) = line r5
        (l7, r7) = line r6
        (l8, r8) = lastLine r7
    in ([l1,l2,l3,l4,l5,l6,l7,l8], r8)
    where
      line bs = (L8.take 80 bs, L8.dropWhile (`L8.elem` "\r\n") (L8.drop 80 bs))
      --   Last line can have two, three or four fields and sometimes
      --   it is not completed to 80 characters.
      lastLine =
          (\(l42, rest1) ->
               let (lRest, rest2) = L8.break (`L8.elem` "\r\n") rest1
                   l    = l42 <> lRest
                   rest = L8.dropWhile (`L8.elem` "\r\n") rest2
               in (l, rest)
          ) . L8.splitAt 42    

-- | Reads GPS navigation record from record lines for GPS satellite.
--   Expects 8 lines as input.  It does not read fields one by one, as
--   parsers do, but by position in the line.
navGpsReadRecord :: [L8.ByteString] -> Maybe NavRecord
navGpsReadRecord ls =
  case ls of
    [l1,l2,l3,l4,l5,l6,l7,l8] -> do
            (prn, _)  <- L8.readInt $ trim $ getField  1 2 l1               -- trim is needed by readInt
            (y  , _)  <- L8.readInt $ trim $ getField  4 4 l1
            (mon, _)  <- L8.readInt $ trim $ getField  9 2 l1
            (d  , _)  <- L8.readInt $ trim $ getField 12 2 l1
            (h  , _)  <- L8.readInt $ trim $ getField 15 2 l1
            (m  , _)  <- L8.readInt $ trim $ getField 18 2 l1
            (s  , _)  <- L8.readInt $ trim $ getField 21 2 l1
  
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
            fitIntervalD  <- readDoubleField $ getField 23 19 l8

            let iode         = round      iodeD
                toe          = realToFrac toeD
                week         = round      weekD             -- conversion is needed for equality comparisons
                svHealth     = round      svHealthD
                iodc         = round      iodcD
                fitInterval  = round      fitIntervalD
            return NavRecord {..}
    _ -> Nothing

-- | Skip unknown record reading lines to begining of other record.
-- Used to skip records of constellations other than GPS.
navSkipUnknownRecord :: L8.ByteString -> L8.ByteString
navSkipUnknownRecord bs =
  let (_, rest) = L8.break (== '\n') bs
      rest' = L8.drop 1 rest
      in if navIsNewRecordLine rest'
           then rest'
           else navSkipUnknownRecord rest'

-- | Returns True if the bs starts with other sign than ' '                
navIsNewRecordLine :: L8.ByteString -> Bool
navIsNewRecordLine bs = L8.take 1 bs /= " "

-- | Insert a navigation record into a 'NavMap'.
-- If there is no entry for the given PRN or epoch, the record is
-- inserted. If an entry already exists, the record is replaced only
-- if the new record has a greater IODE than the existing one.
--
-- This ensures that for each @(week, toe)@ only the navigation
-- record with the maximum IODE is kept.
navGpsInsertRecord :: NavRecord -> NavMap -> NavMap
navGpsInsertRecord r =
  IMS.alter updatePrn key1
  where
    key1 = prn r
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
-- satellite PRN from NavMap. The navigation record with the nearest
-- (week, toe) to the specified observation time is selected.
navGpsSelectEphemeris
    :: GpsTime
    -> IMS.Key
    -> NavMap
    -> Maybe NavRecord
navGpsSelectEphemeris tobs prn navMap = do
    subMap <- IMS.lookup prn navMap
    let wtobs  = gpsTimeToWeekTow tobs
        past   = MS.lookupLE wtobs subMap
        future = MS.lookupGE wtobs subMap
        closest = case (past, future) of
          (Just (wtoeP, rP), Just (wtoeF, rF)) ->
              if abs (diffGpsWeekTow wtobs wtoeP) <= abs (diffGpsWeekTow wtoeF wtobs)
              then Just (wtoeP, rP)
              else Just (wtoeF, rF)
          (Just p, Nothing)  -> Just p
          (Nothing, Just f)  -> Just f
          (Nothing, Nothing) -> Nothing                              
    (_, r) <- closest
    if isEphemerisValid wtobs r
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
                   
obsGpsMapFromRinex :: L8.ByteString -> ObsMap
obsGpsMapFromRinex bs
  | L8.null bs         = error "Empty file"
  | rinexVer /= "3.04" = error "Not RINEX 3.04 file"
  | fileType /= "O"    = error "Not an observation file"
  | otherwise = obsGpsReadRecords (bodyLines bs)
  where
    rinexVer = trim $ getField  0 9 bs
    fileType = trim $ getField 20 1 bs
    bodyLines = L8.lines . skipHeader


obsGpsReadRecords :: [L8.ByteString] -> ObsMap
obsGpsReadRecords [] = error "Cannot find observation data in the file"
obsGpsReadRecords ls0 = loop MS.empty ls0
    where                       
      loop m [] = m
      loop m (l:ls)
          | L8.isPrefixOf ">" l  =
              case  obsGpsReadRecord (l:ls) of
                Just (r, rest) -> loop (obsGpsInsertRecord r m) rest
                Nothing        -> error "Cannot parse observation data"
          | otherwise = error "Unexpected line: expected '>' at start of epoch"

-- | Insert record into ObsMap
obsGpsInsertRecord :: (ObsTime, [Prn]) -> ObsMap -> ObsMap
obsGpsInsertRecord (t, prns) =
    MS.insertWith IMS.union t (IMS.fromList [(p, Nothing) | p <- prns])              

-- | Read  observation time (epoch) with GPS satellite observations
obsGpsReadRecord :: [L8.ByteString] -> Maybe ((ObsTime,[Prn]), [L8.ByteString])
obsGpsReadRecord [] = Nothing                         
obsGpsReadRecord (l:ls) = do
  (y  , _) <- L8.readInt      $ getField  2  4 l
  (mon, _) <- L8.readInt      $ getField  7  2 l
  (d  , _) <- L8.readInt      $ getField 10  2 l
  (h  , _) <- L8.readInt      $ getField 13  2 l
  (m  , _) <- L8.readInt      $ getField 16  2 l
  s        <- readDoubleField $ getField 19 11 l
  let tobs = mkGpsTime (toInteger y) mon d h m (realToFrac s)               
  (n  , _) <- L8.readInt $ getField 33  3 l                -- number of satellites observed in current epoch
                                                           -- (observation time)
  let (obsLines, rest) = splitAt n ls
      gpsLines = filter (\line -> L8.take 1 line == "G") obsLines
  prns    <- mapM (\line -> fmap fst (L8.readInt (getField 1 2 line))) gpsLines
  return ((tobs, prns), rest)
               
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
                                  
-- | Conversion of GPS time to GPS week and time-of-week
gpsTimeToWeekTow
    :: GpsTime
    -> GpsWeekTow
gpsTimeToWeekTow (LocalTime date (TimeOfDay h m s)) =
    let gpsStartDate = fromGregorian 1980 1 6               -- the date from which the GPS time is counted
        days         = diffDays date gpsStartDate           -- number of days since GPS start date
        (w, dow)     = days `divMod` 7                      -- GPS week, GPS day-of-week
        tow          = fromIntegral ( dow * 86400
                                    + toInteger (h * 3600 + m * 60)
                                    )
                     + s
    in (w, tow)

obsGpsAttachEphemerides :: NavMap -> ObsMap -> ObsMap
obsGpsAttachEphemerides navMap =
    MS.mapWithKey $ \tobs prnMap ->
        IMS.mapWithKey (\prn maybeNavRecord ->
            case maybeNavRecord of
                Nothing -> navGpsSelectEphemeris tobs prn navMap
                val     -> val
        ) prnMap      
