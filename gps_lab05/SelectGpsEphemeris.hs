-- 2025-12-13

{- | The program selects a navigation record containing ephemeris from
     the RINEX 3.04 navigation file for a given GPS observation time
     (receiver time of signal reception) and GPS satellite.

     The navigation file data has the key: PRN, toc, iode. However,
     this program does not use this key to select a navigation
     record. Instead, it indexes records by: PRN, (week, toe), iode.
     To make this possible, the type NavMap = IntMap (Map GpsWeekTow
     [(Int, NavRecord)]) has been defined.

     Main steps of the algorithm:
     
     1. Building a map of healthy navigation records from the body of
     a rinex 3.04 navigation file.
     
     2. The navigation record with the nearest (week, toe) to the
     specified observation time and the maximum iode is selected. If
     no record can be selected, the program terminates.

     3. The program then checks whether the selected record lies
     within the fitInterval relative to (week, toe).  If the record satisfies
     this condition, it is printed. Otherwise the program terminates.

     Input:
       - RINEX 3.04 navigation file name                    fn
       - receiver time of signal reception
         (observation time)                                 tobs
       - satellite number                                   prn       


     Output:
       - navigation record with ephemeris                   r

     Print of run:
     Observation time: 2025 08 02 01 00 01.5
                  toe: 2025 08 02 02 00 00
     PRN:  6      toc: 2025 08 02 02 00 00
     af0:         -4.722196608782E-4
     af1:        -1.432454155292E-11
     af2:           0.000000000000E0

     iode:         69
     crs:          -2.368750000000E1
     deltaN:       3.821944913632E-9
     m0:           -2.959416369262E0

     cuc:         -1.283362507820E-6
     e:            3.335768124089E-3
     cus:          4.727393388748E-6
     sqrtA:         5.153617370605E3

     toe:           5.256000000000E5
     cic:          8.940696716309E-8
     omega0:       6.818382481570E-1
     cis:          4.470348358154E-8

     i0:           9.884960063693E-1
     crc:           3.054062500000E2
     omega:       -6.694838801080E-1
     omegaDot:    -7.833540584123E-9

     iDot:       -1.753644474903E-10
     skipped
     week:       2377
     skipped

     skipped
     svHealth:      0
     skipped
     iodc:         69

     ttom:          5.184000000000E5
     fitInterval:     4
----------------------------------

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict    as MS
import           Data.IntMap.Strict                (IntMap)
import qualified Data.IntMap.Strict as IMS
import           Data.Char                         (isSpace)
import           Data.Int                          (Int64)
import           Control.Monad                     (guard)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Calendar                (fromGregorian, diffDays, addDays)
import           Data.Time.LocalTime               (LocalTime (..), TimeOfDay(..))
import           Data.Time.Format                  (formatTime, defaultTimeLocale)
import           Data.Fixed                        (Pico)
import           Text.Printf                       (PrintfType, printf)
import           Data.Function                     (on)
import           Data.List                         (maximumBy)     

-- For readDouble
import qualified Data.ByteString.Unsafe     as BSU (unsafeUseAsCString)    
import           Foreign                           (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types                   (CChar, CDouble(CDouble)) 
import           Foreign.C.String                  (CString)                 
import           System.IO.Unsafe                  (unsafePerformIO)         

type GpsTime    = LocalTime
type GpsWeekTow = (Integer, Pico)                            -- ^ GPS week, time-of-week
type NavMap     = IntMap (Map GpsWeekTow [(Int, NavRecord)]) -- ^ key1: prn, key2: (week r, toe r)
                                                             --   value: list of (iode, r)

-- | GPS navigation data record from RINEX 3.04 navigation file.
data NavRecord = NavRecord
  { prn          :: Int               -- ^ satellite number
  , toc          :: GpsTime           -- ^ clock data reference time
  , af0          :: Double            -- ^ SV clock bias correction coefficient [s]
  , af1          :: Double            -- ^ SV clock drift correction coefficient [s/s]
  , af2          :: Double            -- ^ SV clock drift rate correction coefficient [s/s^2]
  , iode         :: Int               -- ^ issue-of-data, ephemeris; ephemeris data issue number,
  , crs          :: Double            -- ^ orbital radius correction [m]
  , deltaN       :: Double            -- ^ mean motion difference [rad/s]
  , m0           :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc          :: Double            -- ^ latitude argument correction [rad]
  , e            :: Double            -- ^ eccentricity []
  , cus          :: Double            -- ^ latitude argument correction [rad]
  , sqrtA        :: Double            -- ^ sqare root of semi-major axis [m^0.5]
  , toe          :: Pico              -- ^ time of ephemeris in GPS week (time-of-week of ephemeris) [s]
  , cic          :: Double            -- ^ inclination correction [rad]
  , omega0       :: Double            -- ^ longitude of ascending node at toe epoch [rad]
  , cis          :: Double            -- ^ inclination correction [rad]
  , i0           :: Double            -- ^ inclination at reference epoch [rad]
  , crc          :: Double            -- ^ orbital radius corrcetion [m]
  , omega        :: Double            -- ^ argument of perigee [rad]
  , omegaDot     :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot         :: Double            -- ^ rate of inclination angle [rad/s]
  , week         :: Integer           -- ^ number of GPS week for toe and toc
  , svHealth     :: Int               -- ^ SV health, 0 means ok
  , iodc         :: Int               -- ^ issue-of-data, clock; clock data issue number
  , ttom         :: Double            -- ^ transmission time of message - time stamp given by receiver [s]
  , fitInterval  :: Int               -- ^ fit interval, ephemeris validity interval related to toe [h]
  } deriving (Show)


-- Entry point of the program.
main :: IO ()
main = do
  let fn = "source.nav"                                     -- Input: RINEX 3.04 navigation file name
      tobs   = mkGpsTime 2025 08 02 01 00 01.5              -- Input: observation time - receiver time of signal reception
      prn    = 6                                            -- Input: satellite number
  bs <- L8.readFile fn
  let navMap = buildNavMap bs
  case selectGpsEphemeris tobs prn navMap of
    Nothing -> printf "Cannot find valid ephemeris \
                      \for given prn and observation time"
    Just r  -> do                                           -- Output: GPS navigation record 
         printf "Observation time: %s\n"
                (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" tobs)
         printfRecord r


-- | Build a navigation map from healthy GPS navigation records of
-- RINEX 3.04 navigation body.
buildNavMap :: L8.ByteString -> NavMap  
buildNavMap bs
    | L8.null bs         = error "Empty file"
    | rinexVer /= "3.04" = error "Not RINEX 3.04 file"
    | fileType /= "N"    = error "Not navigation file"
    | otherwise = readHealthyGpsNavRecords (skipHeader bs)
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
        label        = trim . L8.takeWhile (not . (`elem` ['\r','\n'])) . L8.drop 60
        dropLine     =      L8.dropWhile (\c -> c == '\r' || c == '\n') . L8.drop 80
        -- The last line very ofthen is not completed to 80 characters
        dropLastLine = L8.dropWhile (`elem` ['\r','\n'])
                     . L8.dropWhile (not . (`elem` ['\r','\n']))
                     . L8.drop 72                           -- don't check before 60 + length "END OF HEADER"

-- | Extracts GPS navigation records from RINEX 3.04 navigation body
-- into a NavMap.
readHealthyGpsNavRecords
    :: L8.ByteString                                        -- ^ body of RINEX navigation file
    -> NavMap
readHealthyGpsNavRecords bs0
    | L8.null bs0 = error "Cannot find navigation data in the file"
    | otherwise   = loop IMS.empty bs0
    where
      loop :: NavMap -> L8.ByteString -> NavMap
      loop m bs
          | L8.null bs = m
          | L8.take 1 bs == "G" =
              let (ls, rest) = recordLines bs
              in case readRecord ls of
                Just r | svHealth r == 0 -> loop (insertRecord r m) rest
                       | otherwise       -> loop m rest
                Nothing  -> error "Cannot read GPS navigation record"
          | otherwise =
              let rest = skipUnknownRecord bs
              in loop m rest        

-- | Consumes GPS navigation record eight lines.  It is based on the
--   knowledge that the content of a line should be 80 characters, but
--   last line often breaks this rule.
recordLines :: L8.ByteString -> ([L8.ByteString], L8.ByteString)
recordLines bs =
    let (l1, r1) = line bs
        (l2, r2) = line r1
        (l3, r3) = line r2
        (l4, r4) = line r3
        (l5, r5) = line r4
        (l6, r6) = line r5
        (l7, r7) = line r6
        (l8, r8) = lastLine r7
    in ([l1,l2,l3,l4,l5,l6,l7,l8], r8)

-- | Consumes a line of 80 characters and a line separator.
line :: L8.ByteString -> (L8.ByteString, L8.ByteString)
line bs =
    let l80  = L8.take 80 bs
        rest = L8.dropWhile (\c -> c == '\r' || c == '\n') (L8.drop 80 bs)
    in (l80, rest)
       
-- | Consumes last line of GPS navigation block.  Last line can have
--   two, three or four fields and sometimes it is not completed to 80
--   characters.
lastLine :: L8.ByteString -> (L8.ByteString, L8.ByteString)
lastLine bs =
    let l    = L8.takeWhile (\c -> not (c == '\r' || c == '\n')) (L8.take 42 bs)
        rest = L8.dropWhile (\c ->      c == '\r' || c == '\n')  (L8.drop (L8.length l) bs)
    in (l, rest)       

-- | Reads GPS navigation record from record lines for GPS satellite.
--   Expects 8 lines as input.  It does not read fields one by one, as
--   parsers do, but by position in the line.
readRecord :: [L8.ByteString] -> Maybe NavRecord
readRecord ls =
  case ls of
    [l1,l2,l3,l4,l5,l6,l7,l8] -> do
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
skipUnknownRecord :: L8.ByteString -> L8.ByteString
skipUnknownRecord bs =
  let (_, rest) = L8.break (== '\n') bs
      rest' = L8.drop 1 rest
      in if isNewRecordLine rest'
           then rest'
           else skipUnknownRecord rest'

-- | Returns True if the bs starts with other sign than ' '                
isNewRecordLine :: L8.ByteString -> Bool
isNewRecordLine bs = L8.take 1 bs /= " "

-- | Inserting a record into NavMap = IntMap (Map GpsWeekTow [(Int,
-- NavRecord)]). It is needed to build the NavMap.
-- IMS.alter checks if there is already an entry for a
-- given PRN.
--   If not, it creates a new map with one entry (week, toe)
-- [(iode, r)]).
--   If so, it updates the existing map.
-- MS.alter checks if there is already a list of records
-- for a given (week, toe)
--   If not, it creates a new list with one element.
--   If so, it appends (iode, r) to the beginning of the list.
insertRecord :: NavRecord -> NavMap -> NavMap
insertRecord r@NavRecord{..} navMap =
    IMS.alter updatePrn prn navMap
  where
    updatePrn Nothing =
        Just (MS.singleton (week, toe) [(iode, r)])
    updatePrn (Just wtoeMap) =
        Just (MS.alter updateWtoe (week, toe) wtoeMap)

    updateWtoe Nothing =
        Just [(iode, r)]
    updateWtoe (Just rs) =
        Just ((iode, r) : rs)

                       
-- | Selects a navigation record for a given observation time and
-- satellite PRN from NavMap.
selectGpsEphemeris
    :: GpsTime
    -> IMS.Key
    -> NavMap
    -> Maybe NavRecord
selectGpsEphemeris tobs prn navMap = do
    wtoeMap <- IMS.lookup prn navMap
    let wtobs  = gpsTimeToWeekTow tobs
        past   = MS.lookupLE wtobs wtoeMap
        future = MS.lookupGE wtobs wtoeMap
        closest = case (past, future) of
          (Just (wtoeP, rsP), Just (wtoeF, rsF)) ->
              if abs (diffGpsWeekTow wtobs wtoeP) <= abs (diffGpsWeekTow wtoeF wtobs)
              then Just (wtoeP, rsP)
              else Just (wtoeF, rsF)
          (Just p, Nothing)  -> Just p
          (Nothing, Just f)  -> Just f
          (Nothing, Nothing) -> Nothing                                
    (_, iodeList) <- closest
    let (_, r) = maximumBy (compare `on` fst) iodeList
    if isEphemerisValid wtobs r
      then Just r
      else Nothing

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

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

-- | Converts GPS week and time-of-week (tow) into GPS time
weekTowToGpsTime
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> GpsTime                                              -- ^ GPS time
weekTowToGpsTime (w, tow) =
    let gpsStartDate = fromGregorian 1980 1 6               -- the date from which the GPS time is counted
        days         = w * 7                                -- number of days since GPS start date
        (towInt, towFrac) = properFraction tow
        (dow,sodInt) = towInt `divMod` 86400
        date         = addDays (days+dow) gpsStartDate
        (h,sohInt)   = sodInt `divMod` 3600
        (m,sInt)     = sohInt `divMod`   60
        s            = fromIntegral sInt + towFrac
    in LocalTime date (TimeOfDay (fromInteger h) (fromInteger m) s)

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
isEphemerisValid (w, tow) r =
    abs diffTime <= halfFitInterval
    where
      diffTime        = diffGpsWeekTow  (w, tow) (week r, toe r)
      halfFitInterval = realToFrac ((fitInterval r) `div` 2 * 3600)

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

-- | Makes GpsTime from numbers.
mkGpsTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsTime
mkGpsTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

-- | Prints a navigation record                          
printfRecord :: Text.Printf.PrintfType t => NavRecord -> t
printfRecord NavRecord{..} =
    printf "             toe: %s\n\
           \PRN: %2d      toc: %s\n\
           \af0:        %19.12E\n\   
           \af1:        %19.12E\n\  
           \af2:        %19.12E\n\n\
                                    
           \iode:       %4d\n\      
           \crs:        %19.12E\n\  
           \deltaN:     %19.12E\n\  
           \m0:         %19.12E\n\n\
                                    
           \cuc:        %19.12E\n\  
           \e:          %19.12E\n\  
           \cus:        %19.12E\n\  
           \sqrtA:      %19.12E\n\n\
                                    
           \toe:        %19.12E\n\  
           \cic:        %19.12E\n\  
           \omega0:     %19.12E\n\  
           \cis:        %19.12E\n\n\
                                    
           \i0:         %19.12E\n\  
           \crc:        %19.12E\n\  
           \omega:      %19.12E\n\  
           \omegaDot:   %19.12E\n\n\
                                    
           \iDot:       %19.12E\n\  
           \skipped\n\              
           \week:       %4d\n\      
           \skipped\n\n\          
                                    
           \skipped\n\              
           \svHealth:   %4d\n\      
           \skipped\n\              
           \iodc:       %4d\n\n\    
                                    
           \ttom:       %19.12E\n\ 
           \fitInterval:  %4d\n\
           \----------------------------------\n"
     (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" (weekTowToGpsTime (week, toe)))
     prn (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" toc)
     af0            
     af1            
     af2            
     iode           
     crs            
     deltaN         
     m0             
     cuc            
     e              
     cus            
     sqrtA          
     ((realToFrac toe)::Double)
     cic            
     omega0         
     cis            
     i0             
     crc            
     omega          
     omegaDot       
     iDot           
     week           
     svHealth       
     iodc           
     ttom           
     fitInterval        

                          
