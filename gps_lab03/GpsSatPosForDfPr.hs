-- 2025-12-23

{- | Estimate ECEF satellite position for dual-frequency pseudorange
     measurement (observation) from broadcast ephemeris. The position
     is calculated at GPS transmission time. The calculation is of low
     precision because the code pseudorange is of low precision and
     the orbital parameters (ephemeris) are approximate.
   
     NOTE 1:
       Three different clocks must be considered:
         - GPS clock
         - satellite clock
         - receiver clock
       All of them count time in GPS time system.

       The signal transmission time tt is by the GPS clock,
       satellite time of signal transmission by satellite clock,
       receiver time of signal reception by receiver clock.

       The term "epoch" refers to a moment in time. The receiver time
       of signal reception by receiver clock is also called the
       observation time, observation epoch, receiver time tag,
       receiver timestamp, measurement time.

     NOTE 2:
       Why is transmission time calculated? The transmission time is
       calculated to calculate the satellite position for the
       pseudorange.  It's irrelevant that transmission times vary for
       different satellites for the selected observation time.  What
       matters is that the satellite's position corresponds to the
       satellite-receiver distance.
              
     Input:
       - observation time
         (receiver time of signal reception)        tobs           (hand copied from RINEX observation file)
       - pseudorange for f1 [m]                     pr1            (hand copied from RINEX observation file)
       - pseudorange for f2 [m]                     pr2            (hand copied from RINEX observation file)
       - navigation data record in RINEX 3.04
         format                                     nav_record.txt (hand copied from a RINEX navigation file)

     Output:
       - signal transmission time by GPS clock      tt
       - satellite position in ECEF [m]
         at transmission time                       (x, y, z)

     Print of run:
     Observation time
     (receiver clock time of signal reception) : 2024 03 07 00 53 01
     Signal transmission time by GPS clock     : 2024 03 07 00 53 00.927812714088

     ECEF satellite position [m]:
     X =  4460302.794944842
     Y = 17049812.692289740
     Z = 19845264.366251267
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Time.Calendar            (fromGregorian, diffDays, addDays)
import           Data.Time.LocalTime           (LocalTime (..), TimeOfDay(..))
import           Data.Time.Format
import           Data.Fixed                    (Pico)    
import           Text.Printf                   (printf)
import qualified Data.ByteString.Char8  as L8
import           Data.Char                     (isSpace)
    
import           Control.Monad                 (guard)
import qualified Data.ByteString.Unsafe as BSU (unsafeUseAsCString)
import           Foreign                       (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types               (CChar, CDouble(CDouble))
import           Foreign.C.String              (CString)
import           System.IO.Unsafe              (unsafePerformIO)


-- | GPS navigation record (a subset of fields from RINEX 3.04 navigation file)
data NavRecord = NavRecord
  { prn          :: Int               -- ^ satellite number
  , toc          :: GpsTime           -- ^ clock data reference time
  , af0          :: Double            -- ^ satellite clock bias correction coefficient [s]
  , af1          :: Double            -- ^ satellite clock drift correction coefficient [s/s]
  , af2          :: Double            -- ^ satellite clock drift rate correction coefficient [s/s^2]
  , crs          :: Double            -- ^ orbital radius correction [m]
  , deltaN       :: Double            -- ^ mean motion difference [rad/s]
  , m0           :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc          :: Double            -- ^ cosine correction to argument of latitude [rad]
  , e            :: Double            -- ^ eccentricity []
  , cus          :: Double            -- ^ sine correction to argument of latitude [rad]
  , sqrtA        :: Double            -- ^ square root of semi-major axis [m^0.5]
  , toe          :: Pico              -- ^ time of ephemeris in GPS week (time-of-week of ephemeris) [s]
  , cic          :: Double            -- ^ cosine correction to inclination [rad]
  , omega0       :: Double            -- ^ longitude of ascending node at toe [rad]
  , cis          :: Double            -- ^ sine correction to inclination [rad]
  , i0           :: Double            -- ^ inclination angle at reference epoch [rad]
  , crc          :: Double            -- ^ cosine correction to orbital radius [m]
  , omega        :: Double            -- ^ argument of perigee [rad]
  , omegaDot     :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot         :: Double            -- ^ rate of inclination angle [rad/s]
  , week         :: Integer           -- ^ GPS week to go with toe
  , fitInterval  :: Int               -- ^ fit interval, ephemeris validity interval related to toe [h]
  } deriving (Show)

type GpsTime    = LocalTime
type GpsWeekTow = (Integer, Pico)

-- | Constants
mu, omegaEDot, c, fRel, f1, f2 :: Double
mu        = 3.986005e14           -- WGS 84 value of earth's universal gravitational constant [m^3/s^2]
omegaEDot = 7.2921151467e-5       -- WGS 84 value of the earth's rotation rate [rad/s]
c         = 299792458.0           -- speed of light [m/s]
fRel      = -4.442807633e-10      -- constant F in the relativistic correction [s/sqrt m]
f1        = 1575.42e6::Double     -- L1 frequency [Hz]
f2        = 1227.60e6::Double     -- L2 frequency [Hz]

-- | Determining the GPS satellite position in ECEF from the GPS
--   ephemeris and for a (GPS week, tow).
satPosECEF
    :: GpsWeekTow                                            -- ^ GPS week, time-of-week
    -> NavRecord                                             -- ^ navigation record with ephemeris
    -> (Double, Double, Double)                              -- ^ satellite position in ECEF [m]
satPosECEF (w, tow) eph =
  let
    a      = sqrtA eph * sqrtA eph                           -- semi-major axis [m]
    n0     = sqrt(mu/(a*a*a))                                -- computed mean motion [rad/sec]       
    n      = n0 + deltaN eph                                 -- corrected mean motion [rad/s]        
    tk     = realToFrac $
             diffGpsWeekTow (w, tow) (week eph, toe eph)     -- time elapsed since toe [s]           
    mk     = m0 eph + n*tk                                   -- mean anomaly at tk [rad]             
    ek     = keplerSolve mk (e eph)                          -- eccentric anomaly [rad]              
    vk     = atan2 (sqrt (1 - e eph *e eph ) * sin ek)
                   (cos ek - e eph)                          -- true anomaly                         
    phik   = vk + omega eph                                  -- argument of latitude                 
    duk    = cus eph * sin (2*phik)
           + cuc eph * cos (2*phik)                          -- argument of latitude correction      
    drk    = crs eph * sin (2*phik)
           + crc eph * cos (2*phik)                          -- radius correction                    
    dik    = cis eph * sin (2*phik)
           + cic eph * cos (2*phik)                          -- inclination correction               
    uk     = phik + duk                                      -- corrected argument of latitude       
    rk     = a * (1 - e eph * cos ek) + drk                  -- corrected radius                     
    ik     = i0 eph + dik + iDot eph * tk                    -- corrected inclination                
    xk'    = rk * cos uk                                     -- xk' in the orbital plane             
    yk'    = rk * sin uk                                     -- yk' in the orbital plane             
    omegak = omega0 eph                                                                                 
           + (omegaDot eph - omegaEDot)*tk
           - omegaEDot * realToFrac (toe eph)                -- corrected longitude of ascending node
    xk     = xk' * cos omegak - yk' * cos ik * sin omegak    -- transformation to ECEF               
    yk     = xk' * sin omegak + yk' * cos ik * cos omegak    -- transformation to ECEF               
    zk     =                    yk' * sin ik                 -- transformation to ECEF
  in (xk,yk,zk)

-- | Iterative solution of Kepler's equation ek = m + e sin ek (Newtona-Raphsona method)
keplerSolve    
    :: Double                                                   -- ^ mean anomaly
    -> Double                                                   -- ^ eccentricity
    -> Double                                                   -- ^ eccentric anomaly [rad]
keplerSolve m e = loop e0 0               
  where
    e0 = m + e * sin m
    loop :: Double -> Int -> Double
    loop eN k
      | k > 20 = error "Kepler method iteration count exceeded"
      | abs (eN' - eN) < 1e-12 = eN'
      | otherwise = loop eN' (k+1)
          where    
            f    = eN - e * sin eN - m  
            fDot =  1 - e * cos eN                           -- derivative of the function f
            eN'  = eN - f/fDot                               -- iterative formula

-- | Pseudorange for dual-frequency receiver (pseudorange corrected for ionospheric effects)
--   based on IS-GPS-200N 20.3.3.3.3.3 ready-made formulas.
pseudorangeDF
    :: Double                                                -- ^ pseudorange for f1    [m]
    -> Double                                                -- ^ pseudorange for f2    [m]
    -> Double                                                -- ^ pseudorange corrected [m]
pseudorangeDF pr1 pr2 = (pr2 - g*pr1)/(1 - g)
    where
      g = (f1/f2)^(2::Int)

-- | Iterative calculation eccentric anomaly
eAnom
    :: GpsWeekTow                                            -- ^ GPS week, time-of-week
    -> NavRecord                                             -- ^ navigation recored with ephemeris
    -> Double                                                -- ^ eccentric anomaly [rad]
eAnom (w, tow) NavRecord{..} =                              
    let a  = (sqrtA)*(sqrtA)                                 -- semi-major axis
        n0 = sqrt(mu/(a*a*a))                                -- computed mean motion [rad/sec]
        n  = n0 + deltaN                                     -- corrected mean motion [rad/s]
        tk = realToFrac $
             diffGpsWeekTow (w, tow) (week, toe)             -- time elapsed since toe [s]
        mk = m0 + n*tk                                       -- mean anomaly for tk
    in keplerSolve mk e                                      -- eccentric anomaly [rad]

-- | Compute relativistic correction for satellite clock
--   based on IS-GPS-200N 20.3.3.3.3.1 ready-made formulas.
relCorr
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> NavRecord                                            -- ^ ephemeris
    -> Pico                                                 -- ^ dtr - relativistic correction [s]
relCorr (w, tow) NavRecord{..} = realToFrac (fRel * e * sqrtA * sin ek)
    where
      ek = eAnom (w, tow) NavRecord{..}                     -- eccentric anomaly [rad]

-- | Compute satellite clock correction
--   based on IS-GPS-200N 20.3.3.3.3.1 ready-made formulas.
clkCorr
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> NavRecord                                            -- ^ ephemeris                                 
    -> Pico                                                 -- ^ dtsv - satellite clock correction [s]
clkCorr (w, tow) NavRecord{..} = realToFrac (af0 + af1*dt + af2*dt^(2::Int))
    where
      dt  = realToFrac $ diffGpsWeekTow (w, tow) (week, tocTow)
      (_, tocTow) = gpsTimeToWeekTow toc
            
-- | Iteratively compute signal transmission time because the transmission
--   time depends on the clock corrections and the clock corrections
--   depend on the transmission time.
transmissionTime            
  :: Double                                                 -- ^ pseudorange pr1 [m]
  -> Double                                                 -- ^ pseudorange pr2 [m]
  -> GpsWeekTow                                             -- ^ time of observation
                                                            --   (receiver time of signal reception)
  -> NavRecord                                              -- ^ navigation recored with ephemeris
  -> GpsWeekTow                                             -- ^ signal transmission time
transmissionTime pr1 pr2 wtob eph =  loop tt0 0
    where
      pr   = pseudorangeDF pr1 pr2
      tsv  = diffSeconds wtob  (realToFrac (pr/c))           -- satelite time of signal transmission
      tt0 = tsv
      loop :: GpsWeekTow -> Int -> GpsWeekTow
      loop tt k
          | k >= 10                  = error "Number of time transmission iterations exceeded"
          | abs (diffGpsWeekTow tt' tt) < 1e-12 = tt'
          | otherwise                           = loop tt' (k+1)
          where
            dtb  = clkCorr tt eph                           -- clock correction
            dtr  = relCorr tt eph                           -- relativistic correction
            dtsv = dtb  + dtr
            tt' = diffSeconds tt0 dtsv

-- | Calculates the number of seconds between two (GPS week, tow).
diffGpsWeekTow
    :: GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> GpsWeekTow                                           -- ^ GPS week, time-of-week
    -> Pico                                                 -- ^ time difference [s]
diffGpsWeekTow (w2,tow2) (w1,tow1) =
    fromInteger (dw * 604800) + dtow
    where
      dw   = w2   - w1
      dtow = tow2 - tow1

-- | Substract seconds from (GPS week,tow).
diffSeconds
    :: GpsWeekTow                                 -- ^ GPS week, time-of-week
    -> Pico                                       -- ^ period of time [s]
    -> GpsWeekTow                                 -- ^ GPS week, time-of-week
diffSeconds (week, tow) secs =
    let ds = tow - secs
        k = floor (ds / 604800)
        tow' = ds - fromIntegral k * 604800
    in (week + k, tow')

-- | Function import: double strtod(const char *nptr, char **endptr)
foreign import ccall unsafe "stdlib.h strtod"
    c_strtod :: CString -> Ptr (Ptr CChar) -> IO CDouble

-- | 2025-11-01 Data.ByteString.Char8 does not have a readDouble function.
--   Reads Double value from Char8 ByteString.
readDouble :: L8.ByteString -> Maybe (Double, L8.ByteString)
readDouble bs = unsafePerformIO $
    BSU.unsafeUseAsCString bs $ \cstr -> 
      alloca $ \endPtr -> do
        val <- c_strtod cstr endPtr
        end <- peek endPtr
        if end == cstr
          then return Nothing
          else do
            let offset = end `minusPtr` cstr
            let rest   = L8.drop offset bs
            return (Just (realToFrac val, rest))

-- | Reads Double value from ByteString field.
--   Its purpose is to stop reading if it cannot read the entire field.
--   After reading, the rest may be empty or consist only of spaces.
readDoubleField :: L8.ByteString -> Maybe Double
readDoubleField bs = do
  (val, rest) <- readDouble bs
  case L8.uncons rest of
    Just (ch, _) | ch=='D' || ch=='d' ->
      error $ "Unsupported number format with 'D': " ++ L8.unpack bs
    _ -> guard (L8.all (== ' ') rest) >> return val                  
                   
takeField :: Int -> Int -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start

-- | Makes GpsTime from numbers.
mkGpsTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsTime
mkGpsTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

-- | Read GPS satellite navigation record of eight lines.
readRecordLines :: L8.ByteString -> ([L8.ByteString], L8.ByteString)
readRecordLines bs0 =
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
              dropLineSep  = L8.dropWhile (`L8.elem` "\r\n")
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
readRecord :: L8.ByteString -> (NavRecord, L8.ByteString)
readRecord bs =
    let (ls, bs') = readRecordLines bs
    in case getRecord ls of
         Just r  -> (r, bs')
         Nothing -> error "Unable to get GPS navigation record from record lines."

-- | Get GPS navigation record from GPS record lines.  Expects 8 lines
-- as input.
getRecord :: [L8.ByteString] -> Maybe NavRecord
getRecord ls =
    case ls of
      [l1,l2,l3,l4,l5,l6,_,l8] -> do
              (prn, _)  <- L8.readInt $ trim $ takeField  1 2 l1       -- trim is needed by readInt
              (y  , _)  <- L8.readInt $ trim $ takeField  4 4 l1
              (mon, _)  <- L8.readInt $ trim $ takeField  9 2 l1
              (d  , _)  <- L8.readInt $ trim $ takeField 12 2 l1
              (h  , _)  <- L8.readInt $ trim $ takeField 15 2 l1
              (m  , _)  <- L8.readInt $ trim $ takeField 18 2 l1
              (s  , _)  <- L8.readInt $ trim $ takeField 21 2 l1
  
              let toc = mkGpsTime (toInteger y) mon d h m (fromIntegral s)

              af0       <- readDoubleField $ takeField 23 19 l1
              af1       <- readDoubleField $ takeField 42 19 l1
              af2       <- readDoubleField $ takeField 61 19 l1
                 
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

              fitIntervalD  <- readDoubleField $ takeField 23 19 l8

              let toe          = realToFrac toeD
                  week         = round      weekD           -- conversion is needed for equality comparisons
                  fitInterval  = round      fitIntervalD
              return NavRecord {..}
      _ -> Nothing

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
    
-- | Ephemeris validity check based on fit interval ephemeris field for
--   a given observation time
isEphemerisValid
  :: GpsWeekTow                                             -- ^ GPS week, time-of-week
  -> NavRecord
  -> Bool
isEphemerisValid (w, tow) r =
    abs diffTime <= halfFitInterval
    where
      diffTime        = diffGpsWeekTow  (w, tow) (week r, toe r)
      halfFitInterval = fromIntegral ((fitInterval r) `div` 2 * 3600)

gpsSatPosForDfPr
  :: GpsTime                                                -- ^ time of observation
                                                            --   (receiver time of signal reception)
  -> Double                                                 -- ^ pseudorange for f1
  -> Double                                                 -- ^ pseudorange for f2
  -> NavRecord                                              -- ^ GPS navigation record
  -> (GpsWeekTow, (Double, Double, Double))                 -- ^ transmission time, satelite ECEF posistion at transmission time
gpsSatPosForDfPr tobs pr1 pr2 r =
    let wtobs = gpsTimeToWeekTow tobs
    in if isEphemerisValid wtobs r
       then let tt      = transmissionTime pr1 pr2 wtobs r  -- Output: signal transmission time by GPS clock [s]
                (x,y,z) = satPosECEF tt r                   -- Output: satelite ECEF position [m]
                in (tt, (x, y, z))
       else error $ "Ephemeris is not valid for " ++
            formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" tobs

main :: IO ()
main = do
  let tobs = mkGpsTime 2024 03 07 00 53 01.0000000          -- Input: time of observation
                                                            --        (receiver time of signal reception)
      pr1  = 21548635.724                                   -- Input: pseudorange for f1 e.g. C1C
      pr2  = 21548628.027                                   -- Input: pseudorange for f2 e.g. C2X
      fn   = "nav_record.txt"                               -- Input: file name
  bs <- L8.readFile fn                                      -- bytestring from "nav_record.txt"
  if L8.take 1 bs == "G"
  then do
      let (r, _) = readRecord bs
          (wtt, (x,y,z)) = gpsSatPosForDfPr tobs pr1 pr2 r  -- Output: signal transmission time by GPS clock,
                                                            --         satelite ECEF position [m] at transmission time
          tt = weekTowToGpsTime wtt
               
      printf "Observation time\n"
      printf "(receiver clock time of signal reception) : %s\n"
              (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" tobs)
      printf "Signal transmission time by GPS clock     : %s\n\n"
              (formatTime defaultTimeLocale "%Y %m %d %H %M %S%Q" tt)
      printf "ECEF satellite position [m]:\n"
      printf "X = %18.9f\n" x
      printf "Y = %18.9f\n" y
      printf "Z = %18.9f\n" z
   else
      printf "Cannot find a GPS navigation record in %s\n" fn   

