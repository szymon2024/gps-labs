-- 2025-11-27

{- | Estimate ECEF satellite position at GPS emission time [s] from
     broadcast ephemeris for dual-frequency pseudorange measurement
     (observation).  The calculated position is of low precision
     because the code pseudorange is of low precision and the orbital
     parameters (ephemeris) are approximate.
     
   
     NOTE 1:
       Three different clocks must be considered:
         - GPS clock
         - satellite clock
         - receiver clock
       All of them count time in GPS time system.

       The signal emission time te is by the GPS clock,
       satellite time of signal emission by satellite clock,
       receiver time of signal reception by receiver clock.

     NOTE 2:
       Why is emission time calculated? The emission time is
       calculated to calculate the satellite position for the
       pseudorange.  It's irrelevant that emission times vary for
       different satellites for the selected observation time.  What
       matters is that the satellite's position corresponds to the
       satellite-receiver distance.
              
     Input:
       - receiver time of signal reception          obsGpsTime     (hand copied from RINEX observation file)
       - pseudorange for f1 [m]                     pr1            (hand copied from RINEX observation file)
       - pseudorange for f2 [m]                     pr2            (hand copied from RINEX observation file)
       - navigation data record in RINEX 3.04
         format                                     nav_record.txt (hand copied from a RINEX navigation file)

     Output:
       - signal emission time by GPS clock [s]      te
       - satellite position in ECEF [m]
         at emission time                           (x, y, z)

     Print of run:
     Receiver clock time of signal reception [w,s]: (2304,348781.000000000000)
     Emission time by GPS clock              [w,s]: (2304,348780.927812714088)

     ECEF satellite position [m]:
     X =  4460302.794944842
     Y = 17049812.692289740
     Z = 19845264.366251267

-}

{-# LANGUAGE RecordWildCards #-}

module GpsSatPosAtEmTime where

import           Data.Time.Calendar            (fromGregorian, diffDays)
import           Data.Time.LocalTime           (LocalTime (..), TimeOfDay(..))
import           Data.Fixed                    (Pico)    
import           Text.Printf                   (printf)
import qualified Data.ByteString.Char8  as BSC
import           Control.Monad                 (guard)
import qualified Data.ByteString.Unsafe as BSU (unsafeUseAsCString)
import           Foreign                       (Ptr, alloca, peek, minusPtr)
import           Foreign.C.Types               (CChar, CDouble(CDouble))
import           Foreign.C.String              (CString)
import           System.IO.Unsafe              (unsafePerformIO)


-- | GPS navigation data (a subset of fields from RINEX 3.04 navigation file)
data NavRecord = NavRecord
  { prn      :: Int               -- ^ satellite number
  , toc      :: GpsTime           -- ^ clock data reference time
  , af0      :: Double            -- ^ satellite clock bias correction coefficient [s]
  , af1      :: Double            -- ^ satellite clock drift correction coefficient [s/s]
  , af2      :: Double            -- ^ satellite clock drift rate correction coefficient [s/s^2]
  , crs      :: Double            -- ^ orbital radius correction [m]
  , deltaN   :: Double            -- ^ mean motion difference [rad/s]
  , m0       :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc      :: Double            -- ^ cosine correction to argument of latitude [rad]
  , e        :: Double            -- ^ eccentricity []
  , cus      :: Double            -- ^ sine correction to argument of latitude [rad]
  , sqrtA    :: Double            -- ^ square root of semi-major axis [m^0.5]
  , toe      :: Pico              -- ^ time of ephemeris in GPS week (time-of-week of ephemeris) [s]
  , cic      :: Double            -- ^ cosine correction to inclination [rad]
  , omega0   :: Double            -- ^ longitude of ascending node at toe [rad]
  , cis      :: Double            -- ^ sine correction to inclination [rad]
  , i0       :: Double            -- ^ inclination angle at reference epoch [rad]
  , crc      :: Double            -- ^ cosine correction to orbital radius [m]
  , omega    :: Double            -- ^ argument of perigee [rad]
  , omegaDot :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot     :: Double            -- ^ rate of inclination angle [rad/s]
  , week     :: Integer           -- ^ GPS week to go with toe
  , fitIntv  :: Int               -- ^ fit interval, ephemeris validity interval related to toe [h]
  } deriving (Show)

type GpsTime    = LocalTime
type GpsWeekTow = (Integer, Pico)

-- | Constants
mu, omegaEDot, c, fRel, f1, f2 :: Double
mu        = 3.986005e14           -- WGS 84 value of earth's universal gravitational constant [m^3/s^2]
omegaEDot = 7.2921151467e-5       -- WGS 84 value of the earth's rotation rate [rad/s]
c         = 299792458.0           -- speed of light [m/s]
fRel      = -4.442807633e-10      -- constant F in the relativistic correction [s/sqrt m]
f1        = 1575.42e6             -- L1 frequency [Hz]
f2        = 1227.60e6             -- L2 frequency [Hz]

-- | Determining the GPS satellite position in ECEF from the GPS
--   ephemeris and for a GPS time.
satPosition         
    :: GpsWeekTow                                            -- ^ GPS week, time-of-week
    -> NavRecord                                             -- ^ ephemeris parameters
    -> (Double, Double, Double)                              -- ^ satellite position in ECEF [m]
satPosition (w, tow) eph =
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
keplerSolve m e = iterate e0 0               
  where
    e0 = m + e * sin m
    iterate :: Double -> Int -> Double
    iterate eN k
      | k > 20 = error "Kepler method iteration count exceeded"
      | abs (eN' - eN) < 1e-12 = eN'
      | otherwise = iterate eN' (k+1)
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
      g = (f1/f2)^2

-- | Euclidean distance between two 3D positions
range :: (Double,Double,Double) -> (Double,Double,Double) -> Double
range (x1,y1,z1) (x2,y2,z2) = sqrt ((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)

-- | Iterative calculation eccentric anomaly
eAnom
    :: GpsWeekTow                                            -- ^ GPS week, time-of-week
    -> NavRecord
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
clkCorr (w, tow) NavRecord{..} = realToFrac (af0 + af1*dt + af2*dt^2)
    where
      dt  = realToFrac $ diffGpsWeekTow (w, tow) (week, tocTow)
      (_, tocTow) = gpsTimeToWeekTow toc
            
-- | Iteratively compute signal emission time because the emission
--   time depends on the clock corrections and the clock corrections
--   depend on the emission time.
emissionTime            
  :: Double                                                 -- ^ pseudorange pr1 [m]
  -> Double                                                 -- ^ pseudorange pr2 [m]
  -> GpsWeekTow                                             -- ^ receiver time of signal reception [s]
  -> NavRecord                                              -- ^ broadcast ephemeris
  -> GpsWeekTow                                             -- ^ signal emission time [s]
emissionTime pr1 pr2 (wr, tr) eph = iterate te0 0
    where
      pr   = pseudorangeDF pr1 pr2
      tsv  = subSeconds (wr, tr)  (realToFrac (pr/c))       -- satelite time of signal emission
      te0 = tsv
      iterate te k
          | k >= 10                  = error "Number of time emission iterations exceeded"
          | abs (diffGpsWeekTow te' te) < 1e-12 = te'
          | otherwise                        = iterate te' (k+1)
          where
            dtb  = clkCorr te eph
            dtr  = relCorr te eph
            dtsv = dtb  + dtr
            te' = subSeconds te0 dtsv

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

-- | Substract seconds from GPS time. It is only used to calculate the
--   signal propagation time. It uses the information that the
--   propagation time is less than 1 second.
subSeconds
    :: GpsWeekTow                                            -- ^ GPS week, time-of-week [s]
    -> Pico                                                  -- ^ seconds [s]
    -> GpsWeekTow                                            -- ^ GPS week, time-of-week [s]
subSeconds (w, tow) t
   | t < 1      = if tow - t >= 0
                  then (w  ,          tow - t)
                  else (w-1, 604800 + tow - t)
   | otherwise  = error "Cannot substract equal or more than 1 s"


-- | Function import: double strtod(const char *nptr, char **endptr)
foreign import ccall unsafe "stdlib.h strtod"
    c_strtod :: CString -> Ptr (Ptr CChar) -> IO CDouble

-- | 2025-11-01 Data.ByteString.Char8 does not have a readDouble function.
--   Reads Double value from Char8 ByteString.
readDouble :: BSC.ByteString -> Maybe (Double, BSC.ByteString)
readDouble bs = unsafePerformIO $
    BSU.unsafeUseAsCString bs $ \cstr -> 
      alloca $ \endPtr -> do
        val <- c_strtod cstr endPtr
        end <- peek endPtr
        if end == cstr
          then return Nothing
          else do
            let offset = end `minusPtr` cstr
            let rest   = BSC.drop offset bs
            return (Just (realToFrac val, rest))

-- | Reads Double value from ByteString field.
--   Its purpose is to stop reading if it cannot read the entire field.
--   After reading, the rest may be empty or consist only of spaces.
readDoubleField :: BSC.ByteString -> Maybe Double
readDoubleField bs = do
  (val, rest) <- readDouble bs
  case BSC.uncons rest of
    Just (c, _) | c=='D' || c=='d' ->
      error $ "Unsupported number format with 'D': " ++ BSC.unpack bs
    _ -> guard (BSC.all (== ' ') rest) >> return val                  
                   
getField :: Int -> Int -> BSC.ByteString -> BSC.ByteString
getField start len = BSC.take len . BSC.drop start

-- | Makes GpsTime from numbers.
mkGpsTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsTime
mkGpsTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

-- | Parse a single navigation data record
readNavRecord :: BSC.ByteString -> Maybe (NavRecord, BSC.ByteString)
readNavRecord bs1 = do
  (sys, _) <- BSC.uncons bs1
  guard (sys == 'G')
  (prn, _) <- BSC.readInt $ getField  1 2 bs1
  (y  , _) <- BSC.readInt $ getField  4 4 bs1
  (mon, _) <- BSC.readInt $ getField  9 2 bs1
  (d  , _) <- BSC.readInt $ getField 12 2 bs1
  (h  , _) <- BSC.readInt $ getField 15 2 bs1
  (m  , _) <- BSC.readInt $ getField 18 2 bs1
  (s  , _) <- BSC.readInt $ getField 21 2 bs1
  
  let toc = mkGpsTime (toInteger y) mon d h m (fromIntegral s)

  af0      <- readDoubleField $ getField 23 19 bs1
  af1      <- readDoubleField $ getField 42 19 bs1
  af2      <- readDoubleField $ getField 61 19 bs1

  let bs2 = dropLine bs1
  crs      <- readDoubleField $ getField 23 19 bs2
  deltaN   <- readDoubleField $ getField 42 19 bs2
  m0       <- readDoubleField $ getField 61 19 bs2

  let bs3 = dropLine bs2
  cuc      <- readDoubleField $ getField  4 19 bs3
  e        <- readDoubleField $ getField 23 19 bs3
  cus      <- readDoubleField $ getField 42 19 bs3
  sqrtA    <- readDoubleField $ getField 61 19 bs3

  let bs4 = dropLine bs3
  toeD     <- readDoubleField $ getField  4 19 bs4
  cic      <- readDoubleField $ getField 23 19 bs4
  omega0   <- readDoubleField $ getField 42 19 bs4
  cis      <- readDoubleField $ getField 61 19 bs4

  let bs5 = dropLine bs4
  i0       <- readDoubleField $ getField  4 19 bs5
  crc      <- readDoubleField $ getField 23 19 bs5
  omega    <- readDoubleField $ getField 42 19 bs5
  omegaDot <- readDoubleField $ getField 61 19 bs5                 

  let bs6 = dropLine bs5
  iDot     <- readDoubleField $ getField  4 19 bs6
  weekD    <- readDoubleField $ getField 42 19 bs6

  let bs7 = dropLine bs6
      bs8 = dropLine bs7
  fitIntvD <- readDoubleField $ getField 23 19 bs8

  let bs'     = dropLastLine bs8
      toe     = realToFrac toeD
      week    = round weekD                                 -- conversion is needed for equality comparisons
      fitIntv = round fitIntvD       
  return (NavRecord {..}, bs')
    where dropLine     = BSC.drop 1 . BSC.dropWhile (/='\n')
                         . BSC.drop 80
          dropLastLine = BSC.drop 1 . BSC.dropWhile (/='\n')
                         . BSC.drop 42                      -- last line can have two, three, four fields

-- | Conversion of GPS time to GPS week and time-of-week
gpsTimeToWeekTow
    :: GpsTime
    -> GpsWeekTow                                            -- ^ GPS week, time-of-week
gpsTimeToWeekTow (LocalTime date (TimeOfDay h m s)) =
    let gpsStartDate = fromGregorian 1980 1 6                -- The date from which the GPS time is counted
        days         = diffDays date gpsStartDate            -- Number of days since GPS start date
        w            = days `div` 7                          -- GPS week
        dow          = days `mod` 7                          -- GPS day-of-week
        tow          = fromIntegral ( dow * 86400
                                    + toInteger (h * 3600 + m * 60)
                                    )
                     + s
    in (w, tow)
    
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

-- Main program:
--   * Converts receiver time of signal reception
--     (called also observation time,
--      observation epoch,
--      receiver time tag,
--      receiver time stamp,
--      measurement time)
--     to receiver GPS week number and time-of-week,
--   * reads broadcast ephemeris from one record file
--     (to use a different navigation record, replace the file content), 
--   * checks ephemeris validity for the given epoch (observation time),
--   * calculates signal emission GPS time and the satellite ECEF position at that time,
--   * prints receiver clock time of signal reception, the emission time, and satellite position.
main :: IO ()
main = do
  let obsGpsTime = mkGpsTime 2024 03 07 00 53 01.0000000               -- Input: receiver time of signal reception
                                                                       --       (receiver time of observation)
      pr1        = 21548635.724                                        -- Input: pseudorange for f1 e.g. C1C
      pr2        = 21548628.027                                        -- Input: pseudorange for f2 e.g. C2X
      fn         = "nav_record.txt"                                    -- Input: file name
      (wr, tr)   = gpsTimeToWeekTow obsGpsTime                         -- receiver GPS week number, time-of-week
  bs <- BSC.readFile fn                                                -- data from "nav_record.txt"
  case readNavRecord bs of
    Nothing  -> putStrLn $ "Can't read navigation record from " ++ fn
    Just (eph, _) ->                                                   -- ephemeris
       if isEphemerisValid (wr, tr) eph
       then do
         let te      = emissionTime pr1 pr2 (wr, tr) eph               -- Output: signal emission time by GPS clock [s]
             (x,y,z) = satPosition te eph                              -- Output: satelite ECEF position [m]
         putStrLn $ "Receiver clock time of signal reception [w,s]: " ++ show (wr, tr)
         putStrLn $ "Emission time by GPS clock              [w,s]: " ++ show te
         putStrLn ""
         printf "ECEF satellite position [m]:\n"
         printf "X = %18.9f\n" x
         printf "Y = %18.9f\n" y
         printf "Z = %18.9f\n" z
       else printf "The ephemeris is out of date for the given time\n"

