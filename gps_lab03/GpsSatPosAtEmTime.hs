-- 2025-11-23

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
       Why is emission time calculated?
       The emission time is calculated to calculate the satellite position for the pseudorange.
       It's irrelevant that emission times vary for different satellites for the selected observation time.
       What matters is that the satellite's position corresponds to the satellite-receiver distance.
              
     Input:
       - receiver time of signal reception          obsTime        (hand copied from RINEX observation file)
       - pseudorange for f1 [m]                     pr1            (hand copied from RINEX observation file)
       - pseudorange for f2 [m]                     pr2            (hand copied from RINEX observation file)
       - navigation data record in RINEX 3.04
         format                                     nav_record.txt (hand copied from a RINEX navigation file)

     Output:
       - te      - signal emission time by GPS clock [s]
       - (x,y,z) - satellite position in ECEF [m] at emission time

     Print of run:
     Receiver clock time of signal reception [w,s]: (2304,348781.000000000000)
     Emission time                           [w,s]: (2304,348780.927812714088)

     ECEF satellite position [m]:
     X =  4460302.794944842
     Y = 17049812.692289740
     Z = 19845264.366251267

-}

{-# LANGUAGE RecordWildCards #-}

module GpsSatPosAtTrTime where

import Data.Time.Calendar                      (fromGregorian, diffDays)
import Data.Time.LocalTime                     (LocalTime (..), TimeOfDay(..))
import Data.Fixed                              (Pico)    
import Text.Printf                             (printf)
import qualified Data.ByteString.Char8  as BSC
import Control.Monad                           (guard)
import qualified Data.ByteString.Unsafe as BSU (unsafeUseAsCString)
import Foreign                                 (Ptr, alloca, peek, minusPtr)
import Foreign.C.Types                         (CChar, CDouble(CDouble))
import Foreign.C.String                        (CString)
import System.IO.Unsafe                        (unsafePerformIO)


-- | GPS navigation data (a subset of fields from RINEX 3.04 navigation file)
data NavRecord = NavRecord
  { prn      :: Int               -- ^ satellite number
  , calToc   :: GpsCalendarTime   -- ^ toc as calendar time - clock data reference time
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
  , toe      :: Pico              -- ^ time of ephemeris in GPS week [s]
  , cic      :: Double            -- ^ cosine correction to inclination [rad]
  , omega0   :: Double            -- ^ longitude of ascending node at toe [rad]
  , cis      :: Double            -- ^ sine correction to inclination [rad]
  , i0       :: Double            -- ^ inclination angle at reference epoch [rad]
  , crc      :: Double            -- ^ cosine correction to orbital radius [m]
  , omega    :: Double            -- ^ argument of perigee [rad]
  , omegaDot :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot     :: Double            -- ^ rate of inclination angle [rad/s]
  , week     :: Integer           -- ^ number of GPS week for toe and toc
  , fitIntv  :: Int               -- ^ fit interval, ephemeris validity interval related to toe [h]
  } deriving (Show)

type GpsCalendarTime = LocalTime

-- | Constants
mu, omegaEDot, c, fRel, f1, f2 :: Double
mu        = 3.986005e14           -- WGS 84 value of earth's universal gravitational constant [m^3/s^2]
omegaEDot = 7.2921151467e-5       -- WGS 84 value of the earth's rotation rate [rad/s]
c         = 299792458.0           -- speed of light [m/s]
fRel      = -4.442807633e-10      -- constant F in the relativistic correction [s/sqrt m]
f1        = 1575.42e6             -- L1 frequency [Hz]
f2        = 1227.60e6             -- L2 frequency [Hz]

-- | Compute satellite position in ECEF coordinates at given GPS time
satPosition         
    :: (Integer, Pico)                                       -- ^ GPS week, time-of-week
    -> NavRecord                                             -- ^ ephemeris parameters
    -> (Double, Double, Double)                              -- ^ satellite position in ECEF [m]
satPosition (w, tow) eph =
  let
    a      = sqrtA eph * sqrtA eph                           -- semi-major axis [m]
    n0     = sqrt(mu/(a*a*a))                                -- computed mean motion [rad/sec]       
    n      = n0 + deltaN eph                                 -- corrected mean motion [rad/s]        
    tk     = realToFrac $
             diffGpsTime (w, tow) (week eph, toe eph)        -- time elapsed since toe [s]           
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
    :: (Integer, Pico)                                       -- ^ GPS week, time-of-week
    -> NavRecord
    -> Double                                                -- ^ eccentric anomaly [rad]
eAnom (w, tow) NavRecord{..} =                              
    let a  = (sqrtA)*(sqrtA)                                 -- semi-major axis
        n0 = sqrt(mu/(a*a*a))                                -- computed mean motion [rad/sec]
        n  = n0 + deltaN                                     -- corrected mean motion [rad/s]
        tk = realToFrac $
             diffGpsTime (w, tow) (week, toe)                -- time elapsed since toe [s]
        mk = m0 + n*tk                                       -- mean anomaly for tk
    in keplerSolve mk e                                      -- eccentric anomaly [rad]

-- | Compute relativistic correction for satellite clock
--   based on IS-GPS-200N 20.3.3.3.3.1 ready-made formulas.
relCorr
    :: (Integer, Pico)                                       -- ^ GPS week, time-of-week
    -> NavRecord                                             -- ^ ephemeris
    -> Pico                                                  -- ^ dtr - relativistic correction [s]
relCorr (w, tow) NavRecord{..} = realToFrac (fRel * e * sqrtA * sin ek)
    where
      ek = eAnom (w, tow) NavRecord{..}                      -- eccentric anomaly [rad]

-- | Compute satellite clock correction
--   based on IS-GPS-200N 20.3.3.3.3.1 ready-made formulas.
clkCorr
    :: (Integer, Pico)                                       -- ^ GPS week, time-of-week
    -> NavRecord                                             -- ^ ephemeris                                 
    -> Pico                                                  -- ^ dtsv - satellite clock correction [s]
clkCorr (w, tow) NavRecord{..} = realToFrac (af0 + af1*dt + af2*dt^2)
    where
      dt  = realToFrac $ diffGpsTime (w, tow) (week, toc)
      (_, toc) = gpsCalTimeToWeekTow calToc
            
-- | Iteratively compute signal emission time because the emission
--   time depends on the clock corrections and the clock corrections
--   depend on the emission time.
emissionTime            
  :: Double                                                  -- ^ pseudorange pr1 [m]
  -> Double                                                  -- ^ pseudorange pr2 [m]
  -> (Integer, Pico)                                         -- ^ receiver time of signal reception [s]
  -> NavRecord                                               -- ^ broadcast ephemeris
  -> (Integer, Pico)                                         -- ^ signal emission time [s]
emissionTime pr1 pr2 (w, trv) eph = iterate te0 0
    where
      pr   = pseudorangeDF pr1 pr2
      tsv  = subSeconds (w, trv)  (realToFrac (pr/c))    -- satelite time of signal emission
      te0 = tsv
      iterate te k
          | k >= 10                  = error "Number of time emission iterations exceeded"
          | abs (diffGpsTime te' te) < 1e-12 = te'
          | otherwise                        = iterate te' (k+1)
          where
            dtb  = clkCorr te eph
            dtr  = relCorr te eph
            dtsv = dtb  + dtr
            te' = subSeconds te0 dtsv

-- | Calculates the number of seconds between two GPS times.
diffGpsTime
    :: (Integer, Pico)                                       -- ^ GPS week, time-of-week [s]
    -> (Integer, Pico)                                       -- ^ GPS week, time-of-week [s]
    -> Pico                                                  -- ^ time difference [s]
diffGpsTime (w2,tow2) (w1,tow1) =
    fromInteger (dw * 604800) + dtow
    where
      dw   = w2   - w1
      dtow = tow2 - tow1

-- | Substract seconds from GPS time. It is only used to calculate the
--   signal propagation time. It uses the information that the
--   propagation time is less than 1 second.
subSeconds
    :: (Integer, Pico)                                       -- ^ GPS week, time-of-week [s]
    -> Pico                                                  -- ^ seconds [s]
    -> (Integer, Pico)                                       -- ^ GPS week, time-of-week [s]
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
mkGpsCalendarTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> GpsCalendarTime
mkGpsCalendarTime y mon d h m s = LocalTime (fromGregorian y mon d) (TimeOfDay h m s)

-- | Parse a single navigation data record
parseNavRecord :: BSC.ByteString -> Maybe NavRecord
parseNavRecord r = do
  let (l1:l2:l3:l4:l5:l6:_:l8:_) = BSC.lines r

  (sys, _) <- BSC.uncons l1                                
  guard (sys == 'G')
  (prn, _) <- BSC.readInt $ getField  1 2 l1     
  (y  , _) <- BSC.readInt $ getField  4 4 l1
  (mon, _) <- BSC.readInt $ getField  9 2 l1
  (d  , _) <- BSC.readInt $ getField 12 2 l1
  (h  , _) <- BSC.readInt $ getField 15 2 l1
  (m  , _) <- BSC.readInt $ getField 18 2 l1
  (s  , _) <- BSC.readInt $ getField 21 2 l1
  
  let calToc = mkGpsCalendarTime (toInteger y) mon d h m (fromIntegral s)

  af0      <- readDoubleField $ getField 23 19 l1
  af1      <- readDoubleField $ getField 42 19 l1
  af2      <- readDoubleField $ getField 61 19 l1

  crs      <- readDoubleField $ getField 23 19 l2
  deltaN   <- readDoubleField $ getField 42 19 l2
  m0       <- readDoubleField $ getField 61 19 l2

  cuc      <- readDoubleField $ getField  4 19 l3
  e        <- readDoubleField $ getField 23 19 l3
  cus      <- readDoubleField $ getField 42 19 l3
  sqrtA    <- readDoubleField $ getField 61 19 l3

  toeD     <- readDoubleField $ getField  4 19 l4
  cic      <- readDoubleField $ getField 23 19 l4
  omega0   <- readDoubleField $ getField 42 19 l4
  cis      <- readDoubleField $ getField 61 19 l4

  i0       <- readDoubleField $ getField  4 19 l5
  crc      <- readDoubleField $ getField 23 19 l5
  omega    <- readDoubleField $ getField 42 19 l5
  omegaDot <- readDoubleField $ getField 61 19 l5                 
                                                               
  iDot     <- readDoubleField $ getField  4 19 l6
  weekD    <- readDoubleField $ getField 42 19 l6

  fitIntvD <- readDoubleField $ getField 23 19 l8

  let toe     = realToFrac toeD
      week    = round weekD
      fitIntv = round fitIntvD
                
  return NavRecord {..}

-- | GPS calendar time to GPS week and time-of-week
gpsCalTimeToWeekTow
    :: GpsCalendarTime                                       -- ^ GPS calendar time
    -> (Integer, Pico)                                       -- ^ GPS week, time-of-week
gpsCalTimeToWeekTow (LocalTime date (TimeOfDay h m s)) =
    let gpsStartDate = fromGregorian 1980 1 6                -- The date from which the GPS time is counted
        days         = diffDays date gpsStartDate            -- Number of days since GPS start date
        w            = days `div` 7                          -- GPS week
        dow          = days `mod` 7                          -- GPS day-of-week
        tow          = fromIntegral ( dow * 86400
                                    + toInteger (h * 3600 + m * 60)
                                    )
                     + s
    in (w, tow)
    
-- | Ephemeris validity check based on fitInterval ephemeris field for given observation time
isEphemerisValid
  :: (Integer, Pico)                                         -- GPS week number, time-of-week
  -> NavRecord
  -> Bool
isEphemerisValid (w, tow) eph 
    |     dw == 0  = abs dtow <= halfFitIntv                -- condition for the same week
    | abs dw == 1  = abs dtow >  halfFitIntv                -- condition for adjacent weeks
    | otherwise    = False
    where
      dw   =   w - week eph                                 -- conversion is needed for equality comparisons
      dtow = tow - toe  eph
      halfFitIntv = realToFrac ((fitIntv eph) `div` 2 * 3600)

-- Main program:
--   * Converts receiver time of signal reception in calendar format
--     (observation time,
--      observation epoch,
--      receiver time tag,
--      receiver time stamp,
--      measurement time)
--     to receiver week number and receiver time-of-week,
--   * reads broadcast ephemeris from one record file
--     (to use a different navigation record, replace the file content), 
--   * checks ephemeris validity for the given epoch (observation time),
--   * calculates signal emission GPS time and the satellite ECEF position at that time,
--   * prints receiver clock time of signal reception, the emission time, and satellite position.
main :: IO ()
main = do
  let obsTime  = mkGpsCalendarTime 2024 03 07 00 53 01.0000000         -- Input: receiver time of signal reception
      pr1      = 21548635.724                                          -- Input: pseudorange for f1 e.g. C1C
      pr2      = 21548628.027                                          -- Input: pseudorange for f2 e.g. C2X
      fn       = "nav_record.txt"                                      -- Input: file name
      (w, trv) = gpsCalTimeToWeekTow obsTime                           -- receiver GPS week number, receiver tow
  navRec <- BSC.readFile fn                                            -- navigation data record
  case parseNavRecord navRec of
    Nothing  -> putStrLn $ "Can't read navigation record from " ++ fn
    Just eph ->                                                        -- ephemeris
       if isEphemerisValid (w,trv) eph
       then do
         let te      = emissionTime pr1 pr2 (w, trv) eph               -- Output: signal emission time by GPS clock [s]
             (x,y,z) = satPosition te eph                              -- Output: satelite ECEF position [m]
         putStrLn $ "Receiver clock time of signal reception [w,s]: " ++ show (w, trv)
         putStrLn $ "Emission time                           [w,s]: " ++ show te
         putStrLn ""
         printf "ECEF satellite position [m]:\n"
         printf "X = %18.9f\n" x
         printf "Y = %18.9f\n" y
         printf "Z = %18.9f\n" z
       else printf "The ephemeris is out of date for the given time\n"

