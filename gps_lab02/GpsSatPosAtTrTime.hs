-- 2025-11-15

{- | Estimate ECEF satellite position at GPS transmission time [s] from broadcast ephemeris
     for dual-frequency pseudorange measurement (observation).
     The calculated position is of low precision because the pseudorange code is of low precision
     and the orbital parameters (ephemeris) are of low quality.
   
     NOTE 1:
       Three different clocks must be considered:
         - GPS clock
         - satellite clock
         - receiver clock
       All of them count time in GPS time system.

       The signal transmission time tTx is by the GPS clock,
       satellite time of signal transmission by satellite clock,
       receiver time of signal reception by receiver clock.

     NOTE 2:
       Checking the validity of the ephemeris is necessary for the accuracy of calculations
       and the correct operation of the wrap week crossover function, which works for a limited time period.
       The program can check the validity of the ephemeris only for normal satellite operations
       when the value of the Fit Interval field is zero, which means that the ephemeris
       is valid within a 4-hour interval.

     NOTE 3:
       Why is transmission time calculated?
       The transmission time is calculated to calculate the satellite position for the pseudorange.
       It's irrelevant that transmission times vary for different satellites for the selected observation time.
       What matters is that the satellite's position corresponds to the satellite-receiver distance.
              
     Input:
       - receiver time of signal reception          obsTime        (hand copied from RINEX observation file)
       - pseudorange for L1 [m]                     pr1            (hand copied from RINEX observation file)
       - pseudorange for L2 [m]                     pr2            (hand copied from RINEX observation file)
       - navigation data record in RINEX 3.04
         format                                     nav_record.txt (hand copied from a RINEX navigation file)

     Output:
       - tTx     - signal transmission time by GPS clock [s]
       - (x,y,z) - satellite position in ECEF [m] at transmission time

     Print of run:
     Receiver clock time of signal reception        [w,s]: (2304, 348781.000000000000)
     Transmission time                              [w,s]: (2304, 348780.927812714130)
     Satellite ECEF position at transmission time [m,m,m]: ( 4460302.794944782, 17049812.692289820, 19845264.366251210)
-}

{-# LANGUAGE RecordWildCards #-}

import Data.Time.Calendar                      (fromGregorian, diffDays)
import Text.Printf                             (printf)
import qualified Data.ByteString.Char8  as BSC
import Control.Monad                           (guard)
import qualified Data.ByteString.Unsafe as BSU (unsafeUseAsCString)
import Foreign                                 (Ptr, alloca, peek, minusPtr)
import Foreign.C.Types                         (CChar, CDouble(CDouble))
import Foreign.C.String                        (CString)
import System.IO.Unsafe                        (unsafePerformIO)

    
-- | GPS ephemeris (a subset of fields from RINEX 3.04 navigation file)
data Ephemeris = Ephemeris
  { prn      :: Int               -- ^ satellite number
  , calToc   :: GpsTime           -- ^ toc as calendar time - clock data reference time
  , af0      :: Double            -- ^ SV clock bias correction coefficient [s]
  , af1      :: Double            -- ^ SV clock drift correction coefficient [s/s]
  , af2      :: Double            -- ^ SV clock drift rate correction coefficient [s/s^2]
  , crs      :: Double            -- ^ orbital radius correction [m]
  , deltaN   :: Double            -- ^ mean motion difference [rad/s]
  , m0       :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc      :: Double            -- ^ latitude argument correction [rad]
  , e        :: Double            -- ^ eccentricity []
  , cus      :: Double            -- ^ latitude argument correction [rad]
  , sqrtA    :: Double            -- ^ square root of semi-major axis [m^0.5]
  , toe      :: Double            -- ^ time of ephemeris in GPS week [s]
  , cic      :: Double            -- ^ inclination correction [rad]
  , omega0   :: Double            -- ^ longitude of ascending node at toe epoch [rad]
  , cis      :: Double            -- ^ inclination correction [rad]
  , i0       :: Double            -- ^ inclination at reference epoch [rad]
  , crc      :: Double            -- ^ orbital radius correction [m]
  , omega    :: Double            -- ^ argument of perigee [rad]
  , omegaDot :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot     :: Double            -- ^ rate of inclination angle [rad/s]
  , week     :: Double            -- ^ number of GPS week for toe and toc
  , fitIntv  :: Double            -- ^ fit interval flag, in normal SV operations is 0
  } deriving (Show)

-- | GPS time in calendar format               
data GpsTime = GpsTime
  { year  :: Integer
  , month :: Int
  , day   :: Int
  , h     :: Int
  , m     :: Int
  , s     :: Double
  } deriving (Show)

-- | Constants
mu, omegaEDot, c, fRel, f1, f2 :: Double
mu        = 3.986005e14           -- WGS 84 value of earth's universal gravitational constant [m^3/s^2]
omegaEDot = 7.2921151467e-5       -- WGS 84 value of the earth's rotation rate [rad/s]
c         = 299792458.0           -- speed of light [m/s]
fRel      = -4.442807633e-10      -- constant F in the relativistic correction [s/sqrt m]
f1        = 1575.42e6             -- L1 frequency [Hz]
f2        = 1227.60e6             -- L2 frequency [Hz]

-- | Determining the GPS satellite position in ECEF from the GPS ephemeris and for a GPS time-of-week
--   based on IS-GPS-200N 20.3.3.4.3 ready-made mathematical formulas.
gpsSatellitePosition         
    :: Double                                                -- ^ GPS time-of-week [s]
    -> Ephemeris                                             -- ^ ephemeris
    -> (Double, Double, Double)                              -- ^ satellite position in ECEF [m]
gpsSatellitePosition  t eph =
  let
    a      = sqrtA eph * sqrtA eph                           -- semi-major axis [m]
    n0     = sqrt(mu/(a*a*a))                                -- computed mean motion [rad/sec]       
    n      = n0 + deltaN eph                                 -- corrected mean motion [rad/s]        
    tk     = wrapWeekCrossover (t - toe eph)                 -- time elapsed since toe [s]           
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
           - omegaEDot * toe eph                             -- corrected longitude of ascending node
    xk     = xk' * cos omegak - yk' * cos ik * sin omegak    -- transformation to ECEF               
    yk     = xk' * sin omegak + yk' * cos ik * cos omegak    -- transformation to ECEF               
    zk     =                    yk' * sin ik                 -- transformation to ECEF
  in (xk,yk,zk)

-- | Iterative solution of Kepler's equation ek = m + e sin ek (Metoda Newtona-Raphsona)
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

-- | Euclidean distance between two positions
range :: (Double,Double,Double) -> (Double,Double,Double) -> Double
range (x1,y1,z1) (x2,y2,z2) = sqrt ((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)

-- | Iterative calculation eccentric anomaly
eAnom :: Double -> Ephemeris -> Double
eAnom t Ephemeris{..} =                              
    let a  = (sqrtA)*(sqrtA)                                 -- semi-major axis
        n0 = sqrt(mu/(a*a*a))                                -- computed mean motion [rad/sec]
        n  = n0 + deltaN                                     -- corrected mean motion [rad/s]
        tk = wrapWeekCrossover (t - toe)                     -- time elapsed since toe [s]
        mk = m0 + n*tk                                       -- mean anomaly for tk
    in keplerSolve mk e                                      -- eccentric anomaly [rad]

-- | Compute relativistic correction
--   based on IS-GPS-200N 20.3.3.3.3.1 ready-made formulas.
relCorr
    :: Double                                                -- ^ tow [s]
    -> Ephemeris                                             -- ^ ephemeris
    -> Double                                                -- ^ dtr - relativistic correction [s]
relCorr t Ephemeris{..} = fRel * e * sqrtA * sin ek
    where
      ek = eAnom t Ephemeris{..}                             -- eccentric anomaly [rad]


-- | Compute broadcast satellite clock correction
--   based on IS-GPS-200N 20.3.3.3.3.1 ready-made formulas.
clkCorr
    :: Double                                                -- ^ tow [s]
    -> Ephemeris                                             -- ^ ephemeris                                 
    -> Double                                                -- ^ dtsv - satellite clock correction [s]
clkCorr t Ephemeris{..} = af0 + af1*dt + af2*dt^2
    where
      dt  = wrapWeekCrossover (t - toc)
      (_, toc) = gpsTimeToWeekTow calToc
            
-- | Determination of the ECEF satellite position at transmission time from broadcast ephemeris
satPosAtEmTime            
  :: Double                                                  -- ^ pseudorange pr1 [m]
  -> Double                                                  -- ^ pseudorange pr2 [m]
  -> Double                                                  -- ^ receiver time of signal reception [s]
  -> Ephemeris                                               -- ^ broadcast ephemeris
  -> (Double, (Double,Double,Double))                        -- ^ signal transmission time [s], satelite ECEF position [m]
satPosAtEmTime pr1 pr2 trv eph = iterateTx tTx0 0
    where
      pr   = pseudorangeDF pr1 pr2
      tsv  = trv - pr/c                                       -- satelite time of signal transmission
      tTx0 = tsv
      iterateTx tTx k
          | k >= 10                  = error "Number of time transmission iterations exceeded"
          | abs (tTx' - tTx) < 1e-12 = (tTx', gpsSatellitePosition tTx' eph)
          | otherwise                = iterateTx tTx' (k+1)
          where
            dtb  = clkCorr tTx eph
            dtr  = relCorr tTx eph
            dtsv = dtb  + dtr
            tTx' = tTx0 - dtsv

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

-- | Parse a single navigation data record
parseNavRecord :: BSC.ByteString -> Maybe Ephemeris
parseNavRecord r = do
  let (l1:l2:l3:l4:l5:l6:_:l8:_) = BSC.lines r

  (sys, _) <- BSC.uncons l1                                
  guard (sys == 'G')
  (prn  , _) <- BSC.readInt $ getField  1 2 l1     
  (year , _) <- BSC.readInt $ getField  4 4 l1
  (month, _) <- BSC.readInt $ getField  9 2 l1
  (day  , _) <- BSC.readInt $ getField 12 2 l1
  (h    , _) <- BSC.readInt $ getField 15 2 l1
  (m    , _) <- BSC.readInt $ getField 18 2 l1
  (s    , _) <- BSC.readInt $ getField 21 2 l1
  
  let calToc = GpsTime (toInteger year) month day h m (fromIntegral s)

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

  toe      <- readDoubleField $ getField  4 19 l4
  cic      <- readDoubleField $ getField 23 19 l4
  omega0   <- readDoubleField $ getField 42 19 l4
  cis      <- readDoubleField $ getField 61 19 l4

  i0       <- readDoubleField $ getField  4 19 l5
  crc      <- readDoubleField $ getField 23 19 l5
  omega    <- readDoubleField $ getField 42 19 l5
  omegaDot <- readDoubleField $ getField 61 19 l5                 
                                                               
  iDot     <- readDoubleField $ getField  4 19 l6
  week     <- readDoubleField $ getField 42 19 l6

  fitIntv  <- readDoubleField $ getField 23 19 l8
                
  return Ephemeris {..}

-- | GPS time in calendar format to GPS week number and GPS time-of-week
gpsTimeToWeekTow
    :: GpsTime                                               -- ^ GPS time in calendar format
    -> (Integer, Double)                                     -- ^ GPS week number, GPS time-of-week
gpsTimeToWeekTow (GpsTime year month day h m s) =
    let date         = fromGregorian year month  day
        gpsStartDate = fromGregorian 1980     1    6         -- The date from which the GPS time is counted
        days         = diffDays date gpsStartDate            -- Number of days since GPS start date
        w            = days `div` 7                          -- GPS week number
        dow          = days `mod` 7                          -- GPS day-of-week
        tow          = fromIntegral ( dow * 86400
                                    + fromIntegral (h * 3600 + m * 60))
                     + s
    in (w, tow)
    
-- | Calculates the correct number of seconds between two GPS tows without week numbers.
--   Formula based on IS-GPS-200N 20.3.3.4.3. page 106.
--   Takes into account cases where tows are in adjacent weeks.
--   Needs entry condition provided earlier in the program assuming that
--   the absolute time difference is less than 302400 s.
--   In practice, this is satisfied by the condition of ephemeris validity.
wrapWeekCrossover
    :: Double                                                -- ^ difference between two tows        [s]
    -> Double                                                -- ^ number of seconds between two tows [s]
wrapWeekCrossover dt
  -- 604800.0 is number of seconds of full week
  -- 302400.0 is number of seconds of half week
  | dt >  302400.0 = dt - 604800.0                           -- tow in adjacent week and earlier then toe
  | dt < -302400.0 = dt + 604800.0                           -- tow in adjacent week and later then toe
  | otherwise      = dt                                      -- tow and toe in the same week


-- | Ephemeris validity check based on fitInterval ephemeris field
--   The program can check the validity of the ephemeris only for Normal SV Operations
--   when the value of the Fit Interval field is zero, which means that the ephemeris
--   is valid within a 4-hour interval.
--   Based on IS-GPS-200N 20.3.3.4.3.1 Curve Fit Intervals.
isEphemerisValid
  :: Integer                                                 -- GPS week number
  -> Double                                                  -- GPS time-of-week
  -> Ephemeris
  -> Bool
isEphemerisValid w trv eph =
    case round (fitIntv eph) of
      0 |     dw == 0  -> abs dt <= twoHours    -- condition for the same week
        | abs dw == 1  -> abs dt >  twoHours    -- condition for adjacent weeks
        | otherwise    -> False
      _                -> error "Ephemeris validity not implemented for non-zero Fit Interval"
    where
      dw       = w   - round (week eph)::Integer                   -- conversion is needed for equality comparisons
      dt       = trv -        toe  eph
      twoHours = 2 * 3600.0


-- Main program:
--   * Converts receiver time of signal reception in calendar format (observation time, observation epoch,
--     receiver time tag, receiver time stamp, measurement time) to receiver week number and receiver time-of-week,
--   * reads broadcast ephemeris from one record file
--     (to use a different navigation record, replace the file content), 
--   * checks ephemeris validity for the given epoch (receiver time tag, receiver time stamp),
--   * calculates signal transmission GPS time and the satellite ECEF position at that time,
--   * prints receiver clock time of signal reception, the transmission time, and satellite position.
--   
-- NOTE: Transmission time can be negative number. It means transmission time is in previous week e.g.
--       transmission time: (2378, -0.0783413625) = (2378-1, 604800-0.0783413625).
main :: IO ()
main = do
  let -- Most of the calculations are time-of-week callculations
      -- and time names refer to time-of-week (tow)
      -- which is enabled by wrapWeekCrossover function.
      obsTime  = GpsTime 2024 03 07 00 53 01.0000000                   -- Input: receiver time of signal reception
      pr1      = 21548635.724                                          -- Input: pseudorange for f1 e.g. C1C
      pr2      = 21548628.027                                          -- Input: pseudorange for f2 e.g. C2X
      fn       = "nav_record.txt"                                      -- Input: file name
      (w, trv) = gpsTimeToWeekTow obsTime                              -- receiver week number, receiver tow
  navRec <- BSC.readFile fn                                            -- navigation data record
  case parseNavRecord navRec of
    Nothing  -> putStrLn $ "Can't read navigation record from " ++ fn
    Just eph ->                                                        -- ephemeris
       if isEphemerisValid w trv eph
       then do
         let (tTx, (x,y,z)) = satPosAtEmTime pr1 pr2 trv eph           -- Output: signal transmission time by GPS clock [s]
                                                                       --         satellite position in ECEF [m] at transmission time
         printf "Receiver clock time of signal reception        [w,s]: (%d, %19.12f)\n"             w trv
         printf "Transmission time                              [w,s]: (%d, %19.12f)\n"             w tTx
         printf "Satellite ECEF position at transmission time [m,m,m]: (%18.9f, %18.9f, %18.9f)\n"  x y z
       else printf "The ephemeris is out of date for the given time\n"
