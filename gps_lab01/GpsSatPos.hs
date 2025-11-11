{-# LANGUAGE RecordWildCards #-}
-- 2025-11-12

{- | Determining the GPS satellite position in ECEF from the GPS ephemeris and a given GPS time.
     Based on IS-GPS-200N.

     NOTE:
     It does not calculate the time difference as absolute time difference using the formula dw*604800+dt,
     where dw is the week difference and dt is time of week difference.
     The time difference is calculated using the wrap week crossover function
     used in receivers. This function has a limited time range of application.

     Input:
       - GPS Ephemeris                       defined in the code as ephExample,
       - GPS Time in calendar format         defined in the code as gpsTime

     Output:
       - ECEF satellite position             (x, y, z)

     Print of run:
     Entered GPS time             (w   , tow) = (2304, 424830.000000000000)
     Ephemeris reference GPS time (week, toe) = (2304, 424800.000000000000)
     Number of seconds since toe              =            30.000000000000

     ECEF satellite position [m]:
     X = 22151566.575334515
     Y = 13275548.286060918
     Z =  7260529.645377433

-}

import Data.Time.Calendar  (fromGregorian, diffDays)
import Text.Printf         (printf)

-- | GPS ephemeris (a subset of fields)
data Ephemeris = Ephemeris
  { crs      :: Double                                       -- ^ orbital radius correction [m]
  , deltaN   :: Double                                       -- ^ mean motion difference [rad/s]
  , m0       :: Double                                       -- ^ mean anomaly at toe epoch [rad]
  , cuc      :: Double                                       -- ^ latitude argument correction [rad]
  , e        :: Double                                       -- ^ eccentricity []
  , cus      :: Double                                       -- ^ latitude argument correction [rad]
  , sqrtA    :: Double                                       -- ^ square root of semi-major axis [m^0.5]
  , toe      :: Double                                       -- ^ time of ephemeris in GPS week [s]
  , cic      :: Double                                       -- ^ inclination correction [rad]
  , omega0   :: Double                                       -- ^ longitude of ascending node at toe epoch [rad]
  , cis      :: Double                                       -- ^ inclination correction [rad]
  , i0       :: Double                                       -- ^ inclination at reference epoch [rad]
  , crc      :: Double                                       -- ^ orbital radius correction [m]
  , omega    :: Double                                       -- ^ argument of perigee [rad]
  , omegaDot :: Double                                       -- ^ rate of node's right ascension [rad/s]
  , iDot     :: Double                                       -- ^ rate of inclination angle [rad/s]
  , week     :: Double                                       -- ^ number of GPS week for toe
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
mu, omegaEDot :: Double
mu        = 3.986005e14           -- WGS 84 value of earth's universal gravitational constant [m^3/s^2]
omegaEDot = 7.2921151467e-5       -- WGS 84 value of the earth's rotation rate [rad/s]

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
    :: Double                                                -- ^ mean anomaly [rad]
    -> Double                                                -- ^ eccentricity []
    -> Double                                                -- ^ eccentric anomaly [rad]
keplerSolve m e = go e0 0               
  where
    e0 = m + e * sin m
    go :: Double -> Int -> Double
    go eN k
      | k > 20 = error "Kepler method iteration count exceeded"
      | abs (eN' - eN) < 1e-12 = eN'
      | otherwise = go eN' (k+1)
          where    
            f     = eN - e * sin eN - m  
            fDot  =  1 - e * cos eN                          -- derivative of the function f
            eN'   = eN - f/fDot                              -- iterative formula

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

-- | GPS time in calendar format to GPS week number and GPS time-of-week
gpsTimeToWeekTow
    :: GpsTime                                               -- ^ GPS time in calendar format
    -> (Integer, Double)                                     -- ^ GPS week no, GPS time-of-week
gpsTimeToWeekTow (GpsTime year month day h m s) =
    let date         = fromGregorian year month  day
        gpsEpochDate = fromGregorian 1980     1    6              
        daysDiff     = diffDays date gpsEpochDate
        w            = daysDiff `div` 7                                     -- GPS week number
        dow          = daysDiff `mod` 7                                     -- GPS day-of-week
        tow          = fromIntegral ( dow * 86400
                                    + fromIntegral (h * 3600 + m * 60))
                     + s
    in (w, tow)

-- | Ephemeris example
ephExample :: Ephemeris
ephExample = Ephemeris
          { crs      =  134.75
          , deltaN   =  4.12374319903e-9
          , m0       = -2.23037324521
          , cuc      =  7.25500285625e-6
          , e        =  1.61300709005e-2
          , cus      =  8.00378620625e-6
          , sqrtA    =  5153.71900558
          , toe      =  424800.0
          , cic      =  7.07805156708e-8
          , omega0   = -2.84371723002
          , cis      =  2.6635825634e-7
          , i0       =  0.967821256634
          , crc      =  226.75
          , omega    = -1.2251476939
          , omegaDot = -7.73246494535e-9
          , iDot     =  2.60367988229e-10
          , week     =  2304
          }

-- | Ephemeris validity check.
--   Assumes that ephemeris is valid for a maximum of 4 hours.
isEphemerisValid
  :: Integer                                                 -- difference in weeks
  -> Double                                                  -- time difference
  -> Bool
isEphemerisValid dw dt
    |     dw == 0  = abs dt <= 4 * 3600.0                    -- condition for the same week
    | abs dw == 1  = abs dt >  4 * 3600.0                    -- condition for adjacent weeks
    | otherwise    = False

-- | Entry condition for the wrapWeekCrossover function.
--   Checking if the ABSOLUTE time difference is less than 302400 s.
entryConForWrap
  :: Integer                                                 -- difference in weeks
  -> Double                                                  -- time difference
  -> Bool
entryConForWrap dw dt
    |     dw == 0  = abs dt <= 302400.0                      -- condition for the same week
    | abs dw == 1  = abs dt >  302400.0                      -- condition for adjacent weeks
    |     dw >  1  = False
      

-- Calculates GPS satelite position for example GPS ephemeris and GPS time.
-- Before doing so, it checks the possibility of using the wrapWeekCrossover function
-- and the validity of the ephemeris.
main :: IO ()
main = do
  let eph       = ephExample                                 -- Input: GPS Ephemeris
      gpsTime   = GpsTime 2024 03 07 22 00 30.0              -- Input: GPS Time in calendar format
      (w, tow)  = gpsTimeToWeekTow gpsTime                   -- GPS week number, GPS time-of-week
      (x, y, z) = gpsSatellitePosition tow eph               -- Output: ECEF satellite position
      weekI     = round (week eph)::Integer                  -- conversion is needed for equality comparisons
      dw        = w   - weekI
      dt        = tow - toe  eph
  if entryConForWrap dw dt
  then if isEphemerisValid dw dt
       then do
         printf "Entered GPS time             (w   , tow) = (%d, %19.12f)\n" w      tow
         printf "Ephemeris reference GPS time (week, toe) = (%d, %19.12f)\n" weekI (toe eph)
         printf "Number of seconds since toe              =      %21.12f\n"  (wrapWeekCrossover (tow-toe eph))
         printf "\n"
         printf "ECEF satellite position [m]:\n"
         printf "X = %18.9f\n" x
         printf "Y = %18.9f\n" y
         printf "Z = %18.9f\n" z
       else printf "The ephemeris is out of date for the given GPS time\n"
  else printf "Cannot use wrap week crossover function. Too much time difference between tow and toe.\n"
