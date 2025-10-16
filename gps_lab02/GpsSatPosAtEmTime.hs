-- 2025-10-16

{- | Calculate satellite ECEF position at emission GPS time [s] from broadcast ephemeris
   | for P1/P2 pseudorange measurement (observation).
   
     NOTE:
       1. Three different clocks must be considered:
            - GPS clock
            - satellite clock
            - receiver clock
       2. Calculations are performed in GPS second-of-week (sow)
       3. The satellite clock time of emission time is defined as
          SV PRN code phase time at message transmission time in IS-GPS-200H.
              
     Input:
       * reception receiver time tag (hand copied from RINEX observation file)
       * C1C pseudorange [m]         (hand copied from RINEX observation file)
       * C2C pseudorange [m]         (hand copied from RINEX observation file) 
       * navigation data record from nav_record.txt

     Output:
       * te      - signal emission time in GPS system time [s]
       * (x,y,z) - satellite position in ECEF [m] at emission time
-}

{-# LANGUAGE RecordWildCards #-}

import Data.Time.Calendar                    (fromGregorian, diffDays, dayOfWeek)
import Text.Printf                           (printf)
import qualified Data.ByteString.Char8 as BS
import Data.Char                             (toUpper)
import Text.Read                             (readMaybe)
import Control.Monad                         (guard)

    
type GPSTime = (Integer, Int, Int, Int, Int, Double)

-- | GPS ephemeris (a subset of fields from RINEX 3.04 navigation file)
data Ephemeris = Ephemeris
  { prn      :: Int               -- ^ satellite number
  , toc      :: GPSTime           -- ^ clock data reference time
  , af0      :: Double            -- ^ SV clock bias correction coefficient [s]
  , af1      :: Double            -- ^ SV clock drift correction coefficient [s/s]
  , af2      :: Double            -- ^ SV clock drift rate correction coefficient
  , crs      :: Double            -- ^ orbital radius correction [m]
  , deltaN   :: Double            -- ^ mean motion difference [rad/s]
  , m0       :: Double            -- ^ mean anomaly at toe epoch [rad]
  , cuc      :: Double            -- ^ latitude argument correction [rad]
  , e        :: Double            -- ^ eccentricity []
  , cus      :: Double            -- ^ latitude argument correction [rad]
  , sqrtA    :: Double            -- ^ sqare root of semi-major axis [m^0.5]
  , toe      :: Double            -- ^ time of ephemeris in GPS week [s]
  , cic      :: Double            -- ^ inclination correction [rad]
  , omega0   :: Double            -- ^ longitude of ascending node at toe epoch [rad]
  , cis      :: Double            -- ^ inclination correction [rad]
  , i0       :: Double            -- ^ inclination at reference epoch [rad]
  , crc      :: Double            -- ^ orbital radius corrcetion [m]
  , omega    :: Double            -- ^ argument of perigee
  , omegaDot :: Double            -- ^ rate of node's right ascension [rad/s]
  , iDot     :: Double            -- ^ rate of inclination angle [rad/s]
  , week     :: Double            -- ^ number of GPS week
  } deriving (Show)

-- | Constants
mu, omegaEDot, c, fRel, f1, f2 :: Double
mu        = 3.986005e14           -- Earth's universal gravitational parameter in WGS84 [m^3/s^2]
omegaEDot = 7.2921151467e-5       -- Earth's rotational speed [rad/s]
c         = 299792458.0           -- speed of light [m/s]
fRel      = -4.442807633e-10      -- constant F in the relativistic correction [s/sqrt m]
f1        = 1575.42e6             -- L1 frequency [Hz]
f2        = 1227.60e6             -- L2 frequency [Hz]

-- | Determining the GPS satellite position from the GPS ephemeris and for a given GPS sow
-- | based on IS-GPS-200H (20.3.3.4.3).
gpsSatellitePosition         
    :: Double                                                  -- ^ time in seconds of week [s]
    -> Ephemeris                                               -- ^ ephemeris
    -> (Double, Double, Double)                                -- ^ satellite position in ECEF
gpsSatellitePosition  t Ephemeris{..} =
  let
    a      = (sqrtA)*(sqrtA)                                    -- semi-major axis
    n0     = sqrt(mu/(a*a*a))                                   -- computed mean motion [rad/sec]
    n      = n0 + deltaN                                        -- corrected mean motion [rad/s]
    tk     = wrapWeekCrossover (t - toe)                        -- time from ephemeris reference epoch toe [s]
    mk     = m0 + n*tk                                          -- mean anomaly for tk
    ek     = keplerSolve mk e                                   -- eccentric anomaly [rad]
    vk     = atan2 (sqrt (1 - e*e) * sin ek) (cos ek - e)       -- true anomaly
    phik   = vk + omega                                         -- argument of latitude
    duk    = cus * sin (2*phik) + cuc * cos (2*phik)            -- argument of latitude correction
    drk    = crs * sin (2*phik) + crc * cos (2*phik)            -- radius correction
    dik    = cis * sin (2*phik) + cic * cos (2*phik)            -- inclination correction
    uk     = phik + duk                                         -- corrected argument of latitude
    rk     = a * (1 - e*cos ek) + drk                           -- corrected radius
    ik     = i0 + dik + iDot*tk                                 -- corrected inclination
    xk'    = rk * cos uk                                        -- xk' in the orbital plane
    yk'    = rk * sin uk                                        -- yk' in the orbital plane
    omegak = omega0
           + (omegaDot - omegaEDot)*tk - omegaEDot*toe          -- corrected longitude of ascending node
    xk     = xk' * cos omegak - yk' * cos ik * sin omegak       -- transformation to ECEF
    yk     = xk' * sin omegak + yk' * cos ik * cos omegak       -- transformation to ECEF
    zk     =                    yk' * sin ik                    -- transformation to ECEF
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
            f     = eN - e * sin eN - m  
            df    = 1 - e * cos eN                              -- derivative of the function f
            eN'   = eN - f/df                                   -- iterative formula

fullWeekSeconds, halfWeekSeconds :: Double
fullWeekSeconds = 604800.0                                      -- number of seconds of full week
halfWeekSeconds = 302400.0                                      -- number of seconds of half week

-- | It wraps the time difference into the ±302400 s (±3.5 days) interval.
wrapWeekCrossover
    :: Double                                                   -- ^ result of time difference in sow [s]
    -> Double                                                   -- ^ time [s]
wrapWeekCrossover t
    | t >  halfWeekSeconds = t - fullWeekSeconds
    | t < -halfWeekSeconds = t + fullWeekSeconds
    | otherwise            = t

-- | Week, sow from calendar data GPS
fromCalendarGPS
    :: Integer -> Int -> Int -> Int -> Int -> Double
    -> (Integer, Double)                                        -- week, second-of-week
fromCalendarGPS y mo d h m s =
    let day = fromGregorian y mo d
        gpsEpochDay = fromGregorian 1980 1 6              
        daysDiff    = diffDays day gpsEpochDay
        w           = daysDiff `div` 7
        dn          = daysDiff `mod` 7
        sow         = fromIntegral (dn * 86400)
                    + fromIntegral (h  *  3600)
                    + fromIntegral (m  *    60)
                    +               s
    in (w, sow)     

-- | Ephemeris validity check
isEphemerisValid :: Integer -> Double -> Ephemeris -> Bool
isEphemerisValid gpsWeek gpsSow eph =
    let dt =   (fromIntegral gpsWeek * fullWeekSeconds + gpsSow  )
             - ((week eph)           * fullWeekSeconds + toe eph )
    in abs dt <= 4 * 3600                                       -- ephemeris valid for a maximum of 4 hours

-- | Pseudorange for dual-frequency P1/P2 receivers
pseudorangeP1P2 :: Double -> Double -> Double
pseudorangeP1P2 c1C c2X = (f1^2*c1C - f2^2*c2X) / (f1^2 - f2^2)

-- | Euclidean distance between two positions
range :: (Double,Double,Double) -> (Double,Double,Double) -> Double
range (x1,y1,z1) (x2,y2,z2) = sqrt ((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)

-- | Iterative calculation eccentric anomaly
eAnom :: Double -> Ephemeris -> Double
eAnom t Ephemeris{..} =                              
    let a  = (sqrtA)*(sqrtA)                                    -- semi-major axis
        n0 = sqrt(mu/(a*a*a))                                   -- computed mean motion [rad/sec]
        n  = n0 + deltaN                                        -- corrected mean motion [rad/s]
        tk = wrapWeekCrossover (t - toe)                        -- time from ephemeris reference epoch toe [s]
        mk = m0 + n*tk                                          -- mean anomaly for tk
    in keplerSolve mk e                                         -- eccentric anomaly [rad]

-- | Compute relativistic correction
relCorr t Ephemeris{..} =
    let ek = eAnom t Ephemeris{..}
    in fRel * e * sqrtA * sin ek

-- | Compute broadcast satellite clock correction
clkCorr :: Double -> Ephemeris -> Double
clkCorr t Ephemeris{..} =
    af0 + af1*dt + af2*dt^2
    where
      (y,m,d,h,mi,s) = toc
      (_,tocSow) = fromCalendarGPS y m d h mi s
      dt = wrapWeekCrossover (t - tocSow)
            
-- | Determination of the satellite position at emission time from broadcast ephemeris
satPosAtEmTime            
  :: Double                                  -- ^ pseudorange P1 [m]
  -> Double                                  -- ^ pseudorange P2 [m]
  -> Double                                  -- ^ receiver clock time of signal reception [s]
  -> Ephemeris                               -- ^ broadcast ephemeris
  -> (Double, (Double,Double,Double))        -- ^ signal emission time [s], satelite ECEF position [m]
satPosAtEmTime c1C c2X trv eph  = iterate te0 0
    where
      pr = pseudorangeP1P2 c1C c2X
      te0 = trv - pr/c
      iterate te k
          | k >= 10                = error "Number of time emission iterations exceeded"
          | abs (te' - te) < 1e-10 = (te', gpsSatellitePosition te eph)
          | otherwise              = iterate te' (k+1)
          where
            dtb  = clkCorr te eph
            dtr  = relCorr te eph
            dtsv = dtb + dtr
            te'  = te0 - dtsv

-- | Repleace 'D' with 'E' and add lacking '0'
normalizeExpo :: String -> String
normalizeExpo = addZero . map toE
  where
    toE c = if toUpper(c) == 'D' then 'E' else c
    addZero ('.':xs)     = '0':'.':xs
    addZero ('+':'.':xs) = '+':'0':'.':xs
    addZero ('-':'.':xs) = '-':'0':'.':xs
    addZero s            = s

readDField :: Int -> Int -> BS.ByteString -> Maybe Double
readDField off len = readMaybe . normalizeExpo . BS.unpack . BS.strip . BS.take len . BS.drop off

parseLine :: BS.ByteString -> Maybe [Double]
parseLine line = mapM f [0..3]
  where f i = readDField (4+(i*19)) 19 line

-- | Parse a single navigation data record
parseNavRecord :: BS.ByteString -> Maybe Ephemeris
parseNavRecord r = do
    let (l1:l2:l3:l4:l5:l6:_) = BS.lines r
    -- parse line 1
        [w1,w2,w3,w4,w5,w6] = BS.words $ BS.take 20 $ BS.drop 3 l1
    (satSys,_) <- BS.uncons l1
    guard (satSys == 'G')
    (prn, _) <- BS.readInt $ BS.take 2 $ BS.drop 1 l1
    (y ,_)   <- BS.readInteger w1
    (m ,_)   <- BS.readInt w2
    (d ,_)   <- BS.readInt w3
    (h ,_)   <- BS.readInt w4
    (mi,_)   <- BS.readInt w5
    s        <- readMaybe $ BS.unpack w6
    let toc = (y,m,d,h,mi,s)
    af0 <- readDField 23 19 l1
    af1 <- readDField 42 19 l1
    af2 <- readDField 61 19 l1
    -- parse lines 2-6
    [_   ,crs,deltaN,m0      ] <- parseLine (l2)
    [cuc ,e  ,cus   ,sqrtA   ] <- parseLine (l3)
    [toe ,cic,omega0,cis     ] <- parseLine (l4)
    [i0  ,crc,omega ,omegaDot] <- parseLine (l5)
    [iDot,_  ,week  ,_       ] <- parseLine (l6)
    return Ephemeris{..}
   
             
-- | Main program:
-- |   * converts receiver time tag to receiver clock time
-- |     of signal reception in GPS week and second-of-week
-- |   * reads broadcast ephemeris from one record file
-- |     (to use a different navigation record, replace the file content), 
-- |   * checks ephemeris validity for the given epoch,
-- |   * calculates signal emission GPS time and the satellite ECEF position at that time 
-- |     (IS-GPS-200H, 20.3.3.4.3),
-- |   * prints receiver clock time (sow) of signal reception, the emission time, and satellite position.
main :: IO ()
main = do
  let (trvw, trv) = fromCalendarGPS    2025 08 02 00 00 00.0000000      -- receiver time week number, receiver time sow
                                                                        -- from receiver time tag
      c1C         = 23628069.060                                        -- pseudorange P1
      c2X         = 23628076.120                                        -- pseudorange P2
      fn          = "nav_record.txt"                                    -- file name
  navRec <- BS.readFile fn                                              -- navigation data record
  case parseNavRecord navRec of
    Nothing  -> putStrLn $ "Can't read navigation record from " ++ fn
    Just eph ->                                                         -- ephemeris
         if isEphemerisValid trvw trv eph
         then do
           let (te, (x,y,z)) = satPosAtEmTime c1C c2X trv eph           -- result
           printf "Receiver clock time of signal reception  : %6.10fs\n" trv
           printf "Emission time                            : %6.10fs\n" te
           printf "Satellite ECEF position at emission time : (%8.6f, %8.6f, %8.6f)\n"  x y z
         else printf "The ephemeris is out of date for the given time"


