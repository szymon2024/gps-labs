{-# LANGUAGE RecordWildCards #-}
-- 2025-10-14

{- EN: Determining the GPS satellite position from the GPS ephemeris and a given GPS time.
       Based on IS-GPS-200H.
   PL: Wyznaczenie pozycji satelity GPS z efemerydy GPS i danego czasu GPS.
       Opracowano na podstawie IS-GPS-200H.
-}

import Data.Time.Calendar  (fromGregorian, diffDays, dayOfWeek)
import Text.Printf         (printf)

-- | EN: GPS ephemeris (a subset of fields)
-- | PL: Efemeryda GPS (podzbiór pól)
data Ephemeris = Ephemeris
  { crs      :: Double            -- ^ EN: orbital radius correction [m]
                                  -- ^ PL: poprawka harmoniczna promienia orbity [m]
  , deltaN   :: Double            -- ^ EN: mean motion difference [rad/s]
                                  -- ^ PL: średnia różnica ruchu [rad/s]
  , m0       :: Double            -- ^ EN: mean anomaly at toe epoch [rad]
                                  -- ^ PL: anomalia średnia w epoce toe [rad]
  , cuc      :: Double            -- ^ EN: latitude argument correction [rad]
                                  -- ^ PL: poprawka harmoniczna argumentu szerokości geograficznej [rad]
  , e        :: Double            -- ^ EN: eccentricity []
                                  -- ^ PL: mimośród, ekscentryczność []
  , cus      :: Double            -- ^ EN: latitude argument correction [rad]
                                  -- ^ PL: poprawka harmoniczna argumentu szerokości geograficznej [rad]
  , sqrtA    :: Double            -- ^ EN: sqare root of semi-major axis [m^0.5]
                                  -- ^ PL: pierwiastek kwadratowy z półosi wielkiej [m^0.5]
  , toe      :: Double            -- ^ EN: time of ephemeris in GPS week [s]
                                  -- ^ PL: czas efemerydy w tygodniu GPS [s]
  , cic      :: Double            -- ^ EN: inclination correction [rad]
                                  -- ^ PL: poprawka harmoniczna inklinacji [rad]
  , omega0   :: Double            -- ^ EN: longitude of ascending node at toe epoch [rad]
                                  -- ^ PL: długość geograficzna węzła wstępującego w epoce toe [rad]
  , cis      :: Double            -- ^ EN: inclination correction [rad]
                                  -- ^ PL: poprawka harmoniczna inklinacji [rad]
  , i0       :: Double            -- ^ EN: inclination at reference epoch [rad]
                                  -- ^ PL: nachylenie orbity w epoce odniesienia [rad]
  , crc      :: Double            -- ^ EN: orbital radius corrcetion [m]
                                  -- ^ PL: poprawka harmoniczna promienia orbity [m]
  , omega    :: Double            -- ^ EN: argument of perigee
                                  -- ^ PL: argument perygeum
  , omegaDot :: Double            -- ^ EN: rate of node's right ascension [rad/s]
                                  -- ^ PL: prędkość zmiany długości geograficznej węzła wstępującego [rad/s]
  , iDot     :: Double            -- ^ EN: rate of inclination angle [rad/s]
                                  -- ^ PL: prędkość zmiany inklinacji [rad/s]
  , week     :: Double            -- ^ EN: number of GPS week
                                  -- ^ PL: numer tygodnia GPS
  } deriving (Show)

-- | EN: Constants
-- | PL: Stałe
mu, omegaEDot :: Double
mu     = 3.986005e14            -- EN: value of Earth's universal gravitational parameters in WGS84 [m^3/s^2]
                                -- PL: stała grawitacyjna Ziemi w WGS84 [m^3/s^2]
omegaEDot = 7.2921151467e-5     -- EN: Earth's rotational speed [rad/s]
                                -- PL: szybkość obrotowa Ziemi w WGS84 [rad/s]

-- | EN: Determining the GPS satellite position from the GPS ephemeris and for a given GPS sow
-- | based on IS-GPS-200H (20.3.3.4.3).
-- | PL: Wyznaczenie pozycji satelity z efemerydy GPS i dla sekund tygodnia GPS
-- | na podstawie IS-GPS-200H (20.3.3.4.3).
gpsSatellitePosition         
    :: Double                             -- ^ EN: time in second-of-week [s]
    -> Ephemeris                          -- ^ EN: ephemeris
    -> (Double, Double, Double)           -- ^ EN: position in ECEF
gpsSatellitePosition  t Ephemeris{..} =
  let
    a    = (sqrtA)*(sqrtA)                                  -- EN: semi-major axis
                                                            -- PL: półoś wielka 
    n0   = sqrt(mu/(a*a*a))                                 -- EN: computed mean motion [rad/sec]
                                                            -- PL: średni ruch [rad/sec]
    n    = n0 + deltaN                                      -- EN: corrected mean motion [rad/s]
                                                            -- PL: skorygowany ruch średni [rad/s]
    tk   = wrapWeekCrossover (t - toe)                      -- EN: time from ephemeris reference epoch toe [s]
                                                            -- PL: czas od epoki odniesienia efemeryd toe [s]
    mk   = m0 + n*tk                                        -- EN: mean anomaly for tk
                                                            -- PL: średnia anomalia
    ek   = keplerSolve mk e                                 -- EN: eccentric anomaly [rad]
                                                            -- PL: anomalia mimośrodowa [rad]
    vk   = atan2 (sqrt (1 - e*e) * sin ek) (cos ek - e)     -- EN: true anomaly
                                                            -- PL: anomalia prawdziwa
    phik = vk + omega                                       -- EN: argument of latitude
                                                            -- PL: argument szerokości geograficznej
    duk  = cus * sin (2*phik) + cuc * cos (2*phik)          -- EN: argument of latitude correction
                                                            -- PL: argument korekcji szerokości geograficznej
    drk  = crs * sin (2*phik) + crc * cos (2*phik)          -- EN: radius correction
                                                            -- PL: korekta promienia
    dik  = cis * sin (2*phik) + cic * cos (2*phik)          -- EN: inclination correction
                                                            -- PL: korekta inklinacji
    uk   = phik + duk                                       -- EN: corrected argument of latitude
                                                            -- PL: skorygowany argument szerokości geograficznej
    rk   = a * (1 - e*cos ek) + drk                         -- EN: corrected radius
                                                            -- PL: skorygowany promień
    ik   = i0 + dik + iDot*tk                               -- EN: corrected inclination
                                                            -- PL: skorygowana inklinacja
    xk'  = rk * cos uk                                      -- EN: xk' in the orbital plane
                                                            -- PL: xk' w płaszczyźnie orbitalnej
    yk'  = rk * sin uk                                      -- EN: yk' in the orbital plane
                                                            -- PL: yk' w płaszczyźnie orbitalnej
    omegak = omega0
           + (omegaDot - omegaEDot)*tk - omegaEDot*toe      -- EN: corrected longitude of ascending node
                                                            -- PL: skorygowana długość geograficzna węzła wstępującego
    xk   = xk' * cos omegak - yk' * cos ik * sin omegak     -- EN: transformation to ECEF
                                                            -- PL: transformacja  do ECEF
    yk   = xk' * sin omegak + yk' * cos ik * cos omegak     -- EN: transformation to ECEF
                                                            -- PL: transformacja yk' do ECEF
    zk   =                    yk' * sin ik
  in (xk,yk,zk)


-- | EN: Iterative solution of Kepler's equation ek = m + e sin ek (Metoda Newtona-Raphsona)
-- | PL: Rozwiązanie iteracyjne równania Keplera ek = m + e sin ek (Metoda Newtona-Raphsona)
keplerSolve    
    :: Double  -- ^ EN: mean anomaly
    -> Double  -- ^ EN: eccentricity
    -> Double  -- ^ EN: eccentric anomaly [rad]
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
            df    = 1 - e * cos eN                          -- EN: df derivative of the function f
                                                            -- PL: df pochodna funkcji f
            eN'   = eN - f/df                               -- EN: iterative formula
                                                            -- PL: formuła iteracyjna

fullWeekSeconds, halfWeekSeconds :: Double
fullWeekSeconds = 604800.0                                  -- EN: number of seconds of full week
                                                            -- PL: liczba sekund całego tygodnia
halfWeekSeconds = 302400.0                                  -- EN: number of seconds of half week
                                                            -- PL: liczba sekund połowy tygodnia

-- | EN: This function safeguards calculations in case GPS time “jumps”
-- |     across the week boundary (e.g. when toe is near the end of a week
-- |     and gpsTime is already in the next week).
-- |     It wraps the time difference into the ±302400 s (±3.5 days) interval.
-- |     Correct behavior is guaranteed as long as the actual difference
-- |     between gpsTime and toe does not exceed one full week,
-- |     which in practice is ensured by the limited validity of ephemerides (a few hours).
-- | PL: Funkcja zabezpiecza obliczenia na wypadek, gdy czas GPS „przeskoczy”
-- |     przez granicę tygodnia (np. gdy toe jest blisko końca tygodnia,
-- |     a gpsTime już w następnym tygodniu).
-- |     Dzięki temu różnica czasu zostaje zawinięta do przedziału ±302400 s (±3,5 dnia).
-- |     Poprawne działanie jest zagwarantowane, o ile rzeczywista różnica
-- |     między gpsTime a toe nie przekracza jednego pełnego tygodnia,
-- |     co w praktyce zapewnia ograniczona ważność efemeryd (maks. kilka godzin).
-- | EN: Wraps time differences across the GPS week boundary.
-- |     Assumes |gpsTime - toe| < 1 week, which is guaranteed by ephemeris validity.
-- | PL: Zawija różnicę czasu przy przejściu przez granicę tygodnia GPS.
-- |     Zakłada |gpsTime - toe| < 1 tydzień, co jest gwarantowane ważnością efemerydy.
wrapWeekCrossover
    :: Double      -- ^ EN: result of time difference in sow [s]
    -> Double      -- ^ EN: time [s]
wrapWeekCrossover t
  | t >  halfWeekSeconds = t - fullWeekSeconds
  | t < -halfWeekSeconds = t + fullWeekSeconds
  | otherwise      = t

-- | EN: (week, sow) from calendar data GPS
-- | PL: (tydzień, sekundy tygodnia) z daty kalendarzowej GPS
fromCalendarGPS
    :: Integer -> Int -> Int -> Int -> Int -> Double
    -> (Integer, Double)                               -- ^ EN: week, seconds of week
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

-- | EN: Ephemeris example
-- | PL: Przykład efemerydy
ephExample :: Ephemeris
ephExample = Ephemeris
          { crs = 134.75
          , deltaN = 4.12374319903e-9
          , m0 = -2.23037324521
          , cuc = 7.25500285625e-6
          , e = 1.61300709005e-2
          , cus = 8.00378620625e-6
          , sqrtA = 5153.71900558
          , toe = 424800.0
          , cic = 7.07805156708e-8
          , omega0 = -2.84371723002
          , cis = 2.6635825634e-7
          , i0 = 0.967821256634
          , crc = 226.75
          , omega = -1.2251476939
          , omegaDot = -7.73246494535e-9
          , iDot = 2.60367988229e-10
          , week = 2304.0
          }

{-
-- | EN: Expected result for ephemeris example
-- | PL: Oczekiwany wynik dla przykładu efemerydy
expectedResult :: (Double, Double, Double)
expectedResult = ( 22121977.179040890
                 , 13231276.153471118
                 , 7440056.739046174
                 )
-}

-- | EN: Ephemeris validity check
-- | PL: Sprawdzenie ważności efemerydy
isEphemerisValid :: Integer -> Double -> Ephemeris -> Bool
isEphemerisValid gpsWeek gpsSow Ephemeris{..} =
    let dt = (fromIntegral gpsWeek * fullWeekSeconds + gpsSow  )
           - (             week    * fullWeekSeconds + toe     )
    in abs dt <= 4 * 3600                               -- EN: ephemeris valid for a maximum of 4 hours
                                                        -- PL: efemeryda ważna maksymalnie 4h


-- | EN: Calculate GPS satelite position for GPS example ephemeris and GPS time
-- | PL: Oblicza pozycję satelity GPS dla przykładowej efemerydy GPS i czasu GPS
main :: IO ()
main = do
  let eph              = ephExample
      (gpsWeek,gpsSow) = fromCalendarGPS 2024 03 07 21 59 30.0
      (x,y,z)          = gpsSatellitePosition gpsSow eph
  if isEphemerisValid gpsWeek gpsSow eph
  then do
       printf "ECEF satellite position [m]:\n"
       printf "X = %18.9f\n" x
       printf "Y = %18.9f\n" y
       printf "Z = %18.9f\n" z
  else printf "The ephemeris is out of date for the given time"
