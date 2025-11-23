2025-11-23

EN:
=====================================================================
            GPS SATELLITE POSITION AT SIGNAL EMISSION TIME
 USING BROADCAST EPHEMERIS AND DUAL-FREQUENCY PSEUDORANGE OBSERVATION
=====================================================================

The calculated position is of low precision because the pseudorange
code is of low precision and the orbital parameters (ephemeris) are
approximate.

Input
-----
- receiver clock time of signal reception     (hand copied from a RINEX observation file)
- pseudorange for f1 [m]                      (hand copied from a RINEX observation file)
- pseudorange for f2 [m]                      (hand copied from a RINEX observation file)
- navigation data record in RINEX 3.04
  format provided in nav_record.txt file      (hand copied from a RINEX navigation file)

Output
------
- signal esmission time by GPS clock [s]
- satellite position in ECEF [m] at emission time

Print of run
------------
Receiver clock time of signal reception [w,s]: (2304,348781.000000000000)
Emission time                           [w,s]: (2304,348780.927812714088)

ECEF satellite position [m]:
X =  4460302.794944842
Y = 17049812.692289740
Z = 19845264.366251267


PL:
=====================================================================
   POZYCJA SATELITY GPS W CHWILI EMISJI SYGNAŁU Z WYKORZYSTANIEM
                      EFEMERYDY ROZGŁOSZENIOWEJ
         I DWUCZĘSTOTLIWOŚCIOWEJ OBSERWACJI PSEUDOODLEGŁOŚCI
=====================================================================

Obliczona pozycja jest niskiej precyzji ponieważ pseudoodległość
kodowa jest niskiej precyzji a parametry orbitalne (efemeryda) są
przybliżeniem.

Wejście
-------
- czas odbiornika odbioru sygnału             (skopiowany ręcznie z pliku obserwacyjnego RINEX)
- pseudoodległość dla f1 [m]                  (skopiowana ręcznie z pliku obserwacyjnego RINEX)
- pseudoodległość dla f2 [m]                  (skopiowana ręcznie z pliku obserwacyjnego RINEX)
- rekord danych nawigacyjnych w formacie
  RINEX 3.04 zawarty w pliku nav_record.txt   (skopiowany ręcznie z pliku nawigacyjnego RINEX)

Wyjście
------
- czas emisji sygnału według zegara GPS [s]
- pozycja satelity w ECEF [m] w momencie emisji

Wydruk uruchomienia
---------------------
Receiver clock time of signal reception [w,s]: (2304,348781.000000000000)
Emission time                           [w,s]: (2304,348780.927812714088)

ECEF satellite position [m]:
X =  4460302.794944842
Y = 17049812.692289740
Z = 19845264.366251267
