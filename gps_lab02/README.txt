2025-11-16

EN:
=====================================================================
         GPS SATELLITE POSITION AT SIGNAL TRANSMISSION TIME
 USING BROADCAST EPHEMERIS AND DUAL-FREQUENCY PSEUDORANGE OBSERVATION
=====================================================================

The implementation follows the specifications of IS-GPS-200N.
The calculated position is of low precision because the pseudorange code is of low precision
and the orbital parameters (ephemeris) are of low quality.

Input
-----
- receiver clock time of signal reception     (hand copied from a RINEX observation file)
- code pseudorange for f1 [m]                 (hand copied from a RINEX observation file)
- code pseudorange for f2 [m]                 (hand copied from a RINEX observation file)
- navigation data record in RINEX 3.04
  format provided in nav_record.txt file      (hand copied from a RINEX navigation file)

Output
------
- signal transmission time by GPS clock [s]
- satellite position in ECEF [m] at transmission time

Print of run
------------
Receiver clock time of signal reception        [w,s]: (2304, 348781.000000000000)
Transmission time                              [w,s]: (2304, 348780.927812714130)
Satellite ECEF position at transmission time [m,m,m]: ( 4460302.794944782, 17049812.692289820, 19845264.366251210)


PL:
=====================================================================
  POZYCJA SATELITY GPS W CHWILI TRANSMISJI SYGNAŁU Z WYKORZYSTANIEM
                      EFEMERYDY ROZGŁOSZENIOWEJ
         I DWUCZĘSTOTLIWOŚCIOWEJ OBSERWACJI PSEUDOODLEGŁOŚCI
=====================================================================

Implementacja opiera się na specyfikacji IS-GPS-200N.
Obliczona pozycja jest niskiej precyzji ponieważ pseudoodległość kodowa jest niskiej precyzji
a parametry orbitalne (efemeryda) są niskiej jakości.

Wejście
-------
- czas odbiornika odbioru sygnału             (skopiowany ręcznie z pliku obserwacyjnego RINEX)
- pseudoodległość kodowa dla f1 [m]           (skopiowana ręcznie z pliku obserwacyjnego RINEX)
- pseudoodległość kodowa dla f2 [m]           (skopiowana ręcznie z pliku obserwacyjnego RINEX)
- rekord danych nawigacyjnych w formacie
  RINEX 3.04 zawarty w pliku nav_record.txt   (skopiowany ręcznie z pliku nawigacyjnego RINEX)

Wyjście
------
- czas transmisji sygnału według zegara GPS [s]
- pozycja satelity w ECEF [m] w momencie transmisji

Wydruk uruchomienia
---------------------
Receiver clock time of signal reception        [w,s]: (2304, 348781.000000000000)
Transmission time                              [w,s]: (2304, 348780.927812714130)
Satellite ECEF position at transmission time [m,m,m]: ( 4460302.794944782, 17049812.692289820, 19845264.366251210)
