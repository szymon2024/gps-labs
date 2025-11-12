2025-11-12

EN:
=====================================================================
         GPS SATELLITE POSITION AT SIGNAL TRANSMISSION TIME
 USING BROADCAST EPHEMERIS AND DUAL-FREQUENCY PSEUDORANGE OBSERVATION
=====================================================================

The implementation follows the specifications of IS-GPS-200N.

Input
-----
- receiver clock time of signal reception     (hand copied from a RINEX observation file)
- pseudorange for L1 [m]                      (hand copied from a RINEX observation file)
- pseudorange for L2 [m]                      (hand copied from a RINEX observation file)
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

Wejście
-------
- czas odbiornika odbioru sygnału             (skopiowany ręcznie z pliku obserwacyjnego RINEX)
- pseudoodległość dla L1 [m]                  (skopiowana ręcznie z pliku obserwacyjnego RINEX)
- pseudoodległość dla L2 [m]                  (skopiowana ręcznie z pliku obserwacyjnego RINEX)
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
