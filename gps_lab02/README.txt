2025-11-10

EN:
=====================================================================
         GPS SATELLITE POSITION AT SIGNAL TRANSMISSION TIME
 USING BROADCAST EPHEMERIS AND DUAL-FREQUENCY PSEUDORANGE OBSERVATION
=====================================================================

The implementation follows the specifications of IS-GPS-200N.

Input
-----
- receiver clock time of signal reception     (hand copied from a RINEX observation file)
- pseudoranges [m]                            (hand copied from a RINEX observation file)
- navigation data record in RINEX 3.04
  format provided in nav_record.txt file      (hand copied from a RINEX navigation file)

Output
------
- signal transmission time by GPS clock [s]
- satellite position in ECEF [m] at transmission time

Run example
-----------
Receiver clock time of signal reception        [w,s]: (2304, 348781.0000000000)
Transmission time                              [w,s]: (2304, 348780.9278127141)
Satellite ECEF position at transmission time [m,m,m]: ( 4460302.794945, 17049812.692290, 19845264.366251)


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
- pseudoodległości [m]                        (skopiowane ręcznie z pliku obserwacyjnego RINEX)
- rekord danych nawigacyjnych w formacie
  RINEX 3.04 zawarty w pliku nav_record.txt   (skopiowany ręcznie z pliku nawigacyjnego RINEX)

Wyjście
------
- czas transmisji sygnału według zegara GPS [s]
- pozycja satelity w ECEF [m] w momencie transmisji

Przykład uruchomienia
---------------------
Receiver clock time of signal reception        [w,s]: (2304, 348781.0000000000)
Transmission time                              [w,s]: (2304, 348780.9278127141)
Satellite ECEF position at transmission time [m,m,m]: ( 4460302.794945, 17049812.692290, 19845264.366251)


