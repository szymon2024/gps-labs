2025-12-05

EN:
=====================================================================
  GPS SATELLITE POSITION FOR DUAL-FREQUENCY PSEUDORANGE OBSERVATION
                      USING BROADCAST EPHEMERIS
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
Receiver clock time of signal reception: 2024 03 07 00 53 01
Signal emission time by GPS clock      : 2024 03 07 00 53 00.927812714088

ECEF satellite position [m]:
X =  4460302.794944842
Y = 17049812.692289740
Z = 19845264.366251267

PL:
=====================================================================
      POZYCJA SATELITY GPS DLA DWUCZĘSTOTLIWOŚCIOWEJ OBSERWACJI
     PSEUDOODLEGŁOŚCI Z WYKORZYSTANIEM EFEMERYDY ROZGŁOSZENIOWEJ
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
Receiver clock time of signal reception: 2024 03 07 00 53 01
Signal emission time by GPS clock      : 2024 03 07 00 53 00.927812714088

ECEF satellite position [m]:
X =  4460302.794944842
Y = 17049812.692289740
Z = 19845264.366251267
