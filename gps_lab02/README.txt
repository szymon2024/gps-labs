2025-10-16

EN:
=====================================================================
       GPS SATELLITE POSITION AT EMISSION SIGNAL TIME OF SIGNAL
 BASED ON THE BROADCAST EPHEMERIS AND P1/P2 PSEUDORANGE OBSERVATION
=====================================================================

This project implements algorithm for estimating the position of a GPS satellite
at the signal emission time, based on a single broadcast ephemeris and L1/L2 pseudorange  measurement.

The implementation follows the specifications of IS-GPS-200H, in particular:
- Section 20.3.3.4.3:   User Algorithm for Ephemeris-Based Satellite Position
- Section 20.3.3.3.3.1: User Algorithm for SV Clock Correction


Input
-----
- Receiver time tag                      (hand copied from a RINEX observation file)
- Pseudoranges: C1C (P1), C2X (P2) [m]   (hand copied from a RINEX observation file)
- Navigation data record in RINEX 3.04
  format provided in nav_record.txt file (hand copied from a RINEX navigation file)


Output
------
- te   : signal emission time in GPS system time [s]
- svPos: satellite position in ECEF [m] at emission time

Run example
-----------
Receiver clock time of signal reception  : 518400.0000000000s
Emission time                            : 518399.9216574042s
Satellite ECEF position at emission time : (-8854167.186785, 11687456.631190, 22214108.110917)


PL:

=====================================================================
	    POZYCJA SATELITY GPS W MOMENCIE EMISJI SYGNAŁU
		NA PODSTAWIE EFEMERYDY ROZGŁOSZENIOWEJ
		 I OBSERWACJI PSEUDOODLEGŁOŚCI P1/P2
=====================================================================

W tym projekcie zaimplementowano algorytm oszacowania położenia satelity GPS w momencie emisji sygnału, na podstawie pojedynczej efemerydy rozgłoszeniowej i pomiaru pseudoodległości L1/L2.

Implementacja opiera się na specyfikacji IS-GPS-200H, w szczególności:
- Section 20.3.3.4.3:   User Algorithm for Ephemeris-Based Satellite Position
- Section 20.3.3.3.3.1: User Algorithm for SV Clock Correction


Wejście
-------
- Znacznik czasu odbiornika                 (skopiowany ręcznie z pliku obserwacyjnego RINEX)
- Pseudoodległości: C1C (P1), C2X (P2) [m]  (skopiowane ręcznie z pliku obserwacyjnego RINEX)
- Rekord danych nawigacyjnych w formacie
  RINEX 3.04 zawarty w pliku nav_record.txt (skopiowany ręcznie z pliku nawigacyjnego RINEX)


Wyjście
-------
- te   : czas emisji sygnału w czasie systemowym GPS [s]
- svPos: pozycja satelity w ECEF [m] w momencie emisji

Przykład uruchomienia
---------------------
Receiver clock time of signal reception  : 518400.0000000000s
Emission time                            : 518399.9216574042s
Satellite ECEF position at emission time : (-8854167.186785, 11687456.631190, 22214108.110917)
