2025-10-15

EN:
======================================================================
			GPS SATELLITE POSITION
======================================================================


A simple Haskell program that computes the position of a GPS satellite from broadcast ephemeris data and a given GPS time.
The algorithm is based on the official specification IS-GPS-200H.  
The program outputs satellite coordinates in the ECEF (Earth-Centered, Earth-Fixed) frame.


Input data in the program code
------------------------------
GPS Ephemeris, GPS Time


Output example
--------------
ECEF satellite position [m]:
X = 22121977.179040890
Y = 13231276.153471118
Z =  7440056.739046174


Notes
-----
All code is contained in a single file: GpsSatPos.hs.


Data flow
---------
GPS Ephemeris + GPS Time  
           |
           V
Ephemeris validity check
           |
           V
Orbital computations  
(Kepler, harmonic corrections,  
inclination, ascending node)  
           |
           V
Transformation to ECEF frame  
           |
           V
Satellite position (X, Y, Z)  



PL:  
======================================================================
			 POZYCJA SATELITY GPS
======================================================================


Prosty program w Haskellu obliczający pozycję satelity GPS na podstawie efemerydy nadanej przez satelitę oraz zadanego czasu GPS.
Algorytm opracowano na podstawie specyfikacji IS-GPS-200H.  
Program wyznacza współrzędne satelity w układzie ECEF (Earth-Centered, Earth-Fixed).


Dane wejściowe w kodzie programu
--------------------------------
efemeryda GPS, czas GPS


Przykładowy wynik
-----------------
ECEF satellite position [m]:
X = 22121977.179040890
Y = 13231276.153471118
Z =  7440056.739046174


Uwagi
-----
Cały kod znajduje się w jednym pliku: GpsSatPos.hs.


Przepływ danych
---------------
efemeryda GPS + czas GPS
           |
           V
Sprawdzenie ważności efemerydy
           |
           V
Obliczenia orbitalne
(Kepler, korekty harmoniczne,  
inklinacja, węzeł wstępujący)  
           |
           V
Transformacja do układu ECEF
           |
           V
Pozycja satelity (X, Y, Z)  
