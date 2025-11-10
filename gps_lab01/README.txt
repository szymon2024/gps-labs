2025-11-10

EN:
======================================================================
    CALCULATION OF THE GPS SATELLITE POSITION FROM ORBIT PARAMETERS
======================================================================

This program calculates the position of a GPS satellite in the ECEF system based on the orbital parameters (ephemeris) transmitted by the satellite in its navigation message and GPS time.

The algorithm was developed based on the IS-GPS-200N specification.

This type of program can be found under the names eph2xyz or eph2pos.


Input data in the program code
------------------------------
- GPS Ephemeris,
- GPS Time

Output example
--------------
                 (w   , tow) = (2304, 424830.0000000000)
                 (week, toe) = (2304, 424800.0000000000)
Number of seconds since toe  =            30.0000000000

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433


PL:  
======================================================================
      OBLICZENIE POZYCJI SATELITY GPS WEDŁUG PARAMETRÓW ORBITY
======================================================================

Program wyznaczający pozycję satelity GPS w układzie ECEF na podstawie parametrów orbity tzw. efemerydy nadanej przez satelitę w wiadomości nawigacyjnej oraz czasu GPS.

Algorytm opracowano na podstawie specyfikacji IS-GPS-200N.

Tego typu program można znaleźć pod nazwami eph2xyz albo eph2pos.


Dane wejściowe w kodzie programu
--------------------------------
- efemeryda GPS,
- czas GPS

Przykładowe wyjście
-------------------
                 (w   , tow) = (2304, 424830.0000000000)
                 (week, toe) = (2304, 424800.0000000000)
Number of seconds since toe  =            30.0000000000

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433
