2025-11-12

EN:
======================================================================
    CALCULATION OF THE GPS SATELLITE POSITION FROM ORBIT PARAMETERS
======================================================================

This program calculates GPS satellite position in ECEF system based on the example orbital parameters (ephemeris) transmitted by the satellite in its navigation message and GPS time.

The algorithm was developed based on the IS-GPS-200N specification.

This type of program can be found under the names eph2xyz or eph2pos.


Input
-----
- GPS Ephemeris,
- GPS Time

Output
------
- ECEF satellite position

Print of run
------------
Entered GPS time             (w   , tow) = (2304, 424830.0000000000)
Ephemeris reference GPS time (week, toe) = (2304, 424800.0000000000)
Number of seconds since toe              =            30.0000000000

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433


PL:  
======================================================================
      OBLICZENIE POZYCJI SATELITY GPS WEDŁUG PARAMETRÓW ORBITY
======================================================================

Program wyznaczający pozycję satelity GPS w układzie ECEF na podstawie przykładowych parametrów orbity tzw. efemerydy nadawanej przez satelitę w wiadomości nawigacyjnej oraz czasu GPS.

Algorytm opracowano na podstawie specyfikacji IS-GPS-200N.

Tego typu program można znaleźć pod nazwami eph2xyz albo eph2pos.


Wejście
-------
- efemeryda GPS,
- czas GPS

Wyjście
-------
- pozycja satelity w ECEF

Wydruk uruchomienia
-------------------
Entered GPS time             (w   , tow) = (2304, 424830.0000000000)
Ephemeris reference GPS time (week, toe) = (2304, 424800.0000000000)
Number of seconds since toe              =            30.0000000000

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433
