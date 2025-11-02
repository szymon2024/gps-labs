2025-11-02

EN:
======================================================================
                         GPS SATELLITE POSITION
======================================================================


Program that computes the position of a GPS satellite from broadcast ephemeris data and a GPS date.
The algorithm is based on the official specification IS-GPS-200N.
The program outputs satellite coordinates in the ECEF (Earth-Centered, Earth-Fixed) frame.
Other versions can be found under the names eph2xyz or eph2pos.


Input data in the program code
------------------------------
GPS Ephemeris, GPS Time


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
                          POZYCJA SATELITY GPS
======================================================================


Program obliczający pozycję satelity GPS na podstawie efemerydy nadanej przez satelitę oraz daty GPS.
Algorytm opracowano na podstawie specyfikacji IS-GPS-200N.
Program wyznacza współrzędne satelity w układzie ECEF (Earth-Centered, Earth-Fixed).
Inne wersje można znaleźć pod nazwami eph2xyz albo eph2pos.


Dane wejściowe w kodzie programu
--------------------------------
efemeryda GPS, czas GPS


Przykładowe wyjście
-------------------
                 (w   , tow) = (2304, 424830.0000000000)
                 (week, toe) = (2304, 424800.0000000000)
Number of seconds since toe  =            30.0000000000

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433
