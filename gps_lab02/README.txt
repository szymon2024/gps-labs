2025-12-08

EN:
======================================================================
              GPS SATELLITE POSITION FOR GIVEN GPS TIME
                  USING BROADCAST ORBITAL PARAMETERS
======================================================================

A program for computing the position of a GPS satellite in the ECEF
coordinate system based on sample orbital parameters (ephemeris),
ephemeris validity interval and GPS time provided as input.
Calculations are performed for (w, tow) where w is GPS week and
tow is time-of-week.

Input
-----
- GPS Ephemeris
- ephemeris validity interval
- GPS Time

Output
------
- ECEF satellite position

Print of run
------------
Entered GPS time            : 2024 03 07 22 00 30
Ephemeris reference GPS time: 2024 03 07 22 00 00
Diference                   :            00 00 30

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433


PL:
=======================================================================
              POZYCJA SATELITY GPS DLA DANEGO CZASU GPS
         Z WYKORZYSTANIEM ROZGŁOSZENIOWYCH PARAMETRÓW ORBITY
=======================================================================

Program do wyznaczania pozycji satelity GPS w układzie ECEF na
podstawie przykładowych parametrów orbity tzw. efemerydy, okresu
ważności efemerydy oraz podanego czasu GPS. Obliczenia są wykonywane
dla (w, tow), gdzie w to tydzień GPS a tow to czas tygodnia.

Wejście
-------
- efemeryda GPS
- okres ważności efemerydy
- czas GPS

Wyjście
-------
- pozycja satelity w ECEF

Wydruk uruchomienia
-------------------
Entered GPS time            : 2024 03 07 22 00 30
Ephemeris reference GPS time: 2024 03 07 22 00 00
Diference                   :            00 00 30

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433
