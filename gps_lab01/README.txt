2025-11-22

EN:
======================================================================
        COMPUTATION OF GPS SATELLITE POSITION BY THE RECEIVER
                BASED ON BROADCAST ORBITAL PARAMETERS
           IN ACCORDANCE WITH THE IS‑GPS‑200 SPECIFICATION
======================================================================

A program for computing the position of a GPS satellite in the ECEF
coordinate system based on sample orbital parameters (ephemerides)
transmitted by the satellite in the navigation message and GPS time
provided as input. The program is not intended for post‑processing
applications.

Input
-----
- GPS Ephemeris
- GPS Time

Output
------
- ECEF satellite position

Print of run
------------
Entered GPS time             (w   , tow) = (2304, 424830.000000000000)
Ephemeris reference GPS time (week, toe) = (2304, 424800.000000000000)
Number of seconds since toe              =            30.000000000000

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433


PL:
======================================================================
           OBLICZANIE POZYCJI SATELITY GPS PRZEZ ODBIORNIK
           NA PODSTAWIE ROZGŁOSZENIOWYCH PARAMETRÓW ORBITY
                  ZGODNIE ZE SPECYFIKACJĄ IS‑GPS‑200
======================================================================

Program do wyznaczania pozycji satelity GPS w układzie ECEF na podstawie
przykładowych parametrów orbity tzw. efemerydy nadawanej przez satelitę
w wiadomości nawigacyjnej oraz podawanego czasu GPS. Program nie jest
przeznaczony do zastosowań w postprocessingu.


Wejście
-------
- efemeryda GPS
- czas GPS

Wyjście
-------
- pozycja satelity w ECEF

Wydruk uruchomienia
-------------------
Entered GPS time             (w   , tow) = (2304, 424830.000000000000)
Ephemeris reference GPS time (week, toe) = (2304, 424800.000000000000)
Number of seconds since toe              =            30.000000000000

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433
