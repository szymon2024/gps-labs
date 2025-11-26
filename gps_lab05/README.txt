2025-11-26

EN:
======================================================================
    SELECTION OF GPS BROADCAST EPHEMERIS FOR THE OBSERVATION TIME
======================================================================

The program selects the ephemeris from a RINEX 3.04 navigation file
for a given observation time and a GPS satellite.  The main selection
criterion is the ephemeris with the nearest (week, toe) and
svHealth==0, for which the observation time is within fitIntv.

Input:				         
  - RINEX 3.04 navigation file name         
  - receiver time of signal reception (GPS observation time)
  - satellite number		         
                                       
Output:				         
  - navigation record (ephemeris)	         
                                       
Print of run:			         
Observation time: 2025 08 02 01 00 01.5
          calToe: 2025 08 02 02 00 00
PRN: 6    calToc: 2025 08 02 02 00 00
af0:      -4.722196608782e-4         
af1:      -1.432454155292e-11        
af2:      0.0                        
iode:     69                         
crs:      -23.6875                   
deltaN:   3.821944913632e-9          
m0:       -2.959416369262            
cuc:      -1.28336250782e-6          
e:        3.335768124089e-3          
cus:      4.727393388748e-6          
sqrtA:    5153.617370605             
toe:      525600.0                   
cic:      8.940696716309e-8          
omega0:   0.681838248157             
cis:      4.470348358154e-8          
i0:       0.9884960063693            
crc:      305.40625                  
omega:    -0.669483880108            
omegaDot: -7.833540584123e-9         
iDot:     -1.753644474903e-10        
week:     2377                       
svHealth: 0                          
iodc:     69                         
ttom:     518400.0                   
fitIntv:  4                          
----------------------------------

PL:
======================================================================
       WYBÓR EFEMERYDY ROZGŁOSZENIOWEJ GPS DLA CZASU OBSERWACJI
======================================================================

Program wybiera efemerydę z pliku nawigacyjnego RINEX 3.04 dla danego
czasu obserwacji i satelity GPS. Głównym kryterium wyboru jest
efemeryda z najbliższym (week, toe) i svHealth==0, dla której czas
obserwacji mieści się w fitIntv.

Wejście:				         
  - nazwa pliku nawigacyjnego RINEX 3.04
  - czas odbiornika odbioru sygnału (czas GPS obserwacji)
  - numer satelity GPS		         
                                       
Output:				         
  - rekord nawigacyjny (efemeryda)
                                       
Wydruk uruchomienia:			         
Observation time: 2025 08 02 01 00 01.5
          calToe: 2025 08 02 02 00 00
PRN: 6    calToc: 2025 08 02 02 00 00
af0:      -4.722196608782e-4         
af1:      -1.432454155292e-11        
af2:      0.0                        
iode:     69                         
crs:      -23.6875                   
deltaN:   3.821944913632e-9          
m0:       -2.959416369262            
cuc:      -1.28336250782e-6          
e:        3.335768124089e-3          
cus:      4.727393388748e-6          
sqrtA:    5153.617370605             
toe:      525600.0                   
cic:      8.940696716309e-8          
omega0:   0.681838248157             
cis:      4.470348358154e-8          
i0:       0.9884960063693            
crc:      305.40625                  
omega:    -0.669483880108            
omegaDot: -7.833540584123e-9         
iDot:     -1.753644474903e-10        
week:     2377                       
svHealth: 0                          
iodc:     69                         
ttom:     518400.0                   
fitIntv:  4                          
----------------------------------
