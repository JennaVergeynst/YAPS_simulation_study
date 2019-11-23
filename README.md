# YAPS SIMULATION STUDY

Simulation study to investigate the sensitivy of YAPS to following parameters: burst type, burst interval, number of observations, reflectivity of the environment, out-of-array positioning and temporal hydrophone resolution.

To run the code, installation of the YAPS package is required: https://github.com/baktoft/yaps (Baktoft, Gjelland, Økland & Thygesen (2017): Positioning of aquatic animals based on time-of-arrival and random walk models using YAPS (Yet Another Positioning Solver). DOI:10.1038/s41598-017-14278-z).

## Effect of burst type
The effect of 3 burst types is tested:
- stable burst interval ('sbi'): the interval between transmissions is fixed
- random burst interval ('rbi'): the interval between transmissions is a random number between a min and max value.
- known burst interval ('pbi'): the interval between transmissions is random, but the sequence of the random intervals is known.

## Effect of burst interval
The 7 tested mean burst intervals are (in s):
1.2, 5, 15, 30, 45, 60, 90

## Effect of pMP
Put the indentifier sim_part on 1 to run the effect of probability on multipath pMP: 5 probabilities equally spread between 0 and 0.1. The code runs 50 tracks of 5000 s, with temporal hydrophone resolution 200 μs (4800 Hz), for each combination of burst type and burst interval.

## Effect of out-of-array
Put sim_part = 2 to run the effect of positioning out-of-array: 
- shift = 0 is inside the array
- shift = 0.5: shifts the simulated tracks a half array-length out-of-array
- shift = 1: shifts the simulated track completely out-of-array

The code runs 50 tracks of 5000 s, with temporal hydrophone resolution 200 μs (4800 Hz) and pMP = 0.025, for each combination of burst type and burst interval.

## Effect of track length
Put sim_part = 3 to run the effect of track lenght or number of observations. The code runs 50 tracks of each of following track lengths: 500, 1000, 2500, 5000 and 10000 s, with temporal hydrophone resolution 200 μs (4800 Hz) and pMP = 0.025, for each combination of burst type and burst interval.

## Effect of temporal hydrophone resolution
Put sim_part = 4 to run the effect of the temporal resolution of the hydrophones: 1000, 200, 130 or 52 μs. The code runs 50 tracks of 5000 s, with pMP = 0.025, for each combination of burst type and burst interval.


