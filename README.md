# SIMULATION STUDY

200 simulated tracks of length 100,000
- correlated random walk, shape=1, scale=0.5
- DielPattern added

For all telemetry tracks: 
- p(NA) = 0.3 (results in about 80% position yield according to yaps paper fig 1)
- p(MP) = 0.03 (aspect quite high multipath in all case studies)

Files to run:
- prepare_trueTracks.R : creates 200 different simulated tracks of 100,000 positions
- prepare_toas.R : creates for each simulated track and each possible combination of settings a toa dataframe with metadata on the first 7 rows and a nametag containing the settings. The different settings are:
    - ping type: sbi, rbi, pseudo-rbi (uncomment the one you run)
    - bi length
    - shift relative to array contour
- run_estimations.R : estimates yaps-track for each toa dataframe and writes away the mean error and metadata (i.e. settings) of the track in a table. The idea is to run this first for the toa dataframes of rbi and its different bi lengths, with different chunk sizes in the paralellization process. The optimal chunk size that results from this run can then be used for the estimations on the remaining toa dataframes.


## Effect of ping type
We compare following ping types:
- stable burst interval (sbi)
- random burst interval (rbi)
- pseudo random burst interval (pseudo-rbi) (i.e. with known sequence of burst intervals)

## Effect of chunk size
Test different chunk sizes for running YAPS in parallel. This is kind of a "parameter optimisation" => we keep the best chunk size for the other runs. **Only relevant for YAPS.**

Test chunk sizes: 250, 500, 1000, 5000, 10000 (For the longest burst interval, the largest chunk size possible will be around 1000 only)

For each track (of 200):
- 1 pingType: rbi (this is the hardest to run, so the optimal parameter found here should work for the other pingTypes)
- 6 burst interval lengths
- 5 chunk sizes
- 6x5x200 = 6,000 runs or estimations

## Effect of bi length
Test sbi’s: 1.2, 5, 15, 25, 67.5, 90  
Test rbi’s: 1.1/1.3, 1/9, 9/21, 17/33; 45/90, 60/120  (min/max bi)   

For each track (of 200):
- 3 pingTypes
- 6 burst interval lengths
- => 3x6x200=3600 runs or estimations (1200 of these are already run in "Effect of chunk size")

## Effect of out-of-array
Shift the track so that it is entirely outside of the array, with different distances to the array contour

Test distances: 0, 100, 200 (i.e. shortest distance between track and array contour)

For each track (of 200):
- 3 distances
- 3 pingTypes
- 6 burst interval lengths
- => 3x6x3x200 = 10800 runs or estimations


## TOTAL:

- 6000+3600-1200+10800 = 15,900 runs for YAPS
- 3600+10800 = 14400 TDOA runs
- 3600+10800 = 14400 VPS runs

