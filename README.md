# SIMULATION STUDY

200 simulated tracks, 5 repetitions for each setting => 1000 estimations for each point to calculate mean and percentiles

For all tracks: 
- p(NA) = 0.3 (results in about 80% position yield according to yaps paper fig 1)
- p(MP) = 0.03 (aspect quite high multipath in both case studies)
- correlated random walk, shape=1, scale=0.5
- DielPattern added

## Effect of track length
Simulate 200 tracks of 10,000 positions.

Test track lengths: 250, 500, 1000, 5000, 10000   
Keep shift (0) constant

For each track (of 200):
- 5 track lengths X 6 sbi’s X 5 repetitions (=150)
- 5 track lengths X 6 rbi’s X 5 repetitions (=150)
- => 60,000 runs or estimations

## Effect of bi length
Take the 250 first positions of each of the 200 tracks.

Test sbi’s: 1.2, 5, 10, 15, 20, 25   
Test rbi’s: 1.1/1.3, 1/9, 5/15, 9/21, 13/27, 17/33 (min/max bi)   
Keep shift (0) and track length (250) constant   


For each track (of 200):
- 6 sbi’s X 5 repetitions (=30)
- 6 rbi’s X 5 repetitions (=30)
- =>12,000 runs or estimations (these are also the 12,000 estimations in “effect of track length” with track length 250)

## Effect of hydrophone shift
Take the 250 first positions of each of the 200 tracks.

Test shifts: 0, 1/3, 1/2, 2/3, 1   
Keep track length (250) constant   

For each track (of 200):
- 5 shifts X 6 sbi’s X 5 repetitions (=150)
- 5 shifts X 6 rbi’s X 5 repetitions (=150)
- => 60,000 runs or estimations (the first 12,000 estimations are the same as the ones in “effect of track length” with track length 250)

## TOTAL:
- 108,000 runs for YAPS (21,600 if no repetitions)
- 21,600 TDOA runs
- 21,600 VPS runs (= 12 rbi and sbi’s x 5 shifts x 4 track lengths)

