rm(list=ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file directory as working directory
PATH = getwd()
library(yaps)
#set.seed(42)

source("wrapper_functions.R")

# Create fixed hydro array
# hx.min <- -250
# hx.max <- 250
# hy.min <- -250
# hy.max <- 250
# hx <- c(hx.min,hx.min,hx.max,hx.max, 0)
# hy <- c(hy.min,hy.max,hy.max,hy.min, 0)
# 
# hydros <- data.frame(hx=hx, hy=hy)
# write.csv(hydros,paste(PATH,'/results/hydros.csv',sep = ''))

# nb_repetitions <- 200 # nb of different simulated tracks for each setting
# n <- 100000

## test with short tracks of 500 positions!
nb_repetitions <- 30
n <- 500

for (r in sequence(nb_repetitions)){
  # x0 <- stats::runif(1,hx.min,hx.max)
  # y0 <- stats::runif(1,hy.min,hy.max)
  trueTrack <- simTrueTrack(model='crw', n = n, deltaTime=1, shape=1, scale=0.5, addDielPattern=TRUE, start_pos = c(0,0))
  ##trueTrack <- simTrueTrack(model='rw', n = n, deltaTime=1, D=10, addDielPattern=TRUE)
  # HERE TEMPORARILY READ IN TRUETRACK instead of simulating!
  # trueTrack <- read.csv(paste(PATH,'/results/trueTracks/trueTrack_',toString(r),'.csv',sep = ''), row.names = 1)
  hydros <- simHydros_adapted(trueTrack=trueTrack)
  write.csv(trueTrack,paste(PATH,'/results_TEST_short_tracks/trueTracks/trueTrack_',toString(r),'.csv',sep = ''))
  write.csv(hydros, paste(PATH,'/results_TEST_short_tracks/hydros/hydros_',toString(r),'.csv',sep = ''))
}



r <- 2
trueTrack <- read.csv(paste(PATH,'/results/trueTracks/trueTrack_',toString(r),'.csv',sep = ''), row.names = 1)
hydros <- simHydros_adapted(trueTrack=trueTrack)
plot(trueTrack$x, trueTrack$y)
points(hydros$hx, hydros$hy, col='red')

