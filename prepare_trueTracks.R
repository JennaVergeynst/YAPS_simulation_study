rm(list=ls())
graphics.off()
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file directory as working directory
PATH = "../../data"
library(yaps)
#set.seed(42)

source("wrapper_functions.R")

## short tracks (no chunks)
nb_repetitions <- 30
track_lengths <- c(500, 1000, 5000, 10000)

for (n in as.list(track_lengths)){
  for (r in sequence(nb_repetitions)){
    # x0 <- stats::runif(1,hx.min,hx.max)
    # y0 <- stats::runif(1,hy.min,hy.max)
    trueTrack <- simTrueTrack(model='crw', n = n, deltaTime=1, shape=1, scale=0.5, addDielPattern=TRUE, start_pos = c(0,0))
    hydros <- simHydros_adapted(trueTrack=trueTrack)
    write.csv(trueTrack,paste(PATH,'/results/trueTracks/trueTrack',toString(n), '_',toString(r),'.csv',sep = ''))
    write.csv(hydros, paste(PATH,'/results/hydros/hydros',toString(n), '_',toString(r),'.csv',sep = ''))
  }
}


# r <- 2
# trueTrack <- read.csv(paste(PATH,'/results/trueTracks/trueTrack_',toString(r),'.csv',sep = ''), row.names = 1)
# hydros <- simHydros_adapted(trueTrack=trueTrack)
# plot(trueTrack$x, trueTrack$y)
# points(hydros$hx, hydros$hy, col='red')

