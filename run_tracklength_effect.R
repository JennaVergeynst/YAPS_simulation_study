# File run on the UGent high performance cluster, 
# on 18 nodes in parallel (3 lengths x 6 bi's)
# run 10 repetitions with use of job array running

r <- Sys.getenv("PBS_ARRAYID") # for job array running on hpc
## On hpc:
setwd('/data/gent/vo/000/gvo00048/vsc41096/YAPS_simulation_study')

## Local:
# rm(list=ls())
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file directory as working directory

PATH <- getwd()
result_path = paste0(PATH, '/results_short_tracks/')

library(yaps)
library("parallel")
library(dplyr)

source("wrapper_functions.R")

pingType <- 'sbi'
track_lengths <- c(500, 1000, 5000)
mean_bi = c(1.2, 5, 15, 25, 67.5, 90)
# make dataframe combining parameters track_legth and mean_bi

combos_list = list()
for (n in as.list(track_lengths)){
  for (mb in as.list(mean_bi)){
    pars_df = data.frame(matrix(ncol = 2, nrow = 1))
    colnames(pars_df) = c('track_length','mean_bi')
    pars_df$track_length = n
    pars_df$mean_bi = mb
    combos_list = append(combos_list, list(pars_df))
  }
}

cl = makeCluster(4)
clusterExport(cl, list("readfiles", "readin_and_estim", "getInp", "runTmb"))
clusterApplyLB(cl, combos_list, readin_and_estim, result_path, dist='NA', r, pingType)

