rm(list=ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file directory as working directory
PATH = getwd()
library(yaps)
#set.seed(42)


source("wrapper_functions.R")

hydros <- read.csv(paste(PATH,'/results/hydros.csv',sep = ''), row.names = 1)
nb_repetitions <- 200 # nb of different simulated tracks for each setting
nb_repetitions <- 3 # nb of different simulated tracks for each setting

# mean_bi = c(1.2, 5, 15, 25, 67.5, 90)
# min_bi = c(1.1, 1, 9, 17, 45, 60)
# max_bi = c(1.3, 9, 21, 33, 90, 120)
# dist_to_array = c(NA, 0, 100, 200) # -NA means no shift

mean_bi = c(5)
min_bi = c(1.1, 1, 9, 17, 45, 60)
max_bi = c(1.3, 9, 21, 33, 90, 120)
dist_to_array = c(NA) # -NA means no shift


for (r in sequence(nb_repetitions)){
  # Read in simulated true track
  trueTrack <- read.csv(paste(PATH,'/results/trueTracks/trueTrack_',toString(r),'.csv',sep = ''), row.names = 1)

  for (dist in as.list(dist_to_array)){
    if (!is.na(dist)){
      # shift the track so that the distance from its closest point 
      # to the array contour equals 'dist' meter
      trueTrack <- shift_trueTrack(trueTrack, dist_to_array=dist)
      # plot(hydros, xlim = c(-250, 1000))
      # lines(trueTrack$x, trueTrack$y, col='red')
      # lines(shifted_trueTrack$x, shifted_trueTrack$y, col='blue')
    }

    for (i in sequence(length(mean_bi))){
      try({
        # for random burst interval
        # pingType <- 'rbi'
        # rbi_min <- min_bi[i]
        # rbi_max <- max_bi[i]
        # sbi_mean=NA
        # sbi_sd=NA
        
        # for stable burst interval
        pingType <- 'sbi'
        sbi_mean <- mean_bi[i]
        sbi_sd <- 1e-4
        rbi_min <- NA
        rbi_max <- NA
        
        # for pseudo-random burst interval
        # ...
        
        
        sim_list <- simulation(trueTrack, hydros, pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd, rbi_min=rbi_min, rbi_max=rbi_max, pNA=0.3, pMP=0.03)
        toa_rev_df <- sim_list[[1]]
        teleTrack <- sim_list[[2]]
        
        ## nametag for datafiles to write out
        nametag = paste0(pingType, toString(mean_bi[i]), '_dist', toString(dist), '_rep', toString(r))
        
        file = paste(PATH,'/results/toa_dfs/toa_df_',nametag,'.csv',sep = '')
        cat(paste0("pingType\t", pingType, "\nrbi_min\t", rbi_min, "\nrbi_max\t", rbi_max, 
                   "\nsbi_mean\t", sbi_mean, "\nsbi_sd\t", sbi_sd, 
                   "\ndist\t", dist, "\nrep\t", r, "\n"), file=file)
        write.table(toa_rev_df, file,sep=",",append=TRUE, row.names = FALSE)
        write.csv(teleTrack,paste(PATH,'/results/teleTracks/teleTrack_',nametag,'.csv',sep = ''))


      })
    }
  }
}

