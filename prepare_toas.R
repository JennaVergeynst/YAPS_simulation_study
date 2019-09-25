## Prepare TOA-dfs and teleTracks, starting from trueTracks and hydros
## Local:
rm(list=ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file directory as working directory

## On hpc:
# setwd('/data/gent/vo/000/gvo00048/vsc41096/YAPS_simulation_study')

PATH = getwd()
library(yaps)

source("wrapper_functions.R")

result_path = paste0(PATH, '/results_short_tracks/')
# nb_repetitions <- 30#200 # nb of different simulated tracks for each setting
repetitions <- 1:30 
track_lengths <- c(500, 1000, 5000)
pingType <- 'sbi'

mean_bi = c(1.2, 5, 15, 25, 67.5, 90)
min_bi = c(1.1, 1, 9, 17, 45, 60)
max_bi = c(1.3, 9, 21, 33, 90, 120)

shifts = c(0, 0.5, 1, 2) # shift = nb of array-lengths the track is moved to the right

for (n in as.list(track_lengths)){
#  for (r in sequence(nb_repetitions)){
  for (r in as.list(repetitions)){
    # Read in simulated true track
    trueTrack <- read.csv(paste(PATH,'/results_short_tracks/trueTracks/trueTrack',toString(n), '_',toString(r),'.csv',sep = ''), row.names = 1)
    hydros <- read.csv(paste(PATH,'/results_short_tracks/hydros/hydros',toString(n), '_',toString(r),'.csv',sep = ''), row.names = 1)
    
    for (shift in as.list(shifts)){
      trueTrack <- shift_trueTrack(trueTrack, hydros, shift)
      for (i in sequence(length(mean_bi))){
        try({
          ## for random burst interval
          if (pingType == 'rbi'){
            rbi_min <- min_bi[i]
            rbi_max <- max_bi[i]
            sbi_mean=NA
            sbi_sd=NA
          }

          
          ## for stable burst interval
          if (pingType=='sbi'){
            sbi_mean <- mean_bi[i]
            sbi_sd <- 1e-4
            rbi_min <- NA
            rbi_max <- NA
          }

          ## for pseudo-random burst interval
          # ...
          
          sim_list <- simulation(trueTrack, hydros, pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd, rbi_min=rbi_min, rbi_max=rbi_max, pNA=0.3, pMP=0.03)
          toa_rev_df <- sim_list[[1]]
          teleTrack <- sim_list[[2]]
          
          ## nametag for datafiles to write out
          nametag = paste0(toString(n), '_', pingType, toString(mean_bi[i]), '_shift', toString(shift), '_rep', toString(r))
          
          file = paste(result_path,'toa_dfs/',pingType,'/toa_df_',nametag,'.csv',sep = '')
          cat(paste0("pingType\t", pingType, "\nrbi_min\t", rbi_min, "\nrbi_max\t", rbi_max, 
                     "\nsbi_mean\t", sbi_mean, "\nsbi_sd\t", sbi_sd, 
                     "\nshift\t", shift, "\nrep\t", r, "\n"), file=file)
          write.table(toa_rev_df, file,sep=",",append=TRUE, row.names = FALSE)
          write.csv(teleTrack,paste(result_path,'teleTracks/',pingType,'/teleTrack_',nametag,'.csv',sep = ''))
          
          
        })
      }
    }
  }
}


# plot(trueTrack$x, trueTrack$y)
# plot(teleTrack$x, teleTrack$y)
# plot(hydros$hx, hydros$hy)
