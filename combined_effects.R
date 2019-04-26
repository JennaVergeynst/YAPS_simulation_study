rm(list=ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file directory as working directory
PATH = getwd()
library(yaps)
#set.seed(42)

source("wrapper_functions.R")

nb_repetitions <- 200 # nb of different simulated tracks for each setting

mean_bi = c(1.2, 5, 15, 25, 67.5, 90)
min_bi = c(1.1, 1, 9, 17, 45, 60)
max_bi = c(1.3, 9, 21, 33, 90, 120)

# # uncomment if varying shifts
# shifts = c(0, 1/3, 1/2, 2/3, 1) # shift of hydros versus x- and y-extend of the track 
# len <- 250 # track length of simulated track
# to_vary <- shifts

# uncomment if varying track lengths
shift = 0 # shift of hydros versus x- and y-extend of the track
len <- c(250,500,1000,5000,10000) # track length of simulated track
to_vary <- len


summary <- data.frame(matrix(ncol = 6, nrow = length(mean_bi)*length(to_vary)*nb_repetitions))
colnames(summary) <- c("rep", "n", "pingType", "mean_bi", "shift", "mean")
index <- 1

for (i in sequence(length(mean_bi))){
  for (j in as.list(to_vary)){
    for (r in sequence(nb_repetitions)){
      try({
      n = j # j if varying length, otherwise = len
      shift = shift # j if varying shift, otherwise = shifts
      
      trueTrack <- simTrueTrack(model='crw', n = n, deltaTime=1, shape=1, scale=0.5, addDielPattern=TRUE)
      #trueTrack <- simTrueTrack(model='rw', n = n, deltaTime=1, D=10, addDielPattern=TRUE)
        
      # for random burst interval
      pingType <- 'rbi'
      rbi_min <- min_bi[i]
      rbi_max <- max_bi[i]
      sbi_mean=NA
      sbi_sd=NA
      
      # for stable burst interval
      # pingType <- 'sbi'
      # sbi_mean <- mean_bi[i]
      # sbi_sd <- 1e-4
      # rbi_min <- NA
      # rbi_max <- NA
      
      sim_list <- simulation(trueTrack, pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd, rbi_min=rbi_min, rbi_max=rbi_max, pNA=0.3, pMP=0.03, shift=shift)
      toa_rev_df <- sim_list[[1]]
      teleTrack <- sim_list[[2]]
      estimated_pos <- sim_list[[3]]
      real_error <- sim_list[[4]]
      estimated_error <- sim_list[[5]]
      hydros <- sim_list[[6]]
  
      ## plot the resulting estimated track
      # plot(y~x, data=trueTrack, type="l", xlim=(c(min(c(hydros$hx, trueTrack$x)), max(c(hydros$hx, trueTrack$x)))), ylim=(c(min(c(hydros$hy, trueTrack$y)), max(c(hydros$hy, trueTrack$y)))), asp=1)
      # points(hy~hx, data=hydros, col="green", pch=20, cex=3)
      # lines(estimated_pos$Y~estimated_pos$X, col="red")
      # lines(teleTrack$y~teleTrack$x, col="green")
      
      ## nametag for datafiles to write out
      nametag = paste0('n', toString(n), '_', pingType, toString(mean_bi[i]), '_shift', toString(round(shift,1)), '_rep', toString(r))
      
      ## write out if info needed for Vemco
      write.csv(real_error,paste(PATH,'/results/real_error_',nametag,'.csv',sep = ''))
      # write.csv(estimated_error,paste(PATH,'/results/estimated_error_',nametag,'.csv',sep = ''))
      # write.csv(toa_rev_df,paste(PATH,'/results/toa_df_',nametag,'.csv',sep = ''))
      # write.csv(hydros,paste(PATH,'/results/hydros_',nametag,'.csv',sep = ''))
      # write.csv(trueTrack,paste(PATH,'/results/trueTrack_',nametag,'.csv',sep = ''))
      # write.csv(estimated_pos,paste(PATH,'/results/yapsTrack_',nametag,'.csv',sep = ''))
      
      # Write out mean, 5%, 95% percentile, and extra information (n, pingtype, mean_bi, shift, rep) in dataframe
      summary[index,"rep"] <- r
      summary[index,"n"] <- n
      summary[index,"pingType"] <- pingType
      summary[index,"mean_bi"] <- mean_bi[i]
      summary[index,"shift"] <- round(j,1)
      summary[index,"mean"] <- mean(real_error)

      index <- index+1
      })
    }
  }
}

write.csv(summary, "summary.csv")
