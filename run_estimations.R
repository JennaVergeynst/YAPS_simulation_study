rm(list=ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file directory as working directory
PATH <- getwd()
toa_path <- paste0(PATH, '/results/toa_dfs/')
tele_path <- paste0(PATH, '/results/teleTracks/')

library(yaps)
library("parallel")
library(dplyr)


#set.seed(42)

source("wrapper_functions.R")

all_toas <- list.files(toa_path)
hydros <- read.csv(paste(PATH,'/results/hydros.csv',sep = ''), row.names = 1)
chunk_list <- c(250, 500, 1000, 5000, 10000)
summary <- data.frame(matrix(ncol = 7, nrow = length(all_toas)*length(chunk_list)))
colnames(summary) <- c("rep", "chunk_size", "pingType", "mean_bi", "dist", "mean", "nb_pos")
index <- 1

######testing######
#filename <- as.list(all_toas)[1]
#filename <- "toa_df_sbi5_distNA_rep1.csv"
###################

for (filename in as.list(all_toas)){
#for (filename in as.list(filename)){
  toa_rev_df <- read.csv(paste0(toa_path,filename), skip = 7)
  metadata <- read.csv(paste0(toa_path,filename), nrows = 7, sep='\t', header = FALSE, row.names = 1, stringsAsFactors = FALSE)
  pingType = metadata["pingType",]
  rbi_min = as.double(metadata["rbi_min",])
  rbi_max = as.double(metadata["rbi_max",])
  
  # the chunk_size loop can be removed once optimal chunksize has been found
  for (chunk_size in as.list(chunk_list)){
    toa_rev_df <- chunk_toa(toa_rev_df,chunk_size)
    toa_list = split(toa_rev_df, toa_rev_df$chunks)
  
    csvtag = substring(filename, 8)
    teleTrack = read.csv(paste0(tele_path,"teleTrack_",csvtag))
    teleTrack$chunks <- toa_rev_df$chunks
    
    cl = makeCluster(4)
    clusterExport(cl, list("estimation", "getInp", "runTmb"))
    est_list <- clusterApplyLB(cl, toa_list, estimation, teleTrack, pingType, hydros, rbi_min=rbi_min, rbi_max=rbi_max)
    estimated_pos <- bind_rows(lapply(est_list, '[[', 1))
    real_error <- unlist(lapply(est_list, '[[', 2))
    estimated_error <- unlist(lapply(est_list, '[[', 3))
    
    part <- toa_list[[3]]
    test <- estimation(part, teleTrack, pingType, hydros, rbi_min=rbi_min, rbi_max=rbi_max)

    ## plot the resulting estimated track
    # plot(y~x, data=trueTrack, type="l", xlim=(c(min(c(hydros$hx, trueTrack$x)), max(c(hydros$hx, trueTrack$x)))), ylim=(c(min(c(hydros$hy, trueTrack$y)), max(c(hydros$hy, trueTrack$y)))), asp=1)
    # points(hy~hx, data=hydros, col="green", pch=20, cex=3)
    # lines(estimated_pos$Y~estimated_pos$X, col="red")
    # lines(teleTrack$y~teleTrack$x, col="green")
    
    write.csv(real_error,paste(PATH,'/results/real_error_chunk_',toString(chunk_size),'_',csvtag,sep = ''))
    # write.csv(estimated_error,paste(PATH,'/results/estimated_error_',nametag,'.csv',sep = ''))
    # write.csv(estimated_pos,paste(PATH,'/results/yapsTrack_',nametag,'.csv',sep = ''))
    
    # Write out mean of real error and extra information (chunk size, pingtype, mean_bi, shift, rep) in dataframe
    summary[index,"rep"] <- metadata["rep",]
    summary[index,"chunk_size"] <- chunk_size
    summary[index,"pingType"] <- pingType
    summary[index,"mean_bi"] <- (rbi_min+rbi_max)/2
    summary[index,"dist"] <- as.integer(metadata["dist",]) # in terms of distance to the array contour
    summary[index,"mean"] <- mean(real_error)
    summary[index,"nb_pos"] <- length(real_error)
  
    index <- index+1
  }
}

write.csv(summary, "summary.csv")
