rm(list=ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file directory as working directory
PATH <- getwd()
toa_path <- paste0(PATH, '/results/toa_dfs/')
library(yaps)
#set.seed(42)

source("wrapper_functions.R")

all_toas <- list.files(toa_path)
hydros <- read.csv(paste(PATH,'/results/hydros.csv',sep = ''), row.names = 1)

summary <- data.frame(matrix(ncol = 6, nrow = length(all_toas)))
colnames(summary) <- c("rep", "n", "pingType", "mean_bi", "dist", "mean")
index <- 1

for (filename in as.list(all_toas)){
  toa_rev_df <- read.csv(paste0(toa_path,filename), skip = 7)
  metadata <- read.csv(paste0(toa_path,filename), nrows = 7, sep='\t', header = FALSE, row.names = 1, stringsAsFactors = FALSE)
  pingType = metadata["pingType",]
  rbi_min = as.double(metadata["rbi_min",])
  rbi_max = as.double(metadata["rbi_max",])
  csvtag = substring(filename, 8)
  
  # take only first 10.000 observations of toa_rev_df
  n <- 10000
  toa_rev_df <- toa_rev_df[1:n,]

  try({
    est_list <- estimation(pingType, hydros, toa_rev_df, rbi_min=rbi_min, rbi_max=rbi_max)
    estimated_pos <- est_list[[1]]
    real_error <- est_list[[2]]
    estimated_error <- est_list[[3]]
    ## plot the resulting estimated track
    plot(y~x, data=trueTrack, type="l", xlim=(c(min(c(hydros$hx, trueTrack$x)), max(c(hydros$hx, trueTrack$x)))), ylim=(c(min(c(hydros$hy, trueTrack$y)), max(c(hydros$hy, trueTrack$y)))), asp=1)
    points(hy~hx, data=hydros, col="green", pch=20, cex=3)
    lines(estimated_pos$Y~estimated_pos$X, col="red")
  #      lines(teleTrack$y~teleTrack$x, col="green")
    
    write.csv(real_error,paste(PATH,'/results/real_error_',csvtag,sep = ''))
    # write.csv(estimated_error,paste(PATH,'/results/estimated_error_',nametag,'.csv',sep = ''))
    # write.csv(estimated_pos,paste(PATH,'/results/yapsTrack_',nametag,'.csv',sep = ''))
    
    # Write out mean, 5%, 95% percentile, and extra information (n, pingtype, mean_bi, shift, rep) in dataframe
    summary[index,"rep"] <- metadata["rep",]
    summary[index,"n"] <- n
    summary[index,"pingType"] <- pingType
    summary[index,"mean_bi"] <- (rbi_min+rbi_max)/2
    summary[index,"dist"] <- as.integer(metadata["dist",])
    summary[index,"mean"] <- mean(real_error)
  
    index <- index+1
  })

}

write.csv(summary, "summary.csv")
