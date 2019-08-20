#rm(list=ls())
#graphics.off()
#args = commandArgs(trailingOnly=TRUE)
ID <- Sys.getenv("PBS_ARRAYID") # for job array running on hpc
## For R-studio:
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file directory as working directory
setwd('/Users/jennavergeynst/OneDrive - UGent/Sharing_code/YAPS_simulation_study')
## Outside R-studio:
#setwd(getSrcDirectory()[1])
## On hpc:
#setwd('/data/gent/vo/000/gvo00048/vsc41096/YAPS_simulation_study')

PATH <- getwd()
toa_path <- paste0(PATH, '/results/toa_dfs/')
tele_path <- paste0(PATH, '/results/teleTracks/')

library(yaps)
library("parallel")
library(dplyr)


source("wrapper_functions.R")

all_toas <- list.files(toa_path)
hydros <- read.csv(paste(PATH,'/results/hydros.csv',sep = ''), row.names = 1)
chunk_list <- c(250, 500, 1000, 5000, 10000)
#chunk_list <- c(250)
summary <- data.frame(matrix(ncol = 10, nrow = 1)) # nrow = length(all_toas)*length(chunk_list)
colnames(summary) <- c("rep", "chunk_size", "chunk_nb", "pingType", "mean_bi", "dist", "mean_real", "mean_est", "nb_pos", "run_time")

filenames <- read.csv('first_ten_reps.csv')

######testing######
#filename <- as.list(all_toas)[1]
#filename <- "toa_df_rbi5_distNA_rep1.csv"
#filename <- args[1]
filename <- filenames$filename[[as.integer(ID)]]
###################


toa_rev_df <- read.csv(paste0(toa_path,filename), skip = 7)
metadata <- read.csv(paste0(toa_path,filename), nrows = 7, sep='\t', header = FALSE, row.names = 1, stringsAsFactors = FALSE)
pingType = metadata["pingType",]
rbi_min = as.double(metadata["rbi_min",])
rbi_max = as.double(metadata["rbi_max",])

csvtag = substring(filename, 8)
teleTrack = read.csv(paste0(tele_path,"teleTrack_",csvtag))

toa_rev_superdf <- data.frame()
teleTrack_superdf <- data.frame()

for (chunk_size in as.list(chunk_list)){
  toa_rev_df <- chunk_toa(toa_rev_df,chunk_size)
  # add tag with chunksize to each chunk nb
  toa_rev_df$chunks <- paste(paste0(chunk_size,"_"), toa_rev_df$chunks, sep="")
  teleTrack$chunks <- toa_rev_df$chunks
  toa_rev_superdf <- bind_rows(toa_rev_superdf, toa_rev_df)
  teleTrack_superdf <- bind_rows(teleTrack_superdf, teleTrack)
}
  
toa_list = split(toa_rev_superdf, toa_rev_superdf$chunks)

# Fill in part of the summary
summary[1,"rep"] <- metadata["rep",]
summary[1,"pingType"] <- pingType
summary[1,"mean_bi"] <- (rbi_min+rbi_max)/2
summary[1,"dist"] <- as.integer(metadata["dist",]) # in terms of distance to the array contour

#part_list <- toa_list[1:4]
#test_result_list <- estimation(part_list[[1]], teleTrack_superdf, pingType, hydros, rbi_min, rbi_max)

cl = makeCluster(4)
clusterExport(cl, list("estimation", "getInp", "runTmb"))
clusterApplyLB(cl, toa_list, estimation, teleTrack_superdf, pingType, hydros, rbi_min=rbi_min, rbi_max=rbi_max, summary, csvtag, PATH)




