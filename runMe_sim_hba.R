rm(list=ls())
graphics.off()
setwd('/home/hbak/H/ms/others/2019-04 JennaVergeynst - simulation study/git/YAPS_simulation_study')
PATH = "../../data"
# devtools::install_github("baktoft/yaps", force=TRUE)

library(yaps)
library(dplyr)
library(plyr)
library(data.table)
#set.seed(42)
source("wrapper_functions.R")
source('funcs_hba.R')

# # # simul params
# crw movement
crw_shape <- 1
crw_scale <- 0.5

# detection params
pNA <- 0.5
pMP <- 0.03
sbi_sd <- 1e-4

# track specifics
# n_tracks <- 30
# track_lengths <- c(500, 1000, 5000)
n_tracks <- 2
track_lengths <- c(1000, 2500, 5000, 10000)
# track_lengths <- c(1000, 2500)

# transmitter specifics
mean_bi = c(1.2, 5, 15, 30, 45, 60, 90)
# mean_bi = c(1.2, 5)
min_bi  = c(1.1, 1, 10, 20, 30, 40, 60)
max_bi  = c(1.3, 9, 20, 40, 60, 80, 120)
ping_types <- c('sbi', 'rbi')
# ping_types <- c('sbi')

shifts = c(0, 0.5, 1, 2) # shift = nb of array-lengths the track is moved to the right
# shifts = c(0, 0.5) # shift = nb of array-lengths the track is moved to the right
# shifts = c(0, 2) # shift = nb of array-lengths the track is moved to the right


# get everything simulated
# return list of simul stuff - e.g. sim_params, sim_dt, toa_list
simul <- getSimul(mean_bi, min_bi, max_bi, n_tracks, track_lengths, shifts, sbi_sd, pNA, pMP, crw_shape, crw_scale)
names(simul)

# # save everything needed to run YAPS to avoid redoing everything everytime...
save(list=c('simul'), file=paste0(PATH, "/sim_dat.rObj"))


# # # When all tracks are simulated and saved, we can start YAPS'ing from here...
rm(list=ls())
path <- "../../data"
setwd('/home/hbak/H/ms/others/2019-04 JennaVergeynst - simulation study/git/YAPS_simulation_study')
load(file=paste0(path, "/sim_dat.rObj"))
source('funcs_hba.R')
max_retries <- 100

library(data.table)
library(yaps)
ls()

# gb <- doRunYapsParallel(sim_dt=simul$sim_dt, toa_list=simul$toa_list, hydros=simul$hydros, max_retries=max_retries)

sim_dt <- simul$sim_dt
tt_shifted_list <- simul$tt_shifted_list

files <- list.files(paste0(path, "/yaps_out"))
sim_ids <- as.numeric(tstrsplit(files, '_')[[2]])
sim_idx <- 1:length(sim_ids)

out <- c()
for(i in 1:length(files)){
	print(i)
	sim_id <- sim_ids[i]
	load(file=paste0(path, "/yaps_out/", files[sim_idx[i]]))
	
	length(tt_shifted_list)
	track_shift_id <- as.numeric(sim_dt[sim_id, 'track_shift_id'])
	
	tt_shift_i <- tt_shifted_list[[track_shift_id]]
	
	tru <- 	data.table(true_time=tt_shift_i$time, true_x=tt_shift_i$x, true_y=tt_shift_i$y)
	est <- data.table(est_x=outTmb$pl$X, est_y=outTmb$pl$Y, est_top=outTmb$pl$top)

	nobs <- apply(inp$datTmb$toa, 2, function(k) sum(!is.na(k)))

	tru[, roll:=true_time]
	est[, roll:=est_top]
	setkey(tru, roll)
	setkey(est, roll)
	
	out_i <- tru[est, roll="nearest"]
	out_i[, est_top:=roll]
	out_i[, roll:=NULL]
	out_i[, ping:=1:.N]
	out_i[, sim_id:=sim_id]
	out_i[, nobs:=nobs]
	
	out <- rbind(out, out_i)
}

out[, dev:=sqrt((true_x - est_x)^2 + (true_y - est_y)^2)]

out2 <- merge(out, sim_dt, by='sim_id', sort=FALSE)
out2[is.na(mean_bi), mean_bi:=(min_bi+max_bi)/2]
summ <- out2[, list('mean_dev' = mean(dev), 'median_dev' = median(dev), 'min_dev' = min(dev), 'max_dev' = max(dev)), by=c('ping_type','shift', 'mean_bi','sim_id','track_id','track_length')]
ggplot(data=summ, aes(x=mean_bi, y=log(median_dev), col=factor(track_length), fill=factor(track_length))) + geom_point() + geom_smooth(se=FALSE) + facet_grid(ping_type~shift)

ggplot(data=summ, aes(x=mean_bi, y=log(median_dev), col=factor(track_length), fill=factor(track_length))) + geom_point() + geom_smooth(se=FALSE) + facet_grid(ping_type~shift)

	
hydros_dt <- data.table(dplyr::bind_rows(hydros, .id="track_id"))

library(ggplot2)
ggplot(data=out2) + geom_point(aes(x=est_top, y=dev, col=factor(shift))) + facet_grid(track_id~ping_type)
ggplot(data=out2) + geom_point(aes(x=est_top, y=dev, col=factor(shift))) + facet_grid(mean_bi~ping_type)
ggplot(data=out2) + geom_point(aes(x=est_top, y=dev, col=factor(track_id))) + facet_grid(shift~ping_type)

ggplot(data=out2[track_id == 31]) + geom_path(aes(x=true_x, y=true_y, group=sim_id),col="blue", size=1.2) + 
	geom_path(aes(x=est_x, y=est_y, col=factor(mean_bi), group=sim_id)) + 
	geom_point(data=hydros_dt[track_id == 31], aes(x=hx, y=hy), col="green") +
	facet_grid(ping_type~shift) + coord_fixed(ratio=1)


ls()







