getSimSetup <- function(sim_part){
	# # # sim setup
	# crw movement
	crw_shape <- 1
	crw_scale <- 0.5
	pNA <- 0.6
	sbi_sd <- 1e-4

	if(sim_part==0){ #sim_part 0 - test - short version of everything
		# track specifics
		n_tracks <- 2
		pMPs <- 0.025
		track_lengths <- c(500, 1000)
		shifts = c(0) # shift = nb of array-lengths the track is moved to the right
		# transmitter and hydro specifics
		mean_bi = c(1.2, 15, 45)
		min_bi  = c(1.1, 10, 30)
		max_bi  = c(1.3, 20, 60)
		ping_types <- c('sbi','rbi','pbi')
		hydro_temp_res <- c(1000, 4800, 19200, 1E6)
	}
	if(sim_part==1){ #sim_part 1 - effect of pMP
		# track specifics
		n_tracks <- 50
		pMPs <- c(0, 0.025, 0.050, 0.075, 0.10)
		track_lengths <- c(5000)
		shifts = c(0)
		# transmitter and hydro specifics
		mean_bi = c(1.2, 5, 15, 30, 45, 60, 90)
		min_bi  = c(1.1, 3, 10, 20, 30, 40, 60)
		max_bi  = c(1.3, 7, 20, 40, 60, 80, 120)
		ping_types <- c('sbi','rbi','pbi')
		hydro_temp_res <- c(4800)
	}
	if(sim_part==2){ #sim_part 2 - effect of shifts
		# track specifics
		n_tracks <- 50
		pMPs <- c(0.025)
		track_lengths <- c(5000)
		shifts = c(0, 0.5, 1) # shift = nb of array-lengths the track is moved to the right
		# transmitter and hydro specifics
		mean_bi = c(1.2, 5, 15, 30, 45, 60, 90)
		min_bi  = c(1.1, 3, 10, 20, 30, 40, 60)
		max_bi  = c(1.3, 7, 20, 40, 60, 80, 120)
		ping_types <- c('sbi','rbi','pbi')
		hydro_temp_res <- c(4800)
	}
	if(sim_part==3){ #sim_part 3 - effect of track length
		# track specifics
		n_tracks <- 50
		pMPs <- 0.025
		track_lengths <- c(500, 1000, 2500, 5000, 10000)
		shifts = c(0)
		# transmitter and hydro specifics
		mean_bi = c(1.2, 5, 15, 30, 45, 60, 90)
		min_bi  = c(1.1, 3, 10, 20, 30, 40, 60)
		max_bi  = c(1.3, 7, 20, 40, 60, 80, 120)
		ping_types <- c('sbi', 'rbi','pbi')
		hydro_temp_res <- c(4800)
	}
	if(sim_part==4){ #sim_part 4 - effect of temporal resolution of hydros
		# track specifics
		n_tracks <- 50
		pMPs <- 0.025
		track_lengths <- c(5000)
		shifts = c(0)
		# transmitter and hydro specifics
		mean_bi = c(1.2, 5, 15, 30, 45, 60, 90)
		min_bi  = c(1.1, 3, 10, 20, 30, 40, 60)
		max_bi  = c(1.3, 7, 20, 40, 60, 80, 120)
		ping_types <- c('sbi', 'rbi','pbi')
		hydro_temp_res <- c(1000, 4800, 19200, 1E6)
	}
	sim_setup <- tibble::lst(
		crw_shape, crw_scale, pNA, pMPs, sbi_sd, n_tracks, track_lengths, shifts, mean_bi, min_bi, max_bi, ping_types, hydro_temp_res
	)
	
	return(sim_setup)
}


# wrapper function to take care of everything simul
getSimul <- function(sim_setup){
	crw_shape      <- sim_setup$crw_shape
	crw_scale      <- sim_setup$crw_scale
	pNA            <- sim_setup$pNA
	pMPs            <- sim_setup$pMP
	sbi_sd         <- sim_setup$sbi_sd
	n_tracks       <- sim_setup$n_tracks
	track_lengths  <- sim_setup$track_lengths
	shifts         <- sim_setup$shifts
	mean_bi        <- sim_setup$mean_bi
	min_bi         <- sim_setup$min_bi
	max_bi         <- sim_setup$max_bi
	ping_types     <- sim_setup$ping_types
	hydro_temp_res <- sim_setup$hydro_temp_res

	# ping type and BI combinations
	bis <- c()
	bis0 <- c()
	for(p in 1:length(ping_types)){
		if(ping_types[p] == 'sbi'){
			bis0 <- rbind(bis0, data.table(mean_bi, min_bi=NA, max_bi=NA, ping='sbi'))
		} else if (ping_types[p] == 'rbi') {
			bis0 <- rbind(bis0, data.table(mean_bi=NA, min_bi=min_bi, max_bi=max_bi, ping='rbi'))
		} else if (ping_types[p] == 'pbi') {
			bis0 <- rbind(bis0, data.table(mean_bi=NA, min_bi=min_bi, max_bi=max_bi, ping='pbi'))
		}
	}
	
	# adding temporal resolution
	for(htr in 1:length(hydro_temp_res)){
		bis0$htr <- hydro_temp_res[htr]
		bis <- rbind(bis, bis0)
	}

	# data.table of true track lengths and id
	# length(tt_dt) = n_tracks * length(track_lengths)
	tt_dt 			<- data.table::data.table(expand.grid(n_track=1:n_tracks, track_length=track_lengths, pMP=pMPs))
	tt_dt[, track_id:=1:.N]

	# data.table of shifted true track lengths, shifts and id
	# length(tt_shifts_dt) = n_tracks * length(track_lengths) * length(shifts)
	tt_shifts_dt 	<- data.table(bind_rows(replicate(length(shifts), tt_dt, simplify = FALSE)), shift=rep(shifts, each=nrow(tt_dt)))
	tt_shifts_dt[, track_shift_id:=1:.N]

	# lists of data.tables with true tracks and hydros for each track
	# length(tt_list) = n_tracks * length(track_lengths)
	# length(hydros_list)  = n_tracks * length(track_lengths)
	tt_list <- apply(tt_dt, 1, FUN=function(k) yaps::simTrueTrack(model="crw", n=k['track_length'], deltaTime=1, shape=crw_shape, scale=crw_scale, addDielPattern=TRUE))
	hydros_list <- llply(tt_list, .fun=function(k) simHydros_adapted(k))
	length(tt_list) == length(hydros_list)

	# list of data.tables with shifted true tracks
	# length(tt_shifted_list) = n_tracks * length(track_lengths) * length(shifts)
	tt_shifted_list <- apply(tt_shifts_dt, 1, FUN=function(k) shift_trueTrack(trueTrack=tt_list[[k['track_id']]], hydros_list[[k['track_id']]], k['shift']))

	# sanity check...
	length(tt_list) == n_tracks * length(track_lengths) & length(tt_shifted_list) == n_tracks * length(track_lengths) * length(shifts) & length(tt_shifted_list) == length(tt_list) * length(shifts)

	# data.table with all combinations of ping_type, BIs, true_tracks and shifts
	# nrow(sim_dt) = n_tracks * length(track_lengths) * length(shifts) * nrow(bis)
	sim_dt <- data.table(bind_rows(
		replicate(nrow(bis), tt_shifts_dt, simplify = FALSE)), 
		ping_type 		=	rep(bis$ping, each=nrow(tt_shifts_dt)),
		mean_bi 		=	rep(bis$mean_bi, each=nrow(tt_shifts_dt)),
		min_bi	 		=	rep(bis$min_bi, each=nrow(tt_shifts_dt)),
		max_bi 			=	rep(bis$max_bi, each=nrow(tt_shifts_dt)),
		htr 			=	rep(bis$htr, each=nrow(tt_shifts_dt))
	)
	sim_dt[, sim_id:= 1:.N]
	
	print(paste0("Total number of tracks to simulate: ", nrow(sim_dt)))

	# get list of TOAs and ss
	# TAKES A LOOOOONG TIME FOR LARGE SETS - RUN THE lines in doGetToas manually instead...
	toa_list <- doGetToas(sim_dt, tt_shifted_list, hydros_list, sbi_sd, pNA)

	# extract nrow(toa) from toa_list and add to sim_dt
	sim_dt[, nrow_toa := laply(toa_list, .fun=function(k) k[[3]])]
	sim_dt[]

	# collect all params in a list for future use...
	sim_params <- list(
		crw_shape = crw_shape,
		crw_scale = crw_scale,
		pNA = pNA,
		pMPs = pMPs,
		sbi_sd = sbi_sd,
		n_tracks = n_tracks, 
		track_lengths = track_lengths,
		mean_bi = mean_bi,
		min_bi = min_bi,
		max_bi = max_bi,
		ping_types = ping_types,
		shifts = shifts,
		bis = bis
	)
	simul <- list(sim_params=sim_params, sim_dt=sim_dt, toa_list=toa_list, hydros_list=hydros_list, tt_list=tt_list, tt_shifted_list=tt_shifted_list)
	return(simul)
}

# wrapper to get toa_list from parallel
doGetToas <- function(sim_dt, tt_shifted_list, hydros_list, sbi_sd, pNA){
	tictoc::tic()
	cl = parallel::makeCluster(getOption("cl.cores", parallel::detectCores()))
	parallel::clusterExport(cl, list("getToas", "tt_shifted_list", "sim_dt", "hydros_list", "sbi_sd", "pNA", "simulation", "simTelemetryTrack", "simToa"), envir = environment())
	toa_list <- parallel::clusterApply(cl, x=1:nrow(sim_dt), fun=function(k) {getToas(k, sim_dt, tt_shifted_list, hydros_list, sbi_sd, pNA)})
	parallel::stopCluster(cl)
	tictoc::toc()
	return(toa_list)
}

# simulate TOA for all rows in sim_dt
getToas <- function(k, sim_dt, tt_shifted_list, hydros_list, sbi_sd, pNA) {
	track_id		<- as.numeric(sim_dt[k, 'track_id'])
	track_shift_id 	<- as.numeric(sim_dt[k, 'track_shift_id'])
	sim_id 			<- as.numeric(sim_dt[k, 'sim_id'])
	ping_type 		<- sim_dt[k, 'ping_type']
	pMP		 		<- sim_dt[k, 'pMP']
	mean_bi			<- as.numeric(sim_dt[k, 'mean_bi'])
	min_bi			<- as.numeric(sim_dt[k, 'min_bi'])
	max_bi			<- as.numeric(sim_dt[k, 'max_bi'])
	htr				<- as.numeric(sim_dt[k, 'htr'])
	hydros			<- hydros_list[[track_id]]
	
	teleTrack <- yaps::simTelemetryTrack(trueTrack=tt_shifted_list[[track_shift_id]], pingType=ping_type, sbi_mean=mean_bi, sbi_sd=sbi_sd, rbi_min=min_bi, rbi_max=max_bi)
	if(ping_type == 'pbi'){
		bi_table <- teleTrack$biTable
		teleTrack <- teleTrack$out
	} else {
		bi_table <- c(1)
	}
	toa_list_sim  <- yaps::simToa(teleTrack, hydros, pingType=ping_type, sigmaToa=1/htr, pNA=pNA, pMP=pMP, tempRes=htr)
	toa <- t(toa_list_sim$toa)
	ss  <- teleTrack$ss
	
	nrow_toa 	<- nrow(toa)
	dimnames(toa) <- NULL

	return(list(toa=toa, ss=ss, nrow_toa=nrow_toa, bi_table=bi_table))
}

# wrapper function to get it running in parallel
doRunYapsParallel <- function(simul, max_retries, sim_part, data_path){
	cl = parallel::makeCluster(getOption("cl.cores", parallel::detectCores()))
	parallel::clusterExport(cl, list("simul", "save_yaps_out", "data_path", "yaps_parallel", "getInp", "max_retries", "sim_part"), envir = environment())
	est_list <- parallel::clusterApplyLB(cl, x=sample(1:nrow(simul$sim_dt)), fun=function(k) {yaps_parallel(k, simul, max_retries=max_retries, data_path=data_path, sim_part)})
	parallel::stopCluster(cl)
	return(est_list)
}

# Runs yaps
yaps_parallel <- function(k, simul, max_retries, data_path, sim_part){
	sim_dt <- simul$sim_dt
	toa_list <- simul$toa_list
	hydros_list <- simul$hydros_list
	
	sim_id 		<- as.numeric(sim_dt[k , 'sim_id'])
	track_id 	<- as.numeric(sim_dt[k , 'track_id'])
	toa 		<- toa_list[[sim_id]]$toa
	ss_dat 		<- toa_list[[sim_id]]$ss
	bi_table 	<- toa_list[[sim_id]]$bi_table
	hydros_yaps <- hydros_list[[track_id]]
	ping_type 	<- as.character(sim_dt[k, 'ping_type'])
	rbi_min 	<- as.numeric(sim_dt[k, 'min_bi'])
	rbi_max 	<- as.numeric(sim_dt[k, 'max_bi'])

	# TOA needs to be trimmed for completely empty rows
	nobs <- apply(toa, 1, function(k) sum(!is.na(k)))
	first_ping <- which(nobs >= 1)[1]
	last_ping <- rev(which(nobs >= 1))[1]
	toa_yaps <- t(toa[first_ping:last_ping, ])
	ss_dat_yaps <- ss_dat[first_ping:last_ping]
	bi_table_yaps <- bi_table[first_ping:last_ping]
	T0 <- min(toa_yaps, na.rm=TRUE)
	toa_yaps <- toa_yaps - T0

	try({
		pl <- c()
		outTmb <- c()
		yaps_done <- 0
		i <- 1

		while(i <= max_retries & yaps_done != 1){
			inp <- yaps::getInp(hydros=hydros_yaps, toa=toa_yaps, E_dist="t", n_ss=1, pingType=ping_type, ss_data_what="data", ss_data=ss_dat_yaps, rbi_min=rbi_min-0.5, rbi_max=rbi_max+0.5, biTable=bi_table_yaps)
			inp$datTmb$Edist <- c(0,0,1) # set error distribution to t
			inp$T0 <- T0

			maxIter <- 	min(floor(300 + i^1.7), 1000)
			tryCatch({
				outTmb <- yaps::runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
			}, error=function(e){})
			# identify cases where runTmb fails and try again...
			if(!is.null(outTmb)) {
				if(sum(unlist(lapply(outTmb$plsd, FUN=is.na))) > 3 ){
					yaps_done <- 0
				} else if(sum(unlist(lapply(outTmb$plsd, FUN=is.na))) > 1  &  i <= 10){
					yaps_done <- 0
				} else if(outTmb[1] == "noOpt"){
					yaps_done <- 0
				} else if(outTmb$pl$logScale > 0){
					yaps_done <- 0
				} else if(median(outTmb$plsd$top, na.rm=TRUE) > 1e-1){
					yaps_done <- 0
				} else {yaps_done <- 1}
			} else {
				yaps_done <- 0
			}
			i <- i+1
		}
	})

	sim_dt_k <- sim_dt[k,]
	gb <- save_yaps_out(k, inp, outTmb, yaps_done, sim_dt_k, data_path, sim_part)
	return(gb)
}

save_yaps_out <- function(k, inp, outTmb, yaps_done, sim_dt_k, data_path, sim_part){
	yaps_status <- ifelse(yaps_done == 0, 'YAPS_FAILED', 'yaps_done')
	ping_type <- inp$datTmb$pingType
	fn <- paste0('simId_', sim_dt_k[,'sim_id'],'_nrow_', sim_dt_k[,'nrow_toa'], '_', ping_type, '_', yaps_status)
	save(inp, outTmb, file=paste0(data_path,'/sim_out/sim_part_',sim_part,'/',fn,'.rObj'))
}

collate_output <- function(sim_part, data_path){
	load(file=paste0(data_path, "/sim_part", sim_part,"_sim_dat.rObj"))

	files <- list.files(paste0(data_path, "/sim_out/sim_part_", sim_part))
	
	failed_idx <- which(grepl('FAIL', files) == TRUE)
	if(length(failed_idx) > 0){
		print("ERROR: YAPS failed on some tracks!!!")
		print(files[failed_idx])
		files <- files[-failed_idx]
	}
	
	sim_ids <- as.numeric(tstrsplit(files, '_')[[2]])
	sim_idx <- 1:length(sim_ids)

	sim_dt <- simul$sim_dt
	tt_shifted_list <- simul$tt_shifted_list
	toa_list <- simul$toa_list
	hydros_list <- simul$hydros_list
	
	out <- list()
	for(i in 1:length(files)){
		if(i %% 500 == 0){
			print(paste0(i, "/", length(files)))
		}
		sim_id <- sim_ids[i]
		load(file=paste0(data_path, "/sim_out/sim_part_",sim_part,'/', files[sim_idx[i]]))
		
		track_shift_id <- as.numeric(sim_dt[sim_id, 'track_shift_id'])
		
		tt_shift_i <- tt_shifted_list[[track_shift_id]]
		
		tru <- 	data.table(true_time=tt_shift_i$time, true_x=tt_shift_i$x, true_y=tt_shift_i$y)
		est <- data.table(est_x=outTmb$pl$X, est_y=outTmb$pl$Y, est_top=outTmb$pl$top + inp$T0, T0=inp$T0)

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
		
		out[[i]] <- out_i
	}
	out <- do.call(rbind.data.frame, out)

	out[, dev:=sqrt((true_x - est_x)^2 + (true_y - est_y)^2)]
	out2 <- merge(out, sim_dt, by='sim_id', sort=FALSE)
	out2[is.na(mean_bi), mean_bi:=(min_bi+max_bi)/2]
	summ <- out2[, list('mean_dev' = mean(dev), 'median_dev' = median(dev), 'min_dev' = min(dev), 'max_dev' = max(dev)), by=c('ping_type','shift', 'mean_bi','sim_id','track_id','track_length','htr','nrow_toa', 'pMP')]
	return(summ)
}

