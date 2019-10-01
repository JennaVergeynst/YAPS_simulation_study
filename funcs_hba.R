# wrapper function to take care of everything simul
getSimul <- function(mean_bi, min_bi, max_bi, n_tracks, track_lengths, shifts, sbi_sd, pNA, pMP, crw_shape, crw_scale){
	# ping type and BI combinations
	bis <- data.table(rbind(cbind(mean_bi, min_bi=NA, max_bi=NA, ping=1), cbind(mean_bi=NA, min_bi, max_bi, ping=2)))
	bis[, ping:=ifelse(ping==1, 'sbi', 'rbi')]

	# data.table of true track lengths and id
	# length(tt_dt) = n_tracks * length(track_lengths)
	tt_dt 			<- data.table::data.table(expand.grid(n_track=1:n_tracks, track_length=track_lengths))
	tt_dt[, track_id:=1:.N]

	# data.table of shifted true track lengths, shifts and id
	# length(tt_shifts_dt) = n_tracks * length(track_lengths) * length(shifts)
	tt_shifts_dt 	<- data.table(bind_rows(replicate(length(shifts), tt_dt, simplify = FALSE)), shift=rep(shifts, each=nrow(tt_dt)))
	tt_shifts_dt[, track_shift_id:=1:.N]

	# lists of data.tables with true tracks and hydros for each track
	# length(tt_list) = n_tracks * length(track_lengths)
	# length(hydros)  = n_tracks * length(track_lengths)
	tt_list <- apply(tt_dt, 1, FUN=function(k) yaps::simTrueTrack(model="crw", n=k['track_length'], deltaTime=1, shape=crw_shape, scale=crw_scale, addDielPattern=TRUE))
	hydros <- llply(tt_list, .fun=function(k) simHydros_adapted(k))
	length(tt_list) == length(hydros)

	# list of data.tables with shifted true tracks
	# length(tt_shifted_list) = n_tracks * length(track_lengths) * length(shifts)
	tt_shifted_list <- apply(tt_shifts_dt, 1, FUN=function(k) shift_trueTrack(trueTrack=tt_list[[k['track_id']]], hydros[[k['track_id']]], k['shift']))

	# sanity check...
	length(tt_list) == n_tracks * length(track_lengths) & length(tt_shifted_list) == n_tracks * length(track_lengths) * length(shifts) & length(tt_shifted_list) == length(tt_list) * length(shifts)

	# data.table with all combinations of ping_type, BIs, true_tracks and shifts
	# nrow(sim_dt) = n_tracks * length(track_lengths) * length(shifts) * nrow(bis)
	sim_dt <- data.table(bind_rows(
		replicate(nrow(bis), tt_shifts_dt, simplify = FALSE)), 
		ping_type 	=	rep(bis$ping, each=nrow(tt_shifts_dt)),
		mean_bi 	=	rep(bis$mean_bi, each=nrow(tt_shifts_dt)),
		min_bi	 	=	rep(bis$min_bi, each=nrow(tt_shifts_dt)),
		max_bi 		=	rep(bis$max_bi, each=nrow(tt_shifts_dt))
	)
	sim_dt[, sim_id:= 1:.N]

	# get list of TOAs ans ss
	toa_list <- doGetToas(sim_dt, tt_shifted_list, hydros, sbi_sd, pNA, pMP)
	# length(toa_list)
	
	# length(toa_list) == nrow(sim_dt)
	# str(toa_list[[1]])

	# extract nrow(toa) from toa_list and add to sim_dt
	sim_dt[, nrow_toa := laply(toa_list, .fun=function(k) k[[3]])]


	# collect all params in a list for future use...
	sim_params <- list(
		crw_shape = crw_shape,
		crw_scale = crw_scale,
		pNA = pNA,
		pMP = pMP,
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
	return(list(sim_params=sim_params, sim_dt=sim_dt, toa_list=toa_list, hydros=hydros, tt_list=tt_list, tt_shifted_list=tt_shifted_list))
}

# wrapper to get toa_list from parallel
doGetToas <- function(sim_dt, tt_shifted_list, hydros, sbi_sd, pNA, pMP){
	cl = parallel::makeCluster(getOption("cl.cores", parallel::detectCores()))
	parallel::clusterExport(cl, list("getToas", "tt_shifted_list", "sim_dt", "hydros", "sbi_sd", "pNA", "pMP", "simulation", "simTelemetryTrack", "simToa"), envir = environment())
	toa_list <- parallel::clusterApplyLB(cl, x=1:nrow(sim_dt), fun=function(k) {getToas(k, sim_dt, tt_shifted_list, hydros, sbi_sd, pNA, pMP)})
	parallel::stopCluster(cl)
	return(toa_list)
}


# simulate TOA for all rows in sim_dt
getToas <- function(k, sim_dt, tt_shifted_list, hydros, sbi_sd, pNA, pMP) {
	track_id		<- sim_dt[k, 'track_id']
	track_shift_id 	<- sim_dt[k, 'track_shift_id']
	sim_id 			<- sim_dt[k, 'sim_id']
	ping_type 		<- sim_dt[k, 'ping_type']
	mean_bi			<- sim_dt[k, 'mean_bi']
	min_bi			<- sim_dt[k, 'min_bi']
	max_bi			<- sim_dt[k, 'max_bi']
	hydros			<- hydros[[track_id]]
	sim <- simulation(trueTrack=tt_shifted_list[[track_shift_id]], hydros=hydros, pingType=ping_type, sbi_mean=mean_bi, sbi_sd=sbi_sd, rbi_min=min_bi, rbi_max=max_bi, pNA=pNA, pMP=pMP)
	# return(sim)
	# extract relevant data from sim
	ss 			<- sim[[1]][, which(colnames(sim[[1]]) == "ss")]
	toa 		<- as.matrix(sim[[1]][, which(colnames(sim[[1]]) != "ss")])
	nrow_toa 	<- nrow(toa)
	dimnames(toa) <- NULL

	return(list(toa=toa, ss=ss, nrow_toa=nrow_toa))
}





# wrapper function to get it running in parallel
doRunYapsParallel <- function(sim_dt, toa_list, hydros, max_retries){
	cl = parallel::makeCluster(getOption("cl.cores", parallel::detectCores()))
	parallel::clusterExport(cl, list("sim_dt", "toa_list", "hydros", "save_yaps_out", "path", "yaps_parallel", "getInp", "max_retries"), envir = environment())
	est_list <- parallel::clusterApplyLB(cl, x=sample(1:nrow(sim_dt)), fun=function(k) {yaps_parallel(k, sim_dt, toa_list, hydros, max_retries=max_retries, path=path)})
	parallel::stopCluster(cl)
	return(est_list)
}

# Runs yaps
yaps_parallel <- function(k, sim_dt, toa_list, hydros, max_retries, path){
	sim_id <- as.numeric(sim_dt[k , 'sim_id'])
	track_id <- as.numeric(sim_dt[k , 'track_id'])
	toa <- toa_list[[sim_id]]$toa
	ss_dat <- toa_list[[sim_id]]$ss
	hydros_yaps <- hydros[[track_id]]
	ping_type <- as.character(sim_dt[k, 'ping_type'])
	rbi_min <- as.numeric(sim_dt[k, 'min_bi'])
	rbi_max <- as.numeric(sim_dt[k, 'max_bi'])

	try({
		pl <- c()
		outTmb <- c()
		yaps_done <- 0
		i <- 1
			# return(yaps_done)
		while(i <= max_retries & yaps_done != 1){

			# TOA needs to be trimmed for completely empty rows
			nobs <- apply(toa, 1, function(k) sum(!is.na(k)))
			first_ping <- which(nobs >= 1)[1]
			last_ping <- rev(which(nobs >= 1))[1]
			toa_yaps <- t(toa[first_ping:last_ping, ])
			ss_dat <- ss_dat[first_ping:last_ping]

			inp <- yaps::getInp(hydros=hydros_yaps, toa=toa_yaps, E_dist="t", n_ss=1, pingType=ping_type, ss_data_what="data", ss_data=ss_dat, rbi_min=rbi_min-0.5, rbi_max=rbi_max+0.5)
			inp$datTmb$Edist <- c(0,0,1) # set error distribution to t

			maxIter <- 	min(floor(300 + i^2), 1000)
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
				# } else if(outTmb$pl$logD_xy > 0){
					# yaps_done <- 0
				} else {yaps_done <- 1}
			} else {
				yaps_done <- 0
			}
			i <- i+1
		}
	})
	
	sim_dt_k <- sim_dt[k,]
	gb <- save_yaps_out(k, inp, outTmb, yaps_done, sim_dt_k, path)
	return(gb)
}

save_yaps_out <- function(k, inp, outTmb, yaps_done, sim_dt_k, path){
	yaps_status <- ifelse(yaps_done == 0, 'YAPS_FAILED', 'yaps_done')
	fn <- paste0('simId_', sim_dt_k[,'sim_id'],'_nrow_', sim_dt_k[,'nrow_toa'], '_', yaps_status)
	save(inp, outTmb, file=paste0(path,'/yaps_out/',fn,'.rObj'))
}





