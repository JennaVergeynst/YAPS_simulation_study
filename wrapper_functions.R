## Temporarily add this function until original YAPS is adapted
simTrueTrack <- function(model='rw', n, deltaTime=1, D=NULL, shape=NULL, scale=NULL, addDielPattern=TRUE, start_pos=NULL){
  try(if(model=='rw'  & is.null(D)) stop("When model == 'rw', D needs to be specified"))
  try(if(model=='crw' & (is.null(shape) | is.null(scale))) stop("When model == 'crw', shape and scale needs to be specified"))
  
  #times
  dt <- rep(deltaTime, n-1)
  time <- cumsum(c(0, dt))
  
  #start position
  if(!is.null(start_pos)){
    x0 <- start_pos[1]
    y0 <- start_pos[2]
  }	else{
    x0 <- stats::runif(1,0,5)
    y0 <- stats::runif(1,0,5)
  }
  
  #RW-model
  if(model == 'rw'){
    x <- cumsum(c(x0, stats::rnorm(n-1, 0, sd=sqrt(2*D*dt))))
    y <- cumsum(c(y0, stats::rnorm(n-1, 0, sd=sqrt(2*D*dt))))
  } else if (model == 'crw'){
    # make weibull distributed steps
    steps <- stats::rweibull(n-1, shape, scale)
    if(addDielPattern){
      # # # # make diel pattern - first 1/4 very little movement, middle half normal-high movement, last 1/4 very little movement
      steps[1:floor(n/8)] <- steps[1:floor(n/8)]/50
      steps[(3*floor(n/8)):(4*floor(n/8))] <- steps[(3*floor(n/8)):(4*floor(n/8))]/50
      steps[(5*floor(n/8)):(6*floor(n/8))] <- steps[(5*floor(n/8)):(6*floor(n/8))]/50
      steps[(7*floor(n/8)):n-1] <- steps[(7*floor(n/8)):n-1]/50
    }
    
    # make clustered turning angles
    theta <- circular::rwrappedcauchy(n-1, mu=circular::circular(0), rho=.99)
    # cumulative angle (absolute orientation)
    Phi <- cumsum(theta)
    # step length components
    dX <- c(x0, steps*cos(Phi))
    dY <- c(y0, steps*sin(Phi))
    # actual X-Y values
    x<-cumsum(dX)
    y<-cumsum(dY)
  }
  return(data.frame(time=time, x=x, y=y))
}



simHydros_adapted <- function(trueTrack){
  hx.min <- min(trueTrack$x) - 25
  hx.max <- max(trueTrack$x) + 25
  hy.min <- min(trueTrack$y) - 25
  hy.max <- max(trueTrack$y) + 25
  
  hx.1 <- min(trueTrack$x) - 5
  hx.2 <- max(trueTrack$x) + 5
  hy.1 <- min(trueTrack$y) - 5
  hy.2 <- max(trueTrack$y) + 5
  
  hx <- c(hx.min,hx.min,hx.max,hx.max, 0, hx.1,  hx.1, hx.2, hx.2)
  hy <- c(hy.min,hy.max,hy.max,hy.min, 0, hy.1,  hy.2, hy.1, hy.2)
  
  hydros <- data.frame(hx=hx, hy=hy)
  
  return(hydros)
}

# shift_hydros <- function(hydros, trueTrack, shift=2/3){
#   x_extend_track = max(trueTrack$x) - min(trueTrack$x)
#   y_extend_track = max(trueTrack$y) - min(trueTrack$y)
#   hydros_shifted = hydros
#   hydros_shifted$hx = hydros$hx+x_extend_track*shift
#   hydros_shifted$hy = hydros$hy+y_extend_track*shift
#   
#   return(hydros_shifted)
# }

shift_hydros <- function(hydros, trueTrack, shift=1/2){
  x_extend = max(hydros$hx) - min(hydros$hx)
  y_extend = max(hydros$hy) - min(hydros$hy)
  hydros_shifted = hydros
  hydros_shifted$hx = hydros$hx+x_extend*shift
  hydros_shifted$hy = hydros$hy+y_extend*shift
  
  return(hydros_shifted)
}

shift_trueTrack <- function(trueTrack, dist_to_array=0){
  min.x <- min(trueTrack$x)
  shift <- 250+dist_to_array-min.x
  trueTrack_shifted <- trueTrack
  trueTrack_shifted$x <- trueTrack$x + shift
  
  return(trueTrack_shifted)
}

simulation <- function(trueTrack, hydros, pingType, sbi_mean=NA, sbi_sd=NA, rbi_min=NA, rbi_max=NA, pNA=0.25, pMP=0.01, sigmaToa=1e-4){
  # shift == FALSE or float
  # Simulate telemetry observations from true track.
  # Format and parameters depend on type of transmitter burst interval (BI) - stable (sbi) or random (rbi).
  pingType <- pingType
  
  if(pingType == 'sbi') { # stable BI
    sbi_mean <- sbi_mean; sbi_sd <- sbi_sd;
    teleTrack <- simTelemetryTrack(trueTrack, ss='rw', pingType=pingType, sbi_mean=sbi_mean, sbi_sd=sbi_sd)
  } else if(pingType == 'rbi'){ # random BI
    pingType <- pingType; rbi_min <- rbi_min; rbi_max <- rbi_max;
    teleTrack <- simTelemetryTrack(trueTrack, ss='rw', pingType=pingType, rbi_min=rbi_min, rbi_max=rbi_max)
  }

  # Convert TelemetryTrack in toa-matrix that can be fed to YAPS
  toa_list <- simToa(teleTrack, hydros, pingType, sigmaToa=sigmaToa, pNA=pNA, pMP=pMP)
  toa <- toa_list$toa
  toa_rev <- t(toa)
  toa_rev_df <- as.data.frame(toa_rev)
  rownames(hydros) = colnames(toa_rev_df) # only add column names of receivers => therefore run this before adding soundspeed
  toa_rev_df$ss <- teleTrack$ss
  
  return(list(toa_rev_df, teleTrack))
}


#' @param toa_data TOA dataframe with receivers and groups_pas in columns.
#' @param chunklen length of each chunk witin each groups_pas 
chunk_toa <- function(toa_data, chunklen){
  toa_data$chunks <- NA
  chunk_counter <- 1
  len <- dim(toa_data)[1]
  n <- ceiling(len/chunklen)
  chunks <- c()
  for (i in as.list(1:(n-1))){ # going until the before-last chunk, because the last chunk gets special treatment
    chunks <- append(chunks, c(rep(chunk_counter,chunklen))) # changed i by chunk_couter
    chunk_counter <- chunk_counter+1
  }
  if (len%%chunklen > 0.75*chunklen){ # take the last chunk separate only if it's 3/4 of chunklen
    chunks <- append(chunks, c(rep(chunk_counter,chunklen))) # changed n by chunk_couter
    chunk_counter <- chunk_counter+1
  }
  else{ # add last chunk to the before-last is the last one is too short
    chunks <- append(chunks, c(rep(chunk_counter-1,chunklen))) # changed n by chunk_couter
    # here don't increase chunk_counter, because previous chunk_counter is used
  }
  toa_data$chunks <- chunks[1:len]

  return(toa_data)
}


estimation <- function(toa_rev_df, teleTrack, pingType, hydros, rbi_min=NA, rbi_max=NA){
  # take only chunk under consideration of teletrack
  # toa_rev_df <- toa_list[[59]]
  set.seed(42) # needed while de-bugging
  teleTrack <- teleTrack[teleTrack$chunks==toa_rev_df$chunks[1],]
  chunk <-   toa_rev_df$chunks[1]
  # remove abundant columns
  toa_rev_df$ss <- NULL
  toa_rev_df$chunks <- NULL
  
  # reformat to matrix
  toa <- t(data.matrix(toa_rev_df))
  toa <- toa - min(toa, na.rm=TRUE) 
  
  # return empty's if run doesn't succeed
  estimated_pos <- data.frame()
  real_error <- c()
  estimated_error <- c()
  
  try({
    pl <- c()
	outTmb <- c()
	yaps_done <- 0
	i <- 1
    while(i <= 100 & yaps_done != 1){
 	    if(pingType == 'sbi'){
			inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=1)
			} else if(pingType == 'rbi'){
			inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max)
		}

		# maxIter <- ifelse(pingType=="sbi", 500, 5000)
		maxIter <- 	100*i^1.5
		tryCatch({
			outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
		}, error=function(e){})
		
		# identify cases where runTmb fails and try again...
		if(!is.null(outTmb)) {
			if(sum(unlist(lapply(outTmb$plsd, FUN=is.na))) != 0){
				yaps_done <- 0
			} else if(outTmb[1] == "noOpt"){
				yaps_done <- 0
			} else if(outTmb$pl$log_t_part > -2.5){
				yaps_done <- 0
			} else {yaps_done <- 1}
		} else {
			yaps_done <- 0
		}
		
		# temp for HBA debugging
		# if(yaps_done == 0){
			# gnu <- "gnu"
			# save(gnu, file=paste0("/home/hbak/H/ms/others/2019-04 JennaVergeynst - simulation study/git/YAPS_simulation_study/results/outTmbs/status_",chunk,"i_",i,".rObj"))
		# }

		i <- i+1
	}
	
	# # temp for HBA debugging
	# if(yaps_done == 0) {outTmb <- 'noYaps'}
	# save(outTmb, inp, teleTrack, file=paste0("/home/hbak/H/ms/others/2019-04 JennaVergeynst - simulation study/git/YAPS_simulation_study/results/outTmbs/outTmb_chunk_",chunk,"i_",i-1,".rObj"))
    # Estimates in pl
    pl <- outTmb$pl
    estimated_pos <- as.data.frame(pl$X)
    colnames(estimated_pos) <- c('X')
    estimated_pos$Y <- pl$Y
    estimated_pos$Time <- pl$top
    
    # Error estimates in plsd
    plsd <- outTmb$plsd
    estimated_pos$Xe <- plsd$X
    estimated_pos$Ye <- plsd$Y
    
    x_diff <- pl$X-teleTrack$x
    y_diff <- pl$Y-teleTrack$y
    real_error <- sqrt(x_diff**2+y_diff**2)
    estimated_error <- sqrt(plsd$X**2+plsd$Y**2)
  })
  
  # return all info
  res_list <- list(estimated_pos, real_error, estimated_error)
}