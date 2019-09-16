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
  hx.min <- min(trueTrack$x) - 5
  hx.max <- max(trueTrack$x) + 5
  hy.min <- min(trueTrack$y) - 5
  hy.max <- max(trueTrack$y) + 5
  
  x_extend_track = max(trueTrack$x) - min(trueTrack$x)
  y_extend_track = max(trueTrack$y) - min(trueTrack$y)

  hx.1 <- hx.min + x_extend_track/3
  hx.2 <- hx.max - x_extend_track/3
  hy.1 <- hy.min + y_extend_track/3
  hy.2 <- hy.max - y_extend_track/3
  
  Ox <- hx.min +  x_extend_track/2
  Oy <- hy.min + y_extend_track/2
  
  hx <- c(hx.min,hx.min,hx.max,hx.max, hx.1, hx.1, hx.2, hx.2) #Ox
  hy <- c(hy.min,hy.max,hy.max,hy.min, hy.1, hy.2, hy.1, hy.2) #Oy
  
  hydros <- data.frame(hx=hx, hy=hy)
  
  return(hydros)
}

simHydros <- function(auto=TRUE, trueTrack=NULL){
  try(if(auto == TRUE & is.null(trueTrack)) stop("When auto is TRUE, trueTrack needs to be supplied"))
  hx.min <- min(trueTrack$x) - 25
  hx.max <- max(trueTrack$x) + 25
  hy.min <- min(trueTrack$y) - 25
  hy.max <- max(trueTrack$y) + 25
  
  hx <- c(hx.min,hx.min,hx.max,hx.max, 0, 500,  500, -500, -500)
  hy <- c(hy.min,hy.max,hy.max,hy.min, 0, 500, -500, -500, 500)
  
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


chunk_estimation <- function(toa_rev_df, teleTrack, pingType, hydros, rbi_min=NA, rbi_max=NA, summary, csvtag, PATH){
  
  # take only chunk under consideration of teletrack
  teleTrack <- teleTrack[teleTrack$chunks==toa_rev_df$chunks[1],]
    
  # remove abundant columns
  toa_rev_df$ss <- NULL
  chunk_col <- toa_rev_df$chunks
  toa_rev_df$chunks <- NULL
  
  # reformat to matrix
  toa <- t(data.matrix(toa_rev_df))
  start_time_chunk <- min(toa, na.rm=TRUE)
  toa <- toa - start_time_chunk
  
  
  if(pingType == 'sbi'){
    inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=1)
  } else if(pingType == 'rbi'){
    inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max)
  }
  
  # return empty's if run doesn't succeed
  estimated_pos <- data.frame()
  real_error <- c()
  estimated_error <- c()
  chunk_info <- c()
  
  # Time!
  ptm <- proc.time()
  
  try({
    pl <- c()
    maxIter <- ifelse(pingType=="sbi", 500, 5000)
    outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
    
    outTmb <- NULL
    attempt <- 1
    
    while(is.null(outTmb) && attempt <=5){
      attempt <- attempt+1
      try({
        outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
      })
    }
    
    # Estimates in pl
    pl <- outTmb$pl
    estimated_pos <- as.data.frame(pl$X)
    colnames(estimated_pos) <- c('X')
    estimated_pos$Y <- pl$Y
    estimated_pos$Time <- pl$top + start_time_chunk
    
    # Error estimates in plsd
    plsd <- outTmb$plsd
    estimated_pos$Xe <- plsd$X
    estimated_pos$Ye <- plsd$Y
    
    x_diff <- pl$X-teleTrack$x
    y_diff <- pl$Y-teleTrack$y
    real_error <- sqrt(x_diff**2+y_diff**2)
    estimated_error <- sqrt(plsd$X**2+plsd$Y**2)
    
    chunk_info <- chunk_col
  })
  
  summary[1,"mean_real"] <- mean(real_error)
  summary[1,"mean_est"] <- mean(estimated_error)
  summary[1,"nb_pos"] <- length(real_error)
  summary[1,"run_time"] <- (proc.time() - ptm)[[3]]
  summary[1,"chunk_size"] <- strsplit(chunk_col[[1]], "_")[[1]][[1]]
  summary[1,"chunk_nb"] <- strsplit(chunk_col[[1]], "_")[[1]][[2]]
  
  tbl_name <- paste0(PATH,'/results/summaries/', pingType, "/summary_chunk_", chunk_col[[1]], "_", csvtag)
  write.table(summary,tbl_name, sep=',', col.names=FALSE, row.names=FALSE)
  write.csv(real_error, paste0(PATH,'/results/real_errors/', pingType, '/real_error_chunk_',chunk_col[[1]], "_", csvtag))
  write.csv(estimated_error, paste0(PATH,'/results/est_errors/', pingType, '/est_error_chunk_',chunk_col[[1]], "_", csvtag))
  write.csv(estimated_pos, paste0(PATH,'/results/yaps_tracks/', pingType, '/yaps_track_',chunk_col[[1]], "_", csvtag))
  # return all info
  # res_list <- list(estimated_pos, real_error, estimated_error, chunk_info)
  
}

readfiles <- function(result_path, n, mean_bi, dist, r, pingType){
  hydros <- read.csv(paste0(result_path,'hydros/hydros',toString(n), '_',toString(r),'.csv'), row.names = 1)
  
  nametag = paste0(toString(n), '_', pingType, toString(mean_bi), '_dist', toString(dist), '_rep', toString(r))
  
  toa_file = paste0(result_path,'toa_dfs/',pingType,'/toa_df_',nametag,'.csv')
  toa_df = read.csv(toa_file, skip = 7)  
  metadata <- read.csv(toa_file, nrows = 7, sep='\t', header = FALSE, row.names = 1, stringsAsFactors = FALSE)
  
  tele_file = paste0(result_path,'teleTracks/',pingType,'/teleTrack_',nametag,'.csv')
  teleTrack = read.csv(tele_file)  
  
  summary <- data.frame(matrix(ncol = 9, nrow = 1))
  colnames(summary) <- c("rep", "track_length", "pingType", "mean_bi", "dist", "mean_real", "mean_est", "nb_pos", "run_time")
  # Fill in part of the summary
  summary[1,"rep"] <- r
  summary[1,"pingType"] <- pingType
  summary[1,"mean_bi"] <- mean_bi
  summary[1,"dist"] <- dist
  
  
  return(list(hydros, toa_df, teleTrack, summary, metadata))
}

readin_and_estim <- function(combo, result_path, dist, r, pingType){
  mean_bi = combo$mean_bi
  n = combo$track_length 
  res = readfiles(result_path, n, mean_bi, dist, r, pingType)
  hydros = res[[1]]
  toa_df = res[[2]]
  teleTrack = res[[3]]
  summary = res[[4]]
  metadata = res[[5]]
  
  nametag = paste0(toString(n), '_', pingType, toString(mean_bi), '_dist', toString(dist), '_rep', toString(r))
  
  # remove abundant columns
  toa_df$ss <- NULL
  
  # reformat to matrix
  toa <- t(data.matrix(toa_df))
  
  if(pingType == 'sbi'){
    inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=1)
  } else if(pingType == 'rbi'){
    rbi_min = as.double(metadata["rbi_min",])
    rbi_max = as.double(metadata["rbi_max",])
    inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max)
  }
  
  # return empty's if run doesn't succeed
  estimated_pos <- data.frame()
  real_error <- c()
  estimated_error <- c()
  
  # Time!
  ptm <- proc.time()
  
  try({
    pl <- c()
    maxIter <- ifelse(pingType=="sbi", 500, 5000)
    outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
    
    outTmb <- NULL
    attempt <- 1
    
    while(is.null(outTmb) && attempt <=5){
      attempt <- attempt+1
      try({
        outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
      })
    }
    
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
  
  summary[1,"mean_real"] <- mean(real_error)
  summary[1,"mean_est"] <- mean(estimated_error)
  summary[1,"nb_pos"] <- length(real_error)
  summary[1,"run_time"] <- (proc.time() - ptm)[[3]]
  summary[1,"track_length"] <- n
  
  tbl_name <- paste0(result_path,'summaries/', pingType, "/summary", nametag,'.csv')
  write.table(summary,tbl_name, sep=',', col.names=FALSE, row.names=FALSE)
  write.csv(real_error, paste0(result_path,'real_errors/', pingType, '/real_error_',nametag,'.csv'))
  write.csv(estimated_error, paste0(result_path,'est_errors/', pingType, '/est_error_',nametag,'.csv'))
  write.csv(estimated_pos, paste0(result_path,'yaps_tracks/', pingType, '/yaps_track_',nametag,'.csv'))
  
}
