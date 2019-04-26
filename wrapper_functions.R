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

simulation <- function(trueTrack, pingType, sbi_mean=NA, sbi_sd=NA, rbi_min=NA, rbi_max=NA, pNA=0.25, pMP=0.01, shift=0, sigmaToa=1e-4){
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
  
  # Simulate hydrophone array
  hydros <- simHydros_adapted(trueTrack=trueTrack)
  hydros <- shift_hydros(hydros, trueTrack, shift=shift)
  
  toa_list <- simToa(teleTrack, hydros, pingType, sigmaToa=sigmaToa, pNA=pNA, pMP=pMP)
  toa <- toa_list$toa
  
  toa_rev <- t(toa)
  toa_rev_df <- as.data.frame(toa_rev)
  rownames(hydros) = colnames(toa_rev_df) # only add column names of receivers => therefore run this before adding soundspeed
  toa_rev_df$ss <- teleTrack$ss
  
  
  # return all info for use by Vemco
  res_list <- list(toa_rev_df, teleTrack, hydros)
  
  return(res_list)
}



estimation <- function(pingType, hydros, toa_rev_df, rbi_min=NA, rbi_max=NA){
  
  toa_rev_df$ss <- NULL
  toa <- t(data.matrix(toa_rev_df))
  
  if(pingType == 'sbi'){
    inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=1)
  } else if(pingType == 'rbi'){
    inp <- getInp(hydros, toa, E_dist="Mixture", n_ss=10, pingType=pingType, sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max)
  }
  
  pl <- c()
  maxIter <- ifelse(pingType=="sbi", 500, 5000)
  outTmb <- runTmb(inp, maxIter=maxIter, getPlsd=TRUE, getRep=TRUE)
  
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
  
  # return all info for use by Vemco
  res_list <- list(estimated_pos, real_error, estimated_error)
  
}