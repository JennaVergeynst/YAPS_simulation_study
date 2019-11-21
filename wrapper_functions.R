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


shift_trueTrack <- function(trueTrack, hydros, shift){
  x_width <- max(hydros$hx)-min(hydros$hx)
  trueTrack_shifted <- trueTrack
  trueTrack_shifted$x <- trueTrack$x + shift*x_width
  
  return(trueTrack_shifted)
}
