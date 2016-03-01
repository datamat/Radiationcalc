# Matthias Haeni
# August 12, 2015

suppressMessages(library("curl"))
suppressMessages(library("jsonlite"))

radcalc <- function(df,lat,lon) {
  phi <- lat*pi/180
  lambda_e <- lon*pi/180
  S <- 1368
  phi_r <- 23.45*pi/180
  d_r <- 173
  d_y <- 365.25
  
  time <- df$ts[1]
  io <- as.numeric(time)
  if(file.exists("../googleAPIkey.txt")) {
    key <- readLines("../googleAPIkey.txt",n=1,warn=FALSE)  
  } else {
    if(file.exists("googleAPIkey.txt")) {
      key <- readLines("googleAPIkey.txt",n=1,warn=FALSE)  
    } else {
      key <- readLines("archive/googleAPIkey.txt",n=1,warn=FALSE)
    }
  }
  url <- paste0("https://maps.googleapis.com/maps/api/timezone/json?location=",
                lat,",",lon,"&timestamp=",io,"&language=en&key=",key)
  tiz <- jsonlite::fromJSON(url); tiz
  
  tt <- tiz$rawOffset/60/60-tiz$dstOffset/60/60
  df$tsUTC <- as.POSIXlt(df$ts+tt*3600,tz="UTC")
  head(df)
  d <- df$tsUTC$yday+1
  tsUTC <- df$tsUTC$hour+df$tsUTC$min/60
  delta_s <- phi_r*cos(2*pi*(d-d_r)/d_y)
  sin_Psi <- sin(phi)*sin(delta_s)-cos(phi)*cos(delta_s)*cos((pi*tsUTC/12-lambda_e))
  sin_Psi[sin_Psi<0] <- 0
  df$K <- S*sin_Psi
  
  df <- df[,c("ts","K")]
  df$day <- format(df$ts,"%Y-%j")
  bar <- aggregate(df,by=list(df$day),max,na.rm=TRUE)
  bar$ts <- as.POSIXct(strptime(bar$day,"%Y-%j"),tz="UTC")
  bar <- bar[,c("ts","K")]
  names(bar)[2] <- "Kmax"
  foo <- merge(df,bar,all=TRUE)
  
  df <- foo[,c("ts","K","Kmax")]
  return(df)
}
