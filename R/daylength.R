#' @title daylength
#' @description This function calculates daylength based on solar angle and latitude as input. The Daylength Function is based on the BASFOR model by David Cameron and Marvel van Ojen.
#' @param solar angle (in radians)
#' @param LAT latitude at which the model is run
#' @export
#' @examples
#' # get solar angle:
#' day<- seq(1,365*4,1)
#' sangle= -23.45*pi/180 * cos((pi/180) * (day +10)) #(radians)
#' plot(sangle)
#' dayl<- seq(1,365*4,1)
#' for (i in 1:length(dayl)){
#'  dayl[i]<-daylength(sangle[i],LAT=48.6542)
#' }
#' plot(dayl)
daylength<-function(sangle,LAT){
  RAD  <- pi / 180.                                                  #  ! (radians deg-1)
  #! amplitude of ~( -40, 40 ) >
  #!WRITE(24,*) atan(-1./tan(RAD*LAT)),atan( 1./tan(RAD*LAT)),DEC,min( atan( 1./tan(RAD*LAT)),DEC)
  DECC <- max(atan(-1./tan(RAD*LAT)),min( atan( 1./tan(RAD*LAT)),sangle)) # ! (radians)
  #0.27789148782806800*24!WRITE(23,*)RAD,DEC,DECC,LAT, DAYL,day
  DAYL <- 0.5 * ( 1. + 2. * asin(tan(RAD*LAT)*tan(DECC)) / pi )        #! (d d-1)
  DAYL <- DAYL*24
  return(DAYL)
}
