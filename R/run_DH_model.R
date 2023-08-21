#' @title run_DH_model
#' @description runs the DH-model, based on the equations in Deleuze et Houllier 1998. Temperature is translated into number of cells produced,
#' Relative water content is translated into volume gain,
#' Cpool is translated into mass gain. The model calculates the resulting density.
#' Outputs a data frame with weekly entries of simulated density (Dens), ring increment ( Incr ), and cell numbers (Nr).
#' @param Tair vector with weekly average air temperature
#' @param Rw vector with weekly average relative soil water content
#' @param Rw_vol whether input for soil moisture is given as volumetric (TRUE) or relative soil (FALSE) water content. default: FALSE
#' @param Cpool vector with weekly average nonstructural carbohydrates
#' @param dayl vector with weekly average daylength ( can be created using the daylength function and with latitude input in this package)
#' daylength for growth initialisation is an addition to the DH-model, it was not originally part of it.
#' @param params vector of parameter values
#' @param DH_plot whether output should be plotted or not, default is TRUE
#' @param week vector with week number of a given year
#' @param year vector with repeated year entries for a given year
#'
#' @export
#' @examples
#' # Other preparations to run the model
#' data(DH_model_ins)
#'w = length(daily_tree_135)
#'
#'DH_out <-  run_DH_model_mod(Tair  = DH_model_ins$Inputs_ABR_4years$Tair,
#'                        Rw        =  DH_model_ins$Inputs_ABR_4years$SW,
#'                        Rw_vol    = FALSE
#'                        Cpool     =  DH_model_ins$Inputs_ABR_4years$Cpool,
#'                        params    =   DH_model_ins$Parameters$best,
#'                        week = DH_model_ins$Inputs_ABR_4years$week,
#'                        year = DH_model_ins$Inputs_ABR_4years$year.datetime.,
#'                        DH_plot = TRUE)
run_DH_model <- function(Tair,Rw,Rw_vol=FALSE,Cpool,dayl = NULL,week, year,params,DH_plot=TRUE) {


  w <- length(week)
  D_n<-rep(0,(w) )
  D_v<-rep(0,(w) )
  D_m<-rep(0,(w) )


  # hand over parameters, this part here is more important when doing a sensitivity analysis an dcalibration
  Tmin = params[1] #  # °C Temperature below with cambial activity, volume and mass increment do not occur.
  Rwmin = params[2] # ( -) minimum relative soil moisture at which cambial activity, volume and mass increment do not occur.
  D_n_max = params[3]
  b = params[4]
  D_v_max = params[5]
  D_m_max = params[9]
  delta = params[6]
  chi = params[7]
  R_w_crit = params[8]
  dayl_min = params[10]

  # R_w_crit and chi serve as parameters "compensating" or adjusting for the
  # probably wrong relative soil moisture values derived from model output ( if sw is from model output..)
  # This is a way to make sure the soil moisture forcing, which is the most uncertain,
  # does not screw things up too much.
  # We could later determine the contribution of these parameters to the "more correct" simulation output.
  # function from Wilkinson et al 2015:
  if(Rw_vol==TRUE){
    Rw<-1-1/(1+(Rw/R_w_crit)^chi)
  }


  ds<-c(1:(w))

  # run DH-model

  # if daylength vector is provided:
  if( !is.null(dayl)){
    for(i in 1:(w)){

      if(Tair[i] >= Tmin & Rw[i] >= Rwmin & dayl[i] >= dayl_min) {

        D_n[i]<- D_n_max * (1-(exp( -b *(Tair[i]-Tmin) )))

        D_v[i]<- D_v_max * (Rw[i])

        D_m[i]<- D_m_max * (1- exp(-delta*Cpool[i]))

      }# weekly loop
    }# phenology loop
  }else{# daylength input check.
    for(i in 1:(w)){

      if(Tair[i] >= Tmin & Rw[i] >= Rwmin) {

        D_n[i]<- D_n_max * (1-(exp( -b *(Tair[i]-Tmin) )))

        D_v[i]<- D_v_max * (Rw[i])

        D_m[i]<- D_m_max * (1- exp(-delta*Cpool[i]))

      }# weekly loop
    }# phenology loop
  } # daylength input check.


  # prepare output and plotting

  Tair.df<-data.frame(Tair=Tair,dates=ds,D_n=D_n)
  Rw.df<-data.frame(Rw=Rw,dates=ds,D_v=D_v)
  Cpool.df<-data.frame(Cpool=Cpool,dates=ds,D_m=D_m)

  Incr<-D_n*D_v
  Dens<-(D_m/D_v)*1000 # to convert dm^3 (Wilkinson 2015) # m^3 ( Cuny et al 2014)
  Dens.df<-data.frame(Dens=Dens,weeks=week, years=year)


  if(DH_plot==TRUE){
    par(mfrow=c(4,1),mar=c(0,5,0,5))#,omar=c(4,0,1,0))

    plot(Tair.df$dates,Tair.df$Tair,ylim=c(0,30),type="l",xaxt='n',xlab="",xaxs="i",yaxs="i",ylab="")
    mtext("Temperature (°C)",side=2,col="black",cex=0.7,line=3,outer=FALSE)
    abline(h=25,col = "lightgray", lty = 3)
    abline(h=2.5,col = "lightgray", lty = 3)
    par(new = T)
    plot(Tair.df$D_n,col="red",ylim=c(0.0,1.5), type ="p",xaxt='n',xlab="",yaxt='n',ylab="",xaxs="i",yaxs="i")
    mtext("cells/week",side=4,col="red",cex=0.7,line=3,outer=FALSE)
    axis(4,ylim=c(0.0,1.5),col="red",col.axis="red",las=1)


    plot(Rw.df$dates,Rw.df$Rw,ylim=c(0,1.52), type="l",xaxt='n',xlab="",xaxs="i",yaxs="i",ylab="")
    mtext("Rw (-)",side=2,col="black",cex=0.7,line=3,outer=FALSE)
    abline(h=0.5,col = "lightgray", lty = 3)
    par(new = T)
    plot(Rw.df$date,Rw.df$D_v,col="red",ylim=c(0.0,0.0074),xaxt='n',xlab="",yaxt='n',ylab="",xaxs="i",yaxs="i")
    mtext(expression(paste(Delta, " volume m3/week")),side=4,col="red",cex=0.7,line=3,outer=FALSE)
    axis(4, ylim=c(0.0,0.0074), col="red",col.axis="red",las=1)


    plot(Cpool.df$dates,Cpool.df$Cpool,ylim=c(0,4),type="l",xaxt='n',xlab="",xaxs="i",yaxs="i",ylab="")
    mtext("Cpool (Kg[C])",side=2,col="black",cex=0.7,line=3,outer=FALSE)
    #lines(Ci, col="grey",lty=3)
    par(new = T)
    plot(Cpool.df$D_m,col="red",ylim=c(0.0,0.0052),xaxt='n',xlab="",yaxt='n',ylab="",xaxs="i",yaxs="i")
    mtext(expression(paste(Delta," mass kg/week")), side=4 ,col="red",cex=0.7,line=3,outer=FALSE)
    axis(4, ylim=c(0.0,0.0052), col="red",col.axis="red",las=1)

    par(mar=c(3,5,0,5))
    plot(Cpool.df$dates,Dens.df$Dens,ylim=c(0,2000),xlab="", xaxt='n', ylab="Density(kg/m3)",xaxs="i",yaxs="i")
    # useful for later, when labelling the plots with a,b,c,d,..
    # mtext(side=3,line=-3.2,"(d)",adj=0,cex=1.2,padj=1,outer=TRUE)

    #axis(1, at=seq(1,52,52), labels=2000)
    axis(1,at= c(1,52,52*2,52*3),unique(Dens.df$year))
    mtext("Years", side=1 ,col="black",cex=1,line=2,outer=FALSE)

  }

  df<-Dens.df
  df$Incr<-Incr
  df$Nr<-D_n

  # post-processing of NaNs to NA: no density: ( more sensible for post-processing and plotting later on)
  df$Dens[is.nan(df$Dens)] <- NA

  return(df)

}
