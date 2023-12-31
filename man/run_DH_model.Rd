% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_DH_model.R
\name{run_DH_model}
\alias{run_DH_model}
\title{run_DH_model}
\usage{
run_DH_model(
  Tair,
  Rw,
  Rw_vol = FALSE,
  Cpool,
  dayl = NULL,
  week,
  year,
  params,
  DH_plot = TRUE
)
}
\arguments{
\item{Tair}{vector with weekly average air temperature}

\item{Rw}{vector with weekly average relative soil water content}

\item{Rw_vol}{whether input for soil moisture is given as volumetric (TRUE) or relative soil (FALSE) water content. default: FALSE}

\item{Cpool}{vector with weekly average nonstructural carbohydrates}

\item{dayl}{vector with weekly average daylength ( can be created using the daylength function and with latitude input in this package)
daylength for growth initialisation is an addition to the DH-model, it was not originally part of it.}

\item{week}{vector with week number of a given year}

\item{year}{vector with repeated year entries for a given year}

\item{params}{vector of parameter values}

\item{DH_plot}{whether output should be plotted or not, default is TRUE}
}
\description{
runs the DH-model, based on the equations in Deleuze et Houllier 1998. Temperature is translated into number of cells produced,
Relative water content is translated into volume gain,
Cpool is translated into mass gain. The model calculates the resulting density.
Outputs a data frame with weekly entries of simulated density (Dens), ring increment ( Incr ), and cell numbers (Nr).
}
\examples{
# Load data and parameters in preparation to run the model
data(DH_model_ins)

# run model:
DH_out <-  run_DH_model(Tair  = DH_model_ins$Inputs_ABR_4years$Tair,
                       Rw        =  DH_model_ins$Inputs_ABR_4years$SW,
                       Rw_vol    = FALSE,
                       Cpool     =  DH_model_ins$Inputs_ABR_4years$Cpool,
                       params    =   DH_model_ins$Parameters$best,
                       week = DH_model_ins$Inputs_ABR_4years$week,
                       year = DH_model_ins$Inputs_ABR_4years$year.datetime.,
                       DH_plot = TRUE)
}
