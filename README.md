# DH-model
Deleuze et Houllier 1998 model with some modifications


Contains some forcing and parameter data for initial try-out of the model.


# Install package 
get help here:
https://stackoverflow.com/questions/21171142/how-to-install-r-package-from-private-repo-using-devtools-install-github
and follow the answer by Brandon Rose MD MPH

# Develop (and install package)
Clone this repo, and open it in Rstudio.
Execute the following commands (might have to install devtools) 
```
devtools::document()
devtools::build()
devtools::install()

```

## run the model
Check out the Package vignette or:

once installed and loaded, use the command
```data(DH_model_ins)```
and look at the object.

This data can be used to run the model using the function:
```run_DH_model```

For more info on how to run the model, do 
```?run_DH_model```


This package was released in conjunction with a workshop on wood formation modelling by Q-NET (https://qwa-net.com/).


## Related Resources/References :

The main principles and equations of the model are from:
Deleuze, C., & Houllier, F. (1998). A Simple Process-based Xylem Growth Model for Describing Wood Microdensitometric Profiles. Journal of Theoretical Biology, 193(1), 99–113. https://doi.org/10.1006/jtbi.1998.0689

Parameters are mostly from:
Wilkinson, S., Ogee, J., Domec, J.-C., Rayment, M., & Wingate, L. (2015). Biophysical modelling of intra-ring variations in tracheid features and wood density of Pinus pinaster trees exposed to seasonal droughts. Tree Physiology, 35(3), 305–318. https://doi.org/10.1093/treephys/tpv010

Forcing data is using the meteorological data, obtained from Cyrille Rathgeber, also partially used in:
Cuny, H. E., & Rathgeber, C. B. K. (2016). Xylogenesis: Coniferous Trees of Temperate Forests Are Listening to the Climate Tale during the Growing Season But Only Remember the Last Words! Plant Physiology, 171(1), 306–317. https://doi.org/10.1104/pp.16.00037



