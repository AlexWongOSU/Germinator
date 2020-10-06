# Germinator
## A R package to analyze ImageJ macro, 'HyphaTracker' (Brunk et al. 2019) data from *E. necator* 96-well fungicide efficacy assay.

`Lookuptable.maker` takes the input of fungicide treatments and builds a design for plate setup. This funtion also returns a lookup table that is used in the `Growth` function 

`Growth` takes the data output from Hyphatracker and the lookup table from `Lookuptable.maker` to assign the treatments to each ROI. Growth will also return a summarized version of the data to plot and use in models.

`drm.prediction` uses the `drm` model created using the `drc` package to create prediction tables for dose response curves with confidence intervals using ggplot instead of base plot. 
