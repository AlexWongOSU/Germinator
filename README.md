# Germinator
## A R package to analyze ImageJ macro, 'HyphaTracker' (Brunk et al. 2019) data from *E. necator* 96-well fungicide efficacy assay.

`Lookup.maker` takes the input of fungicide treatments and builds a design for plate setup. This funtion also returns a lookup table that is used in the `Growth` function 

`Growth` takes the data output from Hyphatracker and the lookup table from `Lookup.maker` to assign the treatments to each ROI. Growth will also return a summarized version of the data to plot and use in models.

`drc.predictions` uses the summarized data and the model created using the `drc` package to plot dose response curves using ggplot instead of base plot. 
