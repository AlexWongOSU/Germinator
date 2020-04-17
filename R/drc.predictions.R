#' Creates a predicitons table to create 95% confidence intervals
#'   that can be used for plotting dose response curves in ggplot2.
#'
#'
#'@param data dataframe
#'@param splitfct variable to split data by. as chaaracter
#'@param nest.fct variable to nest data by
#'@param concrange numeric vector of min and max dose
#'@param smoothness Smoothness of prediction line. Default is 100.
#'@param ... arguments to pass into drm
#'
#'@importFrom stats predict
#'@import purrr
#'@import drc
#'
#'@export


drc.predictions<- function(data, splitfct, nest.fct, concrange = c(0,100), smoothness = 100, ...){
  #Need to use quoting to get the function to work
  split.data<- split(data, data[splitfct])

  nest.fct<- enquo(nest.fct)
  nest.data<- split.data%>%
    map(group_by, !!nest.fct)%>%
    map(nest)

  drmfx<- function(df, ...){
    drm(data = df, ...)
  }

  model.data<- nest.data%>%
    purrr::map(dplyr::mutate, model = purrr::map(data, drmfx, ...))

  predconc<- expand.grid(seq(from = concrange[1], to= concrange[2], length = smoothness))
  predfx<- function(mod){
    predict(object = mod, newdata = predconc, interval = "confidence")
  }

  pred.data<- model.data%>%
    map(mutate, predictions = map(model, predfx))

  return(pred.data)
}
