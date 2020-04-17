#' Creates a predicitons table to create 95% confidence intervals
#'   that can be used for plotting dose response curves in ggplot2.
#'
#'
#'@param data dataframe
#'@param dose Dose variable name
#'@param pesticide character vector of pesticides tested
#'@param iso character vector of isolates tested
#'@param doserange numeric vector of min and max dose
#'@param responsevar variable name of response variable
#'@param fct specific non linear function. Default in LL.3
#'@param ... Additional arguments to pass into drm
#'@param levels Variables to break up data. From largest to smallest
#'@param smoothness Smoothness of prediction line. Default is 100.
#'
#'@importFrom stats predict
#'@import purrr
#'@import drc
#'
#'@export


drc.predictions<- function(data = NULL, dose, pesticide, iso, doserange, responsevar, fct = LL.3(), ..., levels = NULL, smoothness = 100){
  conc.predictions<- expand.grid(Conc = seq(from = doserange[1],
                                       to = doserange[2],
                                       length = smoothness))

   split.data<- data%>%
    split(.$levels[1])

  nest.data<- split.data%>%
    purrr::map(dplyr::group_by, levels[2])%>%
    purrr::map(tidyr::nest)

  drmfx<- function(df)
    drm(responsevar~dose, data = df, fct= fct, ...)

  model.data<- nest.data%>%
    purrr::map(dplyr::mutate, model = purrr::map(data ,drmfx))

  predictionfx<- function(model, pred)
    stats::predict(object = model, newdata = pred, interval = "confidence")

  pred.data<- model.data%>%
    purrr::map(dplyr::mutate, predictions = purrr::map(model, predictionfx, pred = conc.predictions))%>%
    purrr::map(dplyr::mutate, plotting.pred = purrr::map(pred, dplyr::mutate, conc.predictions))
}
