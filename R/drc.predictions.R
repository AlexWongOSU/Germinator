#' Creates a predicitons table to create 95% confidence intervals
#'   that can be used for plotting dose response curves in ggplot2.
#'
#'
#'@param data dataframe used for the model
#'@param model model object from drm
#'@param conc numeric string. Concentrations or doses used in acsending order.
#'@param LOG Logical. True if concentration scale is logistic. Default is TRUE.
#'@param sm.value Numeric. Small value to add to concentrations of zero to
#'  avoid log of 0. Defualt is 0.00005.
#'@param smoothness resolution of confidence intervals. 100 is default
#'
#'@importFrom stats predict
#'
#'@export


drc.predictions<- function(data,
                           model,
                           conc,
                           LOG=TRUE,
                           sm.value = 0.00005,
                           smoothness = 100){
  if (LOG == TRUE) {
    newdata <- expand.grid(conc=exp(seq(log(conc[1]+sm.value),
                                        log(conc[length(conc)]),
                                        length = smoothness)))
  }
  else {
    newdata <- expand.grid(conc=seq(conc[1],
                                    conc[length(conc)]),
                           length = smoothness)
  }
  prediction.model<- predict(model,
                             newdata = newdata,
                             interval = "confidence")
  newdata$pred <- prediction.model[,1]
  newdata$pmin <- prediction.model[,2]
  newdata$pmax <- prediction.model[,3]

  return(newdata)
}
