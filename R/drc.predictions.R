#' Creates a predictions table to create 95% confidence intervals
#'   that can be used for plotting dose response curves in ggplot2.
#'
#'
#'@param model object. drm model
#'@param doses numerical sequence vector of the range of doses tested
#'@param curvevar character string of the variable curves are predicted for.
#'
#'@export


drm.prediction.fx<- function(model, doses, curvevar){
  curves<- length(curvevar)
  df<- data.frame(values = doses)
  for (x in 1:(curves*3)) {
    df[,x+1]<- NA
  }
  n.value <- dim(df)[1]
  n.column<- dim(df)[2]
  for(i in 1:n.value){
    i.current<- df[i,"values"]
    i.pred<- predict(model, data.frame(dose = i.current, Isolate = curvevar), interval = "confidence")
    i.pred.vec<- as.vector(i.pred)
    for (j in 1:n.column) {
      df[i,j+1]<- i.pred.vec[j]
    }
  }

  df<- df[c(1:n.column)]
  df.names<- expand.grid(curvevar, c("prediction", "pred.min", "pred.max"))
  df.names$name<- paste(df.names$Var1, df.names$Var2, sep = ".")
  names(df)<- c("Dose", df.names$name)
  return(df)
}
