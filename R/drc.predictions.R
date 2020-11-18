#' Creates a predictions table to create 95% confidence intervals
#'   that can be used for plotting dose response curves in ggplot2.
#'
#'@param model object. drm model
#'@param length numeric. length of doses to form prediction table
#'
#'@export

drm.prediction<- function(model, length = 100){
  #Extract model variables
  curvevars<- unique(model[["data"]][[4]])
  curves<- length(curvevars)
  curvename<- model[["curveVarNam"]]
  x.name<-as.character(model[["call"]][["formula"]][[3]])
  doses<- seq(min(model[["data"]][[1]]), max(model[["data"]][[1]]), length.out = length)
  #build storage data frame of the dose values to be accessed by predict
  df<- data.frame(values = doses)
  for (x in 1:(curves)){
    df[,x+1]<- NA
  }
  #Extract dimensions of the data frame.
  n.value <- dim(df)[1]
  n.column<- dim(df)[2]
  #Storage matrix of prediction output
  pred.matrix = NULL
  for(i in 1:n.value){
    i.current<- df[i,"values"]
    #data frame used in the `newdata` argument for `predict`
    newdataframe<- expand.grid(dose = i.current, #all interactions of variable and dose
                              name = curvevars)
    names(newdataframe)<- c(x.name, curvename)
    i.pred<- stats::predict(model, newdata = newdataframe,
                            interval = "confidence")
    #rbind the output matrices together
    pred.matrix<- rbind(pred.matrix, i.pred)
  }
  #convert matrix into data frame and add in variables
  pred.df<- as.data.frame(pred.matrix)
  pred.df$CurveID<- rep(curvevars, times = length)
  pred.df$Dose<- rep(doses, each = length(curvevars))
  #reorder and sort data frame
  pred.df<- pred.df[, c("CurveID", "Dose", "Prediction", "Upper", "Lower")]
  names(pred.df)[names(pred.df)=="CurveID"]<- curvename
  names(pred.df)[names(pred.df)== "Dose"]<- x.name
  pred.df<- pred.df[order(pred.df[,1]),]
  return(pred.df)
}
