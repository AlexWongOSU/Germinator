#' Creating lookup table from vectors of fungicides and concentrations
#'
#' @import utils
#' @importFrom tidyr separate
#'
#' @param Treatments A vector of treatments. Pesiticide name and concentration
#'   seperated by a single space.
#' @param Isolates A vector of isolates names.
#'   In order in which they are imaged
#' @param Wells Numeric, how many wells on a plate. Default is 96
#' @param Reps Numeric, How many times is each treatment repeated on the plate.
#'   Default is 4
#' @param timepts Numeric, timepoints evaluated. Default is 2.
#' @param random Logical, treatments randomly assigned to wells.
#'   Default in FALSE
#' @param randseed Numeric, randomization seed to be used. Default is 123456.
#' @param snake Logical. Indicate if the plate was imaged in a snake
#'   like pattern. Default is TRUE.
#'
#' @export



Lookuptable.maker<- function(Treatments = NULL,
                            Isolates = NULL,
                            Wells = 96,
                            Reps = 4,
                            timepts = 2,
                            random = FALSE,
                            randseed = 123456,
                            snake =TRUE) {
  sep.trt<- data.frame(x = Treatments)%>%
    separate(x, c("Fungicide", "Concentration"), sep = " ")

  if (random == TRUE){
    set.seed(randseed)
    RNG1<- sample(nrow(sep.trt))
    RNG2<- sample(nrow(sep.trt))
    RNG3<- sample(nrow(sep.trt))
    RNG4<- sample(nrow(sep.trt))
    Blk1<- sep.trt[RNG1, ]
    Blk2<- sep.trt[RNG2, ]
    Blk3<- sep.trt[RNG3, ]
    Blk4<- sep.trt[RNG4, ]
    Treatment_list<- rbind(Blk1,Blk2,Blk3,Blk4)
    Treatment_list$Fungicide<- Treatment_list$Fungicide[order(match(Treatment_list$Fungicide, sep.trt$Fungicide))]
    Treatment_list$Concentration<- as.numeric(rep(sep.trt$Concentration, each = Reps))
  } else {
    replicated<- do.call("rbind", replicate(Reps, sep.trt, simplify = FALSE))
    Treatment_list<- replicated
    Treatment_list$Fungicide<- Treatment_list$Fungicide[order(match(Treatment_list$Fungicide, sep.trt$Fungicide))]
    Treatment_list$Concentration<- as.numeric(rep(sep.trt$Concentration, each = Reps))
  }

  Treatment<- paste(Treatment_list$Fungicide,Treatment_list$Concentration)
  plate.design<- matrix(Treatment, nrow = 8, ncol = 12)
  colnames(plate.design)<- c(1:12)
  row.names(plate.design)<- LETTERS[1:8]

  if(snake == TRUE){
    Treatment_list$Well<- rep(c(1:8,16:9,17:24,32:25,33:40,48:41,49:56,64:57,65:72,80:73,81:88,96:89))
    Treatment_list<- Treatment_list[order(Treatment_list$Well),]
    LookupTable<- data.frame(Slice = c(1:(Wells*length(Isolates)*timepts)),
                             Well = rep(1:Wells, times = length(Isolates)*timepts),
                             Isolate = rep(Isolates, each = Wells, times = timepts),
                             Timepoint = rep(0:(timepts-1), each = Wells*length(Isolates)),
                             Fungicide = rep(Treatment_list$Fungicide, times= length(Isolates)*timepts),
                             Conc = rep(Treatment_list$Concentration, times = length(Isolates)*timepts),
                             Block = rep(1:Reps, times= timepts*(Wells/Reps)*length(Isolates)))
  } else {
    LookupTable<- data.frame(Slice = c(1:(Wells*length(Isolates)*timepts)),
                             Well = rep(1:Wells, times = length(Isolates)*timepts),
                             Isolate = rep(Isolates, each = Wells, times = timepts),
                             Timepoint = rep(0:(timepts-1), each = Wells*length(Isolates)),
                             Fungicide = rep(Treatment_list$Fungicide, times= length(Isolates)*timepts),
                             Conc = rep(Treatment_list$Concentration, times = length(Isolates)*timepts),
                             Block = rep(1:Reps, times= timepts*(Wells/Reps)*length(Isolates)))
  }

  output<- list("Plate.Layout" = plate.design, "LookupTable" = LookupTable)
  return(output)
}
