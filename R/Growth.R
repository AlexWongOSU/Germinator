#' Process and summarise data from "HyphaTracker" using the design from
#' Plate.Design
#'
#' @import dplyr
#' @importFrom  stats sd
#' @importFrom stats na.omit
#'
#' @param data Dataframe from "HyphaTracker" output.
#' @param Lookup Dataframe lookup table from the Plate.Design output
#' @param badimage numeric vector of image slices to remove from analysis
#' @param size numeric vector. Min and max pixel area of spores
#' for first and second timepoint.
#' @param circ numeric vector. Min and max circularity of spores
#' for first and second timepoint.
#'
#' @return An object of class 'Germinator'
#' @export
#'


Growth<- function(data = NULL,
                  Lookup = NULL,
                  badimage = NULL,
                  size = c(100,200,100,600),
                  circ = c(0.5,0.99,0.15,0.99)){
  data$Fungicide<- Lookup[match(data$Slice, Lookup$Slice), c('Fungicide')]
  data$Conc<- Lookup[match(data$Slice, Lookup$Slice), c('Conc')]
  data$Block<- Lookup[match(data$Slice, Lookup$Slice), c('Block')]
  data$TimePt<- Lookup[match(data$Slice, Lookup$Slice), c('Timepoint')]
  data$Isolate<- Lookup[match(data$Slice, Lookup$Slice), c('Isolate')]

  filtered.data.list<- data%>%
    dplyr::filter(Slice != badimage)%>%
    split(.$Timept)
  filtered.dataT0<- filtered.data[[1]]%>%
    dplyr::filter(Area>size[1])%>%
    dplyr::filter(Area<size[2])%>%
    dplyr::filter(Circ.>circ[1])%>%
    dplyr::filter(Circ.<circ[2])

  filtered.dataT1<- filtered.data[[2]]%>%
    dplyr::filter(Area>size[3])%>%
    dplyr::filter(Area<size[4])%>%
    dplyr::filter(Circ.>circ[3])%>%
    dplyr::filter(Circ.<circ[4])

  filtered.data<- rbind(filtered.dataT0, filtered.dataT1)

  summarized.data<- filtered.data%>%
    group_by(Isolate, Fungicide, Conc, Block, TimePt)%>%
    summarise(meanArea = mean(Area))%>%
    mutate(difference = meanArea - dplyr::lag(meanArea))%>%
    mutate(pctdiff = ((meanArea-dplyr::lag(meanArea))*100) / dplyr::lag(meanArea))%>%
    mutate(growth = ifelse(difference<0, 0.001, difference))%>%
    dplyr::filter(TimePt == 1)%>%
    na.omit()%>%
    group_by(Isolate, Fungicide, Conc)%>%
    mutate(n = length(growth))%>%
    mutate(seGrowth = sd(growth)/sqrt(n))

summarized.data<- as.data.frame(summarized.data)
  output<- list("new.data.df" = data, "GrowthData" = summarized.data)
  return(output)
}
