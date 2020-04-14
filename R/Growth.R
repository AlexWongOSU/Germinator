#' Process and summarise data from "HyphaTracker" using the design from
#' Plate.Design
#'
#' @import dplyr
#' @importFrom  stats sd
#'
#' @param data Dataframe from "HyphaTracker" output.
#' @param Lookup Dataframe lookup table from the Plate.Design output
#' @param badimage numeric vector of image slices to remove from analysis
#'
#' @return An object of class 'Germinator'
#' @export
#'


Growth<- function(data, Lookup, badimage){
  data$Fungicide<- Lookup[match(data$Slice, Lookup$Slice), c('Fungicide')]
  data$Conc<- Lookup[match(data$Slice, Lookup$Slice), c('Conc')]
  data$Block<- Lookup[match(data$Slice, Lookup$Slice), c('Block')]
  data$TimePt<- Lookup[match(data$Slice, Lookup$Slice), c('Timepoint')]
  data$Isolate<- Lookup[match(data$Slice, Lookup$Slice), c('Isolate')]

  summarized.data<- data%>%
    dplyr::filter(Slice != badimage)%>%
    group_by(Isolate, Fungicide, Conc, Block, TimePt)%>%
    summarise(meanArea = mean(Area))%>%
    mutate(difference = meanArea - dplyr::lag(meanArea))%>%
    mutate(pctdiff = ((meanArea-dplyr::lag(meanArea))*100) / dplyr::lag(meanArea))%>%
    mutate(growth = ifelse(difference<0, 0.001, difference))%>%
    dplyr::filter(TimePt == 1)%>%
    group_by(Isolate, Fungicide, Conc)%>%
    mutate(n = length(growth))%>%
    mutate(seGrowth = sd(growth)/sqrt(n))

summarized.data<- as.data.frame(summarized.data)
  output<- list("new.data.df" = data, "GrowthData" = summarized.data)
  return(output)
}
