#' Process and summarize data from "HyphaTracker" using the design from
#' Plate.Design
#'
#' @import dplyr
#' @importFrom  stats sd
#' @importFrom stats na.omit
#'
#' @param data Dataframe from "HyphaTracker" output.
#' @param Lookup Dataframe lookup table from the `Plate.Design` output
#' @param badimage logical, if their are images to be removed from analysis
#'
#' @return An object of class 'Germinator'
#' @export
#'


Growth<- function(data = NULL,
                  Lookup = NULL,
                  badimage = c("")){
  data$Fungicide<- Lookup[match(data$Slice, Lookup$Slice), c('Fungicide')]
  data$Conc<- Lookup[match(data$Slice, Lookup$Slice), c('Conc')]
  data$Block<- Lookup[match(data$Slice, Lookup$Slice), c('Block')]
  data$TimePt<- Lookup[match(data$Slice, Lookup$Slice), c('Timepoint')]
  data$Isolate<- Lookup[match(data$Slice, Lookup$Slice), c('Isolate')]
  data$Well<- Lookup[match(data$Slice, Lookup$Slice), c('Well')]
  data$Row<- Lookup[match(data$Slice, Lookup$Slice), c("Row")]
  data$Column<- Lookup[match(data$Slice, Lookup$Slice), c("Column")]

  new.df<- data

  filtered.data<- new.df%>%
    dplyr::filter(Slice != badimage)

  summarized.data<- filtered.data%>%
    group_by(Isolate, Fungicide, Conc, Block, TimePt)%>%
    summarise(meanArea = mean(Area))%>%
    mutate(difference = meanArea - dplyr::lag(meanArea))%>%
    mutate(pctdiff = ((meanArea-dplyr::lag(meanArea))*100) / dplyr::lag(meanArea))%>%
    mutate(growth = ifelse(difference<0, 0.001, difference))%>%
    dplyr::filter(TimePt != 0)%>%
    na.omit()%>%
    group_by(Isolate, Fungicide, Conc)%>%
    mutate(n = length(growth))%>%
    mutate(seGrowth = sd(growth)/sqrt(n))%>%
    mutate(meangrowth = mean(growth))%>%
    group_by(Isolate)%>%
    mutate(rel.growth = growth/meangrowth[Conc == 0])%>%
    group_by(Isolate, Fungicide, Conc)%>%
    mutate(rel.growth.se = sd(rel.growth)/sqrt(n))

  summarized.data<- as.data.frame(summarized.data)
  output<- list("new.data.df" = new.df, "GrowthData" = summarized.data)
  return(output)
}
