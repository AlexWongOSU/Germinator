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

  new.df<- merge(data, Lookup, by = "Slice")

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
