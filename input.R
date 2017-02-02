lapply(c("dplyr", "tidyr"), require, character.only = TRUE)

readReport <- function(path) {
  report <- read.csv(path)[,c("Date", "Time.Spent..seconds.", "Productivity")]
  report <- dplyr::rename(report, week = Date, time = Time.Spent..seconds., productivity_level = Productivity)
  report <- aggregateTimeAsPercentage(report)
  return( widenReport(report) )
}

aggregateTimeAsPercentage <- function(report) {
  report <- report %>% arrange( week, productivity_level )
  report <- as.data.frame( report %>% group_by(week) %>% mutate(time_percentage = time / sum(time)) )
  report$time <- NULL
  return( report )
}

widenReport <- function(longReport) {
  report.wide <- tidyr::spread(data = longReport, key = productivity_level, value = time_percentage)
  report.wide <- rename(report.wide, very_distracting = `-2`, distracting = `-1`, neutral = `0`, productive = `1`, very_productive = `2`)
  return( report.wide )
}
