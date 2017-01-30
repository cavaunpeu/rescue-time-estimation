library(tidyr)

readReport <- function(path) {
  report <- read.csv(path)[,c("Date", "Time.Spent..seconds.", "Productivity")]
  report <- rename(report, week = Date, time = Time.Spent..seconds., productivity_level = Productivity)
  report <- aggregateTimeAsPercentage(report)
  report.wide <- widenReport(report)
}

aggregateTimeAsPercentage <- function(report) {
  report <- report %>% arrange( week, productivity_level )
  report <- as.data.frame( report %>% group_by(week) %>% mutate(time_percentage = time / sum(time)) )
  report$time <- NULL
  return(report)
}

widenReport <- function(longReport) {
  report.wide <- spread(data = longReport, key = productivity_level, value = time_percentage)
  rename(report.wide, very_low = `-2`, low = `-1`, neutral = `0`, high = `1`, very_high = `2`)
}
