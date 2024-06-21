# set working directory
setwd("/Volumes/6TB/R")
# source gex functions
source("gex_fun.R")
Sys.setenv(TZ="America/New_York")
# assign start/end times
start_time = as.POSIXct(paste0(Sys.Date(), " 09:30:00"), tz = "America/New_York")
end_time   = as.POSIXct(paste0(Sys.Date(), " 16:15:00"), tz = "America/New_York")
# create a sequence of timestamps 1-min apart
TIMES <- seq(from = start_time, to = end_time, by = "1 min")
# in case we need to re-run it, skip over old time Stamps
TIMES <- TIMES[TIMES > Sys.time()]
# pause for the opening bar
pause4NextBar(bar_time = TIMES[1],silent = F)
# for each timestamp collect GEX data
gex_loop = lapply(as.list(2:length(TIMES)), function(i){
  # calculate gex
  all_gamma <- try(get_gex(und_sym = "SPY", exp_date = "2024-04-15"),silent = T)
  # pause for next bar
  pause4NextBar(bar_time = TIMES[i],silent = F)
  # returns all_gamma 
  all_gamma
})