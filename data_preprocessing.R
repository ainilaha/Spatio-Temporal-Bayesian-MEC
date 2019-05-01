library(dplyr)
library(lubridate)


data <- read.csv("data/clean.csv")

# format time
data <- data %>% mutate(start_time = ymd_hms(start_time)) %>%
    mutate(end_time = ymd_hms(end_time))

# compute connected time
data["con_time"] <- as.numeric(difftime(ymd_hms(data$end_time),
                                        ymd_hms(data$start_time),units = "secs"))






data <- data %>% filter(long>=120.8544 & 
                          long<=121.974 & 
                          lat >=30.68889 & 
                          lat<=31.86765)


data <- data %>% mutate(rnd_strat = minute(round_date(start_time, "minute"))) %>%
      mutate(rnd_end = minute(round_date(end_time, "minute")))

sum_time <- data %>% group_by(long,lat,rnd_strat) %>% summarise(sum_time = sum(con_time))
num_conn <- data %>% group_by(long,lat) %>% summarise(num_con = n())