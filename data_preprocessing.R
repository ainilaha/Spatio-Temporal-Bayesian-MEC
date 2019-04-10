library(dplyr)
library(lubridate)


data <- read.csv("data/clean.csv")

# format time
data <- data %>% mutate(start_time = ymd_hms(start_time)) %>%
    mutate(end_time = ymd_hms(end_time))

# compute connected time
data["con_time"] <- as.numeric(difftime(ymd_hms(data$end_time),
                                        ymd_hms(data$start_time),units = "secs"))

