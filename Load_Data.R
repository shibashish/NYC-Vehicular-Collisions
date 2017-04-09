library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


nyc <- read.csv("NYPD_Motor_Vehicle_Cleaned.csv" , stringsAsFactors = FALSE)

# Remove X column:
nyc <- nyc %>%
  select(-X)


# Convert to date
nyc$DATE2 <- mdy(nyc$DATE)


# Separate Time and Date columns
nyc <- separate(nyc, TIME , c("Hour", "Min"), sep = ":")
nyc <- separate(nyc, DATE2 , c("YEAR", "MONTH" , "DAY"), sep = "-")


# Removing observations from 2012 and 2017
nyc <- nyc %>%
  filter(YEAR != '2012' , YEAR != '2017')