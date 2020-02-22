#'
#' Determines an estimate for energy savings due to
#' the installation of a tankless water heater
#'

### INITIALIZATION --------------------------------------------------------------------------------
.x <- c("data.table", "dplyr", "lubridate", "readr", "ggplot2", "broom")
lapply(.x, library, character.only=T)

setwd("~/1) Projects/HouseConsumption/")

### DATA READ & CLEAN -----------------------------------------------------------------------------

# Dropping personally identifying information before github upload
consum <- read.csv("./1000293732202460849-18408.csv", stringsAsFactors = F) %>%
  select(-Name, -Address, -Account, -CUSTOMER.CODE, 
         -PREMISE.CODE, -MeterID)
fwrite(consum, "./1000293732202460849-18408.csv")

demand <- read_csv("./1000293732202460849-18408_Demand.csv") %>%
  select(-Name, -Address, -`Account #`, -MeterID, -`Premise ID`)
fwrite(demand, "./1000293732202460849-18408_Demand.csv")

weather <- fread("./724880-2020.csv") %>%
  mutate(timestamp = ymd_hms(date_local)) %>%
  select(timestamp, temp) %>%
  mutate(HDD = ifelse(temp < 65, (65-temp)/24, 0))

### MAIN ANALYSIS ---------------------------------------------------------------------------------

maindat <- consum %>%
  mutate(timestamp = ymd_hms(gsub("T|Z", " ", startTime))) %>%
  mutate(timestamp = floor_date(timestamp, unit="hour")) %>%
  group_by(timestamp) %>%
  summarise(kwh = mean(value)) %>%
  filter(year(timestamp)==2020) %>%
  merge(weather, by='timestamp')

plot(ggplot(maindat, mapping=aes(x=timestamp,y=kwh)) + 
       geom_point() + geom_line() + 
       geom_vline(xintercept = as.numeric(ymd_hms("2020-01-22 00:00:00"))) + # Time when hot water heater cut out
       geom_vline(xintercept = as.numeric(ymd_hms("2020-01-26 00:00:00")))) # Time when new heater was installed

# Pull same amount of days pre and post with same day of week mix
prePeriod <- maindat %>%
  filter(timestamp < ymd("2020-01-16")) %>% # Was out of town 16th - 20th
  mutate(post=0)
postPeriod <- maindat %>% 
  filter(timestamp >= ymd("2020-01-27")) %>% # post period
  mutate(post=1)

panel <- bind_rows(prePeriod,postPeriod)

reg <- lm(kwh ~ post*HDD, data=panel)

tidy(reg)
glance(reg)

# Savings
tidy(reg)$estimate[4]*mean(postPeriod$HDD) + tidy(reg)$estimate[2]

data.table(HourlyEnergySavings=-tidy(reg)$estimate[4]*mean(postPeriod$HDD) + tidy(reg)$estimate[2],
           DailyEnergySavings=-24*(tidy(reg)$estimate[4]*mean(postPeriod$HDD) + tidy(reg)$estimate[2]),
           AnnualEnergySavings=-24*365*(tidy(reg)$estimate[4]*mean(postPeriod$HDD) + tidy(reg)$estimate[2]),
           AnnualMoneySavings=-.11*24*365*(tidy(reg)$estimate[4]*mean(postPeriod$HDD) + tidy(reg)$estimate[2]))
















