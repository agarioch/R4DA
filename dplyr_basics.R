install.packages("tidyverse")
library(tidyverse)
# load NYC flights dataset from Bureau of transportation statistics
install.packages("nycflights13")
library(nycflights13)
flights
# dplyr filter
jan1 <- filter(flights, month == 1 , day == 1)
(dec25 <- filter(flights, month == 12, day == 25))
(q1.1 <- filter(flights, arr_delay > 2))
filter(flights, dest == "HOU")
filter(flights, month %in% c(7, 8, 9))
filter(flights, dep_delay <= 0 & arr_delay > 2)
filter(flights, dep_time >= 0 & dep_time <= 600)
filter(flights, between(dep_time, 0, 600))
sum(is.na(flights$dep_time))
# dplyr arrange
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))
arrange(flights, desc(is.na(arr_delay)))
arrange(flights, desc(arr_delay))
# Testing basic plots
ggplot(data = flights) + geom_bar(mapping= aes(x= origin))
ggplot(data = flights) + geom_smooth(mapping= aes(x= sched_dep_time, y = arr_delay, color=origin))
# Fastest flight
flights$avg_mph <- flights$distance / (flights$air_time / 60)
select(arrange(flights, desc(avg_mph)), year, month, carrier, flight, origin, dest, distance, air_time, avg_mph)
# dplyr select
select(flights, contains("delay"))
fields <- c("origin", "dest")
select(flights, one_of(fields))
select(flights, contains("TIME"))

# dplyr mutate
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time,
                      avg_mph)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       hours = air_time / 60,
       gain_per_hour = gain / hours)
# dplyr transmute
transmute(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       hours = air_time / 60,
       gain_per_hour = gain / hours)
transmute(flights,
          dep_time,
          hours = dep_time %/% 100,
          minutes = dep_time %% 100)
# offsets
(x <- 1:10) 
lag(x)
lead(x)
cumsum(x)
cummean(x)
select(flights, dep_time, sched_dep_time)
mutate(flights,
       dep_time_mins = dep_time %/% 100 * 60 + dep_time %% 100,
       arr_time_mins = arr_time %/% 100 * 60 + arr_time %% 100,
       sched_dep_time_mins = sched_dep_time %/% 100 * 60 + sched_dep_time %% 100)

attach(flights)
flights$dep_time_mins <- dep_time %/% 100 * 60 + dep_time %% 100
flights$arr_time_mins <- arr_time %/% 100 * 60 + arr_time %% 100
flights$sched_dep_time_mins <- sched_dep_time %/% 100 * 60 + sched_dep_time %% 100
flights$sched_arr_time_mins <- sched_arr_time %/% 100 * 60 + sched_arr_time %% 100
detach(flights)

flights

# arr time in local timezone, thus arr time - dep time != air time
transmute(flights,
          dest,
          origin,
          arr_time,
          arr_time_mins,
          dep_time,
          dep_time_mins,
          air_time,
          arr_delay,
          dep_delay,
          diff = arr_time_mins - dep_time_mins,
          recon = air_time - diff)

min_to_hrs <- function(mins) {
  paste(mins %/%60, "h", mins%%60)
}

# arr - dep time doesn't take into account flights that arrive next day due to delay
arrange(transmute(flights,
          dep_time,
          sched_dep_time,
          dep_time_mins,
          sched_dep_time_mins,
          dep_delay_calc = dep_time_mins - sched_dep_time_mins,
          dep_delay,
          var = dep_delay - dep_delay_calc), desc(var))

# 10 most delayed flights
flights %>%
  filter(min_rank(-(dep_delay)) %in% 1:10)

flights %>%
  top_n(10, dep_delay)

