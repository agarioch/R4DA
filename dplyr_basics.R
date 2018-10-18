library(tidyverse)

# load NYC flights dataset from Bureau of transportation statistics
install.packages("nycflights13")
library(nycflights13)
flights

# ================ dplyr filter ================
jan1 <- filter(flights, month == 1 , day == 1)
(dec25 <- filter(flights, month == 12, day == 25))
(q1.1 <- filter(flights, arr_delay > 2))
filter(flights, dest == "HOU")
filter(flights, month %in% c(7, 8, 9))
filter(flights, dep_delay <= 0 & arr_delay > 2)
filter(flights, dep_time >= 0 & dep_time <= 600)
filter(flights, between(dep_time, 0, 600))
sum(is.na(flights$dep_time))

# ================ dplyr arrange ================ 
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

# ================ dplyr select ================
select(flights, contains("delay"))
fields <- c("origin", "dest")
select(flights, one_of(fields))
select(flights, contains("TIME"))

# ================ dplyr mutate ================
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time,
                      avg_mph)

# check average speed, minutes gained vs. scheduled time and gain per hour of flight time
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

# ================ dplyr transmute================
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

# ================ dplyr summarize ================
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))

# delays by date
by_day <- group_by(flights, date = ISOdate(year, month, day))
(delay_by_day <- summarise(by_day, delay = mean(dep_delay, na.rm = TRUE)))
ggplot(data = delay_by_day, mapping = aes(x = date, y = delay)) + geom_line() + geom_smooth()

# delays by distance/destination
by_dest <- group_by(flights, dest) %>%
  summarize(count = n(),
            dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(count > 20, dest != "HNL")
ggplot(by_dest, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 0.3) +
  geom_smooth(se = FALSE)
# Colombia Met Airport with longest average delay 41.8 hours across 116 flights in the dataset
filter(by_dest, delay > 40)
# 5 destination airports with delay less than 0hrs
filter(by_dest, delay <0) %>%
  summarize(n())

# na delay represents cancelled flights
not_cancelled <- flights %>%
  filter(!is.na(arr_delay), !is.na(dep_delay))

sum(is.na(flights$arr_delay) | is.na(flights$dep_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay),
            n = n())

ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 0.3)

delays %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = delay)) +
    geom_point(alpha = 0.3)

not_cancelled %>%
  group_by(dest) %>%
  summarize(n = n())

not_cancelled %>% count(dest)

not_cancelled %>% count(tailnum, wt = distance)

not_cancelled %>%
  group_by(tailnum) %>%
  summarize(sum(distance))

# Look at the average number of cancelled flights per day, is there a pattern?
cancelled <- flights %>%
  filter(is.na(dep_delay))

cancelled %>%
  group_by(date = ISOdate(year, month, day)) %>%
  summarise(n = n()) %>%
  ggplot(aes(y = n, x = date)) + geom_line(color = "#005eb8")

flights
ggplot(flights, mapping= aes(x=carrier)) + geom_bar()
mean(flights$distance)
flights %>%
  filter(distance > 2000) %>%
  group_by(carrier) %>%
  summarize(avg_dist = mean(distance)) %>%
  ggplot(mapping = aes(y = avg_dist, x = carrier)) + geom_bar(stat = "identity", color = "purple")
