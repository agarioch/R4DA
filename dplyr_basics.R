library(nycflights13)
flightsinstall.packages("tidyverse")
library(tidyverse)
install.packages("nycflights13")
flights
jan1 <- filter(flights, month == 1 , day == 1)
(dec25 <- filter(flights, month == 12, day == 25))
