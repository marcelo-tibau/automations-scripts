# setwd("~/Documents/FLAGCX/Projects/mkay/SentimentFacebook")
# Preprocessing & summarizing data
library(dplyr)
library(tidyr)

# Visualizatin development
library(ggplot2)

riotemp <- read.table("BZRIODJN.txt", header = T, stringsAsFactors = T)

# rename variables
names(riotemp) <- c("Month", "Day", "Year", "Temp")

# create dataframe that represents 1995-2018 historical data
Past <- riotemp %>%
  group_by(Year, Month) %>%
  arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>% # x-axis - label days of the year (1:365)
  ungroup() %>%
  filter(Temp != -99 & Year != 2018) %>% # filter out missing data (identified with '-99' value) & historical data
  group_by(newDay) %>%
  mutate(upper = max(Temp), # max value for each day
         lower = min(Temp), # min value for eache day
         avg = mean(Temp), # mean value for each day
         se = sd(Temp)/sqrt(length(Temp))) %>% # standard error of mean
  mutate(avg_upper = avg + (2.101*se), # calculate 95% CI for mean
         avg_lower = avg - (2.101*se)) %>% # calculate 95% CI for mean
  ungroup()

# create dataframe that represents current year data
Present <- riotemp %>%
  group_by(Year, Month) %>%
  arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>% # create matching x-axis as historical data
  ungroup() %>%
  filter(Temp != -99 & Year == 2018) # filter out missing data & select current year data
  
# create dataframe that represents the lowest temp for each day for the historical data
PastLows <- Past %>%
  group_by(newDay) %>%
  summarise(Pastlow = min(Temp)) # identify lowest temp for each day from 1995-2017

# create dataframe that identifies the days in 2018 in which the temps were lower than all previous years
PresentLows <- Present %>%
  left_join(PastLows) %>% # merge historical lows to current year low data
  mutate(record = ifelse(Temp < Pastlow, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y") # filter for days that represent current year record lows

# create dataframe that represents the highest temp for each day for the historical data
PastHighs <- Past %>%
  group_by(newDay) %>%
  summarise(Pasthigh = max(Temp))

# create dataframe that identifies the days in 2018 in which the temps were higher than all previous years
PresentHighs <- Present %>%
  left_join(PastHighs) %>% # merge historical highs to current year low data
  mutate(record = ifelse(Temp > Pasthigh, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y") # filter for days that represent current year record highs

# function to turn y-axis labels into degree formatted values
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# create y-axis variable
#a <- dgr_fmt(seq(-20, 100, by = 10))
a <- dgr_fmt(seq(40, 100, by = 10))

# create a small dataframe to represent legend symbol for 2018 Temperature
legend_data <- data.frame(x = seq(175, 182), y = rnorm(8, 15, 2))

# Chart - step 1 plot the range of average daily temperatures
p <- ggplot(Past,aes(newDay, Temp)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        # axis.text = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(Past, mapping = aes(x = newDay, ymin = lower, ymax = upper), color = "wheat2", alpha = .1)

print(p)

# Chart - step 2 add the 95% confidence interval around the daily mean temperatures 
p <- p + 
  geom_linerange(Past, mapping = aes(x = newDay, ymin = avg_lower, ymax = avg_upper), color = "wheat4")

print(p)

# Chart - step 3 incorporate current year temperature data
p <- p +
  geom_line(Present, mapping = aes(x = newDay, y = Temp, group = 1)) +
  geom_vline(xintercept = 0, color = "wheat4", linetype = 1, size = 1)

print(p)


# Chart - step 4 current year temperature data
p <- p +
  #geom_hline(yintercept = -20, colour = "white", linetype = 1) +
  #geom_hline(yintercept = -10, colour = "white", linetype = 1) +
  #geom_hline(yintercept = 0, colour = "white", linetype = 1) +
  #geom_hline(yintercept = 10, colour = "white", linetype = 1) +
  #geom_hline(yintercept = 20, colour = "white", linetype = 1) +
  #geom_hline(yintercept = 30, colour = "white", linetype = 1) +
  geom_hline(yintercept = 40, colour = "white", linetype = 1) +
  geom_hline(yintercept = 50, colour = "white", linetype = 1) +
  geom_hline(yintercept = 60, colour = "white", linetype = 1) +
  geom_hline(yintercept = 70, colour = "white", linetype = 1) +
  geom_hline(yintercept = 80, colour = "white", linetype = 1) +
  geom_hline(yintercept = 90, colour = "white", linetype = 1) +
  geom_hline(yintercept = 100, colour = "white", linetype = 1)

print(p)

# Chart - step 5 add the dotted gridlines to the last day of each month.
p <- p + 
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 

print(p)

# Chart - step 6 dress up the axis labels
p <- p +
  coord_cartesian(ylim = c(40, 100)) +
  scale_y_continuous(breaks = seq(40, 100, by = 10), labels = a) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))

print(p)

# Chart - step 7 identify the days in which the current year had the record high and low temperature
p <- p +
  geom_point(data = PresentLows, aes(x = newDay, y = Temp), colour = "blue3") +
  geom_point(data = PresentHighs, aes(x = newDay, y = Temp), colour = "firebrick3")

print(p)

# Chart - step 8 dress up the graphic with the appropriate text
p <- p +
  ggtitle("Rio de Janeiro's Weather in 2018") +
  theme(plot.title = element_text(face = "bold", hjust = .012, vjust = .8, colour = "#3C3C3C", size = 20)) +
  annotate("text", x = 19, y = 98, label = "Temperature", size = 4, fontface = "bold")

# Chart - step 9 provides a little explanation about the data
p <- p +
  annotate("text", x = 132, y = 96, 
           label = "Data represents average daily temperatures. Accessible data dates back to January 1, 1995. Data for 2018 is only available through March 20.", size=3, colour="gray30") +
  annotate("text", x = 132, y = 94, 
           label = "We had 34 days as the hottest since 1995 while had 1 day as the coldest.", size=3, colour="gray30")


# Chart - step 10 annotations to explain highs and lows
# p <- p +
#  annotate("segment", x = 30, xend = 40, y = -5, yend = -10, colour = "blue3") +
#  annotate("text", x = 65, y = -10, label = "We had 35 days that were the", size=3, colour="blue3") +
#  annotate("text", x = 56, y = -14, label = "coldest since 1995", size=3, colour="blue3") +
#  annotate("segment", x = 302, xend = 307, y = 74, yend = 82, colour = "firebrick3") +
#  annotate("text", x = 333, y = 82, label = "We had 19 days that were the", size=3, colour="firebrick3") +
#  annotate("text", x = 324, y = 78, label = "hottest since 1995", size=3, colour="firebrick3")

# Chart - step 11 add a legend to explain the difference between the different data point layers
p <- p +
  annotate("segment", x = 181, xend = 181, y = 45, yend = 65, colour = "wheat2", size = 3) +
  annotate("segment", x = 181, xend = 181, y = 57.7, yend = 52.2, colour = "wheat4", size = 3) +
  geom_line(data = legend_data, aes(x = x, y = y)) +
  annotate("segment", x = 183, xend = 185, y = 57.7, yend = 57.7, colour = "wheat4", size = .5) +
  annotate("segment", x = 183, xend = 185, y = 52.2, yend = 52.2, colour = "wheat4", size = .5) +
  annotate("segment", x = 185, xend = 185, y = 52.2, yend = 57.7, colour = "wheat4", size = .5) +
  annotate("text", x = 196, y = 54.75, label = "NORMAL RANGE", size = 2, colour = "gray30") +
  annotate("text", x = 162, y = 54.75, label = "2018 TEMPERATURE", size=2, colour="gray30") +
  annotate("text", x = 193, y = 65, label = "RECORD HIGH", size=2, colour="gray30") +
  annotate("text", x = 193, y = 45, label = "RECORD LOW", size=2, colour="gray30")

print(p)

