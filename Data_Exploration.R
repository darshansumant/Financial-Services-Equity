# Darshan Sumant
# Jan 20, 2019
# Intro to ggplot2 Assignment
# Exploratory analysis of the CFPB HMDA data

# Install required packages
install.packages("readr")
install.packages("haven")
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("zoo")

# Load required packages
library("readr")
library("haven")
library("plyr")
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")
library("zoo")

# Load all data
del_county = read_csv("datasets/CountyMortgagesPercent-30-89DaysLate-thru-2018-06.csv")
del_state = read_csv("datasets/StateMortgagesPercent-30-89DaysLate-thru-2018-06.csv")
del_metro = read_csv("datasets/MetroAreaMortgagesPercent-30-89DaysLate-thru-2018-06.csv")

# Check loaded data
ls()
dim(del_county)
dim(del_state)
dim(del_metro)

del_county
del_state
del_metro

# Convert to long format
del_county_long <- gather(select(del_county, -c(RegionType, State, FIPSCode)),
                          key = Month,
                          value = delinquency_rate,
                          convert = TRUE,
                          -Name)

# Convert to long format
del_state_long <- gather(select(del_state, -c(RegionType, FIPSCode)),
                         key = Month,
                         value = delinquency_rate,
                         convert = TRUE,
                         -Name)

# Convert to long format
del_metro_long <- gather(select(del_metro, -c(Name, CBSACode)),
                         key = Month,
                         value = delinquency_rate,
                         convert = TRUE,
                         -RegionType) %>% 
  mutate(Month = as.Date(paste(Month,"-01",sep="")))
  #mutate(Month = as.yearmon(Month))


del_county_long
del_state_long
del_metro_long

# Group by Metro Area
del_by_metro <- del_metro_long %>% 
  group_by(RegionType, Month) %>%
  summarise(mean_del = mean(delinquency_rate),
            max_del = max(delinquency_rate),
            min_del = min(delinquency_rate),
            del_diff = max_del - min_del)

# Plot delinquency trends by Region Type
p1 <- ggplot(filter(del_by_metro, grepl("Metro", RegionType)), 
             aes(x=Month, 
                 y=mean_del 
                 ,color=RegionType
                 ,group=RegionType
                 )) + 
  geom_point() + 
  geom_line()

p1 + labs(colour = "Region Type", 
          y = "Mean Delinquency Rate", 
          title = "Delinquency rates have declined after an initial spike following the 2008 crisis",
          subtitle = "seasonality patterns in delinquency are clearly visible",
          caption = "(based on HMDA Mortgage Performance data by CFPB)",
          tag = "A") + 
  scale_x_date(date_labels = "%Y")

# Plot delinquency rates by month
ggplot(del_county_long, 
       aes(x=Month, 
           y=delinquency_rate)) + 
  geom_line(aes(color=Name), show.legend = FALSE)

# Plot delinquency rates by month
ggplot(del_state_long, 
       aes(x=Month, 
           y=delinquency_rate)) + 
  geom_line(show.legend = FALSE)

# Plot delinquency rates by month
ggplot(filter(del_metro_long, RegionType %in% "MetroArea"), 
       aes(x=Month, 
           y=delinquency_rate)) + 
  geom_point(aes(color=RegionType))
