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
install.packages("lubridate")
install.packages("directlabels")

# Load required packages
library("readr")
library("haven")
library("plyr")
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")
library("zoo")
library("lubridate")
library("directlabels")

# Load all data
del_county = read_csv("datasets/CountyMortgagesPercent-30-89DaysLate-thru-2018-06.csv")
del_state = read_csv("datasets/StateMortgagesPercent-30-89DaysLate-thru-2018-06.csv")
del_metro = read_csv("datasets/MetroAreaMortgagesPercent-30-89DaysLate-thru-2018-06.csv")
npa_metro = read_csv("datasets/MetroAreaMortgagesPercent-90-plusDaysLate-thru-2018-06.csv")

# Check loaded data
ls()
dim(del_county)
dim(del_state)
dim(del_metro)
dim(npa_metro)

del_county
del_state
del_metro
npa_metro

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
  mutate(Month = as.Date(paste(Month,"-01",sep="")),
         Mth = format(Month, "%b"))


# Convert to long format
npa_metro_long <- gather(select(npa_metro, -c(Name, CBSACode)),
                         key = Month,
                         value = default_rate,
                         convert = TRUE,
                         -RegionType) %>% 
  mutate(Month = as.Date(paste(Month,"-01",sep="")),
         Mth = format(Month, "%b"))

# Check datasets
del_county_long
del_state_long
del_metro_long
npa_metro_long

# Group by Metro Area
del_by_metro <- del_metro_long %>% 
  group_by(RegionType, Month) %>%
  summarise(mean_del = mean(delinquency_rate),
            max_del = max(delinquency_rate),
            min_del = min(delinquency_rate),
            del_diff = max_del - min_del)


# Group by Metro Area
npa_by_metro <- npa_metro_long %>% 
  group_by(RegionType, Month) %>%
  summarise(mean_npa = mean(default_rate),
            max_npa = max(default_rate),
            min_npa = min(default_rate),
            npa_diff = max_npa - min_npa)


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
          x = "Year",
          y = "Mean Delinquency Rate [% overdue by 30-89 days]", 
          title = "Delinquency rates have declined after an initial spike following the 2008 crisis",
          subtitle = "seasonality patterns in delinquency are clearly visible",
          caption = "(based on HMDA Mortgage Performance data by CFPB)",
          tag = "A") + 
  scale_x_date(date_labels = "%Y")


# Plot disparity in delinquency (max - min) across states
p2 <- ggplot(filter(del_by_metro, grepl("Metro", RegionType)),
             aes(x=format(Month, "%m"),
                 y=del_diff/100,
                 fill=RegionType)) + 
  geom_boxplot()

p2 + labs(colour = "Region Type", 
          x = "Month",
          y = "Disparity in delinquency rate (max - min) across States", 
          title = "Delinquency disparity across states is largely due to Metro Areas",
          subtitle = "there is a subtle seasonal pattern even in delinquency disparity",
          caption = "(based on HMDA Mortgage Performance data by CFPB)",
          tag = "B") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete("Month")


# Check common field names
colnames(del_by_metro)[colnames(del_by_metro) %in% colnames(npa_by_metro)]

# Merge Delinquency rates and Default (NPA) rates by RegionType & Month
tot_by_metro <- merge(del_by_metro, npa_by_metro, by=c("RegionType", "Month"))
tot_by_metro

# Regress mean delinquency & default rates by Month & Region Type
tot_by_metro$pred.mean_del <- predict(lm(mean_del ~ format(Month, "%m") + RegionType, 
                                         data = tot_by_metro))
tot_by_metro$pred.mean_npa <- predict(lm(mean_npa ~ format(Month, "%m") + RegionType, 
                                         data = tot_by_metro))

# Compare trends in delinquency and default (NPA) 
p3 <- ggplot(filter(tot_by_metro, grepl("Metro", RegionType)),
             aes(x=Month),
             group=RegionType) + 
  geom_line(aes(y=mean_del, colour=mean_del)) + 
  geom_col(aes(y=mean_npa/3, colour=mean_npa)) + 
  geom_dl(aes(y=mean_del, label = "delinquency"), 
          method = list(dl.trans(x = x + 1.8, y = y - 1.2), "first.points")) +
  geom_dl(aes(y=mean_npa/3, label = "default"), 
          method = list(dl.trans(x = x - .8, y = y - .2), "last.points")) +
  geom_smooth(aes(y=mean_del, group=RegionType)) + 
  geom_smooth(aes(y=mean_npa/3, group=RegionType))

p3 + scale_y_continuous(labels = scales::comma,
                        sec.axis = sec_axis(~.*3, name = "Mean Default (NPA) rate [% overdue by 90+ days]")) + 
  scale_color_gradientn(colours = terrain.colors(10)) + 
  labs(colour = "Intensity of distress", 
       x = "Year",
       y = "Mean Delinquency rate [% overdue by 30-89 days]", 
       title = "Metro and Non-Metro areas differ in Delinquency rates, but not default rates",
       subtitle = "this indicates disparity in Collection efforts across Metro & Non-Metro areas",
       caption = "(based on HMDA Mortgage Performance data by CFPB)",
       tag = "C") +
  scale_x_date(date_breaks = "1 year", 
               date_minor_breaks = "1 month",
               date_labels = "%Y") + 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))
