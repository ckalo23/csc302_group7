require(maps)
require(mapdata)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(tidyverse)

#pull covid data from OWID
covid_data = read.csv("/Users/ckalo7/Documents/CSC 302/owid-covid-data.csv", header = T)

colnames(covid_data)

#add all entries and their columns for this date to new data set 'today_date'
today_data = covid_data[(covid_data$date == '2022-02-11'), ]

#drop NA values from 'population' and 'people_vaccinated' columns
today_data = today_data %>% drop_na(population)
today_data = today_data %>% drop_na(people_vaccinated)
today_data

#rename column for 'today_data' to match 'map_data'
colnames(today_data)[3] = "region"

#today_data[, c("people_vaccinated", "population")]

#new column for 'population vax rate' to show proportion of people vaccinated from each population
today_data$pop_vax_rate = (today_data$people_vaccinated/today_data$population)
summary(today_data$pop_vax_rate)

#change "USA" name so it matches the 'map_data' nomenclature before left_join func
today_data$region = ifelse(today_data$region == "United States", "USA", today_data$region)

#pull 'map_data' into df called 'map' and (left_join) with the corresponding 'covid_data' entries
map = map_data("world") ##ggplot2
map = left_join(map, today_data, by="region")

#ggplot to display the map scale, geom_polygon to shape/fill countries
#also fill_gradient to change color and na.value to grey NA value countries
gg1 <- ggplot(map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill = pop_vax_rate), color="black") + 
  scale_fill_gradient(name = "% Vaccinated", low = "yellow", high = "#006400", na.value = "grey50") +
  theme(axis.text.x = element_blank(), #these remove the axis, and their text, ticks, and titles
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("COVID-19 Vaccination Rate per Population (as of 2/11/2022)") #title for the map
gg1

#add new map column for 'death_rate' based on proportion of total deaths in population
map$death_rate = map$total_deaths / map$total_cases

center_coord = read_csv("/Users/ckalo7/Documents/CSC 302/world_lat_long_vals.csv")
colnames(new_map)[colnames(new_map) == "region"] <- "country"

center_coord$country[228] = "USA"

full_map = inner_join(new_map, center_coord, by="country")

#full_map %>% drop_na(continent)
#full_map = full_map[(full_map$country != "Antarctica"), ]


gg3 = gg1 + geom_point(data=full_map, aes(x=longitude, y=latitude, size=death_rate, position = "jitter"),
                       color="coral1") + scale_size(range = c(.1,5)) + scale_color_gradient(low = "#ff9999", high = "#660000") +
                      ggtitle("COVID-19 Vaccination Rate vs Death Rate (as of 2/11/2022)")
gg3