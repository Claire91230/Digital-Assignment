install.packages("tidyverse")
library(tidyverse)

#upload the data
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")
world_countries_list <- read_csv("WorldCountriesList.csv")

#join the data
data_join <- world_countries_list %>%
  full_join(unicef_indicator_1, by = join_by(country))%>%
  full_join(unicef_indicator_2, by = join_by(country, year)) %>%
  full_join(unicef_metadata, by = c("country"="country", "year"="time_period"))
  

map_world <- map_data("world")

# Filter the unicef_indicator_2 data (2017-2021)
filtered_data <- unicef_indicator_2 %>%
  filter(year >= 2017 & year <= 2021)

# Left join the filtered data with the map data
map_data_join <- left_join(map_world, filtered_data, by = c("region" = "country"))

# Make the map
ggplot(data = map_data_join) +
  aes(x = long, y = lat, group = group, fill = AdolescentBirthRate) +
  geom_polygon()+
  labs(title = "World map of the births per 1 000 adolescent women from 2017 to 2021" )

#Create a bar chart
filtered_data_2000to2021 <- data_join %>%
  filter(year >= 2000 & year <= 2021)

filtered_data_2000to2021 %>%
  group_by(year) %>%
  summarize(total_AverageAdoBirth = sum(AdolescentBirthRate, na.rm = TRUE)) %>%
  ggplot()+
  aes(year, total_AverageAdoBirth, fill=year)+
  geom_col()+
  labs(x = "Year", y = "Average Adolescent Birth Rate", title = "Adolescent women giving birth over the years (2000 to 2021)")

#Create 2nd bar chart
filtered_data_2020 <- data_join %>%
  filter(year == 2020)

filtered_data_2020 %>%
  group_by(continent) %>%
  summarize(m_AverageAdoBirth = mean(AdolescentBirthRate, na.rm = TRUE)) %>%
  ggplot()+
  aes(continent, m_AverageAdoBirth, fill=continent)+
  geom_col()+
  labs(x = "Continent", y = "Average Adolescent Birth Rate", title = "Adolescent women giving birth depending of the continent")

#Create a scatter plot
filtered_data_2017 <- data_join %>%
  filter(year == 2017)

ggplot(filtered_data_2017) +
  aes(LifeExp, AdolescentBirthRate, size = GDPperCapita, color = continent) +
  geom_point(alpha = 0.7)+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Life Expectancy", y = "Adolescent Birth Rate", title = "Adolescent women giving birth & life expectancy at birth with the GDP of the concerned countries in 2017", size = "GDP per Capita", fill = "Continent")

#Create a time serie chart
filtered_data_2000to2021 <- data_join %>%
  filter(year >= 2000 & year <= 2021)

filtered_data_2000to2021 %>%
  group_by(year) %>%
  summarize(m_AverageAdoBirth = mean(AdolescentBirthRate, na.rm = TRUE)) %>%
  ggplot()+
  aes(year, m_AverageAdoBirth, fill=year)+
  geom_line()+
  labs(x = "Year", y = "Average Adolescent Birth Rate", title = "Adolescent women giving birth over the years (2000 to 2021)")

#mix the bar
# Filter data for years 2000 to 2021
filtered_data_2000to2021 <- data_join %>%
  filter(year >= 2000 & year <= 2021)

# Calculate total and average adolescent birth rates
total_summary <- filtered_data_2000to2021 %>%
  group_by(year) %>%
  summarize(total_AdoBirth = sum(AdolescentBirthRate, na.rm = TRUE))

average_summary <- filtered_data_2000to2021 %>%
  group_by(year) %>%
  summarize(m_AverageAdoBirth = mean(AdolescentBirthRate, na.rm = TRUE))

