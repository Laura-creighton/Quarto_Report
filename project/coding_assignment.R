install.packages("ggplot2")
install.packages("tidyverse")
install.packages("plotly")
install.packages("stringr")

library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(stringr)

unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

readr::spec(unicef_metadata)
readr::spec(unicef_indicator_2)

merged_data <- merge(unicef_metadata, unicef_indicator_2, by = "country", all = TRUE)

cleaned_data <- na.omit(merged_data)

# map 1

world_map <- map_data("world")

map_data_join <- full_join(unicef_indicator_2, world_map, by = c("country" = "region"))

ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "green", high = "red", na.value = "grey") +
  labs(
    title = "Global Distribution of Malnourishment",
    subtitle = "% of Children >5yrs Malnourished") +
  theme_bw()

# table 1

df1 <- data.frame(
  country = c(
    "Belgium", "Netherlands", "Poland", "Estonia", "South Korea",
    "Portugal", "Czech Republic", "Tuvalu", "Saint Lucia", "Tonga"
  ),
  observed_value = c(5.5, 5.6, 6.5, 7.7, 7.9, 10.8, 11.3, 11.4, 11.6, 13.8) 
)


# table 2


df2 <- data.frame(
  country = c(
    "Papua New Guinea", "Yemen", "Timor-Leste", "India", "Burundi",
    "Guatemala", "Madagascar", "Eritrea", "Equatorial Guinea", "Angola"
  ),
  observed_value = c(66.2, 63.8, 62.6, 59.1, 59.0, 57.6, 57.4, 56.0, 55.2, 53.5)
)

# bar chart 1

merged_data <- merged_data %>%
  rename(gdp_per_cap = `GDP per capita (constant 2015 US$)`)

average_gdp_per_cap <- merged_data %>%
  group_by(country) %>%
  summarize(average_gdp_per_cap = mean(gdp_per_cap, na.rm = TRUE))

f_combined <- df1 %>%
  group_by_all() %>%
  summarize_all(~ paste(unique(.), collapse = ", "))

b_combined <- df2 %>%
  group_by_all() %>%
  summarize_all(~ paste(unique(.), collapse = ", "))

merged_df1 <- inner_join(df1, average_gdp_per_cap, by = "country")
merged_df2 <- inner_join(df2, average_gdp_per_cap, by = "country")

combined_df <- rbind(merged_df1, merged_df2)

combined_df$country <- factor(combined_df$country, levels = rev(unique(combined_df$country)))

combined_df <- combined_df %>%
  mutate(color = ifelse(country %in% c("Angola", "Burundi", "Equatorial Guinea", "Eritrea", "Guatemala", "India", "Madagascar", "Papua New Guinea", "Timor-Leste", "Yemen"), "red", "green"))

# Create the bar chart with different colors for specific countries
ggplot(combined_df, aes(x = average_gdp_per_cap, y = country, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  labs(title = "Average GDP per Capita by Country",
       x = "Average GDP per Capita",
       y = "Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# scatter plot 1

country_averages <- unicef_indicator_2 %>%
  group_by(country) %>%
  summarize(average_obs_value = mean(obs_value, na.rm = TRUE))

average_obs_per_country <- merged_data %>%
  group_by(country) %>%
  summarize(average_obs_value = mean(obs_value, na.rm = TRUE))

scatter_data <- inner_join(country_averages, average_gdp_per_cap, by = "country")

average_obs_per_country <- merged_data %>%
  filter(!is.na(obs_value)) %>%  # Filter out rows with NA obs_value
  group_by(country) %>%
  summarize(average_obs_value = mean(obs_value))

filtered_average_obs_per_country <- average_obs_per_country[complete.cases(average_obs_per_country), ]

gdp_vs_obs_data <- left_join(filtered_average_obs_per_country, average_gdp_per_cap, by = "country")

ggplot(gdp_vs_obs_data) +
  aes(average_obs_value, average_gdp_per_cap) +
  geom_point() +
  guides(color = "none") +
  geom_smooth(method = "lm") + 
  labs(x = "Observed Value", y = "GDP Per Capita", title = "Scatter Plot with Linear Regression") +
  theme_minimal()

# time series chart

merged_data$life_exp <- merged_data$`Life expectancy at birth, total (years)`

timeseries_plot_1 <- merged_data %>%
  ggplot() +
  aes(year, life_exp, color = country) + 
  geom_line()

ggplotly(timeseries_plot_1)


