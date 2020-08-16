library(httr)
library(tidyverse)
library(readxl)
library(janitor)
library(ggforce)

GET("https://query.data.world/s/o4fbf2nfqhgeljzcktccdkfb3njjcw", write_disk(tf <- tempfile(fileext = ".xlsx")))

music_sales <- read_excel(tf) %>% 
  clean_names()

music_sales %>% 
  count(x_format) %>% 
  View()

music_sales %>% 
  count(metric) %>% 
  View()

music_sales %>% 
  group_by(year) %>% 
  summarise(tot = sum(value_actual, na.rm = TRUE)) %>% 
  View()

cassette_units <- music_sales %>% 
  filter(str_detect(x_format, "Cassette"), metric == "Units")

cassette_units_year <- cassette_units %>% 
  group_by(year) %>% 
  summarise(units = sum(value_actual, na.rm = TRUE)) %>% 
  mutate(side = if_else(year <= 1990, "Side A", "Side B")) %>% 
  group_by(side) %>% 
  mutate(id = row_number())

ggplot(cassette_units_year, aes(x = id, y = units, colour = units)) +
  geom_link(aes(x = id, xend = id, y = 0, yend = units),
            size = 2,
            lineend = 'butt',
            show.legend = FALSE) +
  scale_y_continuous(limits  = c(0, 550)) +
  scale_color_gradient(low = "gray90", high = "gray10") +
  coord_polar(theta = "y", start = 4.71, clip = "on") +
  facet_wrap(~ side) +
  theme_void() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        panel.spacing = unit(0, "cm"),
        plot.margin = margin(10,10,10,10))








