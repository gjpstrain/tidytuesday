library(tidyverse)
library(ggtext)
library(showtext)
library(ggimage)

## Get data
yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

## Choose fonts
font_add_google("Fjalla One", "Maven Pro")
showtext_auto()

## Woolly Socks Theme
theme_socks <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(colour = "#E8D2A5", fill = "#E8D2A5"),
          panel.grid = element_line(colour = "#E8D2A5"),
          panel.background = element_rect(colour = "#E8D2A5", fill = "#E8D2A5"),
          text = element_text(colour = "#653410", family = "Maven Pro"),
          plot.title = element_text(hjust = 0, colour = "#653410", family = "Fjalla One", size = 25),
          plot.subtitle = element_markdown(size = 12, lineheight = 0),
          axis.title = element_text(color = "#653410", size = 10),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none"
    )
}

## Clean df
df <- yarn %>%
  filter(grepl('sock|socks', permalink)) %>%
  filter(rating_average == 5) %>%
  arrange(desc(rating_total)) %>%
  head(5) %>%
  select(-c(discontinued, gauge_divisor, grams, id, max_gauge, min_gauge,
            rating_average, rating_count, rating_total, thread_size,
            wpi, yardage, yarn_weight_crochet_gauge)) %>%
  mutate(image = "sock.png")

df %>%
  ggplot(aes(x = yarn_company_name, y = yarn_weight_ply)) +
  geom_point(size = 15, shape = 16) +
  theme_socks() +
  theme(plot.subtitle = ggtext::element_markdown()) +
  ylim(2,5) +
  labs(title = "Socks To Be You",
       x = "Yarn Manufacturer",
       subtitle = "
       
The most popular sock yarns currently available fall into two
categories of yarn weight, <span style='color:brown'>**Fingering (4-ply)**</span>
and <span style='color:brown'>**Light Fingering (3-ply)**</span>. 

All use some amount of <span style='color:red'>Merino</span> wool.") +
  coord_flip()

