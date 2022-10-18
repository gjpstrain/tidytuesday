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
          text = element_text(colour = "#653410", family = "Maven Pro", size = 20),
          plot.title = element_text(hjust = 0, colour = "#653410", family = "Fjalla One", size = 38),
          plot.subtitle = element_text(size = 12, hjust = 0, vjust = -3),
          axis.title = element_text(color = "#653410", size = 15),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          plot.caption = element_text(size = 12, hjust = 1),
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
  mutate(image = "sheep.png")

df %>%
  ggplot(aes(x = yarn_company_name, y = yarn_weight_ply)) + 
  geom_image(aes(image = image), size = 0.1, asp = 2) +
  theme_socks() +
  theme(plot.subtitle = ggtext::element_markdown()) +
  ylim(2,5) +
  annotate(geom = "text", x = 1, y = 4, label = "Fingering Yarn", colour ="chocolate4", angle = 0, size = 7, family = "Maven Pro") +
  annotate(geom = "text", x = 3, y = 3, label = "Light Fingering Yarn", colour ="brown4", angle = 0, size = 7, family = "Maven Pro") +
  annotate("segment", x = 1.5, xend = 1.7, y = 4, yend = 4, colour = "black", size=1, arrow=arrow(type = "closed", length = unit(2, "mm"))) +
  annotate("segment", x = 2.3, xend = 1.3, y = 3, yend = 3, colour = "black", size=1, arrow=arrow(type = "closed", length = unit(2, "mm"))) +
  annotate("segment", x = 3.7, xend = 4.7, y = 3, yend = 3, colour = "black", size=1, arrow=arrow(type = "closed", length = unit(2, "mm"))) +
  labs(title = "Socks To Be Ewe!",
       x = "Yarn Manufacturer",
       caption = "Data: Ravelry.com | Graphic: Gabe Strain",
       subtitle = "
       
The most popular sock yarns currently available fall into


two categories of yarn weight, <span style='color:chocolate4'>**Fingering (4-ply)**</span>
and <span style='color:brown4'>**Light Fingering (3-ply)**</span>. 

All use some amount of <span style='color:WHite'>Merino</span> wool.")

