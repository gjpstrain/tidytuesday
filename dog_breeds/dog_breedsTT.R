library(tidyverse)
library(hrbrthemes)
library(ggimage)
library(ggtext)
library(magick)
library(patchwork)
library(png)
library(grid)
library(wesanderson)
library(extrafont)

loadfonts(device = "win")

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')

trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')

breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

friendly_boys <- breed_traits %>%
  select(c("Breed",
           "Affectionate With Family",
           "Good With Young Children",
           "Good With Other Dogs",
           "Openness To Strangers")) %>%
  rowwise() %>%
  mutate(friend_index = as.numeric(sum(c_across("Affectionate With Family":"Openness To Strangers")))) %>%
  select(-c("Affectionate With Family":"Openness To Strangers"))

rank <- breed_rank_all %>%
  select(Breed=Breed, rank="2020 Rank")

friendly_boys_ranked <- friendly_boys %>%
  bind_cols(rank)

friendly_boys_clean <- friendly_boys_ranked %>%
  subset(select = -c(Breed...3)) %>%
  rename(breed = Breed...1)
  
top_11 <- friendly_boys_clean %>%
  filter(friend_index >= 20)

weights <- c(80, 75, 60, 13, 150, 18, 70, 15, 45, 70, 60)

top_11 <- cbind(top_11, weights)

#Creating the dummy row for use with geom_hline()

dummy <- data.frame("dummy", 22, 171, 0, 12)

names(dummy) <- c("breed", "friend_index", "rank", "weights", "rank_normalised")

final_data <- cbind(top_11) %>%
  mutate(rank_normalised = c(1:11))

final_data <- rbind(final_data, dummy)

#making the image raster

my_image <- image_read("dog_pic.png")

raster <- as.raster(my_image)

p <- final_data %>%
  ggplot(aes(x = reorder(breed, desc(rank_normalised)), y = friend_index, size= weights, colour = breed)) +
  geom_point(alpha = 0.8) +
    theme_minimal() +
   theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        axis.text.y = element_text(colour = "white", face = "bold", size = 13)) +
  theme(axis.text.y = element_text(hjust = 0.5)) +
  scale_x_discrete("", labels = c("",
                                  "Irish Red and\nWhite Setter",
                                  "Flat-Coated\nRetriever",
                                  "Keeshonden",
                                  "Coton de Tulear",
                                  "Irish Setter",
                                  "Bichons\nFrises",
                                  "Newfoundland",
                                  "Havanese",
                                  "Siberian\nHusky",
                                  "Golden\nRetriever",
                                  "Labrador")) +
scale_size(range = c(5, 30), name = "weights") +
  scale_color_brewer(palette = "Paired") +
  geom_hline(yintercept = 18, size = 15, colour = "black") +
  geom_hline(yintercept = 22, size = 15, colour = "black") +
  annotate("segment", x = 1, xend = 12, y = 23, yend = 23, colour = "pink", size=3, arrow=arrow()) +
  theme(axis.title.x = element_text(size = 11, colour = "white", face = "bold", hjust = 1, vjust = 8)) +
  theme(title = element_text(colour = "white", size = 13)) +
  labs(title = "The Friendliest Dogs on the AKS, by Weight and Popularity",
       x = "dogs1",
       y = "All these breeds score maximally on the 'friend index', which is a collation of the 'Affectionate With 
       family', 'Good with Young Children', 'Good With Other Dogs', and 'Openness to Strangers' scores.",
       caption = "Source: AKS | Graphic: Gabriel Strain") +
  annotate(geom = "text", x = 6.5, y = 22.5, label = "AKC Breed Ranking (2020)", colour ="pink", angle = 90, size = 7) +
  annotate(geom = "text", x = 11, y = 18.8,label = "The size of each circle\nrepresents the breed's\naverage weight.",
           colour ="white", size = 4, fontface = "bold") +
  annotate(geom = "text", x = 7, y = 21.4,label = "With the middle-ranking dog\nbreeds having the most\nvariability in size",
           colour ="white", size = 4, fontface = "bold") +
  annotate(geom = "text", x = 2.7, y = 18.8,label = "And the most and least\npopular being all of\nmedium size.",
  colour ="white", size = 4, fontface = "bold") +
    coord_flip() 

p + annotation_raster(raster, xmin = 10, xmax = 12, ymin =20.5, ymax = 22.5) +
  theme(plot.title = element_text(hjust = -4))

ggsave("my_plot.png", width = 7.58, height = 10, units = "in", dpi = 400)
