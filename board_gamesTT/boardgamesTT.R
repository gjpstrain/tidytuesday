library(tidytuesdayR)
library(tidyverse)
library(wesanderson)
library(ggimage)

tuesdata <- tidytuesdayR::tt_load('2022-01-25')

ratings <- tuesdata$ratings

restricted_data <- ratings %>%
  select(c(id, name, year, users_rated, thumbnail))

details <- tuesdata$details         

restricted_details <- details %>%
  select(c(id, playingtime))

final_data <- merge(restricted_data, restricted_details, by = "id", row.names = FALSE)

head(final_data)

top_10 <- final_data %>%
  filter(playingtime > 120) %>%
  slice_max(users_rated, n = 10) %>%
  mutate(users_rated_k = users_rated / 1000)

top_10 <- top_10 %>%
  mutate(playingtime = paste(playingtime, "minutes"))

background = "plot_background.jpg"
 
plot <- top_10 %>%
  ggplot(aes(x = reorder(name, users_rated_k), y = users_rated_k, fill = name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = playingtime, fontface = "bold", size = 4),
            hjust = 1.1) +
  scale_fill_manual(values = wes_palette("Darjeeling1", 10, type = "continuous")) +
  scale_x_discrete("", labels = c("Game of Thrones:\nThe Board Game",
                                  "Monopoly",
                                  "Eldritch\nHorror",
                                  "Great Western\nTrail",
                                  "Battlestar Galactica:\nThe Board Game",
                                  "Arkham\nHorror",
                                  "Terra\nMystica",
                                  "Twilight\nStruggle",
                                  "Puerto\nRico",
                                  "Agricola")) +
  coord_flip() +
  theme(axis.text.y = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(colour = "white", face = "bold", size = 12)) +
  theme(axis.text.y = element_text(colour = "white", face = "bold", size = 10)) +
  scale_y_continuous(n.breaks = 7) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "steelblue"),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "Number of Reviews (thousands)",
       title =  "How Long?",
       subtitle = "The Most Popular Board Games That Take at Least 2 Hours to Complete",
       caption = "Source: BoardGameGeek | Graphic: Gabriel Strain") +
  theme(axis.title.x = element_text(colour = "white", face = "bold")) +
  theme(title = element_text(colour = "white")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 22, hjust = -0.1)) +
  theme(plot.subtitle = element_text(hjust = -0.16))


ggbackground(plot, background)












