#Packages

library(tidyverse)
library(showtext)
library(ggtext)
library(textdata)
library(tidytext)
library(ggfx)

# Fonts

font_add_google("Francois One")
font_add_google("Rubik Mono One")
showtext_auto()

#Get data

episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv')
script <- readr::read_csv("https://raw.githubusercontent.com/filmicaesthetic/stringr-things/main/data/stranger_things_all_dialogue.csv")

# Tidying episodes

tidy_episodes <- episodes %>%
  mutate(episode_id = paste0(season, ".", episode))

# Tidying dialogue

tidy_dialogue <- script %>% 
  unnest_tokens(word, dialogue) %>% 
  na.omit() %>%
  mutate(episode_id = paste0(season, ".", episode))

# Getting sentiments 

sentiment_dict <- get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative"))

# Preparing data

sentiment_by_episode <- tidy_dialogue %>%
  inner_join(sentiment_dict) %>%
  inner_join(tidy_episodes) %>%
  group_by(episode_id, sentiment) %>%
  summarise(total_each_sentiment=n())%>%
  arrange(episode_id,desc(total_each_sentiment)) %>% 
  mutate(rank = row_number()) %>%
  mutate(total_each_sentiment = if_else(
    condition = sentiment == "negative",
    true = total_each_sentiment*-1,
    false = total_each_sentiment*1
  )) %>%
  mutate(colour = if_else(
    condition = sentiment == "negative",
    true = "neg",
    false = "pos"
  ))


# Plotting

sentiment_by_episode %>%
  ggplot() +
  geom_bar(aes(x = episode_id, y = total_each_sentiment, fill = colour), stat = "identity") +
  geom_segment(x = 0, xend = 68, y = 0, yend = 0, colour = "orange", size = 2) +
  scale_fill_manual(values = c("#FD121A", "#1222fd")) +
  annotate(geom = "text", x = 1, y = -30, label = "S1", colour ="white", angle = 0, size = 5, family = "Francois One") +
  annotate(geom = "text", x = 9, y = -20, label = "S2", colour ="white", angle = 0, size = 5, family = "Francois One") +
  annotate(geom = "text", x = 18, y = -13, label = "S3", colour ="white", angle = 0, size = 5, family = "Francois One") +
  annotate(geom = "text", x = 25, y = -34, label = "S4", colour ="white", angle = 0, size = 5, family = "Francois One") +
  labs(title = "The Drama of Stranger Things",
       subtitle = "Distribution of <span style='color:#1222fd'>**positive**</span> 
       and <span style='color:#FD121A'>**negative**</span> sentiment words in the dialogue of 
       **Stranger Things**") +
    theme(plot.background = element_rect(fill = "black", colour = "black"),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = with_outer_glow(element_text(colour = "white", size = 45, family = "Rubik Mono One"), sigma = 4, colour = "red"))




























