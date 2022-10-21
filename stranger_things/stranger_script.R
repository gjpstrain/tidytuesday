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
  )) %>%
  ungroup() %>%
  group_by(episode_id) %>%
  mutate(average_drama = mean(total_each_sentiment))

# Plotting

sentiment_by_episode %>%
  ggplot() +
  with_outer_glow(geom_bar(aes(x = episode_id, y = total_each_sentiment, fill = colour), stat = "identity"), colour = "white", sigma = 3) +
  geom_segment(x = 0, xend = 68, y = 0, yend = 0, colour = "#C13EB8", size = 2) +
  geom_line(aes(x = episode_id, y = average_drama, group  = 1), size = 1, colour = "white") +
  geom_point(aes(x = episode_id, y = average_drama), colour = "white") +
  scale_fill_manual(values = c("#C01517", "#3F66DA")) +
  annotate(geom = "text", x = 1, y = -30, label = "S1",
           colour ="white", angle = 0, size = 5, family = "Francois One") +
  annotate(geom = "text", x = 9, y = -20, label = "S2",
           colour ="white", angle = 0, size = 5, family = "Francois One") +
  annotate(geom = "text", x = 18, y = -13, label = "S3",
           colour ="white", angle = 0, size = 5, family = "Francois One") +
  annotate(geom = "text", x = 25, y = -34, label = "S4",
           colour ="white", angle = 0, size = 5, family = "Francois One") +
  annotate(geom = "text", x = 8, y = -40, label = "Season 4 seems to be the most dramatic by far!",
           colour ="white", angle = 0, size = 5, family = "Francois One") +
  annotate(geom = "curve", x = 16, xend = 25, y = -40.2,
           yend = -37, colour = "grey", curvature = 0.06, size = 1, arrow=arrow(type = "closed",
                                                                               length = unit(2, "mm"))) +
  labs(title = "Drama of Stranger Things",
       subtitle = "Distribution of <span style='color:#3F66DA'>**positive**</span> 
       and <span style='color:#FD121A'>**negative**</span> sentiment words in the dialogue of 
       **Stranger Things**",
       caption = "Data: 8flix.com | Graphic: Gabe Strain") +
    theme(plot.background = element_rect(fill = "#37393e", colour = "#37393e"),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.subtitle = element_markdown(colour = "white", family = "Francois One"),
          plot.title = element_text(colour = "white", size = 24, family = "Rubik Mono One"),
          plot.caption = element_text(colour = "#ebe2e1", family = "Francois One"))



