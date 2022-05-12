#get packages

library(tidyverse)

#read in data

nyt_titles <- readr::read_tsv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

#select only 2010-2020

df <- nyt_titles %>%
  filter(year > "2009") %>%
  group_by(year) %>%
  slice(which.max(total_weeks)) %>%
  mutate(title_new = paste(title, "(", total_weeks, "Weeks )"))