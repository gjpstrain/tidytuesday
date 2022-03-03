library(tidyverse)
library(ggtext)
library(ggimage)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

# Count years represented

year_counts <- count(freedom, year) # 2011-2016 are best represented, let's just pick one

freedom_00 <- subset(freedom, year == 2012)

# We need to write the csv out, change a few names so it matches with Kaggle dataset, then read it back in

write_csv(freedom_00, "freedom.csv")

freedom_01 <- read_csv("freedom2.csv")

# Manually download GDP dataset from Kaggle and read it in

gdp <- read_csv("GDP.csv", skip = 4)

gdp_2012 <- gdp %>%
  select(c("Country Name", "2012")) %>%
  rename("country" = "Country Name")

# Using inner_join to add GDPs to freedom df

full_data <- inner_join(freedom_01, gdp_2012, by = "country") %>%
  rename("GDP" = "2012") %>%
  mutate(lib_index = CL + PR) %>%
  select(-c("year", "Region_Code", "Region_Name", "is_ldc", "CL", "PR")) %>%
  mutate(GDP = GDP / 100000000) %>% # in 100s of mil $
  arrange(-GDP) %>%
  head(15)

# Need to remove spaces in names for the labels list to work properly

full_data["8","country"] <- "Russia"
full_data["1","country"] <- "USA"
full_data["6","country"] <- "UK"
full_data["14","country"] <- "SK"

#Creating Country Labels list

labels <- c(
  Russia = "<img src = 'https://cdn.countryflags.com/thumbs/russia/flag-400.png' width = '35' height = '22.5' /><br>Russia",
  China = "<img src = 'https://cdn.countryflags.com/thumbs/china/flag-400.png' width = '35' height = '22.5' /><br>China",
  Italy = "<img src = 'https://cdn.countryflags.com/thumbs/italy/flag-400.png' width = '35' height = '22.5' /><br>Italy",
  France = "<img src = 'https://cdn.countryflags.com/thumbs/france/flag-400.png' width = '35' height = '22.5' /><br>France",
  USA = "<img src = 'https://cdn.countryflags.com/thumbs/united-states-of-america/flag-400.png' width = '35' height = '22.5' /><br>US",
  Japan = "<img src = 'https://cdn.countryflags.com/thumbs/japan/flag-400.png' width = '35' height = '22.5' /><br>Japan",
  Canada = "<img src = 'https://cdn.countryflags.com/thumbs/canada/flag-400.png' width = '35' height = '22.5' /><br>Canada",
  Brazil = "<img src = 'https://cdn.countryflags.com/thumbs/brazil/flag-400.png' width = '35' height = '22.5' /><br>Brazil",
  Germany = "<img src = 'https://cdn.countryflags.com/thumbs/germany/flag-400.png' width = '35' height = '22.5' /><br>Germany",
  UK = "<img src = 'https://cdn.countryflags.com/thumbs/united-kingdom/flag-400.png' width = '35' height = '22.5' /><br>United Kingdom",
  India = "<img src = 'https://cdn.countryflags.com/thumbs/india/flag-400.png' width = '35' height = '22.5' /><br>India",
  Australia = "<img src = 'https://cdn.countryflags.com/thumbs/australia/flag-400.png' width = '35' height = '22.5' /><br>Australia",
  Spain = "<img src = 'https://cdn.countryflags.com/thumbs/spain/flag-400.png' width = '35' height = '22.5' /><br>Spain",
  Mexico = "<img src = 'https://cdn.countryflags.com/thumbs/mexico/flag-400.png' width = '35' height = '22.5' /><br>Mexico",
  SK = "<img src = 'https://cdn.countryflags.com/thumbs/south-korea/flag-400.png' width = '35' height = '22.5' /><br>Indonesia")

# Creating Plot

ggplot(full_data, aes(x = reorder(country, -lib_index), y = lib_index, group = 1)) +
  geom_line(size = 2, aes(colour = lib_index)) +
  geom_point(size = 8, aes(colour = lib_index)) +
  scale_colour_gradient(low = "royalblue2", high = "darkred") +
  theme(axis.text.x = element_text(vjust = 0.5),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "black", colour = NA),
        panel.grid = element_blank(),
        title = element_text(colour = "white", face = "bold"),
        plot.subtitle = ggtext::element_markdown()) +
  labs(title = "Most of the World's Richest Nations Had High Levels of Freedom (With Exceptions) in 2012",
       subtitle = "The 15 Richest Nations (by GDP) ranked from <span style='color:red'>**least**</span> to <span style='color:royalblue2'>**most**</span> free",
       caption = "Source: UN and Freedom House | Graphic: Gabe Strain") +
  scale_x_discrete(name = NULL,
                   labels = labels) +
  theme(axis.text.x = element_markdown(color = "black", size = 9)) +
  annotate(geom = "text", x = 13.5, y = 11,label = "Freedom was calculated\nby summing scores for\nCivil Liberties and\nPolitical Rights.\n\nNF = Not Free\nPF = Partially Free",
           colour ="grey", size = 4, fontface = "bold") +
  annotate(geom = "text", x = 1.6, y = 13,label = "NF",
           colour ="red", size = 4, fontface = "bold") +
  annotate(geom = "text", x = 2.6, y = 11,label = "NF",
           colour ="red", size = 4, fontface = "bold") +
  annotate(geom = "text", x = 3.6, y = 6.3,label = "PF",
           colour ="#874d93", size = 4, fontface = "bold") +
  annotate(geom = "segment", x = 4.6, y = 5.5, xend = 15, yend = 5.5,
           arrow = arrow(), size = 2, colour = "royalblue") +
  annotate(geom = "segment", x = 15, y = 5.5, xend = 4.5, yend = 5.5,
           arrow = arrow(), size = 2, colour = "royalblue") +
  annotate(geom = "text", x = 9.75, y = 6.3,label = "From India onwards, all are categorised as Free",
           colour ="royalblue", size = 4, fontface = "bold")

ggsave("my_plot.png", width = 9.5, height = 6.5, units = "in", dpi = 400)
