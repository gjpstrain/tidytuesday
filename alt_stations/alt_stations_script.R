library(tidyverse)
library(sf)
library(tigris)
library(sysfonts)
library(showtext)
library(ggtext)
library(wesanderson)

showtext_auto()
font_families_google()
font_add_google("League Gothic","LG")

stations <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

stations_tx <- stations %>% filter( STATE=="TX")

stations_tx <- st_as_sf(stations_tx, coords = c("X","Y"),crs = 4326, agr = "constant")

# extracting US states and county data

states <- map_data("state")
counties <- map_data("county")
Texas <- subset(states, region == "texas")

# extracting secondary road data

roads_tx <- primary_secondary_roads(state="Texas")

# stats for EV networks

density <- df %>%
  group_by(EV_NETWORK) %>%
  summarise(count = n())

# Creating new column with 2 largest EV networks and "other"

df <- stations_tx %>%
  select(EV_NETWORK, FUEL_TYPE_CODE, LONGDD, LATDD) %>%
  filter(FUEL_TYPE_CODE == "ELEC") %>%
  mutate(
    type = case_when(
      EV_NETWORK == "ChargePoint Network" ~ "ChargePoint (n = 1256)",
      EV_NETWORK == "Tesla" ~ "Tesla (n = 371)",
      EV_NETWORK == "Tesla Destination" ~ "Tesla (n = 371)",
      EV_NETWORK == "AMPUP" ~ "Other (n = 695)",
      EV_NETWORK == "Blink Network" ~ "Other (n = 695)",
      EV_NETWORK == "Electrify America" ~ "Other (n = 695)",
      EV_NETWORK == "EV Connect" ~ "Other (n = 695)",
      EV_NETWORK == "eVgo Network" ~ "Other (n = 695)",
      EV_NETWORK == "Non-Networked" ~ "Other (n = 695)",
      EV_NETWORK == "SemaCharge Network" ~ "Other (n = 695)",
      EV_NETWORK == "Volta" ~ "Other (n = 695)",
      EV_NETWORK == NA ~ "Other (n = 695)"))

# Stats for EV Networks, then modify and re-run above

df %>%
  group_by(type) %>%
  summarise(count = n())

# plot

p <- ggplot() + geom_polygon(data = Texas, aes(x=long, y = lat),color="gray20") +
  xlab("")+ylab("")+
  coord_fixed(1.3) + 
  geom_sf(data = roads_tx%>%filter(MTFCC=="S1200"),color="#0659ba", size = 0.2)+
  labs(title = "Electric charging stations in Texas",
       subtitle = "A map showing the distribution of electric charging stations along <span style = 'color:#013F87;'>secondary roads</span> in Texas",
       caption = "Data: US DOT | Graphic: Gabe Strain") +
  theme(text = element_text(family="LG",color="#D8E9A8"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.18,.1),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#DF6F0A",color="#DF6F0A"),
        legend.text = element_text(color="white", size = 60),
        strip.background = element_rect(fill = "black",color="black"),
        legend.key = element_rect(fill = "black",color="black"),
        legend.title = element_blank(),
        plot.margin = unit(c(-25,0, -35, 0), "pt"),
        panel.background = element_rect(fill="#DF6F0A",color="#DF6F0A"),
        plot.background = element_rect(fill="#DF6F0A",color="#DF6F0A"),
        plot.title = element_text(size=150,hjust=.4),
        plot.subtitle = element_markdown(size=50,hjust=.4),
        plot.caption = element_text(size=45,hjust=.95)
  ) 

p +
  geom_point(data = df,
             aes(x=LONGDD,y=LATDD, colour = type),
             size=.5,
             alpha=.75,
             shape=20,
             show.legend = TRUE,
             inherit.aes = FALSE) +
  scale_colour_manual(values = wes_palette(n = 3, name = "Darjeeling1"))

ggsave("alternative_fuel_TX_final.png",last_plot(), width=4,height = 5,dpi=500)


