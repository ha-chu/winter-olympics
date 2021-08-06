library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggthemr)
ggthemr('chalk')

# inspect
str(winter)
unique(winter$Sport)
unique(winter$Country)
unique(winter$Event)
unique(winter$Discipline)

# filter out countries
winter <- winter %>% filter(Country %in% c("FRA", "GER", "ITA", "LIE", "SUI", "AUT", "SLO"))
winter <- winter %>% mutate(Count = 1)

# medals by country
winter %>% ggplot(aes(Country, Count, fill = Medal, label = Country)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(nudge_y = -3) +
  scale_fill_manual(values = c("#959FFF", "#172FFF", "#4356FF")) +
  theme(panel.background = element_rect(fill = "#1a1a1a")) + 
  theme(plot.background = element_rect(fill = "#1a1a1a")) +
  theme(axis.text = element_text(color = "#cccccc")) +
  xlab("Country") +
  ylab("Medal Count") + 
  ggtitle("The Alps Countries' Winter Olympics Medal Count, 1924-2014") +
  theme(axis.title.x = element_text(color = "#cccccc", size = 10, margin = margin(10,0,20,0))) + 
  theme(axis.title.y = element_text(color = "#cccccc", size = 10, margin = margin(0,15,0,20))) + 
  theme(plot.title = element_text(color = "#fafafa", hjust = 0.5, face = "plain", margin = margin(20,0,20,0))) +
  theme(axis.line.y = element_line(color = "#b4b4b4")) +
  theme(legend.background = element_rect(fill = "#1A1A1A", color = "#1A1A1A"), legend.key = element_rect(color = "#1a1a1a")) +
  theme(legend.text = element_text(color = "#FAFAFA", margin = margin(0,20,0,0))) + 
  theme(legend.title = element_text(color = "#FAFAFA")) +
  theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  labs(fill = "Medal Type")

# france
france <- winter %>% filter(Country == "FRA")

france %>% ggplot(aes(Discipline, Count, fill = Discipline)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values = c("#1733FF", "#6081FF", "#4123B9", "#B036F3", "#6ec5cc", "#d0215b", "#ef7e65", "#b7fef8", "#075a5d")) +
  coord_polar() +
  theme(panel.background = element_rect(fill = "#1a1a1a"), plot.background = element_rect(fill = "#1a1a1a"), axis.text = element_text(color = "#cccccc")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(legend.background = element_rect(fill = "#1A1A1A", color = "#1a1a1a"), legend.key = element_rect(color = "#1a1a1a"), legend.margin = margin(0,5,0,20)) +
  theme(legend.text = element_text(color = "#fafafa", margin = margin(0,20,0,0)), legend.title = element_text(color = "#fafafa")) +
  ylab("Medal Count") +
  theme(axis.title.y = element_text(margin = margin(0,15,0,20), color = "#cccccc", hjust = 0.75)) +
  ggtitle("France") +
  theme(plot.title = element_text(color = "#fafafa", hjust = 0.5, face = "plain", margin = margin(20,0,10,0)))

# italy
italy <- winter %>% filter(Country == "ITA")

italy %>% ggplot(aes(Discipline, Count, fill = Discipline)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values= c("#1733FF", "#4123b9", "#b036f3", "#7f8fb0", "#ccb4ff", "#075a5d")) +
  coord_polar() +
  theme(panel.background = element_rect(fill = "#1a1a1a"), plot.background = element_rect(fill = "#1a1a1a"), axis.text = element_text(color = "#cccccc")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(legend.background = element_rect(fill = "#1A1A1A", color = "#1a1a1a"), legend.key = element_rect(color = "#1a1a1a"), legend.margin = margin(0,5,0,20)) +
  theme(legend.text = element_text(color = "#fafafa", margin = margin(0,20,0,0)), legend.title = element_text(color = "#fafafa")) +
  ylab("Medal Count") +
  theme(axis.title.y = element_text(margin = margin(0,15,0,20), color = "#cccccc", hjust = 0.75)) +
  ggtitle("Italy") +
  theme(plot.title = element_text(color = "#fafafa", hjust = 0.5, face = "plain", margin = margin(20,0,10,0)))

# germany
germany <- winter %>% filter(Country == "GER")

germany %>% ggplot(aes(Discipline, Count, fill = Discipline)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values= c("#1733FF", "#6081FF", "#411AB9", "#B008F3", "#D0155B", "#EF7E65", "#F9B36D", "#F78EAC", "#B7FFF8", "#645186", "#075B5D", "#F7E4FF")) +
  coord_polar() +
  theme(panel.background = element_rect(fill = "#1a1a1a"), plot.background = element_rect(fill = "#1a1a1a"), axis.text = element_text(color = "#cccccc")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(legend.background = element_rect(fill = "#1A1A1A", color = "#1a1a1a"), legend.key = element_rect(color = "#1a1a1a"), legend.margin = margin(0,5,0,20)) +
  theme(legend.text = element_text(color = "#fafafa", margin = margin(0,20,0,0)), legend.title = element_text(color = "#fafafa")) +
  ylab("Medal Count") +
  theme(axis.title.y = element_text(margin = margin(0,15,0,20), color = "#cccccc", hjust = 0.75)) +
  ggtitle("Germany") +
  theme(plot.title = element_text(color = "#fafafa", hjust = 0.5, face = "plain", margin = margin(20,0,10,0)))

# liechstenstein
liechstenstein <- winter %>% filter(Country == "LIE")

liechstenstein %>% ggplot(aes(Discipline, Count, fill = Discipline)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values= c("#1733FF")) +
  coord_polar() +
  theme(panel.background = element_rect(fill = "#1a1a1a"), plot.background = element_rect(fill = "#1a1a1a"), axis.text = element_text(color = "#cccccc")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(legend.background = element_rect(fill = "#1A1A1A", color = "#1a1a1a"), legend.key = element_rect(color = "#1a1a1a"), legend.margin = margin(0,5,0,20)) +
  theme(legend.text = element_text(color = "#fafafa", margin = margin(0,20,0,0)), legend.title = element_text(color = "#fafafa")) +
  ylab("Medal Count") +
  theme(axis.title.y = element_text(margin = margin(0,15,0,20), color = "#cccccc", hjust = 0.75)) +
  ggtitle("Liechstenstein") +
  theme(plot.title = element_text(color = "#fafafa", hjust = 0.5, face = "plain", margin = margin(20,0,10,0)))

# suisse
suisse <- winter %>% filter(Country == "SUI")

suisse %>% ggplot(aes(Discipline, Count, fill = Discipline)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values= c("#1733FF", "#6081FF", "#4123B9", "#B036f3", "#6ec5cc", "#d0215b", "#F9B36C", "#b7fef8")) +
  coord_polar() +
  theme(panel.background = element_rect(fill = "#1a1a1a"), plot.background = element_rect(fill = "#1a1a1a"), axis.text = element_text(color = "#cccccc")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(legend.background = element_rect(fill = "#1A1A1A", color = "#1a1a1a"), legend.key = element_rect(color = "#1a1a1a"), legend.margin = margin(0,5,0,20)) +
  theme(legend.text = element_text(color = "#fafafa", margin = margin(0,20,0,0)), legend.title = element_text(color = "#fafafa")) +
  ylab("Medal Count") +
  theme(axis.title.y = element_text(margin = margin(0,15,0,20), color = "#cccccc", hjust = 0.75)) +
  ggtitle("Switzerland") +
  theme(plot.title = element_text(color = "#fafafa", hjust = 0.5, face = "plain", margin = margin(20,0,10,0)))

# austria
austria <- winter %>% filter(Country == "AUT")

austria %>% ggplot(aes(Discipline, Count, fill = Discipline)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values= c("#1733FF", "#6081FF", "#4123b9", "#d0215b", "#f78eac", "#b7fef8", "#645186", "#075a5d")) +
  coord_polar() +
  theme(panel.background = element_rect(fill = "#1a1a1a"), plot.background = element_rect(fill = "#1a1a1a"), axis.text = element_text(color = "#cccccc")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(legend.background = element_rect(fill = "#1A1A1A", color = "#1a1a1a"), legend.key = element_rect(color = "#1a1a1a"), legend.margin = margin(0,5,0,20)) +
  theme(legend.text = element_text(color = "#fafafa", margin = margin(0,20,0,0)), legend.title = element_text(color = "#fafafa")) +
  ylab("Medal Count") +
  theme(axis.title.y = element_text(margin = margin(0,15,0,20), color = "#cccccc", hjust = 0.75)) +
  ggtitle("Austria") +
  theme(plot.title = element_text(color = "#fafafa", hjust = 0.5, face = "plain", margin = margin(20,0,10,0)))

# slovenia
slovenia <- winter %>% filter(Country == "SLO")

slovenia %>% ggplot(aes(Discipline, Count, fill = Discipline)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values= c("#1733FF", "#1c6db7")) +
  coord_polar() +
  theme(panel.background = element_rect(fill = "#1a1a1a"), plot.background = element_rect(fill = "#1a1a1a"), axis.text = element_text(color = "#cccccc")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(legend.background = element_rect(fill = "#1A1A1A", color = "#1a1a1a"), legend.key = element_rect(color = "#1a1a1a"), legend.margin = margin(0,5,0,20)) +
  theme(legend.text = element_text(color = "#fafafa", margin = margin(0,20,0,0)), legend.title = element_text(color = "#fafafa")) +
  ylab("Medal Count") +
  theme(axis.title.y = element_text(margin = margin(0,15,0,20), color = "#cccccc", hjust = 0.75)) +
  ggtitle("Slovenia") +
  theme(plot.title = element_text(color = "#fafafa", hjust = 0.5, face = "plain", margin = margin(20,0,10,0)))

# filter alps cities
unique(original$City)
hosts <- winter %>% filter(City %in% c("Chamonix", "St.Moritz", "Garmisch Partenkirchen", "Cortina d'Ampezzo", "Innsbruck", "Grenoble", "Albertville", "Turin"))
coordinates <- data.frame(longitude = c(45.9237, 46.4908, 47.4917, 46.5405, 47.2692, 45.1885, 45.6755, 45.0703), latitude = c(6.8694, 9.8355, 11.0955, 12.1357, 11.4041, 5.7245, 6.3927, 7.6869))

# map
install.packages("sf")
install.packages("rnaturalearthdata")
library(sf)
library(rnaturalearthdata)

world <- ne_countries(continent = "europe", scale = "medium", returnclass = "sf")

map <- ggplot(data = world) +
  geom_sf()
