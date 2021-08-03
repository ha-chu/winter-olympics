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
