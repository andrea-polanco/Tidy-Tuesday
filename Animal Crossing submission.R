#set working directory
setwd("/....")

#load libraries 
library(tidyverse) #data cleaning & ggplot 
library(showtext) #custom fonts 

#load data 
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

#re-label cub as bear because cubs are bears 
villagers$species[villagers$species=="cub"]<-"bear"

#re-label bull as cow because they're the same (just different sex)
villagers$species[villagers$species=="bull"]<-"cow"

#obtain the number of villagers by species type 
species_df <- villagers %>%
  group_by(species) %>%
  count(species) %>%
  mutate(percentage =(n/391)*100) #since there are 391 villagers in total 

#categorize animals by whether they're farmed for meat, eggs, or dairy (binary Yes/No)
species_df <- species_df %>%
  mutate(farmed = case_when(species == "cat" ~ "No",
                            species == "wolf" ~ "No",
                            species == "tiger" ~ "No",
                            species == "squirrel" ~ "No",
                            species == "sheep" ~ "Yes",
                            species == "rhino" ~ "No",
                            species == "rabbit" ~ "Yes",
                            species == "pig" ~ "Yes",
                            species == "penguin" ~ "No", 
                            species == "ostrich" ~ "No",
                            species == "octopus" ~ "No", #at least not yet farmed for 'food'....
                            species == "mouse" ~ "No",
                            species == "monkey" ~ "No",
                            species == "lion" ~ "No", 
                            species == "koala" ~ "No",
                            species == "kangaroo" ~ "No", 
                            species == "horse" ~ "No", #debatable as Canada ships horses overseas to sell as meat
                            species == "hippo" ~ "No",
                            species == "hamster" ~ "No", 
                            species == "gorilla" ~ "No", 
                            species == "goat" ~ "Yes",
                            species == "frog" ~ "No",
                            species == "elephant" ~ "No",
                            species == "eagle" ~ "No", 
                            species == "duck" ~ "Yes",
                            species == "dog" ~ "No", 
                            species == "deer" ~ "No",
                            species == "cow" ~ "Yes", 
                            species == "chicken" ~ "Yes",
                            species == "bird" ~ "No", 
                            species == "bear" ~ "No",
                            species == "anteater" ~ "No",
                            species == "alligator" ~ "No"))

#proportion of species farmed for food:
7/33 #21% of the species featured in Animal Crossing belong to a species that's farmed for meat, dairy, or eggs in real life

#proportion of villagers farmed for food:
farmanimals <- species_df %>%
  filter(farmed == "Yes") 

sum(farmanimals$n) #92 villagers out of 391 are farmed for meat, dairy, or eggs in real life 
92/391 #24% of all villagers featured in Animal Crossing belong to a species that that's farmed for meat, dairy, or eggs in real life

#load fonts
font_add(family="title", "AnimalCrossing.ttf") #registers the font 
showtext_auto() #loads the font 

#graph frequency of species featured in Animal Crossing 
ggplot(species_df, aes(x = reorder(species, n), y = n, fill = farmed)) +
  scale_fill_manual(name = "Farmed For Meat, Dairy, and/or Eggs", 
                    values = c("Yes" = "#9dffb0",
                               "No" = "#81f1f7")) +
  geom_bar(position = position_dodge(), stat = "identity") +
  coord_flip(ylim=c(0,33)) +
  theme(legend.position = "bottom", 
        text = element_text(size = 13),
        panel.background = element_rect("white"),
        panel.border = element_rect(fill = NA), 
        panel.grid.major.x = element_line(colour = "grey"),
        axis.title.x = element_text(family = "title", color = "#39363D", size = 15),
        axis.title.y = element_text(family = "title", color = "#39363D", size = 15),
        legend.title = element_text(family = "title", color = "#39363D", size = 14),
        legend.text = element_text(family = "title", color = "#39363D", size = 12),
        legend.direction = "vertical",
        title = element_text(color = "black", size = 15, face = "bold"),
        plot.title = element_text(family = "title", hjust = 0.5)) +
  geom_text(aes(y = 0, label = n), hjust = -0.3, size = 4) + 
  labs (x ="Species", 
        y = "Count", 
        title = "Number of Animal Crossing Villagers By Species")
