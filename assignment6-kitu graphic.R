# load packages
library(ggplot2)
library(ggmap)
library(maps)
library(readr)
library(dplyr)

# load data
load(file = "Kitu/College/Senior Year/Fall Quarter/Stats 140SL/business.RData")

# look at categories
categories <- as.data.frame(table(business$categories.0))

# subset only certain cuisines
food <- c("Mexican", "Chinese", "Italian", "Indian", "Greek", "Thai", "Vietnamese")
business <- business[categories.0 %in% food, c(8, 11:13, 22:29)]

# import state abbreviations
state_abbreviations <- read_csv("~/Kitu/College/Senior Year/Geographic/state_abbreviations.csv")

# change abbreviation to full name
business$usstates <- ifelse(business$state %in% state_abbreviations$Abbreviation, state_abbreviations$State, "")

# subset to only US states
business <- business[usstates != "", ]

# clean US states
business$usstates <- tolower(business$usstates)

# count how many total restaurants per cuisine exists
table(business$categories.0)

# create 2 way table for total count within each cuisine
table <- as.data.frame(table(business$usstates, business$categories.0))

# make proportion variable, because count is not appropriate (there are lots of Mexican restaurants)
table$prop <- 0
table$prop <- ifelse(table$Var2 == "Chinese", table$Freq/768, table$prop)
table$prop <- ifelse(table$Var2 == "Greek", table$Freq/138, table$prop)
table$prop <- ifelse(table$Var2 == "Indian", table$Freq/129, table$prop)
table$prop <- ifelse(table$Var2 == "Italian", table$Freq/697, table$prop)
table$prop <- ifelse(table$Var2 == "Mexican", table$Freq/1236, table$prop)
table$prop <- ifelse(table$Var2 == "Thai", table$Freq/186, table$prop)
table$prop <- ifelse(table$Var2 == "Vietnamese", table$Freq/115, table$prop)

# choose highest prop for each state
top <- table %>%
  group_by(Var1) %>%
  top_n(n = 1, wt = prop)

# left join to all_states dataframe
all_states <- map_data("state") # load US map data
names(top)[1] <- "region"
all_states <- left_join(all_states, top)
names(all_states)[7] <- "Cuisine"

# clean up dataframe
all_states <- all_states[ , -6]
all_states <- all_states[complete.cases(all_states), ]

# make map of lower 48 states
ggplot() + geom_polygon(data = all_states, aes(x = long, y = lat, group = group, fill = Cuisine), color = "white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "white"), 
        axis.line = element_line(colour = "white"),
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle(label = "Most Popular Yelp Cuisine by State") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "")