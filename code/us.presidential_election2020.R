library(tidyverse)
library(usmap)


elect.us <- read_csv("/Users/takayukitamura/Documents/R_Computing/presidential_election/data/voting.csv")

elect.us <- elect.us %>% 
  rename(State = state) %>% 
  mutate("Winner" = if_else(trump_win == 1, "Trump", "Biden")) %>% 
  select(State, Winner)

elect.us %>% 
  group_by(Winner) %>% 
  summarise(n = length(state))

elect.us <- elect.us[order(elect.us$State), ]

statepop <- statepop %>% 
  filter(!abbr == "PR")

elect.us$fips <- statepop$fips
elect.us$abbr <- statepop$abbr

head(elect.us)

plot_usmap(data = elect.us,
           values = "Winner",
           labels = TRUE,
           alpha = 0.8,
           label_color = "white") +
  scale_fill_manual(values = c("blue", "red"),
                    breaks = c("Biden","Trump")) +
  labs(title = "US Presidential Election 2020 results by State") +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5),
    legend.position = c(0.36, -0.05),
    legend.direction = "horizontal",
    legend.key.width = unit(0.8, "in"),
    legend.key.height = unit(0.4, "in"),
    legend.text = element_text(size = 15)
  )

ggsave("us_presidential_election_2020.png", height = 4.5, width = 6)

