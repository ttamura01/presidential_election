library(tidyverse)
library(ggtext)
library(scales)
library(glue)
library(patchwork)
library(maps)
library(usmap)

college_votes <- read_csv("https://f.hubspotusercontent30.net/hubfs/7540639/Electoral_College.csv") %>% 
  rename_all(tolower) %>% 
  rename(state = full_state, college_votes = electoral_college_votes) %>% 
  arrange(desc(college_votes)) 

swing_states <- tribble(~state, ~status,
                        "Georgia", TRUE, 
                        "Michigan", TRUE,
                        "Nevada", TRUE,
                        "North Carolina", TRUE,
                        "Pennsylvania", TRUE,
                        "Virginia", TRUE,
                        "Wisconsin", TRUE)

data <- college_votes %>% 
  full_join(swing_states, by = "state") %>% 
  mutate(status = replace_na(status, FALSE)) %>% 
  mutate(state = tolower(state))

 data %>% 
  mutate(percentage = college_votes/sum(college_votes)*100) %>%
  group_by(status) %>%
   filter(status == TRUE) %>% 
  summarise(total_votes = sum(college_votes),total_percentage = sum(percentage) )

states_map <- map_data("state")


data %>% 
  mutate(state = tolower(state)) %>% 
  filter(state %in% c("alaska", "hawaii"))

state_centroids <- states_map %>% 
  group_by(region) %>% 
  summarize(long = mean(range(long)), lat = mean(range(lat))) %>% 
  left_join(data, by = c("region" = "state")) %>% 
  rename(state = region)

map_data <- states_map %>%
  left_join(data, by = c("region" = "state")) %>% 
  rename(state = region)

ggplot() +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = status), color = "white") +
  geom_text(data = state_centroids, aes(x = long, y = lat, label = college_votes), color = "black", size = 3) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray"), 
                    labels = c("Non-Swing State", "Swing State")) +
  coord_map() +
  labs(title = "US Electoral College Votes and Swing States",
       subtitle = "7 swing states total college votes: 96, accounts for 17.8% of total votes(442, including Alaska 3, Hawaii 4)") +
  theme_minimal() +
  theme(
    legend.title = element_blank()
  )

ggsave("us_presidential_election.png", height = 4.5, width = 6)


##########################################################################
college_votes <- read_csv("https://f.hubspotusercontent30.net/hubfs/7540639/Electoral_College.csv") %>% 
  rename_all(tolower) %>% 
  rename(state = full_state, college_votes = electoral_college_votes) %>% 
  arrange(desc(college_votes)) 

swing_states <- tribble(~state, ~status,
                        "Georgia", TRUE, 
                        "Michigan", TRUE,
                        "Nevada", TRUE,
                        "North Carolina", TRUE,
                        "Pennsylvania", TRUE,
                        "Virginia", TRUE,
                        "Wisconsin", TRUE)

data <- college_votes %>% 
  full_join(swing_states, by = "state") %>% 
  mutate(status = replace_na(status, FALSE))

state_centroids <- tribble(
  ~state, ~x, ~y,
  "alabama", -86.6848, 32.6010,
  "alaska", -152.4044, 61.3025,
  "arizona", -111.9301, 34.1682,
  "arkansas", -92.3899, 34.7519,
  "california", -119.6816, 37.1841,
  "colorado", -105.3111, 38.9972,
  "connecticut", -72.739, 41.5827,
  "delaware", -75.5148, 39.3185,
  "district of columbia", -77.0090, 38.8898,
  "florida", -81.5158, 27.7663,
  "georgia", -83.6431, 32.6415,
  "hawaii", -157.4983, 20.2927,
  "idaho", -114.4788, 44.3509,
  "illinois", -89.5031, 40.0417,
  "indiana", -86.2816, 40.2735,
  "iowa", -93.214, 42.0329,
  "kansas", -98.3804, 38.498,
  "kentucky", -85.3021, 37.8223,
  "louisiana", -91.9623, 30.9734,
  "maine", -69.2428, 44.6939,
  "maryland", -76.6413, 39.0639,
  "massachusetts", -71.8083, 42.2302,
  "michigan", -84.5603, 43.3266,
  "minnesota", -94.6362, 45.6945,
  "mississippi", -89.6665, 32.7416,
  "missouri", -92.2884, 38.4561,
  "montana", -110.3261, 46.9219,
  "nebraska", -99.784, 41.1254,
  "nevada", -117.0554, 38.3135,
  "new hampshire", -71.5724, 43.4525,
  "new jersey", -74.671, 40.2989,
  "new mexico", -106.1126, 34.8405,
  "new york", -74.9481, 42.1657,
  "north carolina", -79.8064, 35.6301,
  "north dakota", -99.784, 47.5289,
  "ohio", -82.7649, 40.3888,
  "oklahoma", -97.4943, 35.5653,
  "oregon", -120.5542, 44.572,
  "pennsylvania", -77.1945, 40.5908,
  "rhode island", -71.5065, 41.6809,
  "south carolina", -80.945, 33.8569,
  "south dakota", -99.4388, 44.2998,
  "tennessee", -86.6923, 35.7478,
  "texas", -99.5034, 31.0545,
  "utah", -111.6703, 40.15,
  "vermont", -72.5778, 44.0459,
  "virginia", -78.1696, 37.7693,
  "washington", -120.7401, 47.4009,
  "west virginia", -80.6227, 38.4912,
  "wisconsin", -89.6385, 44.2685,
  "wyoming", -107.551, 42.7559
)

data <- data %>%
  mutate(state = tolower(state)) %>%
  inner_join(state_centroids, by = c("state" = "state"))

ggplot(data = data, aes(fill = status)) +
  geom_map(map = us_map(), aes(map_id = state), color = "white") +
  expand_limits(x = us_map()$x, y = us_map()$y) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray"), 
                    labels = c("Swing State", "Non-Swing State")) +
  theme(legend.position = "right") +
  geom_text(aes(x = long, y = lat, label = college_votes), size = 3, color = "black") +
  labs(title = "US Electoral College Votes and Swing States",
       fill = "Swing State")





