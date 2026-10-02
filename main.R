library(tidyverse)
library(sf)
library(tmap)
library(googlesheets4)

# Extract scores and remove nuisance characters

score_table <- read_csv("data/score_table.csv") %>%
  mutate(across(where(is.character), ~ str_remove_all(.x, "['\u2018\u2019]")))

# Extract results from Google Sheets

gs4_auth()

results <- read_sheet("https://docs.google.com/spreadsheets/d/1CXOzJLPv7tH9tiLc9Rb0oiaJCUdygjYVqCG-N2BeC-8/edit",
                 sheet = "Form Responses 1") %>%
  
  # Set correct timezone
  mutate(Timestamp = lubridate::force_tz(Timestamp, "Europe/London")) %>%
  
  # Remove nuisance characters
  mutate(across(where(is.character), ~ str_remove_all(.x, "['\u2018\u2019]"))) %>%
  
  # Rename variables
  rename(
    team = `1. What is your team number?`,
    location = `2. Which destination, station, or bonus points are you recording? (select one per form)`,
    ) %>%
  
  # Check for photo verification
  mutate(photo_verified = if_else(is.na(`Photo verification`)|`Photo verification`!= "Y",FALSE,TRUE)) %>%
  
  # Filter out test inputs
  filter(str_detect(team, "Team")) %>% 
  
  # Join to score data
  left_join(score_table, by = "location")

# Produce summary results of scores for each team
summary_results <- results %>%
  group_by(team) %>%
  
  # Remove duplicates - each team can only score once for each location
  distinct(location, .keep_all = TRUE) %>%
  
  summarise(
    
    # Score for visiting stations (do not need photo verification)
    station_score = sum(score*(type=="station")),
    
    # Score for visiting landmarks, with photo verification
    landmark_score = sum(score*(type=="landmark")*photo_verified),
    
    # Compute bonus for using the London express trains
    train_bonus = sum(score*(type=="bonus")),
    
    # Compute total stations
    total_stations = sum(1*(type=="station")),
    
    # Flag for whether Pontefract Baghill was visited
    visited_baghill = (sum(1*(location=="Pontefract Baghill (station)"))>0),
    
    # Compute vonus for visitng 45+ stations including Baghill
    station_bonus = ((total_stations>=45) & visited_baghill)*2500,
    
    # Flag for whether a bus only landmark was visited
    visited_bus_only = (sum(1*bus_only*photo_verified)>0),
    
    # Compute penalty for not visiting bus only landmark
    bus_only_penalty = (!visited_bus_only)*-1000,
    
    # Compute total score
    total_score = station_score + landmark_score + train_bonus + station_bonus + bus_only_penalty
    )

# Format table of final scores
final_scores <- summary_results %>%
  select(team, total_score, station_score, landmark_score, train_bonus, station_bonus, bus_only_penalty, total_score) %>%
  arrange(desc(total_score))

# Plot locations visited by each team on a map

tmap_mode("view")

results %>%
  filter(type != "bonus") %>%
  st_as_sf(
    coords = c("easting", "northing"),
    crs = 27700
    ) %>%
  filter(team=="Team 5") %>%
  tm_shape() +
  tm_dots(
    fill = "team",
    size = 0.6
          ) +
  tm_basemap("OpenStreetMap")

# Plot bar chart with final scores

summary_results <- results %>%
  group_by(team) %>%
  
  # Remove duplicates - each team can only score once for each location
  distinct(location, .keep_all = TRUE) %>%
  
  summarise(
    
    # Score for visiting stations (do not need photo verification)
    station_score = sum(score*(type=="station")),
    
    # Score for visiting landmarks, with photo verification
    landmark_score = sum(score*(type=="landmark")*photo_verified),
    
    # Compute bonus for using the London express trains
    train_bonus = sum(score*(type=="bonus")),
    
    # Compute total stations
    total_stations = sum(1*(type=="station")),
    
    # Flag for whether Pontefract Baghill was visited
    visited_baghill = (sum(1*(location=="Pontefract Baghill (station)"))>0),
    
    # Compute vonus for visitng 45+ stations including Baghill
    station_bonus = ((total_stations>=45) & visited_baghill)*2500,
    
    # Flag for whether a bus only landmark was visited
    visited_bus_only = (sum(1*bus_only*photo_verified)>0),
    
    # Compute penalty for not visiting bus only landmark
    bus_only_penalty = (!visited_bus_only)*-1000,
    
    # Compute total score
    total_score = station_score + landmark_score + train_bonus + station_bonus + bus_only_penalty
    )

# Format table of final scores
final_scores <- summary_results %>%
  select(team, total_score, station_score, landmark_score, train_bonus, station_bonus, bus_only_penalty, total_score) %>%
  arrange(desc(total_score))

# Plot locations visited by each team on a map

tmap_mode("view")

results %>%
  filter(type != "bonus") %>%
  st_as_sf(
    coords = c("easting", "northing"),
    crs = 27700
    ) %>%
  #filter(team=="Team 5") %>%
  tm_shape() +
  tm_dots(
    fill = "team",
    size = 0.6
          ) +
  tm_basemap("OpenStreetMap")

# Plot bar chart with final scores

score_breakdown <- final_scores %>%
  mutate(
    bus_only = bus_only_penalty+1000
  ) %>%
  select(
    team,
    station_score,
    landmark_score,
    train_bonus,
    station_bonus,
    bus_only
  ) %>%
  pivot_longer(
    cols = -team,
    names_to = "score_type",
    values_to = "score"
  ) %>%
  mutate(
    score_type = factor(
      score_type,
      levels = c(
        "station_bonus",
        "train_bonus",
        "station_score",
        "landmark_score",
        "bus_only"
      ),
      labels = c(
        "Bonus for 45+ stations incl. Baghill",
        "Bonus for London-bound trains",
        "Score for stations",
        "Score for landmarks",
        "Avoiding bus-only penalty"
      )
    )
  )

ggplot(
  score_breakdown,
  aes(
    x = team,
    y = score,
    fill = score_type
  )
) +
  geom_col() +
  scale_y_continuous(
    labels = function(x) x - 1000,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Team",
    y = "Score",
    fill = "Score component"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Time series analysis

# Build score history through the day
score_history <- results %>%
  
  # Rename this if your timestamp column has a different name
  rename(timestamp = Timestamp) %>%
  
  # Ensure timestamps are correctly ordered
  
  arrange(team, timestamp) %>%
  group_by(team) %>%
  
  # Keep the first submission for each location
  distinct(location, .keep_all = TRUE) %>%
  
  # Points contributed by each individual submission
  mutate(
    station_points = if_else(
      type == "station",
      coalesce(score, 0),
      0
    ),
    
    landmark_points = if_else(
      type == "landmark" & photo_verified,
      coalesce(score, 0),
      0
    ),
    
    train_points = if_else(
      type == "bonus",
      coalesce(score, 0),
      0
    ),
    
    # Cumulative scoring components
    station_score = cumsum(station_points),
    landmark_score = cumsum(landmark_points),
    train_bonus = cumsum(train_points),
    
    total_stations = cumsum(type == "station"),
    
    visited_baghill = cumany(
      location == "Pontefract Baghill (station)"
    ),
    
    station_bonus = if_else(
      total_stations >= 45 & visited_baghill,
      2500,
      0
    ),
    
    visited_bus_only = cumany(
      coalesce(bus_only, FALSE) & photo_verified
    ),
    
    # Original scoring rule remains unchanged
    bus_only_penalty = if_else(
      visited_bus_only,
      0,
      -1000
    ),
    
    total_score = station_score +
      landmark_score +
      train_bonus +
      station_bonus +
      bus_only_penalty
  ) %>%
  
  ungroup()

event_start <- min(score_history$timestamp, na.rm = TRUE)

starting_scores <- score_history %>%
  distinct(team) %>%
  mutate(
    timestamp = event_start,
    total_score = -1000
  )

score_history_plot <- bind_rows(
  starting_scores,
  score_history %>% select(team, timestamp, total_score)
) %>%
  arrange(team, timestamp)

ggplot(
  score_history_plot,
  aes(
    x = timestamp,
    y = total_score,
    colour = team,
    group = team
  )
) +
  geom_step(
    linewidth = 1,
    direction = "hv"
  ) +
  scale_x_datetime(
    date_labels = "%H:%M",
    date_breaks = "1 hour"
  ) +
  labs(
    x = "Time",
    y = "Score",
    colour = "Team",
    title = "Team scores throughout the day"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

