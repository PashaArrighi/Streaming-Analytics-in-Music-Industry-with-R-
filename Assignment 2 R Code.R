library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)

# Load the dataset
spotify_dataset <- read.csv("spotify-2023.csv")

# Display the structure of the dataset
glimpse(spotify_dataset)

# Renaming columns to more intuitive names
spotify_dataset <- spotify_dataset %>%
  rename(
    artist_name = artist.s._name,
    valence_score = valence_.,
    energy_level = energy_.,
    danceability_score = danceability_.,
    acoustic_level = acousticness_.,
    instrumental_level = instrumentalness_.,
    liveness_score = liveness_.,
    speechiness_score = speechiness_.)

# Identify unique non-numeric entries in specific columns
unique(grep("[^0-9]", spotify_dataset$in_shazam_charts, value = TRUE))
unique(grep("[^0-9]", spotify_dataset$in_deezer_playlists, value = TRUE))
unique(grep("[^0-9]", spotify_dataset$streams, value = TRUE))

# Clean data: Remove commas and convert to integers
spotify_cleaned <- spotify_dataset %>%
  mutate(
    in_shazam_charts = as.integer(gsub(",", "", in_shazam_charts)),
    in_deezer_playlists = as.integer(gsub(",", "", in_deezer_playlists)),
    streams = as.integer(gsub(",", "", streams))
  )

# Verify the structure after conversion
glimpse(spotify_cleaned)

# Re-check for unique non-numeric values
unique(grep("[^0-9]", spotify_cleaned$in_shazam_charts, value = TRUE))
unique(grep("[^0-9]", spotify_cleaned$in_deezer_playlists, value = TRUE))
unique(grep("[^0-9]", spotify_cleaned$streams, value = TRUE))

# Save the cleaned dataset to a new CSV file
write.csv(spotify_cleaned, "spotify-2023-cleaned.csv", row.names = FALSE)

# Load the cleaned data
spotify_data <- read.csv("spotify-2023-cleaned.csv")

# Visualization 1: Top 10 Artists by Total Streams
top_artists <- spotify_data %>%
  group_by(artist_name) %>%
  summarise(total_streams = sum(streams, na.rm = TRUE)) %>%
  arrange(desc(total_streams)) %>%
  top_n(10, total_streams)

ggplot(top_artists, aes(x = reorder(artist_name, total_streams), y = total_streams, fill = artist_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Artists by Total Streams", x = "Artist", y = "Total Streams") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Visualization 2: Top 10 Most Streamed Songs
top_10_songs <- spotify_data %>%
  arrange(desc(streams)) %>%
  head(10)

ggplot(top_10_songs, aes(x = reorder(track_name, streams), y = streams, fill = artist_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Streamed Songs", x = "Song Name", y = "Streams") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom")

# Visualization 3: Correlation Heatmap of Song Features
numeric_features <- spotify_data %>%
  select(valence_score, energy_level, danceability_score, acoustic_level, instrumental_level, liveness_score, speechiness_score)

correlation_matrix <- cor(numeric_features, use = "complete.obs")
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", col = brewer.pal(n = 8, name = "RdYlBu"))

# Visualization 4: Energy vs. Danceability Scatter Plot
ggplot(spotify_data, aes(x = energy_level, y = danceability_score)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  labs(title = "Energy vs. Danceability", x = "Energy Level", y = "Danceability Score") +
  theme_minimal()

# Visualization 5: Distribution of Songs by Release Year
ggplot(spotify_data, aes(x = released_year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Songs by Release Year", x = "Release Year", y = "Count of Songs") +
  theme_minimal()
