# 10K_most_popular_songs

# Ensuring project settings are in U.S. English 
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load required packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# Setting up my working directory
setwd("C:/Users/LuisUZ/Desktop/DataAnalyticsGoogleCourse/dataCase2/csv")

# Load the required package
library(readr)

# Import the CSV file containing the 10k most popular songs from the range 1960-2023
TenK_most_popular_tracks <- read_csv("top10kTracks1960-2023.csv")

# Create a new data frame as a shallow copy of TenK_most_popular_tracks
new_tenk_songs_frame <- data.frame(TenK_most_popular_tracks)

# Load the required package
library(dplyr)
library(forcats)

# Sort the dataset in descending order of popularity
sorted_data <- TenK_most_popular_tracks %>%
  arrange(desc(Popularity))

# Show the top 50 most popular tracks (changed from 100)
top_50_popular_tracks <- head(sorted_data, 50)  # Updated to top 50

# Print the top 10 tracks
print(top_50_popular_tracks)

# Extract the year from the "Album Release Date" column
top_50_popular_tracks <- top_50_popular_tracks %>%
  mutate(Year = year(mdy(`Album Release Date`)))  # Assuming "Album Release Date" is the correct column name

# Define year intervals (e.g., every 5 years)
year_intervals <- seq(1960, 2023, by = 5)

# Create a new column for year intervals
top_50_popular_tracks <- top_50_popular_tracks %>%
  mutate(YearInterval = cut(Year, breaks = year_intervals, labels = year_intervals[-1]))  # Use [-1] to exclude the first interval

# Replace NA with ">2020-2023" using forcats::fct_explicit_na()
top_50_popular_tracks$YearInterval <- fct_explicit_na(top_50_popular_tracks$YearInterval, na_level = "2021-2023")

# Create a bar chart with year intervals
bar_chart <- ggplot(top_50_popular_tracks, aes(x = YearInterval)) +
  geom_bar(fill = "#1AA64B") +
  labs(
    title = "Count of Top 50 Most Popular Songs by Year Interval",  # Updated to top 50
    x = "Year Interval",
    y = "Count"
  ) +
  theme_minimal()

# Display the bar chart
print(bar_chart)

# Calculate the cumulative popularity scores by year
cumulative_popularity <- top_50_popular_tracks %>%
  group_by(Year) %>%
  summarise(Cumulative_Popularity = sum(Popularity))

# Create a line chart for cumulative popularity
line_chart <- ggplot(cumulative_popularity, aes(x = Year, y = Cumulative_Popularity)) +
  geom_line(color = "#1AA64B") +
  labs(
    title = "Cumulative Popularity of Top 50 Most Popular Songs Over Time",  # Updated to top 50
    x = "Year",
    y = "Cumulative Popularity"
  ) +
  theme_minimal()

# Display the line chart
print(line_chart)


# Load required packages
library(tidyverse)
library(lubridate)
install.packages("RColorBrewer")

# Filter the top 50 songs and store them in a separate data frame
top_50_popular_tracks <- head(sorted_data, 50)

# Extract the year from the "Album Release Date" column
top_50_popular_tracks <- top_50_popular_tracks %>%
  mutate(Year = year(mdy(`Album Release Date`)))  # Assuming "Album Release Date" is the correct column name

# Create a bar chart to visualize genre distribution for the top 50 songs
genre_distribution <- top_50_popular_tracks %>%
  mutate(Artist_Genres = strsplit(`Artist Genres`, ",")) %>%
  tidyr::unnest(Artist_Genres) %>%
  group_by(Artist_Genres) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))  # Sort by count in descending order

# Select the top 10 genres
top_10_genres <- genre_distribution %>%
  top_n(10, Count)

# Create the bar chart with tooltips and blue text labels for the top 10 genres
bar_chart_genre <- ggplot(top_10_genres, aes(x = reorder(Artist_Genres, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "#1AA64B") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3, color = "blue") +  # Change text color to blue
  labs(
    title = "Distribution of Top 10 Genres Among Top 50 Songs",
    x = "Genre",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Display the bar chart
print(bar_chart_genre)

str(top_50_popular_tracks)



# Calculate the average duration for each genre
genre_duration_avg <- top_50_popular_tracks %>%
  group_by(`Artist Genres`) %>%
  summarise(
    Total_Duration = sum(`Track Duration (ms)`) / 1000,  # Convert milliseconds to seconds
    Num_Tracks = n()
  ) %>%
  mutate(Avg_Duration = Total_Duration / Num_Tracks) %>%
  arrange(desc(Avg_Duration))

# Print the average duration for each genre
print(genre_duration_avg)

# Create a function to find the top N genres (excluding "pop") within a list of genres
find_top_genres <- function(genre_list, N) {
  # Count the frequency of each genre (excluding "pop")
  genre_counts <- table(genre_list[genre_list != "pop"])
  # Sort the genres by frequency in descending order
  sorted_genres <- names(sort(genre_counts, decreasing = TRUE))
  # Return the top N genres (excluding "pop")
  return(sorted_genres[1:N])
}

# Filter the dataset to include only songs with the "pop" genre
pop_songs <- top_50_popular_tracks %>%
  filter("pop" %in% `Artist Genres`)

# Find the top genres (excluding "pop") for all songs with the "pop" genre
top_pop_genres <- find_top_genres(unlist(pop_songs$`Artist Genres`), 5)

# Print the top genres (excluding "pop")
print(top_pop_genres)



# Create a function to find the top N genres (excluding "pop") within a list of genres
find_top_genres <- function(genre_list, N) {
  # Count the frequency of each genre (excluding "pop")
  genre_counts <- table(genre_list[genre_list != "pop"])
  # Sort the genres by frequency in descending order
  sorted_genres <- names(sort(genre_counts, decreasing = TRUE))
  # Return the top N genres (excluding "pop")
  return(sorted_genres[1:N])
}

# Filter the dataset to include only songs with the "pop" genre
pop_songs <- top_50_popular_tracks %>%
  filter("pop" %in% `Artist Genres`)

# Find the top genres (excluding "pop") for all songs with the "pop" genre
top_pop_genres <- find_top_genres(unlist(pop_songs$`Artist Genres`), 3)

# Print the top genres (excluding "pop")
print(top_pop_genres)


# Load required packages
library(dplyr)
library(ggplot2)

# Count the number of songs for each artist, including features, from 'top_50_popular_tracks'
artist_counts <- top_50_popular_tracks %>%
  mutate(Artist = strsplit(`Artist Name(s)`, ", ")) %>%
  unnest(Artist) %>%
  count(Artist) %>%
  arrange(desc(n))

# Create a pie chart
pie_chart <- ggplot(artist_counts, aes(x = "", y = n, fill = Artist)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convert to a pie chart
  labs(
    title = "Top Artists in the Top 50 Songs",
    fill = "Artist"
  ) +
  theme_void()  # Remove unnecessary elements

# Display the pie chart
print(pie_chart)

# Load required packages
library(dplyr)
library(ggplot2)

# Count the number of songs for each artist, including features, from 'top_50_popular_tracks'
artist_counts <- top_50_popular_tracks %>%
  mutate(Artist = strsplit(`Artist Name(s)`, ", ")) %>%
  unnest(Artist) %>%
  count(Artist) %>%
  arrange(desc(n))

# Create a bar chart
bar_chart <- ggplot(artist_counts, aes(x = reorder(Artist, -n), y = n, fill = Artist)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top Artists in the Top 50 Songs",
    x = "Artist",
    y = "Count",
    fill = "Artist"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Display the bar chart
print(bar_chart)

# Assuming you've already loaded the required packages and defined 'top_50_popular_tracks' data frame

# Count the number of songs for each artist, including features, from 'top_50_popular_tracks'
artist_counts <- top_50_popular_tracks %>%
  mutate(Artist = strsplit(`Artist Name(s)`, ", ")) %>%
  unnest(Artist) %>%
  count(Artist) %>%
  arrange(desc(n))

# Select the top 10 artists
top_10_artists <- head(artist_counts, 10)

# Create a bar chart with labels showing the count values for the top 10 artists within the bars
bar_chart <- ggplot(top_10_artists, aes(x = reorder(Artist, -n), y = n, fill = Artist)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = 2, size = 5, color = "white") +  # Add labels within the bars
  labs(
    title = "Top 10 Artists in the Top 50 Songs",
    x = "Artist",
    y = "Count",
    fill = "Artist"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Display the bar chart
print(bar_chart)


# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Sort the dataset in descending order of popularity
sorted_data <- TenK_most_popular_tracks %>%
  arrange(desc(Popularity))

# Create a new data frame with the top 100 most popular songs
top_100_popular_tracks <- head(sorted_data, 100)

# Extract the year from the "Album Release Date" column
top_100_popular_tracks <- top_100_popular_tracks %>%
  mutate(Year = as.integer(substring(`Album Release Date`, nchar(`Album Release Date`) - 3, nchar(`Album Release Date`))))  # Extract the year

# Split the "Year" column into two columns: start year and end year
top_100_popular_tracks <- top_100_popular_tracks %>%
  group_by(`Artist Name(s)`) %>%
  arrange(`Artist Name(s)`, Year) %>%
  summarise(HitCount = n(), YearStart = first(Year), YearEnd = last(Year))

# Calculate the year range as a character string
top_100_popular_tracks <- top_100_popular_tracks %>%
  mutate(
    YearRange = ifelse(YearStart == YearEnd, as.character(YearStart), 
                       paste(YearStart, "-", YearEnd))
  )

# Define a custom color palette with 34 distinct colors
custom_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5",
  "#393b79", "#e7ba52", "#ad494a", "#d6616b", "#7b4173", "#637939", "#d6616b", "#637939", "#8c6d31", "#843c39",
  "#d5a4b2", "#a55194", "#ce6dbd", "#bd9e39", "#8ca252"
)

# Create a bar chart with the extended custom color palette
bar_chart_artist_hits <- ggplot(top_100_popular_tracks, aes(x = `Artist Name(s)`, y = HitCount, fill = YearRange)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette) +  # Use the extended custom palette
  labs(
    title = "Artist Hits by Year Range (Top 100 most heard Songs)",
    x = "Artist Name(s)",
    y = "Hit Count",
    fill = "Year Range"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Display the bar chart with the extended custom color palette
print(bar_chart_artist_hits)

# Create a new data frame with the top 50 most popular songs
top_50_popular_tracks_chart <- head(sorted_data, 50)

# Extract the year from the "Album Release Date" column
top_50_popular_tracks_chart <- top_50_popular_tracks_chart %>%
  mutate(Year = as.integer(substring(`Album Release Date`, nchar(`Album Release Date`) - 3, nchar(`Album Release Date`))))  # Extract the year

# Split the "Year" column into two columns: start year and end year
top_50_popular_tracks_chart <- top_50_popular_tracks_chart %>%
  group_by(`Artist Name(s)`) %>%
  arrange(`Artist Name(s)`, Year) %>%
  summarise(HitCount = n(), YearStart = first(Year), YearEnd = last(Year))

# Calculate the year range as a character string
top_50_popular_tracks_chart <- top_50_popular_tracks_chart %>%
  mutate(
    YearRange = ifelse(YearStart == YearEnd, as.character(YearStart), 
                       paste(YearStart, "-", YearEnd))
  )

# Create a bar chart with the extended custom color palette for the top 50 most heard songs
bar_chart_artist_hits_top_50 <- ggplot(top_50_popular_tracks_chart, aes(x = `Artist Name(s)`, y = HitCount, fill = YearRange)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette) +  # Use the extended custom palette
  labs(
    title = "Artist Hits by Year Range (Top 50 most heard Songs)",
    x = "Artist Name(s)",
    y = "Hit Count",
    fill = "Year Range"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Display the bar chart with the extended custom color palette for the top 50 most heard songs
print(bar_chart_artist_hits_top_50)


# # Export the top_50_songs data frame as a CSV file
# write.csv(top_50_songs, "top_50_songs.csv", row.names = FALSE)

# Load required packages
library(dplyr)

# Sort the dataset in descending order of popularity
sorted_data <- TenK_most_popular_tracks %>%
  arrange(desc(Popularity))

# Create a new data frame with the top 1000 most popular songs
top_1000_popular_tracks <- head(sorted_data, 1000)

# Extract the year from the "Album Release Date" column
top_1000_popular_tracks <- top_1000_popular_tracks %>%
  mutate(Year = as.integer(substring(`Album Release Date`, nchar(`Album Release Date`) - 3, nchar(`Album Release Date`))))  # Extract the year

# Split the "Year" column into two columns: start year and end year
top_1000_popular_tracks <- top_1000_popular_tracks %>%
  group_by(`Artist Name(s)`) %>%
  arrange(`Artist Name(s)`, Year) %>%
  summarise(HitCount = n(), YearStart = first(Year), YearEnd = last(Year))

# Calculate the year range as a character string
top_1000_popular_tracks <- top_1000_popular_tracks %>%
  mutate(
    YearRange = ifelse(YearStart == YearEnd, as.character(YearStart), 
                       paste(YearStart, "-", YearEnd))
  )

# Sort the artists based on the length of their year ranges in descending order
top_20_artists_longest_year_ranges <- top_1000_popular_tracks %>%
  arrange(desc(YearEnd - YearStart)) %>%
  head(20)

# Print or view the resulting data frame
print(top_20_artists_longest_year_ranges)

# Load required packages
library(dplyr)

# Sort the dataset in descending order of popularity
sorted_data <- TenK_most_popular_tracks %>%
  arrange(desc(Popularity))

# Create a new data frame with the top 1000 most popular songs
top_1000_popular_tracks <- head(sorted_data, 1000)

# Extract the year from the "Album Release Date" column
top_1000_popular_tracks <- top_1000_popular_tracks %>%
  mutate(Year = as.integer(substring(`Album Release Date`, nchar(`Album Release Date`) - 3, nchar(`Album Release Date`))))  # Extract the year

# Split the "Year" column into two columns: start year and end year
top_1000_popular_tracks <- top_1000_popular_tracks %>%
  group_by(`Artist Name(s)`) %>%
  arrange(`Artist Name(s)`, Year) %>%
  summarise(HitCount = n(), YearStart = first(Year), YearEnd = last(Year), `Artist Genres` = first(`Artist Genres`))

# Calculate the year range as a character string
top_1000_popular_tracks <- top_1000_popular_tracks %>%
  mutate(
    YearRange = ifelse(YearStart == YearEnd, as.character(YearStart), 
                       paste(YearStart, "-", YearEnd))
  )

# Sort the artists based on the length of their year ranges in descending order
top_25_artists_longest_year_ranges <- top_1000_popular_tracks %>%
  arrange(desc(YearEnd - YearStart)) %>%
  head(25)

# Print or view the resulting data frame
print(top_25_artists_longest_year_ranges)

# Export the top 25 artists with the longest year ranges to a CSV file
write.csv(top_25_artists_longest_year_ranges, file = "top_25_artists_longest_year_ranges.csv", row.names = FALSE)

# Check if the CSV file has been created successfully
file.exists("top_25_artists_longest_year_ranges.csv")

#Make sure...

# Convert song duration from ms to seconds
top_50_popular_tracks$`Song Duration (s)` <- top_50_popular_tracks$`Track Duration (ms)` / 1000

# Load required packages
library(ggplot2)

# Define the green color
dot_color <- "#1AA64B"

# Create a scatterplot with larger dots and green color
scatterplot <- ggplot(top_50_popular_tracks, aes(x = Popularity, y = `Song Duration (s)`)) +
  geom_point(size = 3, color = dot_color) +  # Increase dot size and set color
  labs(
    title = "Popularity vs. Song Duration (Top 50 Songs)",
    x = "Popularity Score",
    y = "Song Duration (seconds)"
  ) +
  theme_minimal()

# Display the scatterplot
print(scatterplot)

# Export the top_50_popular_tracks
write.csv(top_50_popular_tracks, file = "top_50_popular_tracks.csv", row.names = FALSE)

# Check if the CSV file has been created successfully
file.exists("top_50_popular_tracks.csv")

install.packages("plotly")
# Load the necessary packages
library(plotly)
library(dplyr)

# Create a table with counts of songs by year
year_counts <- top_50_popular_tracks %>%
  group_by(Year) %>%
  summarise(Count = n())

# Create an interactive treemap chart with a title and subtitle as annotations
treemap_chart <- plot_ly(
  data = year_counts,
  labels = ~Year,
  parents = "",
  values = ~Count,
  text = ~paste("Count: ", Count),  # Add labels with count values
  type = "treemap",
  branchvalues = "total"  # Ensure proportional box sizes
) %>%
  layout(
    annotations = list(
      text = "Top 50 most popular songs on Spotify",  # Updated to top 50
      x = 0.5,  # Adjust the x-coordinate to center the subtitle
      y = -0.15,  # Adjust the y-coordinate to position the subtitle
      showarrow = FALSE,
      xref = "paper",
      yref = "paper",
      font = list(size = 14)  # Adjust the font size of the subtitle
    ),
    title = "Top 50 most popular tracks by Year",  # Updated to top 50
    xaxis = list(title = "Year"),
    yaxis = list(title = "Count"),
    margin = list(l = 10, r = 10, b = 10, t = 40),
    paper_bgcolor = "lightgray"
  )

# Display the treemap chart
treemap_chart

# Export year_counts data frame as a CSV file
write.csv(year_counts, "year_counts.csv", row.names = FALSE)

# Check if the CSV file has been created successfully
file.exists("year_counts.csv")

# Count the number of songs with more than one genre
songs_with_multiple_genres <- top_50_popular_tracks %>%
  filter(str_count(`Artist Genres`, ",") > 0)

# Get the count and percentage
count_of_songs <- nrow(songs_with_multiple_genres)
percentage <- (count_of_songs / 50) * 100

count_of_songs
percentage

# Load the ggplot2 library
library(ggplot2)

# Create a data frame for visualization
data <- data.frame(
  Category = c("Songs with Multiple Genres", "Songs with Single Genre"),
  Count = c(count_of_songs, 50 - count_of_songs)
)

# Create a bar chart
bar_chart <- ggplot(data, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), color = "white", vjust = 1.5, size = 5) +
  labs(title = "Songs with Multiple Genres vs. Songs with Single Genre",
       y = "Count",
       fill = "Category") +
  scale_fill_manual(values = c("#1AA64B", "#29683E")) +
  theme_minimal() +
  theme(legend.position = "none")

# Display the bar chart
print(bar_chart)






















