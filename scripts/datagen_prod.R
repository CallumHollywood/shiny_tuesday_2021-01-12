



library(tidyverse)
library(tidytuesdayR)

tt <- tt_load('2021-01-12')

artists <- tt$artists %>% janitor::clean_names() %>% rename(artist_id = id, artist_url = url)
artwork <- tt$artwork %>% janitor::clean_names()


artwork_artist <- artwork %>% 
  left_join(artists, by = "artist_id")


artwork_artist_to_file <- artwork_artist  %>% 
  separate(place_of_birth, sep = ",", c("metropolis", 'country')) %>% 
  mutate(decade = floor(acquisition_year / 10) * 10) %>% 
  mutate(country = str_trim(country)) %>% 
  group_by(decade) %>% 
  mutate(
    avg_height = mean(height),
    avg_width = mean(width)
  ) %>% 
  select(decade, acquisition_year, artist, artist_id ,medium, gender, height, width, country, url) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) 


skimr::skim(artwork_artist_to_file)


write_csv(artwork_artist_to_file, "scripts/data/artwork_artist_to_file.csv")  
