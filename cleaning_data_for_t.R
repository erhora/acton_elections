# Packages ----------------------------------------------------------------
library(tidyverse)




# In this file, I first work through an individual example to establish
# a processing procedure that I will use on other files.
# I call it a function, but I really am using a for loop (that could be 
# defined as a function later on) to mass process my files.
# I am working with cleaned data, but data that aren't ready just yet 
# for Tableau. After the loop, they are.




# Developing a Function- Doing by Hand ------------------------------------
election <- read_csv("data/r_processed/special_town_election_2021_06_29.csv") %>% 
  filter(candidate != "machine")

registered_voters <- election %>% 
  filter(
    election_name %in% c("total_registered_voters", "total_number_registered_voters", "totalregistered,voters") |
      candidate %in% c("totalregistered,voters") |
      str_detect(election_name, "registered")
    ) %>% 
  pivot_longer(
    cols = c(contains("pct_")),
    values_to = "votes",
    names_to = "precinct"
  ) %>%
  select(c(precinct, votes))




votes_cast_possible_pct <- election %>% 
  pivot_longer(
    cols = c(contains("pct_")),
    values_to = "votes",
    names_to = "precinct"
  ) %>%
  inner_join(
    registered_voters,
    by = c("precinct")
  ) %>% 
  rename(
    cast_votes = votes.x,
    total_possible_votes = votes.y
  ) %>% 
  mutate(
    registered_votes_pct = cast_votes / total_possible_votes * 100
  ) %>% 
  select(-c(cast_votes))


votes_cast_possible_pct %>% 
  view()

votes_cast_cast_pct <- election %>% 
  pivot_longer(
    cols = c(contains("pct_")),
    values_to = "votes",
    names_to = "precinct"
  ) %>% 
  group_by(election_name, precinct) %>% 
  summarize(
    total_votes_cast = sum(votes)
  ) %>% 
  left_join(
    election %>% 
      pivot_longer(
        cols = c(contains("pct_")),
        values_to = "votes",
        names_to = "precinct"
      ),
    by = c("precinct", "election_name")
  ) %>% 
  mutate(
    cast_votes_pct = votes / total_votes_cast * 100
  ) %>% 
  select(
    c(election_name, candidate, votes, total_votes_cast, precinct, cast_votes_pct)
  )


cleaned_data <- inner_join(
  election %>% 
    pivot_longer(
      cols = c(contains("pct_")),
      values_to = "votes",
      names_to = "precinct"
    ),
  votes_cast_possible_pct
) %>% 
  inner_join(
    votes_cast_cast_pct
  )


cleaned_data






# Making a Function -------------------------------------------------------
# Checking other data ----
read_csv("data/r_processed/state_election_2020_11_03.csv")
read_csv("data/r_processed/special_town_election_2021_06_29.csv")


# Updating Loop- the total number of registered voters / total registered voters.

list_files <- dir("data/r_processed/", pattern = ".csv")


for (i in list_files){
  
  input_file <- paste0("data/r_processed/", i)
  
  election <- read_csv(input_file)
  
  registered_voters <- election %>% 
    filter(
      election_name %in% c("total_registered_voters", "total_number_registered_voters", "totalregistered,voters") |
        str_detect(election_name, "registered") |
        candidate %in% c("totalregistered,voters")
    ) %>% 
    pivot_longer(
      cols = c(contains("pct_")),
      values_to = "votes",
      names_to = "precinct"
    ) %>%
    select(c(precinct, votes))
  
  
  
  
  votes_cast_possible_pct <- election %>% 
    filter(
      tolower(candidate) != "total"
    ) %>% 
    pivot_longer(
      cols = c(contains("pct_")),
      values_to = "votes",
      names_to = "precinct"
    ) %>%
    inner_join(
      registered_voters,
      by = c("precinct")
    ) %>% 
    rename(
      cast_votes = votes.x,
      total_possible_votes = votes.y
    ) %>% 
    mutate(
      registered_votes_pct = cast_votes / total_possible_votes * 100
    ) %>% 
    select(-c(cast_votes))
  
  
  
  
  votes_cast_cast_pct <- election %>%
    filter(
      tolower(candidate) != "total"
    ) %>% 
    pivot_longer(
      cols = c(contains("pct_")),
      values_to = "votes",
      names_to = "precinct"
    ) %>% 
    group_by(election_name, precinct) %>% 
    summarize(
      total_votes_cast = sum(votes)
    ) %>% 
    left_join(
      election %>% 
        pivot_longer(
          cols = c(contains("pct_")),
          values_to = "votes",
          names_to = "precinct"
        ),
      by = c("precinct", "election_name")
    ) %>% 
    mutate(
      cast_votes_pct = votes / total_votes_cast * 100
    ) %>% 
    select(
      c(election_name, candidate, votes, total_votes_cast, precinct, cast_votes_pct)
    )
  
  
  cleaned_data <- inner_join(
    election %>% 
      pivot_longer(
        cols = c(contains("pct_")),
        values_to = "votes",
        names_to = "precinct"
      ),
    votes_cast_possible_pct
  ) %>% 
    inner_join(
      votes_cast_cast_pct
    ) %>% 
    filter(
      candidate != "machine"
      # !str_detect(election_name, "preliminary")
    )
  
  
  output_file_name <- paste0("data/data_for_tableau/processed_", str_split(input_file, "/")[[1]][3])
  
  write_csv(
    cleaned_data,
    output_file_name
  )

}