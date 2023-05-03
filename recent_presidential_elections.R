# Loading Packages --------------------------------------------------------
library(tidyverse)




# The presidential elections require an extra layer of processing.
# I am taking the four most recent presidential elections that I
# selected and combining them onto one sheet. 
# These elections are different from local elections because there are
# national parties. It's not the best practice to do this, but I
# manually defined who the Republicans and Democrats were and then 
# matched the records to the list. 
# I made the executive decision to group all other parties other than
# the Democratic and Republican Parties to be called "Other"
# because they are not relevant if they cannot secure more than 3% of
# the national vote. In my dashboard, I only have so much room.




# Additional Data Cleaning ------------------------------------------------
# A Simple Way of getting the column names and types
example_csv <- read_csv("data/data_for_tableau/processed_state_election_2020.csv")

master_presidential_elections <- example_csv %>% 
  mutate(
    year = "placeholder"
  ) %>% 
  head(0)


# Start of for loop
list_files <- dir("data/data_for_tableau/", pattern = "state_election_\\d{4}.csv")


for (i_file in list_files){
  i_year <- str_extract(i_file, "\\d{4}")

  i_filepath <- paste0("data/data_for_tableau/", i_file)
  i_csv <- read_csv(i_filepath) %>%
    filter(
      str_detect(election_name, "esident")
    ) %>%
    mutate(
      year = i_year
    )

  master_presidential_elections <- master_presidential_elections %>%
    rbind(i_csv)
}

# This list will grow as more people run for president.
# This is the list that I'm working with.
list_repub <- c("mccain", "romney,and,ryan", "trump,and,pence")
list_dems <- c("obama", "obama,and,biden", "clinton,and,kaine", "biden,and,harris")




# Improved Cleaning for Tableau- Presidential Elections -------------------
master_presidential_elections %>%
  mutate(
    election_name = "President / Vice President",
    party = case_when(
      candidate %in% list_repub ~ "Republican",
      candidate %in% list_dems ~ "Democrat",
      candidate %in% c("blank", "blanks") ~ "Blanks",
      candidate == "scattered" ~ "Scattered",
      TRUE ~ "Other"
    ),
    candidate_name = str_sub(str_extract(
      candidate, 
      "[A-z]+,?"
    )
    ),
    candidate_name = ifelse(
      str_detect(candidate_name, ","),
      str_sub(candidate_name, end = -2L),
      str_sub(candidate_name, end = -1L)
    ),
    candidate_name = paste0(
      toupper(str_sub(candidate_name, end = 1L)),
      str_sub(candidate_name, start = 2L)
    ),
    candidate_name = ifelse(
      str_detect(candidate_name, "^Mc"),
      paste0(
        toupper(str_sub(candidate_name, end = 1L)), 
        str_sub(candidate_name, start = 2L, end = 2L),
        toupper(str_sub(candidate_name, start = 3L, end = 3L)),
        str_sub(candidate_name, start = 4L)
      ),
      paste0(toupper(str_sub(candidate_name, end = 1L)), str_sub(candidate_name, start = 2L))
    ),
    candidate_name = case_when(
      candidate == "blank" ~ "Blanks",
      TRUE ~ candidate_name
    )
  ) %>%
  select(c(candidate, candidate_name, everything())) %>% 
  write_csv("data/data_for_tableau/master/processed_presidential_elections_all.csv")