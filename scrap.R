# Loading Packages --------------------------------------------------------
library(tidyverse)
library(pdftools)
library(sf)




# This file is my messy compilation of performing the pdf reader cleaning
# tasks that later get refined in my individual files.
# While this file does not directly contribute to my final project,
# it served as a critical foundation for my refined data processing
# techniques needed to populate my dashboard.

# This file should only be read if more information is wanted. 
# Understanding this file is not critical to understanding my project.




# Geographic Information --------------------------------------------------
acton_precincts <- read_sf("data/WARDSPRECINCTS_POLY.shp") %>% 
  janitor::clean_names() %>% 
  filter(town == "ACTON")


acton_precincts %>% 
  ggplot(aes(fill = precinct)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void()




# Data Cleaning -----------------------------------------------------------
# Specific Election
pdf_text <- pdftools::pdf_text("data/Annual Town Election 3-30-2021.pdf")

strsplit(pdf_text[2], "                                            ")

pdf_text

strsplit(pdf_text, "\n\n")

sample_row <- strsplit(strsplit(pdf_text[2], "\n\n")[[1]][2], "\n")[[1]][2]
sample_row_name <- strsplit(strsplit(pdf_text[2], "\n\n")[[1]][2], "\n")[[1]][3]
sample_row_name <- strsplit(strsplit(pdf_text[1], "\n\n")[[1]][5], "\n")[[1]][5]
sample_row_name <- strsplit(strsplit(pdf_text[2], "\n\n")[[1]][5], "\n")[[1]][13]



sample_row_name

# 44 spaces after the BLANK
substr(sample_row, start = 6, stop = 49)
# 35 spaces after the name 'Bernice Baran'
substr(sample_row_name, start = 14, stop = 48)
sample_row_name

row_processed <- str_replace_all(
  str_replace_all(
    string = sample_row_name, 
    pattern = "[  -]", 
    replacement = ","
    ), 
  ",,", 
  ""
)


row_processed

if (length(str_split(row_processed,",")[[1]][1]) < 4){
  row_processed_name <- str_extract(
    row_processed,
    "[A-z]+"
  )
}else{
  print("False")
}


str_extract(
  row_processed, 
  "[A-z]+,?[A-z]\\.?,?[A-z]+?"
)


row_processed_name <- str_extract(
  row_processed, 
  "[A-z]+,?[A-z]+\\.?,?[A-z]+"
)

row_processed_name

row_processed_name %>% 
  nchar()

row_processed_values <- sample_row_name %>% 
  substr(
    start = row_processed_name %>% 
      nchar() + 2,
    stop = 1000
  ) %>% 
  str_replace_all(
    pattern = "[ -]",
    replacement = ","
  ) %>% 
  str_extract_all(
    pattern = "\\d+"
  )

row_processed_values <- str_extract_all(sample_row_name, "\\d+\\.?\\d+?")


# Each Precinct
list(row_processed_values[[1]][1:6])[[1]]


precinct_results_over <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6"
)


precinct_results_ind <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6",
  row_processed_values[[1]][1],
  row_processed_values[[1]][2],
  row_processed_values[[1]][3],
  row_processed_values[[1]][4],
  row_processed_values[[1]][5],
  row_processed_values[[1]][6]
)

precinct_results_ind <- lapply(precinct_results_ind, as.numeric) %>% 
  as_tibble()

precinct_results_over %>% 
  rbind(precinct_results_ind)


# strsplit(pdf_text, "\n\n")
# print("-------------------------------------------------")
# 
# 
# strsplit(pdf_text, "\n\n")[[1]]
# 
# length(strsplit(pdf_text, "\n\n")[[2]])
# 
# strsplit(pdf_text, "\n\n")[[2]]
# 
# 
# strsplit(pdf_text, "\n\n")[[1]][1]


pdf_text[[1]]

test_list <- c(1, 2, NA)

test_list[!is.na(test_list)]

test_list[c(2:length(test_list))]

if (is.na(any(test_list))){
  print("true")
} else{
  print("false")
}


elections_list <- list()
overall_processed_rows <- list()

precinct_results_master <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"candidate", ~"election_name"
)


for (page in seq(strsplit(pdf_text, "\n\n"))){
  page_entry_length <- length(strsplit(pdf_text, "\n\n")[[page]])
  
  # Skipping Machine Ballot counts for now
  for (election_group in seq(2, page_entry_length-1)){
    processed_rows <- list()
    
    precinct_results_over <- tribble(
      ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"candidate"
    )
    
    group <- strsplit(pdf_text, "\n\n")[[page]][election_group]

    if (group != ""){
      
      group_rows <- (strsplit(group,"\n"))[[1]]

      if (group_rows[1] == ""){
        election_name <- group_rows[c(2)]
        group_rows <- (group_rows[3:length(group_rows)])
      }else{
        election_name <- group_rows[c(1)]
        group_rows <- (group_rows[2:length(group_rows)])
      }
      
      for (row in group_rows){
        
        sample_row_name <- row
        

        row_processed <- str_replace_all(
          str_replace_all(
            string = sample_row_name,
            pattern = "[  -]",
            replacement = ","
          ),
          ",,",
          ""
        )

        
        if (length(str_split(row_processed,",")[[1]]) < 4){
          row_processed_name <- str_extract(
            row_processed,
            "[A-z]+"
          )
        }else{
          row_processed_name <- str_extract(
            row_processed, 
            "[A-z]+,?[A-z]+\\.?,?[A-z]+"
          )
          print(row_processed)
          row_processed_name <- row_processed_name[!is.na(row_processed_name)][1]
        }
        
        candidate_name <- row_processed_name
        
        row_processed_name %>%
          nchar()
        

        row_processed_values <- str_extract_all(sample_row_name, "\\d+")[[1]][1:6]
        

        precinct_results_ind <- tribble(
          ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6",
          row_processed_values[1],
          row_processed_values[2],
          row_processed_values[3],
          row_processed_values[4],
          row_processed_values[5],
          row_processed_values[6]
        )

        precinct_results_ind <- lapply(precinct_results_ind, as.numeric) %>%
          as_tibble()
        
        precinct_results_ind <- precinct_results_ind %>% 
          mutate(
            candidate = candidate_name
          )
        

        print(precinct_results_ind)
        precinct_results_over <- precinct_results_over %>%
          rbind(precinct_results_ind) %>%
          drop_na()


      }
      
    precinct_results_over <- precinct_results_over %>% 
      mutate(
        election_name = janitor::make_clean_names(election_name)
      )
    
    precinct_results_master <- precinct_results_master %>%
      rbind(precinct_results_over)

    }
    
  }
  
}


precinct_results_master %>% 
  print(n = Inf)




# Voting Times ----
other_pdf <- pdftools::pdf_text("data/Special Town Election 6-29-2021 FINAL.pdf")

other_pdf[1]

strsplit(pdf_text[1], "\n\n")[[1]][1]
strsplit(other_pdf[1], "\n\n")[[1]]



machine_time <- strsplit(strsplit(pdf_text[1], "\n\n")[[1]][1], "\n")[[1]]
machine_text <- machine_time[c(7:length(machine_time)-3)]

sample_row_t <- machine_text[1] %>% 
  str_replace(":","") %>% 
  str_extract_all("\\d+")

time_row <- sample_row_t[[1]][2:7]

time_row

precinct_results_time <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6",
  time_row[1],
  time_row[2],
  time_row[3],
  time_row[4],
  time_row[5],
  time_row[6]
)


#### Running a successful Loop

counter_time <- 8

precinct_results_time_all <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"hour"
)

for (i in seq(machine_text)){
  sample_row_t <- machine_text[i] %>% 
    str_replace(":","") %>% 
    str_extract_all("\\d+")
  
  time_row <- sample_row_t[[1]][2:7]
  
  precinct_results_time <- tribble(
    ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"hour",
    time_row[1],
    time_row[2],
    time_row[3],
    time_row[4],
    time_row[5],
    time_row[6],
    counter_time
  )
  counter_time <- counter_time + 1
  precinct_results_time_all <- precinct_results_time_all %>% 
    rbind(precinct_results_time)
}

precinct_results_time_all







# Joining Tables ----------------------------------------------------------
acton_precincts %>% 
  mutate(
    precinct = paste0("pct_", as.character(precinct))
  )

precinct_results_master %>% 
  select(election_name) %>% 
  distinct()


map_joined <- precinct_results_master %>% 
  pivot_longer(
    cols = contains("pct"),
    names_to = "precinct",
    values_to = "votes"
  ) %>% 
  left_join(
    acton_precincts %>% 
      mutate(
        precinct = paste0("pct_", as.character(precinct))
      )
  )

map_joined %>% 
  select(candidate) %>% 
  distinct()

map_joined %>% 
  filter(
    election_name == "school_committee",
    candidate == "ANDREW,SCHWARTZ"
    ) %>% 
  ggplot(aes(fill = votes)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void()



# Maps --------------------------------------------------------------------
map_joined_election_total <- map_joined %>%
  filter(
    # election_name == "school_committee",
    candidate != "TOTAL"
  ) %>% 
  select(c(candidate, election_name, precinct, votes)) %>% 
  pivot_wider(
    names_from = precinct,
    values_from = c(votes)
  ) %>% 
  mutate(
    total = pct_1 + pct_2 + pct_3 + pct_4 + pct_5 + pct_6
  ) %>% 
  group_by(election_name) %>% 
  summarize(
    election_total = sum(total)
  ) %>% 
  ungroup()



map_props <- map_joined %>% 
  left_join(map_joined_election_total) %>% 
  filter(
    # election_name == "school_committee",
    candidate != "TOTAL"
  ) %>% 
  select(c(candidate, election_name, precinct, votes, election_total)) %>% 
  pivot_wider(
    names_from = precinct,
    values_from = c(votes)
  ) %>% 
  mutate(
    cand_total = pct_1 + pct_2 + pct_3 + pct_4 + pct_5 + pct_6,
    pct_1_prop_c = pct_1/cand_total,
    pct_2_prop_c = pct_2/cand_total,
    pct_3_prop_c = pct_3/cand_total,
    pct_4_prop_c = pct_4/cand_total,
    pct_5_prop_c = pct_5/cand_total,
    pct_6_prop_c = pct_6/cand_total,
    pct_1_prop_e = pct_1/election_total,
    pct_2_prop_e = pct_2/election_total,
    pct_3_prop_e = pct_3/election_total,
    pct_4_prop_e = pct_4/election_total,
    pct_5_prop_e = pct_5/election_total,
    pct_6_prop_e = pct_6/election_total
  ) %>% 
  pivot_longer(
    cols = contains("prop"),
    names_to = "pct",
    values_to = "prop_votes"
  ) %>% 
  mutate(
    precinct = str_extract(pct, "\\d{1}"),
    
  ) %>% 
  left_join(
    acton_precincts
  )
  

map_props %>% 
  glimpse()

map_props %>% 
  filter(
    election_name == "school_committee",
    candidate == "ANDREW,SCHWARTZ",
    !(str_detect(pct, "prop_e"))
  ) %>%
  ggplot(aes(fill = prop_votes)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void() +
  scale_fill_gradient(
    low = "white",
    high = "#28632e"
  )

map_props %>% 
  filter(
    election_name == "school_committee",
    candidate == "ANDREW,SCHWARTZ",
    !(str_detect(pct, "prop_c"))
  ) %>%
  ggplot(aes(fill = prop_votes)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void() +
  scale_fill_gradient(
    low = "white",
    high = "#28632e"
  )

map_props %>% 
  filter(
    election_name == "board_of_selectmen",
    candidate == "FRANCESCA,A.,ARSENAULT",
    !(str_detect(pct, "prop_c"))
  ) %>%
  ggplot(aes(fill = prop_votes)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void() +
  scale_fill_gradient(
    low = "white",
    high = "#28632e"
  )

map_props %>% 
  filter(
    election_name == "board_of_selectmen",
    candidate == "JON,BENSON",
    !(str_detect(pct, "prop_c"))
  ) %>%
  ggplot(aes(fill = prop_votes)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void() +
  scale_fill_gradient(
    low = "white",
    high = "#28632e"
  )



#### This is a good stopping point (2023-02-05):
# I have the proportion of votes for the candidate both for 
# out of all the votes that the candidate could secure and
# out of all possible votes (within candidate versus overall).

#### Thing to do next:
## How to compare precincts
# Other than than making the map prettier (title, legend name)
# and perhaps labeling things (which precinct is which),
# I need to find a way to compare the votes by candidate versus
# votes overall (precinct 4 tends to vote the most).
# Perhaps other landmarks such as where the schools are and 
# busy locations would help conceptualize which precinct is 
# most important to capture.

## Political leaning of each precinct:
# There are republicans in Acton, and I'm curious if they tend to
# live in groups, or if they are spread out. If a progressive 
# candidate is running, they shouldn't waste time trying to convince
# people who are not aligned with them politically. 
# These elections are theoretically non-partisan, but which political
# party a resident aligns with will likely affect which candidate 
# they feel favorable towards.
# The challenge with this is that I will have to read in another .pdf
# file with its own weird formatting, so that will be copy, paste, modify.

## Saving data
# I will find a way to save my cleaned data that will be accessible in
# Tableau.





#### This is a good stopping point (2023-02-12):
# I have since sat here and fixed up my pdf cleaning scripts for each
# election (special election, presidential election 2020 and midterm 2022).
# I will look more into who votes where and what fraction of registered
# voters in a precinct vote. I need to check one more time that the data
# are loaded correctly, but things seem fine so far. I am happy with this 
# evening of work.
