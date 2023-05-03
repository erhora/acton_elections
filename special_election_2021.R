# Loading Packages --------------------------------------------------------
library(tidyverse)
library(pdftools)
library(sf)
library(gganimate)



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
pdf_text <- pdftools::pdf_text("data/Special Town Election 6-29-2021 FINAL.pdf")


elections_list <- list()
overall_processed_rows <- list()

precinct_results_master <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"candidate", ~"election_name"
)


for (page in seq(strsplit(pdf_text, "\n\n"))){
  page_entry_length <- length(strsplit(pdf_text, "\n\n")[[page]])
  
  # Skipping Machine Ballot counts for now
  # I can get away with the seq starting at one because there is only 1 page!
  for (election_group in seq(4, page_entry_length-1)){
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
  write_csv("data/r_processed/special_town_election_2021_06_29.csv")




# Voting Times ----
other_pdf <- pdftools::pdf_text("data/Special Town Election 6-29-2021 FINAL.pdf")


machine_time <- strsplit(strsplit(pdf_text[1], "\n\n")[[1]][2], "\n")[[1]]
machine_text <- machine_time[c(6:length(machine_time)-3)]

machine_text


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



precinct_results_time_all %>% 
  write_csv("data/r_processed/special_town_election_2021_06_29_machine.csv")







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
    election_name == "select_board",
    candidate == "HIMAJA,NAGIREDDY"
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
    precinct = str_extract(pct, "\\d{1}")
  ) %>% 
  left_join(
    acton_precincts
  )


map_props %>%
  glimpse()

map_props %>%
  filter(
    election_name == "select_board",
    candidate == "HIMAJA,NAGIREDDY",
    !(str_detect(pct, "prop_e"))
  ) %>%
  ggplot() +
  geom_sf(
    aes(geometry = geometry, 
        fill = prop_votes),
    color = "#cfcfcf"
    ) +
  theme_void() +
  scale_fill_gradient(
    low = "white",
    high = "#28632e"
  ) +
  labs(
    fill = "Prop of\nCast Votes"
  )

map_props %>%
  filter(
    election_name == "select_board",
    candidate == "HIMAJA,NAGIREDDY",
    !(str_detect(pct, "prop_c"))
  ) %>%
  ggplot(aes(fill = prop_votes)) +
  geom_sf(
    aes(geometry = geometry),
    color = "#cfcfcf"
    ) +
  theme_void() +
  scale_fill_gradient(
    low = "white",
    high = "#28632e"
  ) +
  labs(
    fill = "Prop of\nCandidate\nVotes"
  )



precincts_animation <- precinct_results_time_all %>% pivot_longer(
  cols = contains("pct"),
  names_to = "precinct",
  values_to = "votes"
) %>% 
  mutate(
    precinct = str_extract(precinct, "\\d{1}"),
    votes = as.numeric(votes)
  ) %>% 
  left_join(acton_precincts) %>% 
  ggplot() + 
  geom_sf(
    aes(
      geometry = geometry,
      fill = votes
      ),
    color = "#cfcfcf"
  ) +
  theme_void() +
  scale_fill_gradient(
    low = "white",
    high = "#28632e"
  ) +
  # transition_manual() if transition_time() doesn't work
  transition_manual(
    hour
  ) +
  labs(
    caption = "Hour: {current_frame}"
  ) +
  theme(
    plot.caption = element_text(size = 30)
  )


animate(
  precincts_animation,
  fps = 10,
  res = 600,
  height = 6,
  width = 10,
  units = "in", 
  end_pause = 4
)

anim_save(
  "saved_plots/special_election_precincts_hours.gif", 
  animation = last_animation()
)