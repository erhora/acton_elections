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


acton_precincts_corrected <- acton_precincts %>% 
  mutate(
    corrected_precinct = case_when(
      precinct == 1 ~ "Pct 1",
      precinct == 2 ~ "Pct 2",
      precinct == 3 ~ "Pct 3",
      precinct == 4 ~ "Pct 4",
      precinct == 5 ~ "Pct 5",
      precinct == 6 ~ "Pct 6"
    )
  ) %>% 
  st_transform(
    crs = 3395
  ) %>% 
  st_write("data/acton_precincts_corrected.shp")

# CRS code 3395


acton_precincts_7 <- st_read("data/updated_precincts/wardsprecincts2022_poly/WARDSPRECINCTS2022_POLY.shp") %>% 
  janitor::clean_names() %>% 
  filter(town == "ACTON") %>% 
  st_transform(
    crs = 3395
  ) %>% 
  st_write("data/acton_precincts_2023.shp")


acton_precincts_7 %>% 
  ggplot(aes(fill = precinct)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void()






# Data Cleaning -----------------------------------------------------------
# Specific Election
pdf_text <- pdftools::pdf_text("data/Annual Town Election 4.25.23.pdf")

pdf_text



strsplit(pdf_text[2], "\n\n\n\n")[[1]]


test_string <- "No,234"

str_detect(tolower(str_remove_all(test_string, ",")), pattern = "no\\d+")
# 
# test_string %>% 
#   str_sub(end=2L)

# str_replace_all(tolower(test_string),"xxx",replacement = "0")

elections_list <- list()
overall_processed_rows <- list()

precinct_results_master <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"pct_6a", ~"pct_7", ~"candidate", ~"election_name"
)


for (page in seq(strsplit(pdf_text, "\n\n"))){
  page_entry_length <- length(strsplit(pdf_text, "\n\n")[[page]])
  
  # Skipping Machine Ballot counts for now
  for (election_group in seq(1, page_entry_length)){
    processed_rows <- list()
    
    precinct_results_over <- tribble(
      ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"pct_6a", ~"pct_7", ~"candidate"
    )
    
    group <- strsplit(pdf_text, "\n\n")[[page]][election_group]
    
    group <- str_remove_all(group, "#")
    group <- str_remove_all(group, "-")
    # print(group)
    
    if (!is.na(group) & group != ""){
      
      group_rows <- (strsplit(group,"\n"))[[1]]
      
      if (group_rows[1] == ""){
        election_name <- group_rows[c(2)]
        group_rows <- (group_rows[3:length(group_rows)])
      }else{
        election_name <- group_rows[c(1)]
        group_rows <- (group_rows[2:length(group_rows)])
      }
      
      for (row in group_rows){
        if (!is.na(row) & row != ""){
          
          sample_row_name <- row
          
          sample_row_name <- str_replace_all(tolower(sample_row_name),"xxx",replacement = "0")
          
          
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
            # print(row_processed)
            row_processed_name <- row_processed_name[!is.na(row_processed_name)][1]
          }
          
          if (!is.na(row_processed_name)){
            candidate_name <- row_processed_name
          }
          # else{
          #   print("here")
          #   print(row_processed)
          #   candidate_name <- "no"
          else{
            # print(group)
            if(str_detect(tolower(str_remove_all(row_processed, ",")), pattern = "no\\d+")){
              candidate_name <- "no"
            }
          }
          
          
          
          # print(candidate_name)
          
          row_processed_name %>%
            nchar()
          
          
          row_processed_values <- str_extract_all(sample_row_name, "\\d+")[[1]][1:8]
          
          
          precinct_results_ind <- tribble(
            ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"pct_6a", ~"pct_7",
            row_processed_values[1],
            row_processed_values[2],
            row_processed_values[3],
            row_processed_values[4],
            row_processed_values[5],
            row_processed_values[6],
            row_processed_values[7],
            row_processed_values[8]
          )
          
          precinct_results_ind <- lapply(precinct_results_ind, as.numeric) %>%
            as_tibble()
          
          precinct_results_ind <- precinct_results_ind %>% 
            mutate(
              candidate = candidate_name
            )
          
          
          # print(precinct_results_ind)
          precinct_results_over <- precinct_results_over %>%
            rbind(precinct_results_ind) %>%
            drop_na()
          
        }
      }
      
      precinct_results_over <- precinct_results_over %>% 
        mutate(
          election_name = janitor::make_clean_names(election_name)
        )
      
      precinct_results_master <- precinct_results_master %>%
        rbind(precinct_results_over)
      
    }
    
    if(is.na(group)){
      # print("here")
    }
    
  }
  
}

precinct_results_master

# total_voters <- c()
# total_voters_row <- strsplit(pdf_text, "\n\n")[[1]][5] %>% 
#   str_remove_all("#") %>% 
#   str_remove_all("-") %>% 
#   str_split(" ")
# 
# 
# 
# for (item in total_voters_row[[1]]){
#   if (!is.na(item) & nchar(item)>=1& !(tolower(item) %in% c("total", "registered", "voters"))){
#     # print(item)
#     total_voters = append(total_voters, item)
#   }
# }
# 
# total_voters

# total_tribble <- tribble(
#   ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"candidate", ~"election_name",
#   total_voters[1],
#   total_voters[2],
#   total_voters[3],
#   total_voters[4],
#   total_voters[5],
#   total_voters[6],
#   "total registered voters", 
#   "total_registered_voters"
# )

# 
# precinct_results_master <- precinct_results_master %>%
#   rbind(total_tribble) %>% 
#   print(n = Inf)

precinct_results_master <- precinct_results_master %>% 
  rename(
    pct_6u = pct_6
  ) %>% 
  mutate(
    election_name = str_remove_all(str_remove_all(str_remove_all(election_name, "_pct_\\d+"), "total"), "a_$"),
    pct_6 = pct_6u + pct_6a
  ) %>% 
  select(
    -c(pct_6u, pct_6a) 
  ) %>% 
  select(
    c(pct_1, pct_2, pct_3, pct_4, pct_5, pct_6, everything())
  ) %>% 
  mutate(
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
)
  
precinct_results_master %>% 
  write_csv("data/r_processed/state_election_2023_04_25.csv")






read_csv("data/data_for_tableau/processed_state_election_2023_04_25.csv") %>% 
  mutate(
    precinct = as.numeric(str_replace_all(precinct, "pct_", ""))
  ) %>% 
  inner_join(
    acton_precincts_7 %>% 
      mutate(
        precinct = as.numeric(precinct)
      )
) %>% 
  mutate(
    precinct = as.character(precinct)
  ) %>% 
  st_write("data/data_for_tableau/town_election_2023_04_25_results.shp")
