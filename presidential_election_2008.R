# Loading Packages --------------------------------------------------------
library(tidyverse)
library(pdftools)
library(sf)
library(gganimate)



# Geographic Information --------------------------------------------------
acton_precincts <- read_sf("data/wardsprecincts2022_poly/WARDSPRECINCTS2022_POLY.shp") %>% 
  janitor::clean_names() %>% 
  filter(town == "ACTON")


acton_precincts %>% 
  ggplot(aes(fill = precinct)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void()




# Data Cleaning -----------------------------------------------------------
# Specific Election
pdf_text <- pdftools::pdf_text("data/Pres Elect 11-4-08 FINAL.pdf")

pdf_text


# Determining separator between election races
strsplit(pdf_text[1], "\n\n\n")[[1]][4]


# Figuring out parsing challenges
test_string <- "No,234"

str_detect(tolower(str_remove_all(test_string, ",")), pattern = "no\\d+")

test_string %>%
  str_sub(end=2L)

str_replace_all(tolower(test_string),"xxx",replacement = "0")




elections_list <- list()
overall_processed_rows <- list()

precinct_results_master <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"candidate", ~"election_name"
)


for (page in seq(strsplit(pdf_text, "\n\n\n"))){
  page_entry_length <- length(strsplit(pdf_text, "\n\n\n")[[page]])
  
  # Skipping Machine Ballot counts for now
  for (election_group in seq(1, page_entry_length)){
    processed_rows <- list()
    
    precinct_results_over <- tribble(
      ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"candidate"
    )
    
    group <- strsplit(pdf_text, "\n\n\n")[[page]][election_group]
    
    group <- str_remove_all(group, "#")
    group <- str_remove_all(group, "-")

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
          row_processed_name <- row_processed_name[!is.na(row_processed_name)][1]
        }
        
        if (!is.na(row_processed_name)){
          candidate_name <- row_processed_name
        }
        else{
          if(str_detect(tolower(str_remove_all(row_processed, ",")), pattern = "no\\d+")){
            candidate_name <- "no"
          }
        }
        
        
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
    }
    
  }
  
}



precinct_results_master %>% 
  write_csv("data/r_processed/state_election_2008.csv")