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





# Data Cleaning -----------------------------------------------------------
# Specific Election
pdf_text <- pdftools::pdf_text("data/State Election 11-8-22 Final v1.pdf")

pdf_text
strsplit(pdf_text[1], "\n\n")
strsplit(pdf_text[3], "\n\n")[[1]]
strsplit(strsplit(pdf_text[3], "\n\n")[[1]][5], "\n")[[1]]
sample_processed <- str_replace_all(strsplit(strsplit(pdf_text[5], "\n\n")[[1]][3], "\n")[[1]][1], "\\s+", ",")
rev(str_split(sample_processed,",")[[1]])
print("PCT" %in% strsplit(sample_processed, ",")[[1]])

pdf_text[4]

elections_list <- list()
overall_processed_rows <- list()

precinct_results_master <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"pct_6a", ~"pct_7", ~"candidate", ~"election_name"
)

test_list <- c("9:00", "am", "20")


for (page in seq(strsplit(pdf_text, "\n\n"))){
  page_entry_length <- length(strsplit(pdf_text, "\n\n")[[page]])
  
  # Skipping Machine Ballot counts for now
  for (election_group in seq(1, page_entry_length-1)){
    processed_rows <- list()
    
    precinct_results_over <- tribble(
      ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"pct_6a", ~"pct_7", ~"candidate"
    )
    
    group <- strsplit(pdf_text, "\n\n")[[page]][election_group]
    
    # print(group)
    if (
      !is.na(group) & 
      group != ""
      ){
      
      group_rows <- (strsplit(group,"\n"))[[1]]

      if (group_rows[1] == ""){
        election_name <- group_rows[c(2)]
        group_rows <- (group_rows[3:length(group_rows)])
      }else{
        election_name <- group_rows[c(1)]
        group_rows <- (group_rows[2:length(group_rows)])
      }
      # print("^^^^^^^^^^")
      # # print()
      # print(election_name)
      # print(group_rows)
      # print(!"PCT" %in% (str_split(str_replace_all(group_rows, "\\s+", ","), ",")[[1]]))
      # print(str_split(str_replace_all(group_rows, "\\s+", ","), ",")[[1]])
      # print("$$")
      # print("^^^^^^^^^^")
      
      # if (!is.na(group_rows[1])
      #     # (("acton" %in% str_split(row_processed,",")[[1]]))
      #     ){
      #   heading_issue <- 1
      #   print(group_rows[1])
      #   print("heading_isse")
      #   print(row_processed_name)
      # }else{
      #   heading_issue <- 0
      # }
      # print("")
      # print(heading_issue)
      # print("")
      
      for (row in group_rows){
        print(row)
        print(str_split(str_replace_all(row, "\\s+", ","), ",")[[1]])
        if (!is.na(row) & 
            row != "" & 
            !("PCT" %in% str_split(str_replace_all(row, "\\s+", ","), ",")[[1]])
            ){
        
          sample_row_name <- row
          
          sample_row_name <- str_replace_all(tolower(sample_row_name),"xxx",replacement = "0")
          
          # print(sample_row_name)
          # print("-----------")
          
          # row_processed <- str_replace_all(
          #   str_replace_all(
          #     string = sample_row_name,
          #     pattern = "[  -]",
          #     replacement = ","
          #   ),
          #   ",,",
          #   ""
          # )
          row_processed <- str_replace_all(sample_row_name, "\\s+", ",")
          # print("%%%%%")
          # print(row_processed)
          # print("%%%%%")
  
          # print("*****************")
          # print(sample_row_name)
          # print(row_processed)
          # print(str_split(row_processed,",")[[1]])
          # print(!("acton" %in% str_split(row_processed,",")[[1]]))
          # print(!("machine" %in% str_split(row_processed,",")[[1]]))
          # print(!("am" %in% str_split(row_processed,",")[[1]]))
          # print(!("pm" %in% str_split(row_processed,",")[[1]]))
          # print("*****************")
          
          # print(str_split(row_processed,",")[[1]])
          # print("~~~~~~~~~")
          
          print(row_processed)
          if (!is.na(row_processed) & 
              (!("machine" %in% str_split(row_processed,",")[[1]])) &
              (!("acton" %in% str_split(row_processed,",")[[1]])) &
              (!("am" %in% str_split(row_processed,",")[[1]])) &
              (!("pm" %in% str_split(row_processed,",")[[1]])) &
              (!("totals" %in% str_split(row_processed,",")[[1]])) &
              (!("pct" %in% str_split(row_processed,",")[[1]]))
              ){
            # print("no machines")
          
            row_processed <- rev(str_split(row_processed,",")[[1]])
            
            # print(row_processed)
            
            if (length(tail(str_split(row_processed,",")[[1]], 1)) < 4){
              row_processed_name <- str_extract(
                tail(row_processed, 1),
                "[A-z]+"
              )
            }else{
              row_processed_name <- str_extract(
                tail(row_processed, 1), 
                "[A-z]+,?[A-z]+\\.?,?[A-z]+"
              )
    
              row_processed_name <- row_processed_name[!is.na(row_processed_name)][1]
            }
            # print("-----")
            # print(row_processed)
            if("overseas" %in% row_processed){
              row_processed_name <- "overseas"
            }
            
            if("hand" %in% row_processed){
              row_processed_name <- "hand_count"
            }
            
            # print("*")
            # print(rev(row_processed))
            # print(row_processed_name)
            # print(heading_issue)
            # if(heading_issue == 1){
            #   print("heading issue on our hands")
            #   print(paste(rev(row_processed), collapse = ""))
            # }
            # print("*")
            if (!is.na(row_processed_name)){
              candidate_name <- row_processed_name
            }
            # else{
            #   print("here")
            #   print(row_processed)
            #   candidate_name <- "no"
            else{
              # print(group)
              # print("error here")
              # print(row_processed)
              # print("^^^^")
              # print(length(row_processed))
              if (length(row_processed) > 5){
                # print("still here")
                if(str_detect(tolower(str_remove_all(row_processed, ",")), pattern = "no\\d+")){
                  candidate_name <- "no"
                }
              }else{
                "nothing here"
              }
            }
          
            
            # print(sample_row_name)
            # print(row_processed)
            # print(row_processed_name)
            # print("====================")
          # print("-------------")
          # print(candidate_name)
          # print("-------------")
          
          row_processed_name %>%
            nchar()
          
          
          row_processed_values <- str_extract_all(sample_row_name, "\\d+")[[1]]
          
          print(length(row_processed_values))
          print(typeof(row_processed_values))
          print(row_processed_values)
          
          if (
            length(row_processed_values > 5) &
            length(row_processed_values < 13)
            ){
          
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
          
          # print(election_name)
          
          precinct_results_ind <- precinct_results_ind %>% 
            mutate(
              candidate = candidate_name,
              election_name = election_name
            )
          print(precinct_results_ind)
          
          
          # print(precinct_results_ind)
          precinct_results_over <- precinct_results_over %>%
            rbind(precinct_results_ind) %>%
            drop_na()
          
          }else{
          print("here i am once again")
          election_name <- group_rows[1]
        }
        
        # precinct_results_over <- precinct_results_over %>%
          
  
        precinct_results_master <- precinct_results_master %>%
          rbind(precinct_results_over)
          }
        # print("=========================================")
      
        }
      }
    
    if(is.na(group)){
      # print("here")
    }
      
    }else{
      print("finally here")
    }
    
    
    
  }
  
  
}


precinct_results_master <- precinct_results_master %>%
  filter(candidate != "machine") %>%
  distinct(pct_1, pct_2, candidate, .keep_all = TRUE) %>% 
  mutate(
    election_name = str_remove_all(janitor::make_clean_names(election_name), "_\\d+")
  ) %>%
  print(n = Inf)

precinct_results_master %>% 
  write_csv("data/r_processed/state_election_2022_11_08.csv")
# 
# 
# precinct_results_time_all <- precinct_results_master %>% 
#   filter(candidate == "machine") %>% 
#   mutate(
#     election_name = "tallies"
#     )
# 
# precinct_results_master
# precinct_results_time_all
# 
# 



# Voting Times ----
other_pdf <- pdftools::pdf_text("data/State Election 11-8-22 Final v1.pdf")


machine_time <- strsplit(strsplit(pdf_text[1], "\n\n")[[1]][3], "\n")[[1]]
machine_text <- machine_time[c(6:length(machine_time)-4)]

machine_text


#### Running a successful Loop

counter_time <- 8

precinct_results_time_all <- tribble(
  ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"pct_6a", ~"pct_7", ~"hour"
)

for (i in seq(machine_text)){
  sample_row_t <- machine_text[i] %>% 
    str_replace(":","") %>% 
    str_extract_all("\\d+")
  
  time_row <- sample_row_t[[1]]
  print(time_row)
  
  precinct_results_time <- tribble(
    ~"pct_1", ~"pct_2", ~"pct_3", ~"pct_4", ~"pct_5", ~"pct_6", ~"pct_6a", ~"pct_7", ~"hour",
    time_row[2],
    time_row[3],
    time_row[4],
    time_row[5],
    time_row[6],
    time_row[7],
    time_row[8],
    time_row[9],
    counter_time
  )
  counter_time <- counter_time + 1
  precinct_results_time_all <- precinct_results_time_all %>% 
    rbind(precinct_results_time)
}

precinct_results_time_all %>% 
  write_csv("data/r_processed/state_election_2022_11_08_machine.csv")