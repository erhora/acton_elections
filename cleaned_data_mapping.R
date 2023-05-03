# Loading Packages --------------------------------------------------------
library(tidyverse)
library(sf)
library(RColorBrewer)


# This information is not present in my dashboard, but this exercise helped
# me plan which kinds of figures and information would be present in my
# dashboard.




# Geographic Information --------------------------------------------------
acton_precincts <- read_sf("data/WARDSPRECINCTS_POLY.shp") %>% 
  janitor::clean_names() %>% 
  filter(town == "ACTON")

acton_precincts %>% 
  view()


acton_precincts %>% 
  ggplot(aes(fill = precinct)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void()

precinct_results_master <- read_csv("data/data_for_tableau/processed_special_town_election_2021_06_29.csv") %>% 
  mutate(
    precinct = str_remove_all(precinct, "pct_")
  )



map_joined <- precinct_results_master %>% 
  left_join(
    acton_precincts
  )

map_joined %>% 
  view()


map_joined %>% 
  filter(
    election_name == "select_board",
    candidate == "HIMAJA,NAGIREDDY"
  ) %>% 
  ggplot(aes(fill = votes)) +
  geom_sf(aes(geometry = geometry)) +
  theme_void()


pct_registered_voters <- map_joined %>% 
  filter(
    election_name == "select_board",
    votes > 10
  ) %>% 
  ggplot(aes(fill = registered_votes_pct)) +
  geom_sf(aes(geometry = geometry)) +
  labs(
    title = "Percentage of Registered Voters Support",
    fill = "Percentage of\nRegistered Voters"
  ) + 
  scale_fill_continuous(
    high = "#132B43", 
    low = "white"
      ) +
  theme_void() +
  facet_wrap(~candidate)
pct_registered_voters

ggsave(
  pct_registered_voters,
  filename = "saved_plots/se_pct_registered_voters.png",
  dpi = 400
  )




pct_votes_cast <- map_joined %>% 
  filter(
    election_name == "select_board",
    votes > 10
  ) %>% 
  ggplot(aes(fill = cast_votes_pct)) +
  geom_sf(aes(geometry = geometry)) +
  labs(
    title = "Percentage of Votes Cast",
    fill = "Percentage of\nVotes Cast"
  ) + 
  scale_fill_continuous(
    high = "#132B43", 
    low = "white"
  ) +
  theme_void() +
  facet_wrap(~candidate)
pct_votes_cast

ggsave(
  pct_votes_cast,
  filename = "saved_plots/se_pct_votes_cast.png",
  dpi = 400
)


voter_turnout <- map_joined %>% 
  group_by(election_name, precinct) %>% 
  summarize(
    total_votes = sum(registered_votes_pct)
  ) %>% 
  inner_join(acton_precincts, by = "precinct") %>% 
  ggplot(aes(fill = total_votes)) +
  geom_sf(aes(geometry = geometry)) +
  labs(
    title = "Voter Turnout",
    fill = "Percentage of\nVotes Cast\nFrom Registered Voters"
  ) + 
  scale_fill_continuous(
    high = "#132B43", 
    low = "white"
  ) +
  theme_void()
voter_turnout

ggsave(
  voter_turnout,
  filename = "saved_plots/se_pct_voter_turnout.png",
  dpi = 400
)


precinct_results_master %>% 
  filter(
    tolower(candidate) != "blank" &
      tolower(candidate) != "scattered" &
      tolower(candidate) != "jon,benson"
  ) %>% 
  group_by(candidate) %>% 
  summarize(
    total_votes = sum(votes)
  )

1194/(759 + 1194)
