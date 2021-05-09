# Got all of this code from Yao's Github (https://github.com/itsyaoyu/nyt-2020-scraper)
# Loading in necessary libraries

library(rjson)
library(tidyverse)

# Pulling in the scraped data

data <- fromJSON(file = "https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/results.json")

# Getting the data for each state from the scraped data

states <- as_tibble(as_tibble(data)$data[1])

# Pulling votes for each state of the two top candidates

votes <- states %>% 
  mutate(state = map_chr(races, ~.x[["state_name"]]),
         party_1 = map_chr(races, ~.x[["candidates"]][[1]][["party_id"]]),
         votes_1 = map_dbl(races, ~.x[["candidates"]][[1]][["votes"]]),
         party_2 = map_chr(races, ~.x[["candidates"]][[2]][["party_id"]]),
         votes_2 = map_dbl(races, ~.x[["candidates"]][[2]][["votes"]]),
         total = votes_1 + votes_2) %>% 
  select(-races)

# Splitting the party for organization

first <- votes %>% 
  select(state, party_1, votes_1) %>% 
  rename(party = party_1,
         votes = votes_1)

second <- votes %>% 
  select(state, party_2, votes_2) %>% 
  rename(party = party_2,
         votes = votes_2)

# Matching the parties and then pivoting to long data

votes_clean <- rbind(first, second) %>% 
  pivot_wider(names_from = "party", values_from = "votes") %>% 
  mutate(total = republican + democrat,
         republican = republican / total * 100,
         democrat = democrat / total * 100) %>% 
  select(-total) %>% 
  pivot_longer(republican:democrat, names_to = "party", values_to = "actual_pv2p") %>% 
  mutate(state = state.abb[match(state, state.name)])

# Comparing my prediction with the scraped results

pred <- read_csv("data/election_simulation_results.csv") %>% 
  drop_na() %>% 
  group_by(state) %>% 
  mutate(d_pv2p = sim_dvotes_2020 / (sim_rvotes_2020 + sim_dvotes_2020),
         r_pv2p = 1 - d_pv2p) %>% 
  summarise(d_pv2p = mean(d_pv2p),
            r_pv2p = mean(r_pv2p),
            d_margin = d_pv2p - r_pv2p) %>% 
  select(1:3) %>% 
  pivot_longer(d_pv2p:r_pv2p, names_to = "party", values_to = "pred_pv2p") %>% 
  mutate(party = recode(party, d_pv2p = "democrat",
                        r_pv2p = "republican"),
         pred_pv2p = pred_pv2p * 100)

pred_compare <- votes_clean %>% 
  inner_join(pred, by = c("state", "party")) %>% 
  mutate(diff = actual_pv2p - pred_pv2p) %>% 
  filter(party == "democrat")

write_csv(pred_compare, "data/pred_compare.csv")

