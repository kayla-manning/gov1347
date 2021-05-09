#######################################################
# SET-UP
#######################################################

library(tidyverse)
library(usmap)
library(janitor)
library(readxl)
library(broom)
library(gridExtra)
library(ggpubr)
library(gt)

# funding data at state & county level

grants_state <- read_csv("data/fedgrants_bystate_1988-2008.csv")
grants_county <- read_csv("data/fedgrants_bycounty_1988-2008.csv")

# loading in Census population data with HHS COVID numbers to find spending per
# capita

state_pops_2000s <- read_excel("data/state_pops_2000s.xls", skip = 3) %>% 
  clean_names() %>% 
  rename(state = x1) %>% 
  mutate(state = gsub("\\.", "", state),
         state = state.abb[match(state, state.name)]) %>% 
  drop_na(state) %>% 
  pivot_longer(3:12, names_to = "year", values_to = "population") %>% 
  mutate(year = gsub("x", "", year),
         year = as.numeric(year)) %>% 
  select(state, year, population)

state_populations <- read_excel("data/state_populations.xlsx", skip = 3) %>% 
  clean_names() %>% 
  rename(state = x1) %>% 
  drop_na(census) %>% 
  mutate(state = gsub("\\.", "", state),
         state = state.abb[match(state, state.name)]) %>% 
  drop_na(state) %>% 
  pivot_longer(4:13, names_to = "year", values_to = "population") %>% 
  mutate(year = gsub("x", "", year),
         year = as.numeric(year)) %>% 
  select(state, year, population) %>% 
  bind_rows(state_pops_2000s)

# covid funding from HHS

covid_funding <- read_csv("data/covid_funding.csv", skip = 1) %>% 
  clean_names() %>% 
  mutate(award_amount = gsub("\\$", "", award_amount),
         award_amount = as.numeric(gsub(",", "", award_amount)),
         sign = ifelse(str_detect(award_amount, "-"), -1, 1),
         award_amount = gsub("-", "", award_amount) %>% as.numeric(),
         award_amount = award_amount * sign) %>% 
  select(-sign) %>% 
  left_join(state_populations %>% filter(year == 2019), by = "state") %>% 
  rename(covid_award = award_amount,
         pop_2019 = population)

# data for last week's "both model"

economy <- read_csv("data/econ.csv") %>% 
  clean_names()
approval <- read_csv("data/q3_approval.csv")
all_polls <- read_csv("data/pollavg_1968-2016.csv")
popvote_state <- read_csv("data/popvote_bystate_1948-2016.csv")

vote_econ <- popvote %>% 
  full_join(economy, by = "year") %>% 
  full_join(approval, by = "year") %>% 
  full_join(all_polls %>% 
              filter(weeks_left == 6) %>% 
              group_by(year,party) %>% 
              summarise(avg_support = mean(avg_support)))

# data for time-for-change model (from lab)

popvote_df <- read_csv("data/popvote_1948-2016.csv")
pvstate_df <- read_csv("data/popvote_bystate_1948-2016.csv")
economy_df <- read_csv("data/econ.csv")
approval_df <- read_csv("data/approval_gallup_1941-2020.csv")
pollstate_df <- read_csv("data/pollavg_bystate_1968-2016.csv")
fedgrants_df <- read_csv("data/fedgrants_bystate_1988-2008.csv")

#######################################################
# VISUALIZING FEDERAL SPENDING
#######################################################

# replicating plot from lab

grant_state_type <- fedgrants_state_df %>%
  filter(!is.na(state_year_type)) %>%
  group_by(state_year_type) %>%
  summarise(mean=mean(grant_mil, na.rm=T), se=sd(grant_mil, na.rm=T)/sqrt(n())) %>%
  mutate(state_year_type = case_when(state_year_type == "swing + nonelection" ~ "Swing and Nonelection",
                                     state_year_type == "swing + election year" ~ "Swing and Election",
                                     state_year_type == "core + nonelection" ~ "Core and Nonelection",
                                     state_year_type == "core + election" ~ "Core and Election")) %>% 
  ggplot(aes(x = state_year_type, y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se)) +
  geom_col(fill = "red3") +
  geom_errorbar(width = 0.2) +
  coord_flip() +
  xlab("Type of State and Year") + 
  ylab("Federal Grant Spending ($MM)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12),
        title = element_text(size = 17)) +
  labs(title = "Overall Federal Grant Spending")


# replicating, but this time with per capita spending

pc_grant_state_type <- fedgrants_state_df %>%
  inner_join(state_populations, by = c("state_abb" = "state", "year")) %>% 
  filter(!is.na(state_year_type)) %>%
  group_by(state_year_type) %>%
  summarise(mean = mean(grant_mil / population * 10^6, na.rm=T), 
            se = sd(grant_mil / population * 10^6, na.rm=T) / sqrt(n())) %>%
  mutate(state_year_type = case_when(state_year_type == "swing + nonelection" ~ "Swing and Nonelection",
                                     state_year_type == "swing + election year" ~ "Swing and Election",
                                     state_year_type == "core + nonelection" ~ "Core and Nonelection",
                                     state_year_type == "core + election" ~ "Core and Election")) %>% 
  ggplot(aes(x = state_year_type, y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se)) +
  geom_col(fill = "red3") +
  geom_errorbar(width = 0.2) +
  coord_flip() +
  xlab("Type of State and Year") + 
  ylab("Per Capita Federal Grant Spending ($)") +
  labs(title = "Per Capita Federal Grant Spending") +
  theme_minimal() +
  theme(axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12),
        title = element_text(size = 17))

ggarrange(grant_state_type, pc_grant_state_type, ncol = 1)

ggsave("figures/incumbency/grant_spend_type.jpg")

# exploring state populations

fedgrants_state_df %>% 
  inner_join(state_populations, by = c("state_abb" = "state", "year")) %>% 
  drop_na(state_year_type) %>% 
  mutate(swing_core = case_when(
    str_detect(state_year_type, "core") ~ "Core",
    str_detect(state_year_type, "swing") ~ "Swing")) %>% 
  group_by(swing_core) %>% 
  summarize(mean = mean(population), se = sd(population) / sqrt(n())) %>% 
  ggplot(aes(swing_core, mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se)) +
  geom_col(fill = "red3") +
  geom_errorbar() +
  coord_flip() +
  labs(title = "Comparing Average Populations in Swing and Core States",
       y = "Average State Population",
       x = "Type of State") +
  theme_minimal() +
  theme(axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12),
        title = element_text(size = 17))

ggsave("figures/incumbency/state_type_populations.jpg")

#######################################################
# VISUALIZING COVID-19 SPENDING
#######################################################

# wanting to look at spending in election vs non-election years
# getting data ready

x <- grants_state %>% 
  mutate(term_year = case_when(
    year %% 4 == 0 ~ 4,
    year %% 4 == 1 ~ 1,
    year %% 4 == 2 ~ 2,
    year %% 4 == 3 ~ 3),
    term_year = as_factor(term_year),
    swing_core = case_when(
      str_detect(state_year_type, "core") ~ "core",
      str_detect(state_year_type, "swing") ~ "swing")) %>% 
  left_join(state_populations, by = c("state_abb" = "state", "year"))

# want to make a heat map of spending in states

spending_2008 <- x %>% 
  filter(year == 2008) %>% 
  group_by(state_abb) %>% 
  summarise(avg_spending = mean(grant_mil)) %>% 
  ungroup() %>%  
  select(state_abb, avg_spending) %>% 
  rename(state = state_abb)

# heat map of covid spending... load in correct data

state_covid <- covid_funding %>% 
  group_by(state, pop_2019) %>% 
  summarise(total_spending = sum(covid_award)) %>%
  drop_na(state) %>% 
  mutate(covid_pc_spending = total_spending / pop_2019) %>% 
  filter(state != "AK")

plot_usmap(data = state_covid, values = "covid_pc_spending", labels = TRUE) +
  scale_fill_gradient(
    high = "red3",
    low = "white",
    name = "COVID-19 Aid \nPer Capita ($)"
  ) +
  theme_void() +
  labs(title = "COVID-19 Awards by State")

# isolating core vs swing states

states_type_2008 <- x %>% 
  filter(year == 2008) %>% 
  select(state_abb, swing_core) %>% 
  drop_na(swing_core)

swing_2008 <- states_type_2008 %>% 
  filter(swing_core == "swing") %>% 
  pull(state_abb)

core_2008 <- states_type_2008 %>% 
  filter(swing_core == "core",
         state_abb != "AK") %>% 
  pull(state_abb)

covid_swing <- plot_usmap(data = state_covid, values = "covid_pc_spending", labels = TRUE,
           include = swing_2008) +
  scale_fill_gradient(
    high = "red3",
    low = "white",
    name = "COVID-19 Aid \nPer Capita ($)",
    limits = c(40, 170)
  ) +
  theme_void() +
  labs(title = "Swing States")

covid_core <- plot_usmap(data = state_covid, values = "covid_pc_spending", labels = TRUE,
           include = core_2008) +
  scale_fill_gradient(
    high = "red3",
    low = "white",
    name = "COVID-19 Aid \nPer Capita",
    limits = c(40, 170)
  ) +
  theme_void() +
  labs(title = "Core States")

ggarrange(covid_swing, covid_core, common.legend = TRUE, legend = "right", ncol = 1)

ggsave("figures/incumbency/covid_type_aid.jpg")


# per capita spending for swing/core states

x %>% 
  inner_join(state_covid, by = c("state_abb" = "state")) %>% 
  group_by(swing_core) %>% 
  summarise(avg_covid_pc_spending = mean(covid_pc_spending),
            se = sd(covid_pc_spending) / sqrt(n()),
            conf_low = avg_covid_pc_spending - 1.96 * se,
            conf_high = avg_covid_pc_spending + 1.96 * se) %>% 
  drop_na(swing_core) %>% 
  ggplot(aes(swing_core, avg_covid_pc_spending)) +
  geom_col(position = "dodge", fill = "red3") +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
  theme_classic() +
  labs(title = "Comparing COVID-19 Grants in Core and Swing States",
       x = "Type of State",
       y = "Average Per Capita Federal Grant Spending ($)",
       fill = "Year of Term") +
  scale_x_discrete(labels = c("Core", "Swing")) +
  coord_flip()
  


#######################################################
# COMPARING MODELS, BOTH_MOD VS TIME-FOR-CHANGE
#######################################################

# last week's model

both_mod <- vote_econ %>% 
  filter(quarter == 1) %>% 
  lm(pv ~ gdp_growth_qt + avg_support * incumbent, data = .)

both_mod %>% 
  summary()

# OOS validation was at 84.6% proper classification last week

both_oos <- both_pv %>% 
  mutate(predicted_pv = both_leave_one_out(year, incumbent)) %>%
  drop_na(predicted_pv) %>% 
  pivot_wider(names_from = party, values_from = c(actual_pv, predicted_pv)) %>% 
  group_by(year) %>% 
  summarise(actual_pv_democrat = mean(actual_pv_democrat, na.rm = T),
            actual_pv_republican = mean(actual_pv_republican, na.rm = T),
            predicted_pv_democrat = mean(predicted_pv_democrat, na.rm = T),
            predicted_pv_republican = mean(predicted_pv_republican, na.rm = T)) %>% 
  mutate(predict_dem_win = ifelse(predicted_pv_democrat > predicted_pv_republican, T, F),
         actual_dem_win = ifelse(actual_pv_democrat > actual_pv_republican, T, F),
         correct_class = ifelse(predict_dem_win == actual_dem_win, T, F)) %>% 
  pull(correct_class) %>% 
  mean()
both_cross_val <- both_oos

# making predictions with the both model
# put Biden at receiving ~51.2% and Trump ~47.6%

biden_predict <- predict(both_mod, tibble(gdp_growth_qt = trump_q1_gdp,
                                          avg_support = biden_poll,
                                          incumbent = FALSE))
biden_predict


trump_predict <- predict(both_mod, tibble(gdp_growth_qt = trump_q1_gdp,
                                          avg_support = trump_poll,
                                          incumbent = TRUE))
trump_predict

both_prediction <- trump_predict / (trump_predict + biden_predict)

#######################################################

# time-for-change model... getting data ready

tfc_df <- popvote_df %>%
  filter(incumbent_party) %>%
  select(year, candidate, party, pv, pv2p, incumbent) %>%
  inner_join(
    approval_df %>% 
      group_by(year, president) %>% 
      slice(1) %>% 
      mutate(net_approve = approve - disapprove) %>%
      select(year, incumbent_pres = president, net_approve, poll_enddate),
    by="year"
  ) %>%
  inner_join(
    economy_df %>%
      filter(quarter == 2) %>%
      select(gdp_growth_qt, year),
    by="year"
  ) %>% 
  clean_names()

# fitting model, has adj. r-squared of 61.7%

tfc_mod <- tfc_df %>% 
  lm(pv2p ~ gdp_growth_qt + net_approve + incumbent, data = .)
tfc_mod %>% 
  summary()

tfc_mod %>% 
  tidy() %>% 
  gt() %>% 
  tab_header(title = "Time for Change Regression Output")

# evaluating OOS fit

tfc_leave_one_out <- function(x, y)
{
  outsamp_mod <- tfc_df %>% 
    filter(year != x,
           incumbent != y) %>% 
    lm(pv2p ~ gdp_growth_qt + net_approve + incumbent, data = .)
  
  outsamp_pred <- predict(outsamp_mod, tfc_df %>% 
                            filter(year == x,
                                   incumbent == y))
  outsamp_pred
}

tfc_pv2p <- tfc_df %>%  
  group_by(year, incumbent, party) %>% 
  summarise(actual_pv2p = mean(pv2p)) %>% 
  drop_na(actual_pv2p)

# making tibble comparing actual vs predicted pv and classification

tfc_validation <- tfc_pv2p %>% 
  mutate(predicted_pv2p = tfc_leave_one_out(year, incumbent)) %>%
  drop_na(predicted_pv2p) %>% 
  mutate(predicted_classification = ifelse(predicted_pv2p > 50, TRUE, FALSE),
         actual_classification = ifelse(actual_pv2p > 50, TRUE, FALSE),
         right_class = ifelse(predicted_classification == actual_classification, TRUE, FALSE))
tfc_validation %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(title = "Leave-One-Out Classification for Time for Change Model",
             subtitle = "") %>% 
  cols_label(year = "Year",
             incumbent = "Incumbent",
             party = "Party",
             predicted_pv2p = "Predicted Two-Party Vote Share",
             actual_pv2p = "Actual Two-Party Vote Share",
             predicted_classification = "Predicted Classification",
             actual_classification = "Actual Classification",
             right_class = "Correct Classification") %>% 
  tab_footnote(locations = cells_column_labels(columns = vars(right_class, predicted_classification)),
               footnote = c("Correctly predicted the two-party popular vote winner of 66.6% of the elections",
                            "Classified as TRUE if the predicted two-party popular vote is greater than 50% and FALSE otherwise")) 

# correctly classified 66.7% (2/3) of past elections -- important to note that
# this model works for incumbent party only

tfc_cross_val <- mean(tfc_validation$right_class)

q2_gdp_growth <- vote_econ %>% 
  filter(year == 2020,
         quarter == 2) %>% 
  pull(gdp_growth_qt)

# getting Trump's net approval by following the same procedure that the model
# was fit with (grouping by year/president and slicing the top row)

trump_net_approval <- approval_df %>%
  filter(president == "Donald Trump", year == 2020) %>% 
  group_by(year, president) %>% 
  slice(1) %>% 
  mutate(net_approval = approve - disapprove) %>% 
  pull(net_approval)

tfc_prediction <- predict(tfc_mod, tibble(gdp_growth_qt = q2_gdp_growth, 
                        net_approve = trump_net_approval,
                        incumbent = TRUE))

#######################################################

# comparing the models

model_names <- c("Regression from Blog Post #3", "Time for Change")
variables <- c("pv ~ gdp_growth_qt + avg_support * incumbent", 
               "pv2p ~ gdp_growth_qt + net_approve + incumbent")
cross_val <- c(both_cross_val %>% round(3), tfc_cross_val %>% round(3))
adj_r_sq <- c(summary(both_mod)$adj.r.squared %>% round(3), 
              summary(tfc_mod)$adj.r.squared %>% round(3))
predictions <- c(paste0(round(both_prediction * 100, 3), "%"), 
                 paste0(tfc_prediction %>% round(3), "%"))

tibble(Model = model_names, 
       Predictors = variables, 
       "Out-of-Sample Performance" = cross_val,
       "Adjusted R-Squared" = adj_r_sq,
       "Trump's Predicted Two-Party Vote Share" = predictions) %>%
  gt() %>% 
  tab_header("Model Comparison") %>% 
  tab_footnote(footnote = "The percentage of times the model correctly classified the winner of the two-party vote share in a leave-one-out cross-validation",
               locations = cells_column_labels(columns = 
                                                 vars("Out-of-Sample Performance"))) %>% 
  tab_footnote(footnote = "The regression from blog post #3 predicted the overall vote share for both candidates, so I used this to calculate Trump's two-party vote share",
               locations = cells_column_labels(columns = vars("Trump's Predicted Two-Party Vote Share"))) %>% 
  tab_footnote(footnote = "My regression from blog post #3 uses Q1 GDP growth, while the Time for Change model uses Q2 GDP growth",
               locations = cells_column_labels(columns = vars(Predictors))) %>% 
  cols_align("left")
  

