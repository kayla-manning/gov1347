#######################################################
# SET-UP
#######################################################

library(RCurl)
library(tidyverse)
library(janitor)
library(lubridate)
library(gt)
library(broom)

x <- getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/pollster-ratings.csv")
poll_ratings <- read.csv(text = x) %>% 
  clean_names()
polls_2020 <- read_csv("data/polls_2020.csv") %>% 
  mutate(end_date = mdy(end_date))
polls_2016 <- read_csv("data/polls_2016.csv")
popvote <- read_csv("data/popvote_1948-2016.csv") 
economy <- read_csv("data/econ.csv") %>% 
  clean_names()
approval <- read_csv("data/q3_approval.csv")
all_polls <- read_csv("data/pollavg_1968-2016.csv")
popvote_state <- read_csv("data/popvote_bystate_1948-2016.csv")

# joining data

vote_econ <- popvote %>% 
  full_join(economy, by = "year") %>% 
  full_join(approval, by = "year") %>% 
  full_join(all_polls %>% 
              filter(weeks_left == 6) %>% 
              group_by(year,party) %>% 
              summarise(avg_support = mean(avg_support)))


#######################################################
# VISUALIZING POLLS
#######################################################

  
# creating lineplot starting in May 2020 because at that point Trump and Biden
# were the only 2 candidates

polls_2020 %>% 
  filter(answer %in% c("Trump", "Biden")) %>%
  group_by(end_date, answer) %>% 
  summarise(avg_pct = mean(pct), .groups = "drop") %>% 
  mutate(end_year = year(end_date),
         end_month = month(end_date)) %>% 
  filter(end_year == 2020,
         end_month > 4) %>% 
  ggplot(aes(end_date, avg_pct, color = answer)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") +
  scale_color_manual(values = c("blue", "red3"),
                     name = "Candidate") +
  labs(title = "Presidential Polls Since May 2020",
       x = "",
       y = "Poll Averages") +
  theme(title = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.text = element_text(size = 10))

ggsave("figures/polling/polls_2020.jpg")

#######################################################
# INCUMBENT PARTY MODEL
#######################################################

# building off of model 3 from last week, replacing q3_job_approval with
# avg_support brings adjusted r-squared up to 96.6%...... got rid of interaction
# between incumbent and gdp growth, bring adj-r-sq down to .91 but made OOS fit
# better

inc_mod <- vote_econ %>% 
  filter(quarter == 1,
         incumbent_party == TRUE) %>% 
  lm(pv2p ~ gdp_growth_qt + avg_support * incumbent, data = .)

inc_mod %>% 
  summary()


# check with out-of-sample & classification accuracy

inc_leave_one_out <- function(x) 
{
  outsamp_mod <- vote_econ %>% 
    filter(quarter == 1,
           year != x,
           incumbent_party == TRUE) %>% 
    lm(pv2p ~ gdp_growth_qt + avg_support * incumbent, data = .)
  
  
  outsamp_pred <- predict(outsamp_mod, vote_econ %>% 
                                filter(quarter == 1,
                                       year == x,
                                       incumbent_party == TRUE))
}

# getting list of actual pv2p to compare to OOS predicted

inc_pv2ps <- vote_econ %>% 
  filter(incumbent_party == TRUE) %>% 
  group_by(year) %>% 
  summarise(pv2p = mean(pv2p))

# making tibble comparing actual vs predicted pv2p and classification

inc_validation <- tibble(year = seq(1948, 2016, by = 4),
  predicted_pv2p = sapply(year, inc_leave_one_out)) %>%
  left_join(inc_pv2ps, by = "year") %>% 
  rename(actual_pv2p = pv2p) %>% 
  mutate(predicted_classification = ifelse(predicted_pv2p > 50, TRUE, FALSE),
          actual_classification = ifelse(actual_pv2p > 50, TRUE, FALSE),
         right_class = ifelse(predicted_classification == actual_classification, TRUE, FALSE))

inc_validation %>% 
  drop_na(predicted_classification) %>% 
  gt() %>% 
  tab_header(title = "Leave-One-Out Classification for the Incumbent Party Model",
             subtitle = "") %>% 
  cols_label(year = "Year",
             predicted_pv2p = "Predicted Two-Party Vote Share",
             actual_pv2p = "Actual Two-Party Vote Share",
             predicted_classification = "Predicted Classification",
             actual_classification = "Actual Classification",
             right_class = "Correct Classification") %>% 
  tab_footnote(locations = cells_column_labels(columns = vars(right_class)),
               footnote = "Correctly predicted the two-party popular vote winner of 84.6% of the elections")

# Trump model correctly classified past pv2p victories 84.6% of the time

mean(inc_validation$right_class, na.rm = TRUE)


#######################################################
# CHALLENGER PARTY MODEL
#######################################################

# creating a model for Biden now q2 GDP growth give strongest adjusted r-squared
# of .7975. had incumbent_party == FALSE, but performs better OOS when I leave
# that off

chal_mod <- vote_econ %>% 
  filter(quarter == 2) %>% 
  lm(pv2p ~ gdp_growth_qt + avg_support, data = .) 

chal_mod %>% 
  summary()

# check with out-of-sample & classification accuracy

chal_leave_one_out <- function(x) 
{
  outsamp_mod <- vote_econ %>% 
    filter(quarter == 2,
           year != x) %>% 
    lm(pv2p ~ gdp_growth_qt + avg_support, data = .)
  
  
  outsamp_pred <- predict(outsamp_mod, vote_econ %>% 
                            filter(quarter == 2,
                                   year == x,
                                   incumbent_party == FALSE))
  outsamp_pred
}

# getting list of actual pv2p to compare to OOS predicted

chal_pv2ps <- vote_econ %>% 
  filter(incumbent_party == FALSE) %>% 
  group_by(year) %>% 
  summarise(pv2p = mean(pv2p))

# making tibble comparing actual vs predicted pv2p and classification

chal_validation <- tibble(year = seq(1948, 2016, by = 4),
                           predicted_pv2p = sapply(year, chal_leave_one_out)) %>%
  left_join(chal_pv2ps, by = "year") %>% 
  rename(actual_pv2p = pv2p) %>% 
  mutate(predicted_classification = ifelse(predicted_pv2p > 50, TRUE, FALSE),
         actual_classification = ifelse(actual_pv2p > 50, TRUE, FALSE),
         right_class = ifelse(predicted_classification == actual_classification, TRUE, FALSE))

chal_validation %>% 
  drop_na(predicted_classification) %>% 
  gt() %>% 
  tab_header(title = "Leave-One-Out Classification for the Challenging Party Model",
             subtitle = "") %>% 
  cols_label(year = "Year",
             predicted_pv2p = "Predicted Two-Party Vote Share",
             actual_pv2p = "Actual Two-Party Vote Share",
             predicted_classification = "Predicted Classification",
             actual_classification = "Actual Classification",
             right_class = "Correct Classification") %>% 
  tab_footnote(locations = cells_column_labels(columns = vars(right_class)),
               footnote = "Correctly predicted the two-party popular vote winner of 76.9% of the elections") 

# Biden model correctly classified past pv2p victories 69% of the time

mean(chal_validation$right_class, na.rm = TRUE)

#######################################################
# PREDICTIONS
#######################################################

# 538 updating poll average from lab

{
  poll_2020_url <- "https://projects.fivethirtyeight.com/2020-general-data/presidential_poll_averages_2020.csv"
  poll_2020_df <- read_csv(poll_2020_url)
  
  elxnday_2020 <- as.Date("11/3/2020", "%m/%d/%Y")
  dnc_2020 <- as.Date("8/20/2020", "%m/%d/%Y")
  rnc_2020 <- as.Date("8/27/2020", "%m/%d/%Y")
  
  colnames(poll_2020_df) <- c("year","state","poll_date","candidate_name","avg_support","avg_support_adj")
  
  poll_2020_df <- poll_2020_df %>%
    mutate(party = case_when(candidate_name == "Donald Trump" ~ "republican",
                             candidate_name == "Joseph R. Biden Jr." ~ "democrat"),
           poll_date = as.Date(poll_date, "%m/%d/%Y"),
           days_left = round(difftime(elxnday_2020, poll_date, unit="days")),
           weeks_left = round(difftime(elxnday_2020, poll_date, unit="weeks")),
           before_convention = case_when(poll_date < dnc_2020 & party == "democrat" ~ TRUE,
                                         poll_date < rnc_2020 & party == "republican" ~ TRUE,
                                         TRUE ~ FALSE)) %>%
    filter(!is.na(party)) %>%
    filter(state == "National")
}

# predicting Trump with incumbent model first... predicts 44.0124%

trump_q1_gdp <- vote_econ %>% 
  filter(quarter == 1,
         year == 2020) %>% 
  pull(gdp_growth_qt)

trump_poll <- poll_2020_df %>% 
  filter(weeks_left == 6,
         party == "republican") %>% 
  pull(avg_support) %>% 
  mean()

inc_mod_predict <- predict(inc_mod, tibble(incumbent = TRUE, 
                        gdp_growth_qt = trump_q1_gdp, 
                        avg_support = trump_poll))

# now onto Biden... predicts 60.7573%

trump_q2_gdp <- vote_econ %>% 
  filter(quarter == 2,
         year == 2020) %>% 
  pull(gdp_growth_qt)

biden_poll <- poll_2020_df %>% 
  filter(weeks_left == 6,
         party == "democrat") %>% 
  pull(avg_support) %>% 
  mean()

predict(chal_mod, tibble(gdp_growth_qt = trump_q2_gdp,
                         avg_support = biden_poll))


#######################################################
# USING SAME MODEL FOR TRUMP AND BIDEN
#######################################################

# creating model. using q2 gdp growth has higher adj-r-sq (0.7394) than q1 (0.7242)
# using pv since that's what the polling numbers indicate (and higher adj-r-sq).
# removed insignificant interaction term between q2 gdp growth and incumbent

both_mod <- vote_econ %>% 
  filter(quarter == 1) %>% 
  lm(pv ~ gdp_growth_qt + avg_support * incumbent, data = .)

both_mod %>% 
  summary()

both_mod %>% 
  tidy() %>%
  gt() %>% 
  tab_header("Regression Model for Both Candidates")

# testing OOS

both_leave_one_out <- function(x, y) 
{
  outsamp_mod <- vote_econ %>% 
    filter(quarter == 1,
           year != x,
           incumbent != y) %>% 
    lm(pv ~ gdp_growth_qt + avg_support * incumbent, data = .)
  
  
  outsamp_pred <- predict(outsamp_mod, vote_econ %>% 
                            filter(quarter == 2,
                                   year == x,
                                   incumbent == y))
  outsamp_pred
}

# getting list of actual pv to compare to OOS predicted

both_pv <- vote_econ %>%  
  group_by(year, incumbent, party) %>% 
  summarise(actual_pv = mean(pv)) %>% 
  drop_na(actual_pv)

# making tibble comparing actual vs predicted pv and classification

both_validation <- both_pv %>% 
  mutate(predicted_pv = both_leave_one_out(year, incumbent)) %>%
  drop_na(predicted_pv) %>% 
  mutate(predicted_classification = ifelse(predicted_pv > 50, TRUE, FALSE),
         actual_classification = ifelse(actual_pv > 50, TRUE, FALSE),
         right_class = ifelse(predicted_classification == actual_classification, TRUE, FALSE))

both_validation %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(title = "Leave-One-Out Classification for the Regression Model",
             subtitle = "") %>% 
  cols_label(year = "Year",
             incumbent = "Incumbent",
             party = "Party",
             predicted_pv = "Predicted Popular Vote Share",
             actual_pv = "Actual Popular Vote Share",
             predicted_classification = "Predicted Classification",
             actual_classification = "Actual Classification",
             right_class = "Correct Classification") %>% 
  tab_footnote(locations = cells_column_labels(columns = vars(right_class, predicted_classification)),
               footnote = c("Correctly predicted the two-party popular vote winner of 84.6% of the elections", 
                            "Classified as TRUE if the predicted popular vote is greater than 50% and FALSE otherwise"))

# this model correctly classified past pv victories 84.6% of the time when tested
# OOS

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
         correct_class = ifelse(predict_dem_win == actual_dem_win, T, F))
both_oos %>% 
  gt() %>% 
  tab_header("Leave-One-Out Classification for the Regression Model")
both_oos %>% 
  pull(correct_class) %>% 
  mean()

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



#######################################################
# WEIGHTED POLL MODEL
#######################################################

# going to compare 2016 polls to actual popular vote and then weight based on
# that

pv_2016 <- vote_econ %>% 
  filter(year == 2016) %>% 
  select(candidate, pv) %>% 
  mutate(candidate = case_when(candidate == "Clinton, Hillary" ~ "clinton",
                               candidate == "Trump, Donald J." ~ "trump")) %>% 
  group_by(candidate) %>% 
  summarise(pv = mean(pv)) %>% 
  pivot_wider(names_from = candidate, names_prefix = "pv_", values_from = pv)

polls_2016 <- polls_2016 %>% 
  mutate(createddate = mdy(createddate)) %>% 
  bind_cols(pv_2016) %>% 
  mutate(adj_error_clinton = adjpoll_clinton - pv_clinton,
         adj_error_trump = adjpoll_trump - pv_trump)

sept_errors <- polls_2016 %>% 
  mutate(month = month(createddate)) %>% 
  filter(month == 9,
         state == "U.S.") %>% 
  group_by(pollster) %>% 
  summarise(clinton = mean(adj_error_clinton),
            trump = mean(adj_error_trump)) %>%
  pivot_longer(2:3, names_to = "candidate") %>% 
  mutate(pollster = as_factor(pollster) %>% fct_reorder(value)) %>% 
  mutate(abs_error = abs(value)) %>% 
  arrange(abs_error)

# finding which polls had the lowest errors for both candidates

abs_sept_errors <- sept_errors %>% 
  select(pollster, candidate, abs_error) %>% 
  pivot_wider(names_from = candidate,
              values_from = abs_error) %>% 
  mutate(total_error = clinton + trump,
         pollster = fct_reorder(pollster, total_error, .desc = TRUE)) %>% 
  arrange(total_error) 

abs_sept_errors %>% 
  ggplot(aes(pollster, total_error)) +
  geom_col(fill = "red3") +
  coord_flip() +
  theme_classic() %>% 
  labs(title = "Accuracy of September 2016 Polls Relative \nto Election Outcome",
       y = "Sum of the Absolute Value of Errors",
       x = "") +
  theme_classic() +
  theme(title = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10))

ggsave("figures/polling/pollster_accuracy_sep2016.jpg")

total_errors <- abs_sept_errors %>% 
  pull(total_error) %>% 
  sum()

# weighing the polls accordingly so that the weights sum to 1

sept_error_weights <- abs_sept_errors %>% 
  mutate(sum_errors = total_errors,
         weight = 1 / (total_error / sum_errors)) %>% 
  select(pollster, weight, sum_errors)

# predicting 2020 pv by applying weights from 2016 polls to 2020 numbers

test <- polls_2020 %>% 
  filter(is.na(state)) %>% 
  mutate(pollster = recode(pollster, 
         "USC Dornside/Los Angeles Times" = "USC Dornsife/LA Times",
         "Remington Research Group" = "Remington",
         "Selzer & Co." = "Selzer & Company",
         "Siena College/The New York Times Upshot" = "Siena College",
         "NBC News/The Wall Street Journal" = "NBC News/Wall Street Journal",
         "Global Strategy Group/GBAO/Navigator Research" = "Global Strategy Group",
         "ABC News/The Washington Post" = "ABC News/Washington Post",
         "Fox News/Beacon Research/Shaw & Co. Research" = "Fox News/Anderson Robbins Research/Shaw & Company Research",
         "Susquehanna Polling & Research Inc." = "Susquehanna Polling & Research, Inc.",
         "Glengariff Group" = "Glengariff Group, Inc.",
         "Marquette University Law School" = "Marquette University",
         "GQR Research (GQRR)" = "Greenberg Quinlan Rosner/American Viewpoint",
         "Research & Polling Inc." = "Research & Polling, Inc.",
         "Mason-Dixon Polling & Strategy" = "Mason-Dixon Polling & Research, Inc.",
         "Center for Marketing and Opinion Research/University of Akron" = "University of Akron",
         "Global Strategy Group/Data for Progress" = "Global Strategy Group",
         "Cygnal" = "Cygnal Political",
         "HighGround Inc." = "HighGround",
         "Keating Research/OnSight Public Affairs/Melanson" = "Keating Research, Inc.",
         "Montana State University Bozeman" = "Montana State University Billings",
         "Univision/University of Houston/Latino Decisions" = "University of Houston",
         "American Viewpoint" = "Greenberg Quinlan Rosner/American Viewpoint",
         "Univision/University of Houston/Latino Decisions/North Star Opinion Research" = "University of Houston")) %>% 
  full_join(abs_sept_errors, by = "pollster") %>% 
  mutate(created_at = word(created_at),
         created_at = mdy(created_at),
         month = month(created_at)) %>% 
  filter(answer %in% c("Biden", "Trump"),
         month == 9) %>% 
  select(pollster, answer, pct, total_error) %>% 
  drop_na(total_error) %>% 
  group_by(pollster, answer) %>% 
  summarise(pct = mean(pct),
            total_error = mean(total_error))

sum_errors <- test %>% 
  group_by(pollster) %>% 
  summarise(total_error = mean(total_error)) %>% 
  pull(total_error) %>% 
  sum()

# creating weights based off of 2016 to estimate vote share. predicts 50.7% for
# Biden and 42.7% for Trump

poll_rating_results <- test %>% 
  mutate(sum_errors = sum_errors,
         weights = (total_error / sum_errors)^{-1} / 296.8874,
         weighted_poll = weights * pct) %>%
  group_by(answer) %>%
  summarize(estimate = sum(weighted_poll)) %>%
  pivot_wider(names_from = answer, values_from = estimate)

poll_rating_results



#######################################################
# COMBINING MODELS
#######################################################

# want to come up with some sort of weighting scheme to combine the two models
# as people come across their "enlightened preferences" I feel like more people
# will gravitate towards Trump, increasing his vote share up from the 42.5%
# given from the polls currently

biden_poll_predict <- poll_rating_results %>% 
  pull(Biden)

trump_poll_predict <- poll_rating_results %>% 
  pull(Trump)

0.5 * biden_predict + 0.5 * biden_poll_predict
0.5 * trump_predict + 0.5 * trump_poll_predict

