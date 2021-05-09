
# setup

# loading packages that I need for this model

library(tidyverse)
library(ggplot2)
library(janitor)
library(GGally)
library(broom)
library(purrr)
library(gt)
library(googlesheets4)
library(lubridate)
library(magrittr)

# uploading data from Canvas

economy <- read_csv("data/econ.csv") %>% 
  clean_names()
local <- read_csv("data/local.csv") %>% 
  clean_names()
popvote <- read_csv("data/popvote_1948-2016.csv")
popvote_state <- read_csv("data/popvote_bystate_1948-2016.csv") %>% 
  clean_names()

# getting google sheets data on presidential job approval. beginning by creating an
# empty dataframe

approval <- tibble(start_date = as_datetime(1/1/1000), end_date = as_datetime(1/1/1000), 
                   approving = as.double(1), disapproving = as.double(1), 
                   unsure_no_data = as.double(1))

# writing a function to read each sheet and add onto the empty sheet

for (i in 1:13) 
{
  president <- read_sheet("https://docs.google.com/spreadsheets/d/1iEl565M1mICTubTtoxXMdxzaHzAcPTnb3kpRndsrfyY/edit?ts=5bd7f609#gid=1067240630",
                          sheet = i,
                          col_types = "TTddd") %>% 
    clean_names()
  approval <- rbind(approval, president)
}

approval <- approval %>% 
  slice(-1)

# filtering for and taking average of Q3 in election years because marks end of
# Q3 and numbers closest to current election

approval <- approval %>% 
  arrange(desc(start_date)) %>% 
  mutate(year = year(end_date),
         month = month(end_date)) %>% 
  filter(month %in% 7:9,
         year %in% seq(1948, 2020, by = 4)) %>% 
  group_by(year) %>% 
  summarise(q3_job_approval = mean(approving))

# writing this into a csv for future reference

write_csv(approval, "data/q3_approval.csv")
  

# joining national data together & state data together

national <- economy %>% 
  left_join(popvote, by = "year") %>% 
  left_join(approval, by = "year")

incumbents <- national %>% 
  select(year, party, incumbent, incumbent_party, gdp_growth_qt, quarter)

state <- local %>% 
  left_join(popvote_state, by = c("year", "state_and_area" = "state")) %>% 
  filter(!str_detect(state_and_area, "County|city")) %>% 
  select(-1) %>% 
  pivot_longer(cols = c(d_pv2p, r_pv2p), names_to = "party", values_to = "pv2p") %>% 
  mutate(party = recode(party, "d_pv2p" = "democrat", "r_pv2p" = "republican"),
         quarter = case_when(month %in% c("01", "02", "03") ~ 1,
                             month %in% c("04", "05", "06") ~ 2,
                             month %in% c("07", "08", "09") ~ 3,
                             month %in% c("10", "11", "12") ~ 4)) %>% 
  full_join(incumbents, by = c("year", "party", "quarter"), suffix = c("_state", "_national")) %>% 
  rename(state = state_and_area) %>% 
  drop_na(state)


# exploring variables that might be good predictors Q2 gdp_growth_qt,
# gdp_growth_yr, rdi_growth, and q3 job approval have the strongest correlation
# with incumbent vote share

national %>% 
  filter(incumbent_party = TRUE,
         quarter == 2) %>% 
  select(pv2p, gdp_growth_qt, gdp_growth_yr, rdi_growth, inflation, unemployment) %>% 
  ggpairs()


#############################################################################################################

###### MODEL 1 ######

# adding incumbent and incumbent party to the example from lab shows that
# sitting presidents do better

# adjusted r-squared of 0.383

model1 <- national %>% 
  filter(quarter == 2,
         incumbent_party == TRUE) %>% 
  lm(pv2p ~ gdp_growth_qt * incumbent, data = .)

model1_arsq <- model1 %>% 
  glance() %>% 
  pull(adj.r.squared)

# want to save this regression table to link to in the blog post

model1 %>% 
  tidy() %>% 
  gt() %>% 
  tab_header("Linear Regression Relating Incumbency and Q2 GDP Growth Rate with Incumbent Party Two-Party Vote Share") %>% 
  gtsave("../figures/economy/inc_gdp_reg.html")

# predicting Trump's vote share using this model.... only 18.4% so not very realistic

q2_2020_gdp <- national %>% 
  filter(year == 2020,
         quarter == 2) %>% 
  pull(gdp_growth_qt)

model1_prediction <- predict(model1, tibble(gdp_growth_qt = q2_2020_gdp,
                               incumbent = TRUE))

# going to make a model excluding a single year and then test the fit using
# leave-one-out cross validation

leave_out_one_nat <- function(x) {
  
outsamp_inc_mod <- national %>% 
  filter(quarter == 2,
         year != x,
         incumbent_party == TRUE) %>% 
  lm(pv2p ~ gdp_growth_qt * incumbent, data = .)


outsamp_inc_pred <- predict(outsamp_inc_mod, national %>% 
                              filter(quarter == 2,
                                     year == x,
                                     incumbent_party == TRUE))

(national %>% filter(year == x, quarter == 2, incumbent_party == TRUE) %>% pull(pv2p)) - outsamp_inc_pred

}

residuals <- tibble(year = seq(1948, 2016, by = 4), residuals = sapply(year, leave_out_one_nat))

ggplot(residuals, aes(year, residuals)) +
  geom_point() +
  theme_classic() +
  geom_hline(yintercept = 0, color = "red3") +
  labs(title = "Out-of-Sample Residuals for Model 1", x = "Year", y = "Residuals")

ggsave("figures/economy/inc_gdp_resid.jpg", width = 7, height = 4)

# graphing pv2p ~ gdp_growth_qt, faceting by incumbent and incumbent party

# facet labels for incumbent party
incumbent_party_labs <- c("Incumbent Party", "Non-Incumbent Party")
names(incumbent_party_labs) <- (c(TRUE, FALSE))

# facet labels for incumbent candidate
incumbent_labs <- c("Incumbent Candidate", "Non-Incumbent Candidate")
names(incumbent_labs) <- (c(TRUE, FALSE))

national %>% 
  drop_na(incumbent, incumbent_party) %>% 
  filter(quarter == 2,
         !(incumbent == TRUE & incumbent_party == FALSE)) %>% 
  ggplot(aes(gdp_growth_qt, pv2p)) +
  geom_point() +
  facet_wrap(~ incumbent_party + incumbent,
             labeller = labeller(incumbent_party = incumbent_party_labs,
                                 incumbent = incumbent_labs)) +
  xlim(c(-2.5, 2.5)) +
  geom_smooth(method = "lm", se = 0, color = "red3") +
  theme_classic() +
  labs(title = "Relationship Between Incumbency and Q2 GDP Growth with Two-Party Vote Share",
       x = "Q2 GDP Growth",
       y = "Two-Party Popular Vote Share") +
  theme(strip.text.x = element_text(size = 12))

ggsave("figures/economy/inc_gdp_q2.jpg")



#############################################################################################################

###### MODEL 2 ######

# adding presidential approval to the above model.. adjusted r-squared of 74!

model2 <- national %>% 
  filter(quarter == 2,
         incumbent_party == TRUE) %>% 
  lm(pv2p ~ (gdp_growth_qt + q3_job_approval) * incumbent, data = .)

model2_arsq <- model2 %>% 
  glance() %>% 
  pull(adj.r.squared)

# saving regression table for reference in blog post

model2 %>% 
  tidy() %>% 
  gt() %>% 
  tab_header("Linear Regression Relating Incumbency, Q2 GDP Growth Rate, and Job Approval with Incumbent Party Two-Party Vote Share") %>% 
  gtsave("../figures/economy/inc_gdp_approval.html")

# predicting Trump's numbers

q2_2020_gdp <- national %>% 
  filter(year == 2020,
         quarter == 2) %>% 
  pull(gdp_growth_qt)

trump_approval <- national %>% 
  filter(year == 2020) %>% 
  pull(q3_job_approval)

# still predicts Trump only getting 38.476% of vote share

model2_prediction <- predict(model2, tibble(gdp_growth_qt = q2_2020_gdp,
                               q3_job_approval = trump_approval,
                               incumbent = TRUE)) %>% 
  extract(1)

# plugging in economic approval numbers to see; predicts 41.411%

predict(model2, tibble(gdp_growth_qt = q2_2020_gdp,
                       q3_job_approval = 50.5,
                       incumbent = TRUE)) %>% 
  extract(1)

# going to make a model excluding a single year and then test the fit using
# leave-one-out cross validation

leave_out_one_nat <- function(x) {
  
  outsamp_inc_mod <- national %>% 
    filter(quarter == 2,
           year != x,
           incumbent_party == TRUE) %>% 
    lm(pv2p ~ (gdp_growth_qt + q3_job_approval) * incumbent, data = .)
  
  
  outsamp_inc_pred <- predict(outsamp_inc_mod, national %>% 
                                filter(quarter == 2,
                                       year == x,
                                       incumbent_party == TRUE))
  
  (national %>% filter(year == x, quarter == 2, incumbent_party == TRUE) %>% pull(pv2p)) - outsamp_inc_pred
  
}

residuals <- tibble(year = seq(1948, 2016, by = 4), residuals = sapply(year, leave_out_one_nat))

ggplot(residuals, aes(year, residuals)) +
  geom_point() +
  theme_classic() +
  geom_hline(yintercept = 0, color = "red3") +
  labs(title = "Out-of-Sample Residuals for Model 2", x = "Year", y = "Residuals")

ggsave("figures/economy/inc_gdp_approval_resid.jpg", width = 7, height = 4)

national %>% 
  filter(incumbent_party == TRUE,
         quarter == 1) %>% 
  ggplot(aes(q3_job_approval, pv2p)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm", se = 0, color = "red3") +
  labs(title = "Relationship Between Job Approval and Two-Party Vote Share",
       x = "Q3 Job Approval", y = "Two-Party Popular Vote Share")

ggsave("figures/economy/approval_plot.jpg")

#############################################################################################################

###### MODEL 3 ######

# going to recreate model 2 but with q1 economic numbers instead. adjusted
# r-squared of 84.2!

model3 <- national %>% 
  filter(quarter == 1,
         incumbent_party == TRUE) %>% 
  lm(pv2p ~ (gdp_growth_qt + q3_job_approval) * incumbent, data = .)

model3_arsq <- model3 %>% 
  glance() %>% 
  pull(adj.r.squared)

# saving regression table for reference in blog post

model3 %>% 
  tidy() %>% 
  gt() %>% 
  tab_header("Linear Regression Relating Incumbency, Q1 GDP Growth Rate, and Job Approval with Incumbent Party Two-Party Vote Share") %>% 
  gtsave("../figures/economy/inc_q1_gdp_approval.html")

# predicting Trump's numbers

q1_2020_gdp <- national %>% 
  filter(year == 2020,
         quarter == 1) %>% 
  pull(gdp_growth_qt)

trump_approval <- national %>% 
  filter(year == 2020) %>% 
  pull(q3_job_approval)

# predicts Trump only getting 42.497% of vote share

model3_prediction <- predict(model3, tibble(gdp_growth_qt = q1_2020_gdp,
                                     q3_job_approval = trump_approval,
                                     incumbent = TRUE)) %>% 
  extract(1)

# plugging in economic approval numbers to see; predicts 46.494%

predict(model3, tibble(gdp_growth_qt = q1_2020_gdp,
                       q3_job_approval = 50.5,
                       incumbent = TRUE)) %>% 
  extract(1)

# going to make a model excluding a single year and then test the fit using
# leave-one-out cross validation

leave_out_one_nat <- function(x) {
  
  outsamp_inc_mod <- national %>% 
    filter(quarter == 1,
           year != x,
           incumbent_party == TRUE) %>% 
    lm(pv2p ~ (gdp_growth_qt + q3_job_approval) * incumbent, data = .)
  
  
  outsamp_inc_pred <- predict(outsamp_inc_mod, national %>% 
                                filter(quarter == 1,
                                       year == x,
                                       incumbent_party == TRUE))
  
  (national %>% filter(year == x, quarter == 1, incumbent_party == TRUE) %>% pull(pv2p)) - outsamp_inc_pred
  
}

residuals <- tibble(year = seq(1948, 2016, by = 4), residuals = sapply(year, leave_out_one_nat))

ggplot(residuals, aes(year, residuals)) +
  geom_point() +
  theme_classic() +
  geom_hline(yintercept = 0, color = "red3") +
  labs(title = "Out-of-Sample Residuals for Model 3", x = "Year", y = "Residuals")

ggsave("figures/economy/inc_q1_gdp_approval_resid.jpg", width = 7, height = 4)

# making plot showing relationship between q1 numbers and vote share

national %>% 
  filter(incumbent_party == TRUE,
         quarter == 2) %>% 
  ggplot(aes(gdp_growth_qt, pv2p)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm", se = 0, color = "red3") +
  labs(title = "Relationship Between Q2 GDP Growth and Two-Party Vote Share",
       x = "Q3 Job Approval", y = "Two-Party Popular Vote Share") 

#############################################################################################################

# creating a table to compare the models

tibble(model = c("1", "2", "3"),
       predictors = c("Q2 GDP Growth * Incumbency",
                      "(Q2 GDP Growth + Job Approval) * Incumbency",
                      "(Q1 GDP Growth + Job Approval) * Incumbency"),
       incumbency_interaction = c("2.188", 
                                  "0.154 (Q2 GDP Growth) & 0.149 (Q3 Job Approval)",
                                  "3.337 (Q1 GDP Growth) & 0.259 (Q3 Job Approval)"),
       incumbency_significance = c("Insignificant (p = 0.350)", "Insignificant (p = 0.915 & p = 0.242)",
                                   "Significant (p = 0.032 & p = 0.016)"),
       adj_r_squared = c(model1_arsq, model2_arsq, model3_arsq),
       prediction = c(model1_prediction, model2_prediction, model3_prediction)
       ) %>% 
  mutate(adj_r_squared = round(adj_r_squared, 3),
         prediction = round(prediction, 3)) %>% 
  gt() %>% 
  tab_header("Model Comparison") %>% 
  cols_label(model = "Model",
           predictors = "Predictors",
           incumbency_interaction = "Interaction with Incumbency",
           incumbency_significance = "Significance of Interaction Term(s)",
           adj_r_squared = "Adjusted R-Squared",
           prediction = "Predicted Vote Share") %>% 
  tab_options(table.font.size = 20,
              heading.title.font.size = 30,
              column_labels.font.size = 25)

#############################################################################################################


# replicating NYT q2 graph

national %>% 
  filter(quarter == 2) %>% 
  mutate(negative = as_factor(ifelse(gdp_growth_qt < 0, 1, 0))) %>% 
  ggplot(aes(year, gdp_growth_qt, fill = negative)) +
  geom_col() +
  theme_classic() +
  scale_fill_manual(values = c("gray", "red3")) +
  labs(title = "Q2 2020 Marks a Historic Drop in GDP",
       x = "Year",
       y = "Q2 GDP Growth from Previous Quarter") +
  theme(legend.position = "none",
        text = element_text(size = 15))

ggsave("figures/economy/q2gdp.jpg")


