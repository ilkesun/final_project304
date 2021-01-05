#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Computing in the 
# Humanities and Social Sciences (CHASS) at the University of Toronto. The data
# is 2017 General Social Survey: Families Cycle 31. The 2017 GSS raw data was 
# previously cleaned by Rohan Alexander and Sam Caetano. 
# Author: Ilke Sun
# Data: 21 December 2020
# Contact: ilke.sun@mail.utoronto.ca
# License: MIT

library(haven)
library(tidyverse)

setwd("~/Desktop/Final Project")
raw_data_p <- read_csv("Raw/gss.csv")
raw_data_p <- labelled::to_factor(raw_data_p)

reduced_data_p <- 
  raw_data_p %>% 
  select(age,
         sex,
         province,
         income_respondent,
         education)

rm(raw_data_p)

reduced_data_p = na.omit(reduced_data_p)

reduced_data_p <-
  reduced_data_p %>% 
  mutate(male = if_else(sex == "Male", 1, 0)) %>% 
  select(province,
         age,
         male,
         income_respondent,
         education)

reduced_data_p <-
  reduced_data_p %>% 
  filter(age >= 18)

reduced_data_p$age <- floor(reduced_data_p$age)
reduced_data_p$income <- reduced_data_p$income_respondent

reduced_data_p <-
  reduced_data_p %>% 
  rowwise %>% 
  mutate(education = case_when(
    education == "Less than high school diploma or its equivalent" ~ "No Highschool",
    education == "High school diploma or a high school equivalency certificate" ~ "Completed Highschool",
    education == "Trade certificate or diploma" ~ "Some College",
    education == "College, CEGEP or other non-university certificate or di..." ~ "Some College",
    education == "University certificate or diploma below the bachelor's level" ~ "Some University",
    education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" ~ "Bachelor's Degree",
    education == "University certificate, diploma or degree above the bach..." ~ "Degree above Bachelor's"
  ))

reduced_data_p <-
  reduced_data_p %>% 
  mutate(agegroup = case_when(age >= 18 & age <= 24 ~ "ages18to24",
                              age >= 25 & age <= 34 ~ "ages25to34",
                              age >= 35 & age <= 44 ~ "ages35to44",
                              age >= 45 & age <= 54 ~ "ages45to54",
                              age >= 55 & age <= 64 ~ "ages55to64",
                              age >= 65 & age <= 74 ~ "ages65to74",
                              age >= 75 ~ "ages75plus"))

reduced_data_p$education <- factor(reduced_data_p$education)
reduced_data_p$male <- factor(reduced_data_p$male)
reduced_data_p$agegroup <- factor(reduced_data_p$agegroup)
reduced_data_p$province <- factor(reduced_data_p$province)
reduced_data_p$income <- factor(reduced_data_p$income)

reduced_data_p$education <- factor(reduced_data_p$education, levels = 
                                     c("No Highschool", "Completed Highschool",
                                       "Some College", "Some University",
                                       "Bachelor's Degree", 
                                       "Degree above Bachelor's"))

reduced_data_p$agegroup <- factor(reduced_data_p$agegroup, levels = 
                                    c("ages18to24", "ages25to34",
                                      "ages35to44", "ages45to54",
                                      "ages55to64", "ages65to74", 
                                      "ages75plus"))

reduced_data_p$income <- factor(reduced_data_p$income, levels = 
                                  c("Less than $25,000", "$25,000 to $49,999", 
                                    "$50,000 to $74,999", "$75,000 to $99,999",
                                    "$100,000 to $ 124,999", "$125,000 and more"))

reduced_data_p <-
  reduced_data_p %>% 
  count(agegroup, province, male, income, education, .drop = group_by_drop_default) %>% 
  group_by(agegroup, province, male, income, education, .drop = group_by_drop_default)

reduced_data_p$n <- as.integer(reduced_data_p$n)

reduced_data_p <- 
  reduced_data_p %>% 
  group_by(province) %>% 
  mutate(total = sum(n)) %>% 
  mutate(cell_prop_of_division_total = n / total) %>% 
  ungroup() %>% 
  select(-total)

reduced_data_p <-
  reduced_data_p %>% 
  select(province,
         male,
         agegroup,
         income,
         education,
         n,
         cell_prop_of_division_total
  )

write_csv(reduced_data_p, "census_data.csv")
         