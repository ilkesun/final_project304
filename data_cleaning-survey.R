#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from 2019 Canadian 
# Election Study 2019. The data was collected through a phone survey and it is 
# one of the largest polls for the 43rd Canadian Election. The data was collected
# with the partnership of Stephenson, Laura B., Allison Harell, Daniel Rubenson 
# and Peter John Loewen.
# Author: Ilke Sun
# Date: 21 December 2020
# Contact: ilke.sun@mail.utoronto.ca 
# License: MIT

library(haven)
library(tidyverse)
library(janitor)
setwd("~/Desktop/Final Project")
raw_data_s <- read_dta("Raw/2019CES.dta")
raw_data_s <- labelled::to_factor(raw_data_s)

reduced_data_s <- 
  raw_data_s %>% 
  select(cps19_yob,
         cps19_gender,
         cps19_province,
         cps19_education,
         cps19_income_number,
         cps19_votechoice)

rm(raw_data_s)
reduced_data_s = na.omit(reduced_data_s)

reduced_data_s$cps19_yob = as.numeric(as.character(reduced_data_s$cps19_yob))
reduced_data_s <-
  reduced_data_s %>% 
  mutate(age = 2019 - cps19_yob)

reduced_data_s <-
  reduced_data_s %>%
  clean_names() %>% 
  rename(gender = cps19_gender,
         province = cps19_province,
         education = cps19_education,
         vote = cps19_votechoice,
         income_number = cps19_income_number)

#reduced_data_s <-
#  reduced_data_s %>% 
#  filter(province != "Northwest Territories",
#         province != "Nunavut",
#         province != "Yukon")

reduced_data_s <- 
  reduced_data_s %>%
  filter(gender != "Other (e.g. Trans, non-binary, two-spirit, gender-queer)") %>%
  filter(education != "Don't know/ Prefer not to answer") %>% 
  mutate(male = if_else(gender == "A man", 1, 0))

reduced_data_s <-
  reduced_data_s %>% 
  select(province,
         age,
         male,
         education,
         income_number,
         vote)
                                                
reduced_data_s <-
  reduced_data_s %>% 
  rowwise %>% 
  mutate(education = case_when(
    education == "Completed elementary school" ~ "No Highschool",
    education == "Some elementary school" ~ "No Highschool",
    education == "No schooling" ~ "No Highschool",
    education == "Some secondary/ high school" ~ "No Highschool",
    education == "Completed secondary/ high school" ~ "Completed Highschool",
    education == "Some technical, community college, CEGEP, College Classique"
    ~ "Completed Highschool",
    education == "Completed technical, community college, CEGEP, College Classique"
    ~ "Some College",    
    education == "Some university" ~ "Some University",
    education == "Bachelor's degree" ~ "Bachelor's Degree",
    education == "Professional degree or doctorate" ~ "Degree above Bachelor's",
    education == "Master's degree" ~ "Degree above Bachelor's"
  ))

reduced_data_s <- 
  reduced_data_s %>% 
  mutate(income = case_when(income_number < 25000 ~ "Less than $25,000",
                            income_number >= 25000 &  income_number < 50000
                            ~ "$25,000 to $49,999",
                            income_number >= 50000 & income_number < 75000 
                            ~ "$50,000 to $74,999", 
                            income_number >= 75000 & income_number < 100000
                            ~ "$75,000 to $99,999", 
                            income_number >= 100000 & income_number < 125000
                            ~ "$100,000 to $ 124,999",
                            income_number >= 125000 ~ "$125,000 and more"))

reduced_data_s <-
  reduced_data_s %>% 
  filter(vote != "Don't know/ Prefer not to answer")

reduced_data_s <-
  reduced_data_s %>% 
  mutate(SupportGP = if_else(vote == "Green Party", 1, 0),
         SupportLP = if_else(vote == "Liberal Party", 1, 0),
         SupportCP = if_else(vote == "Conservative Party", 1, 0),
         SupportNDP = if_else(vote == "ndp", 1, 0),
         SupportPP = if_else(vote == "People's Party", 1, 0),
         SupportAnother = if_else(vote == "Another party (please specify)", 1, 
                                  0),
         SupportBQ = if_else(SupportGP == 0 & 
                             SupportLP == 0 &
                             SupportCP == 0 &
                             SupportNDP == 0 &
                             SupportAnother == 0 &
                             SupportPP == 0, 1, 0)
         )

reduced_data_s <- 
  reduced_data_s %>%  
  select(-vote)

reduced_data_s <-
  reduced_data_s %>% 
  mutate(agegroup = case_when(age >= 18 & age <= 24 ~ "ages18to24",
                              age >= 25 & age <= 34 ~ "ages25to34",
                              age >= 35 & age <= 44 ~ "ages35to44",
                              age >= 45 & age <= 54 ~ "ages45to54",
                              age >= 55 & age <= 64 ~ "ages55to64",
                              age >= 65 & age <= 74 ~ "ages65to74",
                              age >= 75 ~ "ages75plus"))


reduced_data_s <-
  reduced_data_s %>% 
  select(province, male, agegroup, income, education, age, SupportLP, SupportCP, 
         SupportNDP, SupportBQ, SupportGP, SupportPP, SupportAnother)

reduced_data_s$education <- factor(reduced_data_s$education)
reduced_data_s$male <- factor(reduced_data_s$male)
reduced_data_s$agegroup <- factor(reduced_data_s$agegroup)
reduced_data_s$province <- factor(reduced_data_s$province)
reduced_data_s$income <- factor(reduced_data_s$income)

reduced_data_s$education <- factor(reduced_data_s$education, levels = 
                                     c("No Highschool", "Completed Highschool",
                                       "Some College", "Some University",
                                       "Bachelor's Degree", 
                                       "Degree above Bachelor's"))

reduced_data_s$agegroup <- factor(reduced_data_s$agegroup, levels = 
                                    c("ages18to24", "ages25to34",
                                      "ages35to44", "ages45to54",
                                      "ages55to64", "ages65to74", 
                                      "ages75plus"))

reduced_data_s$income <- factor(reduced_data_s$income, levels = 
                                c("Less than $25,000", "$25,000 to $49,999", 
                                  "$50,000 to $74,999", "$75,000 to $99,999",
                                  "$100,000 to $ 124,999", "$125,000 and more"))

reduced_data_s <-
  reduced_data_s %>% 
  filter(province != "Nunavut",
         province != "Yukon",
         province != "Northwest Territories")

write_csv(reduced_data_s, "survey_data.csv")