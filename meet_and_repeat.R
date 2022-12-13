# --------- #
# Week 6 DW #
# --------- #

library(tidyverse)
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep =" ", header = T)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

## Q1-3
# structure of the data sets
void <- list(BPRS, RATS) %>% lapply(FUN = str)
# summaries of the vars
list(BPRS, RATS) %>% lapply(FUN = summary)
# get the distributions of the variables
void <- list(BPRS, RATS) %>% lapply(function(x) boxplot.matrix(as.matrix(x)))

RATS <- RATS %>% 
  # create factors of the variables against which
  # the rest of the vars are pivoted
  mutate(ID = factor(ID)) %>% 
  mutate(Group = factor(Group))
BPRS <- BPRS %>% 
  # create factors of the variables against which
  # the rest of the vars are pivoted
  mutate(treatment = factor(treatment)) %>% 
  mutate(subject = factor(subject))
RATSL <- RATS %>% 
  pivot_longer(cols=-c(ID,Group), names_to = "WD", values_to = "Weight")  %>%
  # Create a time variable
  mutate(Time = as.integer(substr(WD,3,4))) %>%
  arrange(Time) %>% 
  # Remove the old time variable
  dplyr::select(-WD)
BPRSL <- BPRS %>%
  pivot_longer(cols = -c(treatment, subject), names_to = "Week", values_to = "BPRS") %>% 
  # Replace the week variable with a numeric var
  mutate(Week = as.integer(substr(Week, 5, 5))) %>% 
  arrange(Week)

## Q4
# print the data structure 
void <- list(BPRSL, RATSL) %>% lapply(FUN = str)

# create tibbles which calculate the number of unique vars per variable
# these numbers are the same for variables along which the pivoting
# longer is made, and higher for variables which are pivoted, i.e.
# time and weight.
tibble(n_uniq = apply(BPRSL, 2, function(x) length(unique(x))),
       var_name = names(n_uniq)) -> n_bprsl

tibble(n_uniq = apply(RATSL, 2, function(x) length(unique(x))),
       var_name = names(n_uniq)) -> n_ratsl

# get the number of unique vars per variable in both datasets
n_bprsl
n_ratsl
# save for analysis section
write_csv(RATSL, "data/RATSL.csv")
write_csv(BPRSL, "data/BPRSL.csv")