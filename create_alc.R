# ---------------------------------------------------- #
# Juho Koistinen ------------------------------------- #
# 21/11/2022 ----------------------------------------- #
# This script manipulates the data as per instructions #
# ---------------------------------------------------- #

library(readr)
library(tidyverse)
# Q3
student_por <- read_delim("data/student-por.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
student_mat <- read_delim("data/student-mat.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
lapply(list(student_por, student_mat), dim)
lapply(list(student_por, student_mat), str)
# Q4
free_cols <- c("failures", "paid", "absences", "G1", "G2", "G3")
join_cols <- setdiff(colnames(student_por), free_cols)
student_joined <- inner_join(student_por, student_mat, by = join_cols, suffix = c(".por", ".mat")) 
alc <- student_joined %>% dplyr::select(all_of(join_cols))
dim(alc)
str(alc)
# Q5
for(col_name in free_cols) {
  
  two_cols <- student_joined %>% dplyr::select(starts_with(col_name))
  first_col <- two_cols %>% dplyr::select(1) %>% .[[1]]

  if(is.numeric(first_col)) {
    alc[col_name] <- round(rowMeans(two_cols))
  } else { 
    alc[col_name] <- first_col
  }
}
# Q6
alc <- alc %>% 
  mutate(alc_use = map2_dbl(.x = Walc, .y = Dalc, ~mean(c(.x, .y)))) %>% 
  mutate(high_use = map_lgl(.x = alc_use, ~.x>2))
# Q7
glimpse(alc)
write_csv(alc, "data/alc.csv")