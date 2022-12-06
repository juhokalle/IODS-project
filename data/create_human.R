library(readr)
library(tidyverse)
hd <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv")
gii <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv", na = "..")
lapply(list(hd,gii), dim)
lapply(list(hd,gii), summary)
lapply(list(hd,gii), names)
names(hd) <- c("HDI_rank", "Country", "HDI", "Life_Exp", "Edu_Exp", "Mean_school", "GNI", "GNI_minus_HDIR")
names(gii) <- c("GII_rank", "Country", "GII", "Mat_Mor", "Ado_Birth", "Parli_F", "Edu2_F", "Edu2_M", "Labo_F", "Labo_M")
gii <- gii %>% mutate("Edu2_FM" = Edu2_F/Edu2_M,
                      "Labo_FM" = Labo_F / Labo_M)
human <- inner_join(hd, gii, by = "Country")
write_csv(human, "data/human.csv")

# --------------------------- #
# Section Week 5 starts here! #
# --------------------------- #
human <- read_csv("data/human.csv")
human <- human %>% mutate(GNI %>% as.numeric())
human <- human %>% dplyr::select("Country", "Edu2_FM", "Labo_FM", "Edu_Exp", "Life_Exp", "GNI", "Mat_Mor", "Ado_Birth", "Parli_F")
# save for checking the number of rows w. NAs
all_obs <- nrow(human)
# filter out rows w. NAs
human <- human %>% filter(complete.cases(.))
# should be less than or equal to all_obs
compl_obs <- nrow(human)
# print number of rows w. NAs
print(paste0("Number of obs. w. missing vals: ", all_obs-compl_obs))
# exclude last 7 obs. as they refer to regions
human <- human %>% head(-7) %>% as.data.frame()
# set row names by countries
rownames(human) <-  human$Country
# exclude country variable: I think it's easier to work with the data such that 
# the Country variable is its own column
# human <- human %>% select(-Country)
dim(human)
human %>% str
write_csv(human, "data/human.csv")
