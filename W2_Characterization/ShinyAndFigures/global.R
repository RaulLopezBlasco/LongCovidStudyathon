library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(readr)

# functions -----
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

# data -----
lsc_table <- read_csv("data/lsc_table.csv", 
                                     show_col_types = FALSE) 
du_table <- read_csv("data/du_table.csv", 
                     show_col_types = FALSE) 
lsc_hu <- read_csv("data/lsc_hu.csv", 
                     show_col_types = FALSE) 
lsc_vacc <- read_csv("data/lsc_vacc.csv", 
                     show_col_types = FALSE) 

# # add condition concept groups
# snomed_groupings <- read_csv(here::here("data", "snomed_groupings.csv"), 
#          show_col_types = FALSE)
# # chapters
# chapter_names <- read_csv(here::here("data", "chapters.csv"), 
#          show_col_types = FALSE)
# snomed_groupings <- snomed_groupings %>% 
#   left_join(chapter_names)
# snomed_groupings <- snomed_groupings %>% 
#   mutate(name=if_else(is.na(name), "Other", name))
# 
# lsc_table <- lsc_table %>% 
# left_join(snomed_groupings %>% 
#             select("concept_id", "chapter"), 
#           relationship = "many-to-many")  %>% 
#   left_join(chapter_names)



# add condition groupings ----
snomed_groupings <- read_csv("data/snomed_groupings.csv",
                      show_col_types = FALSE)

lsc_table <- lsc_table %>%
  left_join(snomed_groupings) %>% 
  mutate(icd_group=ifelse(is.na(icd_group), "Other", icd_group))

# add drug groupings ----
atc_groupings<-read_csv("data/atc_groupings.csv",
         show_col_types = FALSE)

du_table <- du_table %>%
  left_join(atc_groupings) %>% 
  mutate(atc_group=ifelse(is.na(atc_group), "Other", atc_group))


a<-du_table %>% 
  group_by(cohort_name,
           concept_name,
           window_name,
           overlap,
           concept_type,
           database_name, 
           # atc_group,
           index) %>% 
  tally()
