library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(readr)

###  network results -----
load("C:/Users/eburn/OneDrive - Nexus365/long covid studyathon/Processed_Results/W2_Characterization_Data/dataShiny_Charac2.RData")
du_table <- bind_rows(du_table %>% mutate(index = "base"),
                      du_tablenew %>% mutate(index = "event"))

# ls conditions  -----
lsc_table <- lsc_table %>% 
  filter(concept_count!="<5") %>%
  filter(denominator_count !="<5") %>%
  mutate(concept_count= as.numeric(concept_count)) %>%
  mutate(denominator_count= as.numeric(denominator_count)) %>%
  mutate(concept = paste0(concept_name, " (concept id: ",
                          concept_id, ")")) %>%
  mutate(proportion = round(concept_count/denominator_count,4))

write.csv(lsc_table,
          here::here("data", "lsc_table.csv"),
          row.names = FALSE)

# ls drug  -----
du_table <- du_table %>%
  filter(concept_count!="<5") %>%
  filter(denominator_count !="<5") %>%
  mutate(concept_count= as.numeric(concept_count)) %>%
  mutate(denominator_count= as.numeric(denominator_count)) %>%
  mutate(concept = paste0(concept_name, " (concept id: ",
                          concept_id, ")")) %>%
  mutate(proportion = round(concept_count/denominator_count,4))

write.csv(du_table,
          here::here("data", "du_table.csv"),
          row.names = FALSE)

# lsc_hu  -----
write.csv(lsc_hu,
          here::here("data", "lsc_hu.csv"),
          row.names = FALSE)

# lsc_hu  -----
write.csv(lsc_vacc,
          here::here("data", "lsc_vacc.csv"),
          row.names = FALSE)


