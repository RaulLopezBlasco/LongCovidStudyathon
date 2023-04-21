library(dplyr)
library(CodelistGenerator)
db <- DBI::dbConnect(duckdb::duckdb(), 
                     dbdir = "D:/pharmetrics_plus_202203_vocab.duckdb")
cdm <- CDMConnector::cdm_from_con(db,
                                  cdm_schema = "main",
                                  cdm_tables = tidyselect::all_of(c(
                                    "concept",
                                    "concept_relationship",
                                    "concept_ancestor",
                                    "concept_synonym",
                                    "vocabulary"
                                  )))

# conditions -----
chapters <- read_csv("data/chapters_selected.csv", 
         show_col_types = FALSE) %>% 
  filter(!is.na(name))
icd_groups <- unique(chapters$name)

snomed_groupings <- list()
# for each chapter of interest, we'll get all the relevant codes
for(i in seq_along(icd_groups)){
  print(paste0("Getting ", i, " of ", length(icd_groups)))
 working_group <- icd_groups[[i]]
 
 # get
 working_chapters <- chapters %>% 
   filter(name==working_group) %>% 
   pull(chapter)
 
 # hierarchy concepts
 icd_h_codes <- cdm$concept %>% 
   filter(concept_class_id == "ICD10 Chapter") %>% 
   filter(concept_name %in% working_chapters) 
 
 # get icd descendants
 icd_codes <- cdm$concept_relationship %>% 
   inner_join(icd_h_codes %>% 
                select("concept_id") %>% 
                rename("concept_id_1"="concept_id")) %>% 
   select("concept_id_2") %>% 
   rename("concept_id"="concept_id_2") %>% 
   left_join(cdm$concept) %>% 
   select("concept_id") %>%
   distinct() %>% 
   compute()
 
 # get snomed mappings
 snomed_icd<- cdm$concept %>% 
   inner_join(cdm$concept_relationship %>%
                filter(relationship_id =="Maps to") %>% 
               inner_join(icd_codes %>% 
                            rename("concept_id_1" = "concept_id")) %>% 
               rename("concept_id" = "concept_id_2") %>% 
                select("concept_id") %>% 
                distinct()) %>% 
   distinct() %>% 
   compute()
 
 # add descendants
 working_codes <-getDescendants(cdm, snomed_icd %>% pull(concept_id)) %>% 
   select(concept_id) 
 
 snomed_groupings[[i]] <- working_codes %>% 
   mutate(icd_group = working_group)
}

snomed_groupings <- bind_rows(snomed_groupings)

write.csv(snomed_groupings, 
          here::here("data", "snomed_groupings.csv"), 
          row.names = FALSE)

# atc -----
getConceptClassId(cdm, domain = "drug", standardConcept = "classification")

atc_groups <- cdm$concept %>% 
  filter(vocabulary_id == "ATC") %>% 
  filter(concept_class_id == "ATC 1st") %>% 
  collect()


atc_groupings <- list()
for(i in seq_along(atc_groups)){
  working_group <- atc_groups$concept_name[i]
  working_code <- atc_groups %>% 
    filter(concept_name == working_group) %>% 
    pull(concept_id)
  atc_groupings[[i]] <- getDescendants(cdm, working_code) %>% 
    select("concept_id") %>% 
    distinct() %>% 
    mutate(atc_group = stringr::str_to_sentence(working_group))
}
atc_groupings <- bind_rows(atc_groupings)


write.csv(atc_groupings, 
          here::here("data", "atc_groupings.csv"), 
          row.names = FALSE)
