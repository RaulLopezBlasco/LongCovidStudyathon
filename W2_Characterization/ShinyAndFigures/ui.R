#### PACKAGES -----
options(encoding = "UTF-8")

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)
library(here)
library(scales)
library(dplyr)
library(tidyr)
library(tidyr)

#### UI -----
ui <-  fluidPage(theme = shinytheme("spacelab"),
                 
# title ------ 
# shown across tabs
titlePanel("Long COVID characterisation"),
               
# set up: pages along the side -----  
                 navlistPanel(
                   
                   
## Introduction  ----- 
tabPanel("Background", 
  tags$h3("Background"),
     tags$hr(),
  tags$h5("Preliminary results")
), 
## LSC: single cohort ------ 
"Large scale characterisation",
tabPanel("Health conditions: individual cohorts",	
         tags$h3("Health conditions: individual cohorts"),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_db",
                         label = "Database",
                           choices = unique(lsc_table$database_name),
                         selected = unique(lsc_table$database_name),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_cohort_name",
                         label = "Cohort",
                         choices = unique(lsc_table$cohort_name),
                         selected = unique(lsc_table$cohort_name)[1] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_time_window",
                         label = "Time window",
                         choices = unique(lsc_table$window_name),
                         selected = unique(lsc_table$window_name)[1] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_icd_chapter",
                         label = "ICD Group",
                         choices = sort(unique(lsc_table$icd_group)),
                         selected = sort(unique(lsc_table$icd_group)),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),

         tabsetPanel(type = "tabs",
         tabPanel("Table of estimates", 
         DTOutput('tbl_lsc') %>% withSpinner()),
         tabPanel("Plot", 
                  div(style="display: inline-block;vertical-align:top; width: 150px;",
                              pickerInput(inputId = "lsc_plot_colour",
                                          label = "Colour by",
                                          choices = c("Database",
                                                      "Cohort",
                                                      "Time window",
                                                      "ICD Group"),
                                          selected = "ICD Group",
                                          options = list(
                                            `actions-box` = TRUE,
                                            size = 10,
                                            `selected-text-format` = "count > 3"),
                                          multiple = FALSE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_plot_facet",
                         label = "Facet by",
                         choices = c("Database",
                                     "Cohort",
                                     "Time window",
                                     "ICD Group"),
                         selected = "Database",
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = FALSE)
         ),
                  plotlyOutput('gg_lsc', height = "800px") %>% withSpinner() 
                  )
         )
),
## LSC: comparing cohort ------ 
tabPanel("Health conditions: cohort comparison",	
         tags$h3("Health conditions: cohort comparison"),
         
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_comp_db",
                         label = "Database",
                         choices = unique(lsc_table$database_name),
                         selected = unique(lsc_table$database_name),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_comp_cohort_name_1",
                         label = "Target cohort",
                         choices = unique(lsc_table$cohort_name),
                         selected = unique(lsc_table$cohort_name)[1] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = FALSE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_comp_cohort_name_2",
                         label = "Comparator cohort",
                         choices = unique(lsc_table$cohort_name),
                         selected = unique(lsc_table$cohort_name)[2] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = FALSE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_comp_time_window",
                         label = "Time window",
                         choices = unique(lsc_table$window_name),
                         selected = unique(lsc_table$window_name)[1] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsc_comp_icd_chapter",
                         label = "ICD Group",
                         choices = sort(unique(lsc_table$icd_group)),
                         selected = sort(unique(lsc_table$icd_group)),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         tabsetPanel(type = "tabs",
                     tabPanel("Table of estimates", 
                              DTOutput('tbl_lsc_comp') %>% withSpinner()
                     ),
                     tabPanel("Plot",
                     div(style="display: inline-block;vertical-align:top; width: 150px;",
                         pickerInput(inputId = "lsc_comp_plot_colour",
                                     label = "Colour by",
                                     choices = c("Time window",
                                                 "ICD Group"),
                                     selected = "ICD Group",
                                     options = list(
                                       `actions-box` = TRUE,
                                       size = 10,
                                       `selected-text-format` = "count > 3"),
                                     multiple = FALSE)
                     ),
                              plotlyOutput('gg_lsc_comp', height = "800px") %>% withSpinner()
                     )
         )
),



## DU: single cohort ------ 
tabPanel("Drug utilisation: individual cohorts",	
         tags$h3("Large scale characterisation: individual cohorts"),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_db",
                         label = "Database",
                         choices = unique(du_table$database_name),
                         selected = unique(du_table$database_name),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_cohort_name",
                         label = "cohort_name",
                         choices = unique(du_table$cohort_name),
                         selected = unique(du_table$cohort_name)[1] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_index",
                         label = "Time window",
                         choices = unique(du_table$index),
                         selected = unique(du_table$index)[1] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = FALSE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_time_window",
                         label = "Time window",
                         choices = unique(du_table$window_name),
                         selected = unique(du_table$window_name)[1] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_atc_group",
                         label = "ATC Group",
                         choices = unique(du_table$atc_group),
                         selected = unique(du_table$atc_group),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         
         tabsetPanel(type = "tabs",
                     tabPanel("Table of estimates", 
                              DTOutput('tbl_lsd') %>% withSpinner()),
                     tabPanel("Plot",
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "lsd_plot_colour",
                                              label = "Colour by",
                                              choices = c("Database",
                                                          "Cohort",
                                                          "Time window",
                                                          "ATC Group"),
                                              selected = "ATC Group",
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = FALSE)
                              ),
                              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  pickerInput(inputId = "lsd_plot_facet",
                                              label = "Facet by",
                                              choices = c("Database",
                                                          "Cohort",
                                                          "Time window",
                                                          "ATC Group"),
                                              selected = "Database",
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                              multiple = FALSE)
                              ),
                              plotlyOutput('gg_lsd', height = "800px") %>% withSpinner() 
                     )
         )
),






# ## DU: comparing cohort ------ 
tabPanel("Drug utilisation: cohort comparison",
         tags$h3("Drug utilisation: cohort comparison"),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_comp_db",
                         label = "Database",
                         choices = unique(du_table$database_name),
                         selected = unique(du_table$database_name),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_comp_cohort_name_1",
                         label = "Target cohort",
                         choices = unique(du_table$cohort_name),
                         selected = unique(du_table$cohort_name)[1] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = FALSE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_comp_cohort_name_2",
                         label = "Comparator cohort",
                         choices = unique(du_table$cohort_name),
                         selected = unique(du_table$cohort_name)[2] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = FALSE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_comp_time_window",
                         label = "Time window",
                         choices = unique(du_table$window_name),
                         selected = unique(du_table$window_name)[1] ,
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),
         div(style="display: inline-block;vertical-align:top; width: 150px;",
             pickerInput(inputId = "lsd_comp_atc_group",
                         label = "ATC Group",
                         choices = unique(du_table$atc_group),
                         selected = unique(du_table$atc_group),
                         options = list(
                           `actions-box` = TRUE,
                           size = 10,
                           `selected-text-format` = "count > 3"),
                         multiple = TRUE)
         ),

         tabsetPanel(type = "tabs",
                     tabPanel("Table of estimates",
                              DTOutput('tbl_lsd_comp') %>% withSpinner()),
                     # tabPanel("Plot",
                     #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                     #              pickerInput(inputId = "lsd_comp_plot_colour",
                     #                          label = "Colour by",
                     #                          choices = c("Database",
                     #                                      "Cohort",
                     #                                      "Time window",
                     #                                      "ATC Group"),
                     #                          selected = "ATC Group",
                     #                          options = list(
                     #                            `actions-box` = TRUE,
                     #                            size = 10,
                     #                            `selected-text-format` = "count > 3"),
                     #                          multiple = FALSE)
                     #          ),
                     #          div(style="display: inline-block;vertical-align:top; width: 150px;",
                     #              pickerInput(inputId = "lsd_comp_plot_facet",
                     #                          label = "Facet by",
                     #                          choices = c("Database",
                     #                                      "Cohort",
                     #                                      "Time window",
                     #                                      "ATC Group"),
                     #                          selected = "Database",
                     #                          options = list(
                     #                            `actions-box` = TRUE,
                     #                            size = 10,
                     #                            `selected-text-format` = "count > 3"),
                     #                          multiple = FALSE)
                     #         ),
                     #          plotlyOutput('gg_lsd', height = "800px") %>% withSpinner()
                     # )
         )
),

## Health care resource utilisation -----
"Health care resource utilisation",
tabPanel("Health care resource utilisation",	
         tags$h3("Health care resource utilisation"),
         tabsetPanel(type = "tabs",
                     tabPanel("Table of estimates", 
                              DTOutput('tbl_lsc_hu') %>% withSpinner()
                     ),
                     tabPanel("Plot"
                     )
         )
),
## Vaccination -----
"Vaccination data",
tabPanel("Vaccination data",	
         tags$h3("Vaccination data"),
         tabsetPanel(type = "tabs",
                     tabPanel("Table of estimates", 
                              DTOutput('tbl_lsc_vacc') %>% withSpinner()
                     ),
                     tabPanel("Plot"
                     )
         )
),
## Treatment Patterns -----
"Treatment Patterns",
tabPanel("Treatment Patterns",	
         tags$h3("Treatment Patterns"),
),
# close -----
                                                   ))



