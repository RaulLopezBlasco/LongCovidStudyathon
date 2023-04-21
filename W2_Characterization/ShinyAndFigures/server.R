#### SERVER ------
server <-	function(input, output, session) {
  
  # get lsc -----
  get_lsc <-reactive({
    table <- lsc_table  %>% 
      filter(database_name %in% input$lsc_db) %>% 
      filter(window_name %in% input$lsc_time_window) %>% 
      filter(cohort_name %in%  input$lsc_cohort_name) %>%
      filter(icd_group %in%  input$lsc_icd_chapter) %>% 
      distinct()

    table
  })
  
  
  # lsc table -----
  output$tbl_lsc <-  renderDataTable({
    
    table <- get_lsc() %>% 
      select(any_of(c("cohort_name",
                      "window_name",
                      "name" ,
                     "concept",   
                     "proportion",
                    "database_name"))) %>% 
      distinct() %>% 
      pivot_wider(names_from = c("database_name", "window_name",
                                 "cohort_name"),
                  names_sep = ": ",
                  values_from = c("proportion"
                                  ))
  
    db_count <- get_lsc() %>% 
      select("database_name", "cohort_name",
             "window_name",
             "denominator_count") %>% 
      distinct() %>% 
      mutate(db_cohort = paste0(database_name, " ",
                                window_name,
                                ": ", cohort_name)) %>% 
      mutate(name=paste0(database_name,  " ",
                         window_name,
                         " (",
                         input$lsc_cohort_name,
                         " [n: ",
                         nice.num.count(denominator_count),
                         "])"))
    
    for(i in seq_along(colnames(table))){
     if(names(table)[i] %in%  db_count$db_cohort){
       names(table)[i] <- db_count %>% 
         filter(db_cohort == names(table)[i]) %>% 
         pull(name)
     }
    }
    
    perc_cols<- stringr::str_subset(colnames(table), 
                                    paste("cohort_name", "concept", sep = "|"),
                                    negate = TRUE)
    
    
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(pageLength = 94
              )) %>% 
      formatPercentage(perc_cols, 2)
  })
  
  # lsc plot -----
  output$gg_lsc <- renderPlotly({

 colour_by <-  case_match(
      input$lsc_plot_colour,
      "ICD Group" ~ "icd_group",
      "Database" ~ "database_name",
      "Cohort" ~ "cohort_name",
      "Time window" ~ "window_name"
    )
  
 facet_by <-  case_match(
   input$lsc_plot_facet,
   "ICD Group" ~ "icd_group",
   "Database" ~ "database_name",
   "Cohort" ~ "cohort_name",
   "Time window" ~ "window_name"
 )  
    
   plot <- get_lsc() %>% 
      mutate(icd_group=if_else(is.na(icd_group), "Other", icd_group)) %>% 
      arrange(icd_group) %>% 
      mutate(id = 1:nrow(.)) %>% 
     unite("plot_group", 
           c(all_of(colour_by)), remove = FALSE, sep = "; ") %>% 
     unite("plot_facet", 
           c(all_of(facet_by)), remove = FALSE, sep = "; ") %>% 
      ggplot(aes(group = concept,
                 colour = plot_group)) +
      geom_point(aes(id, 
                     proportion),
                 size=1.5,
                 shape=19,
                 alpha=0.75) +
      theme_minimal() +
      facet_wrap(.~plot_facet, scales = "free",
                 nrow=1) +
      theme_minimal() +
      scale_y_continuous(labels = percent,
                         limits=c(0,NA),expand = c(0,0.03))+
      theme(panel.spacing.x=unit(0, "lines"),
            panel.spacing.y=unit(0, "lines"),
            legend.title = element_blank(),
            legend.position = "bottom",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            strip.text = element_text(size=14, face="bold"),
            strip.background = element_rect( fill="#f7f7f7"),
            legend.text=element_text(size=13))+
      guides(shape=guide_legend(nrow=3))+
      ylab("Prevalence")
    
   ggplotly(plot) %>%
    layout(legend = list(
      orientation = "h",
      title = list(text = "")
    )
    )
   
   
    
  })
  
  # get lsc_comp ----
  get_lsc_comp <-reactive({
    table <- lsc_table  %>% 
      filter(database_name %in% input$lsc_comp_db) %>% 
      filter(window_name %in% input$lsc_comp_time_window) %>% 
      filter(icd_group %in%  input$lsc_comp_icd_chapter) %>% 
      select(any_of(c("cohort_name",
                      "name" ,
                      "concept",   
                      "icd_group",
                      "window_name",
                      "proportion",
                      "database_name"))) %>% 
      distinct() 
    
    table_target <- table %>% 
      filter(cohort_name %in%  
               input$lsc_comp_cohort_name_1) %>% 
      select(!"cohort_name") %>% 
      mutate(database_name= paste0(database_name, "_a_target")) %>% 
      pivot_wider(names_from = c(database_name),
                  values_from = c("proportion"
                  ))
    
    table_comparator <- table %>% 
      filter(cohort_name %in%  
               input$lsc_comp_cohort_name_2) %>% 
      select(!"cohort_name") %>% 
      mutate(database_name= paste0(database_name, "_b_comparator")) %>% 
      pivot_wider(names_from = c(database_name),
                  values_from = c("proportion"))
    
    
    table <- table_target %>% 
      full_join(table_comparator)
    
    table <- table %>% 
      relocate(sort(names(.))) %>% 
      relocate("concept")
    
    names(table) <- stringr::str_replace_all(names(table), "_a_", "_")
    names(table) <- stringr::str_replace_all(names(table), "_b_", "_")
    
    table
  })
  
  # lsc_comp table -----
  output$tbl_lsc_comp <-  renderDataTable({
    
    table <- get_lsc_comp() %>% 
      select(!c("icd_group", "window_name" )) %>% 
      distinct()
    
    perc_cols<- stringr::str_subset(colnames(table), 
                                    "concept", 
                                    negate = TRUE)
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(pageLength = 94
              )) %>% 
      formatPercentage(perc_cols, 2)
  })
  
  # lsc_comp plot -----
  
  output$gg_lsc_comp <- renderPlotly({
    
    
    colour_by <-  case_match(
      input$lsc_comp_plot_colour,
      "ICD Group" ~ "icd_group",
      "Time window" ~ "window_name"
    )

   plot_data <- get_lsc_comp() %>% 
     pivot_longer(!c("concept", "icd_group",
                     "window_name"), 
                  names_to = "database_cohort", 
                  values_to = "proportion") %>% 
     separate(col = "database_cohort", 
                          c("database_name", "cohort")) %>% 
     pivot_wider(names_from = cohort, 
                 values_from = c("proportion"
                 ))
      
      
   plot <-  plot_data %>% 
     unite("plot_group", 
           c(all_of(colour_by)), remove = FALSE, sep = "; ") %>% 
     ggplot(aes(group = concept,
                colour = plot_group)) +
     geom_abline(linetype="dashed", intercept = 0)+
     geom_point(aes(target, comparator),
                size=1.5,
                shape=19,
                alpha=0.75)+
     facet_wrap(.~database_name, scales = "free",
                nrow=1) +
     theme_minimal() +
     scale_y_continuous(labels = percent,
                        limits=c(0,NA),expand = c(0,0.03)) +
     scale_x_continuous(labels = percent,
                        limits=c(0,NA),expand = c(0,0.03))+
     theme(panel.spacing.x=unit(0, "lines"),
           panel.spacing.y=unit(0, "lines"),
           legend.title = element_blank(),
           legend.position = "bottom",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           axis.text=element_text(size=18),
           axis.title=element_text(size=18,face="bold"),
           strip.text = element_text(size=18, face="bold"),
           strip.background = element_rect( fill="#f7f7f7"),
           legend.text=element_text(size=13))+
     guides(shape=guide_legend(nrow=3))
   
   ggplotly(plot) %>%
     layout(legend = list(
       orientation = "h",
       title = list(text = "")
     )
     )
    
  })
  
  
  # get lsd -----
  get_lsd <-reactive({
    table <- du_table  %>% 
      filter(database_name %in% input$lsd_db) %>% 
      filter(window_name %in% input$lsd_time_window) %>% 
      filter(cohort_name %in%  input$lsd_cohort_name) %>% 
      filter(atc_group %in%  input$lsd_atc_group) %>% 
      filter(index %in% input$lsd_index) %>% 
      distinct()

    table
  })
  
  # lsd table -----
  output$tbl_lsd <-  renderDataTable({

    table <- get_lsd() %>% 
      select(any_of(c("cohort_name",
                      "index",
                      "window_name",
                      "name" ,
                      "concept",   
                      "proportion",
                      "database_name"))) %>% 
       distinct() %>% 
      pivot_wider(names_from = c("database_name", "window_name",
                                 "cohort_name"),
                  values_from = c("proportion" 
                  ))
    
    
    db_count <- get_lsc() %>% 
      select("database_name", "cohort_name",
             "window_name",
             "denominator_count") %>% 
      distinct() %>% 
      mutate(db_cohort = paste0(database_name, " ",
                                window_name,
                                ": ", cohort_name)) %>% 
      mutate(name=paste0(database_name,  " ",
                         window_name,
                         " (",
                         input$lsc_cohort_name,
                         " [n: ",
                         nice.num.count(denominator_count),
                         "])"))
    
    for(i in seq_along(colnames(table))){
      if(names(table)[i] %in%  db_count$db_cohort){
        names(table)[i] <- db_count %>% 
          filter(db_cohort == names(table)[i]) %>% 
          pull(name)
      }
    }
    
    perc_cols<- stringr::str_subset(colnames(table), 
                                    paste("cohort_name", "concept", sep = "|"),
                                    negate = TRUE)

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(pageLength = 94
              )) %>%
      formatPercentage(perc_cols, 2)
    
  })
  
  # lsd plot -----
  output$gg_lsd <- renderPlotly({
    
    colour_by <-  case_match(
      input$lsd_plot_colour,
      "ATC Group" ~ "atc_group",
      "Database" ~ "database_name",
      "Cohort" ~ "cohort_name",
      "Time window" ~ "window_name"
    )
    
    facet_by <-  case_match(
      input$lsd_plot_facet,
      "ATC Group" ~ "atc_group",
      "Database" ~ "database_name",
      "Cohort" ~ "cohort_name",
      "Time window" ~ "window_name"
    )  
    
  plot <- get_lsd() %>% 
      mutate(atc_group=if_else(is.na(atc_group), "Other", atc_group)) %>% 
    unite("plot_group", 
          c(all_of(colour_by)), remove = FALSE, sep = "; ") %>% 
    unite("plot_facet", 
          c(all_of(facet_by)), remove = FALSE, sep = "; ") %>% 
      arrange(plot_group) %>% 
      mutate(id = 1:nrow(.)) %>% 
      ggplot(aes(group = concept,
                 colour = plot_group)) +
      geom_point(aes(id, 
                     proportion),
                 size=1.5,
                 shape=19,
                 alpha=0.75) +
      theme_minimal() +
      facet_wrap(.~plot_facet, scales = "free_x",
                 nrow=1) +
    theme_minimal()+
      scale_y_continuous(labels = percent,
                         limits=c(0,NA),expand = c(0,0.03))+
      theme(panel.spacing.x=unit(0, "lines"),
            panel.spacing.y=unit(0, "lines"),
            legend.title = element_blank(),
            legend.position = "bottom",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            strip.text = element_text(size=14, face="bold"),
            strip.background = element_rect( fill="#f7f7f7"),
            legend.text=element_text(size=13))+
      guides(shape=guide_legend(nrow=3))+
      ylab("Prevalence")

  ggplotly(plot) %>%
    layout(legend = list(
      orientation = "h",
      title = list(text = "")
    )
    )
    
  })
  
  # get lsd_comp ----
  get_lsd_comp <-reactive({
    table <- du_table  %>%
      filter(database_name %in% input$lsd_comp_db) %>%
      filter(window_name %in% input$lsd_comp_time_window) %>%
      filter(atc_group %in%  input$lsd_comp_atc_group) %>%
      select(any_of(c("cohort_name",
                      "name" ,
                      "concept",
                      "atc_group",
                      "window_name",
                      "proportion",
                      "database_name"))) %>%
      distinct()

    table_target <- table %>%
      filter(cohort_name %in%
               input$lsd_comp_cohort_name_1) %>%
      select(!"cohort_name") %>%
      mutate(database_name= paste0(database_name, "_a_target")) %>%
      pivot_wider(names_from = c(database_name),
                  values_from = c("proportion"
                  ))

    table_comparator <- table %>%
      filter(cohort_name %in%
               input$lsd_comp_cohort_name_2) %>%
      select(!"cohort_name") %>%
      mutate(database_name= paste0(database_name, "_b_comparator")) %>%
      pivot_wider(names_from = c(database_name),
                  values_from = c("proportion"))


    table <- table_target %>%
      full_join(table_comparator)

    table <- table %>%
      relocate(sort(names(.))) %>%
      relocate("concept")

    names(table) <- stringr::str_replace_all(names(table), "_a_", "_")
    names(table) <- stringr::str_replace_all(names(table), "_b_", "_")

    table
  })
  # lsd_comp table -----
  output$tbl_lsd_comp <-  renderDataTable({

    table <- get_lsd_comp() %>% 
      select(!c("window_name", "atc_group"))

    perc_cols<- stringr::str_subset(colnames(table),
                                    "concept",
                                    negate = TRUE)

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(pageLength = 94
              )) %>%
      formatPercentage(perc_cols, 2)
  })




  # ------
  output$tbl_lsc_hu <-  renderDataTable({
    datatable(lsc_hu,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB'
              )) 
  })
  
  output$tbl_lsc_vacc <-  renderDataTable({
    datatable(lsc_vacc,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             pageLength = 94,
                             dom = 'tB'
              )) 
  })
  

  # close -----
}