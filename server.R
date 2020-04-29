source("helper.R")

server <- function(input,output, session) {
  
  output$class_count_chart <- renderPlotly({nodes %>%
      plot_sys_class_count %>% 
      ggplotly()})
  
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges) %>% 
      visOptions(selectedBy = list(variable="DEPT"),
                 highlightNearest = list(enabled=T,
                                         degree = list(from=1, to=1),
                                         algorithm = "hierarchical"),
                 ) %>% 
      tweak_graph()
  })
  
  # df_filter <- reactiveVal(nodes)
  
  df_filter <- reactive({nodes %>%
      filter(DEPT %in% input$dept) %>%
      filter(CLASSIFICATION %in% input$classification) %>%
      filter(HOSTING_MODEL %in% input$hosting)
  })
  
  choice_list <- reactive({
    df_filter() %>% 
      make_choice_list
  })
  
  observe({updateSelectizeInput(session, "system_select", choices = choice_list())})
  
  
  # Chosen system
  
  chosen_system <- reactiveVal()
  
  observeEvent(input$submit_system, {chosen_system(input$system_select)})
  
  ## Build system inspector
  
  output$inspect_system <- renderUI({system_inspector(chosen_system())})
  output$sys_feature_table <- renderTable({nodes %>%
      filter(id == chosen_system()) %>% 
      select(matches("^[A-Z]", ignore.case=F))})
  
  output$inspected_network <- renderVisNetwork(vis_inspect(nodes, edges, chosen_system()))
  
  system_inspector <- function(system_select){
    if (is.null(system_select)) {
      h3({"Please select a system !!!"})
    }
    
    else{
      fillPage(
        box(title=div("Selected system: ", {nodes %>% filter(id==system_select) %>% pull(FULL_NAME)}),
            status="primary",
            tableOutput("sys_feature_table"),
            width=12),

        box(title="View system",
            status="primary",
            width=12,
            visNetworkOutput("inspected_network", width = "100%")),
            actionButton("change_system", "Change to selected system", class="pull-right")
        
      )
    }
    
  }
  
  observeEvent(input$change_system, {chosen_system(input$inspected_network_selected)})
  
  ## Debugging etc.
  
  observe({cat("selected: ", input$system_select, "\n")})
  observe({cat("chosen: ", chosen_system(), "\n")})

}