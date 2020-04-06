source("helper.R")

server <- function(input,output, session) {
  
  output$class_count_chart <- renderPlotly({nodes %>%
      plot_sys_class_count %>% 
      ggplotly()})
  
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges) %>% 
      visOptions(selectedBy = list(variable="Department")) %>% 
      tweak_graph()
  })
  
  
  df_filter <- reactive({nodes %>% 
      filter(Department %in% input$dept) %>%
      filter(Classification %in% input$classification) %>% 
      filter(Hosting %in% input$hosting)
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
      filter(ID == chosen_system()) %>% 
      select(matches("^[A-Z]", ignore.case=F))})
  
  output$inspected_network <- renderVisNetwork(vis_inspect(nodes, edges, chosen_system()))
  
  system_inspector <- function(system_select){
    if (is.null(system_select)) {
      h3({"Please select a system !!!"})
    }
    
    else{
      fillPage(
        box(title=div("Selected system: ", {nodes %>% filter(ID==system_select) %>% pull(Name)}),
            status="primary",
            tableOutput("sys_feature_table"),
            width=6),
        box(title="Download",
            status="primary",
            width=6,
            downloadButton("bia_download", label = "Download BIA form"),
            downloadButton("network_png_download", label = "Download network interface diagram")),
        box(title="View system",
            status="primary",
            width=12,
            actionButton("change_system", "Change!"),
            visNetworkOutput("inspected_network", width = "100%"))
        
      )
    }
    
  }
  
  observeEvent(input$change_system, {chosen_system(input$inspected_network_selected)})
  
  ## Debugging etc.
  
  observe({cat("selected: ", input$system_select, "\n")})
  observe({cat("chosen: ", chosen_system(), "\n")})
}