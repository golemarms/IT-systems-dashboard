source("helper.R")

server <- function(input,output, session) {
  
  output$class_count_chart <- renderPlotly({nodes %>%
                                            plot_sys_class_count %>% 
                                            ggplotly()})
  
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges, main="Network Visualization") %>% 
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
      h3("Selected system: ", {nodes %>% filter(ID==system_select) %>% pull(Name)}),
      br(),
      tableOutput("sys_feature_table"),
      actionButton("change_system", "Change!"),
      br(),
      visNetworkOutput("inspected_network", width = "100%"),
      downloadButton("bia_download", label = "Download BIA form")
      
      )
    }
    
  }
  
  observeEvent(input$change_system, {chosen_system(input$inspected_network_selected)})
  
  ## Debugging etc.
  
  observe({cat("selected: ", input$system_select, "\n")})
  observe({cat("chosen: ", chosen_system(), "\n")})
}