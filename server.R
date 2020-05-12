source("helper.R")

server <- function(input, output, session) {
  
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
      visLegend(useGroups=F, addNodes=lnodes) %>% 
      tweak_graph()
  })
  
  
  df_filter <- reactive({nodes %>%
      filter(DEPT %in% input$dept) %>%
      filter(CLASSIFICATION %in% input$classification) %>%
      filter(HOSTING_MODEL %in% input$hosting)
  })
  
  choice_list <- reactive({
    df_filter() %>% 
      make_choice_list
  })
  
  update_all <- make_mult_updater(list(make_checkbox_updator(session, "dept", {nodes %>% pull(DEPT) %>% levels}),
                                        make_checkbox_updator(session, "hosting", {nodes %>% pull(HOSTING_MODEL) %>% levels}),
                                        make_checkbox_updator(session, "classification", {nodes %>% pull(CLASSIFICATION) %>% levels})))
  observeEvent(input$toggle_all, {update_all()})
  
  
  update_depts <- make_checkbox_updator(session, "dept", {nodes %>% pull(DEPT) %>% levels})
  observeEvent(input$toggle_depts, {update_depts()})
  
  update_hosting <- make_checkbox_updator(session, "hosting", {nodes %>% pull(HOSTING_MODEL) %>% levels})
  observeEvent(input$toggle_hosting, {update_hosting()})
  
  update_class <- make_checkbox_updator(session, "classification", {nodes %>% pull(CLASSIFICATION) %>% levels})
  observeEvent(input$toggle_class, {update_class()})
  
  # Update dropdown list of systems
  observe({updateSelectizeInput(session, "system_select", choices = choice_list())})
  
  
  ## Chosen/selected  system
  
  chosen_system <- reactiveVal()
  selected_isnull <- reactive({nchar(input$system_select)==0})
  
  updated_selected_null <- function(is_null){
    if (is_null) {
      updateButton(session=session,
                   inputId="choose_system",
                   label="No system selected",
                   style= "secondary", 
                   disabled=T)
    }
    
    else{
      updateButton(session=session,
                   inputId="choose_system",
                   label="Choose",
                   style="primary",
                   disabled=F)
    }
  }
  
  observeEvent(selected_isnull(), {updated_selected_null(selected_isnull())})
  
  observeEvent(input$choose_system, {chosen_system(input$system_select)})
  
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
            actionButton(inputId="switch_system",
                         label="Switch to selected system",
                         class="pull-right",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        
      )
    }
    
  }
  
  observeEvent(input$switch_system, {chosen_system(input$inspected_network_selected)})
  
  ## Debugging etc.
  
  observe({cat("selected_isnull: ", selected_isnull(), "\n")})
  observe({cat("selected: ", input$system_select, "\n")})
  observe({cat("chosen: ", chosen_system(), "\n")})

}