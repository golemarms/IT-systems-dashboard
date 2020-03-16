source("helper.R")

server <- function(input,output, session) {
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges, main="Network Visualization") %>% 
      visNodes(shape="database", width="100%") %>% 
      visGroups(groupname = "Official (Open)", color="chartreuse") %>% 
      visGroups(groupname = "Official (Closed)", color="#98F5FF") %>% 
      visGroups(groupname = "Restricted", color="orange") %>% 
      visGroups(groupname = "Confidential", color="red") %>% 
      visOptions(highlightNearest = list(enabled=T, degree=list(from=1, to=1)),
                 selectedBy = list(variable="Classification"))
  })
  
  # observe({print(input$dept)})
  # observe({print(input$hosting)})
  
  df_filter <- reactive({nodes %>% 
      filter(Department %in% input$dept) %>%
      filter(Classification %in% input$classification) %>% 
      filter(Hosting %in% input$hosting)})
  
  observe({updateSelectizeInput(session, "system_select", choices = df_filter() %>% pull(Name))})
  
}