source("helper.R")

ui <- fluidPage(
        titlePanel("IDTD systems"),
        
          tabsetPanel(
            tabPanel("Overview",
                      plotlyOutput("class_count_chart", height="200px"),  
                      visNetworkOutput("network", width = "100%", height = "600px")
                    ),
            tabPanel("System inspector",
                      sidebarLayout(
                        sidebarPanel(
                          h2("Select a system"),
                          width=3,
                          checkboxGroupInput("dept", "Department",
                                             {nodes %>% pull(Department) %>% levels},
                                             {nodes %>% pull(Department) %>% levels}),
                          
                          checkboxGroupInput("hosting", "Hosting",
                                             {nodes %>% pull(Hosting) %>% levels},
                                             {nodes %>% pull(Hosting) %>% levels}),
                          
                          checkboxGroupInput("classification", "Classification", {nodes %>% pull(Classification) %>% levels}, {nodes %>% pull(Classification) %>% levels}),
                          selectizeInput("system_select", "Select system", {nodes %>% make_choice_list()}),
                          actionButton("submit_system", "Select") ## Make sure to put in validation
                        ),
                        mainPanel(
                          uiOutput("inspect_system")
                        )
                      )
            ), 
            
            tabPanel("All systems"),
            tabPanel("All links")
        )
)
