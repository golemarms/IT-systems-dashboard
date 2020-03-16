source("helper.R")

ui <- fluidPage(
        titlePanel("IDTD systems"),
        
          tabsetPanel(
            tabPanel("Overview",
                        visNetworkOutput("network", width = "100%", height = "600px")
                    ),
            tabPanel("By system",
                      sidebarLayout(
                        sidebarPanel(
                          h2("Select a system"),
                          width=3,
                          checkboxGroupInput("dept", "Department", {nodes %>% pull(Department) %>% levels}, {nodes %>% pull(Department) %>% levels}),
                          checkboxGroupInput("hosting", "Hosting", {nodes %>% pull(Hosting) %>% levels}, {nodes %>% pull(Hosting) %>% levels}),
                          checkboxGroupInput("classification", "Classification", {nodes %>% pull(Classification) %>% levels}, {nodes %>% pull(Classification) %>% levels}),
                          selectizeInput("system_select", "Select system", {nodes %>% pull(Name) %>% levels}),
                          actionButton("submit_system", "Select") ## Make sure to put in validation
                        ),
                        mainPanel(
                          h2("Please select a system !")
                        )
                      )
            ), 
            
            tabPanel("All systems"),
            tabPanel("All links")
        )
)
