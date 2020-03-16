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
                        selectizeInput("system", "Select system", {nodes %>% pull(Name) %>% levels}),
                        actionButton("submit_system", "Select") ## Make sure to put in validation
                        ),
                        mainPanel(
                        )
                      )
            ), 
            
            tabPanel("All systems"),
            tabPanel("All links")
        )
)
