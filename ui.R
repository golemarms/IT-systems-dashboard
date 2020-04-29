source("helper.R")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon=icon("tachometer-alt")),
    menuItem("System Inspector", tabName = "system_inspector", icon=icon("search-plus")),
    menuItem("All systems", tabName = "all_systems", icon=icon("list")),
    menuItem("Submit new system", tabName = "submit", icon=icon("plus"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "overview",
            fluidRow(valueBox(n_systems, "Total number of systems", color="green", icon=icon("check-circle")),
                     valueBox(floor(n_systems/4), "In progress", color="yellow", icon=icon("cogs")),
                     valueBox(floor(n_systems/3), "Pending approval", color="red", icon=icon("exclamation-triangle"))),
            box(title="Summary of systems",
                plotlyOutput("class_count_chart", height="600px"),
                status="info",
                width=4),
            box(title="Network visualisation",
                width=8,
                status="info",
                visNetworkOutput("network", width = "100%", height = "600px"))
    ),
    
    tabItem(tabName = "system_inspector",
            sidebarLayout(
              sidebarPanel(
                width=3,
                h2("Select a system"),
                # Selec
                checkboxGroupInput("dept", "Department",
                                   {nodes %>% pull(DEPT) %>% levels},
                                   {nodes %>% pull(DEPT) %>% levels}),
                
                checkboxGroupInput("hosting", "Hosting",
                                   {nodes %>% pull(HOSTING_MODEL) %>% levels},
                                   {nodes %>% pull(HOSTING_MODEL) %>% levels}),
                
                checkboxGroupInput("classification", "Classification",
                                   {nodes %>% pull(CLASSIFICATION) %>% levels},
                                   {nodes %>% pull(CLASSIFICATION) %>% levels}),
                
                selectizeInput("system_select", "Select system", {nodes %>% make_choice_list()}),
                actionButton("submit_system", "Choose"), ## Make sure to put in validation
                h2("Download"),
                downloadButton("bia_download", label = "Download BIA form"),
                downloadButton("network_png_download", label = "Download network interface diagram")
              ),
              mainPanel(
                uiOutput("inspect_system")
              )
            )
    )
  )
)


dashboardPage(
  dashboardHeader(title = "IDTD systems"),
  sidebar,
  body
)