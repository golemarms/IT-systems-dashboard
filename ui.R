source("helper.R")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon=icon("tachometer-alt")),
    menuItem("Network visualization", tabName = "network_vis", icon=icon("project-diagram")),
    menuItem("System Inspector", tabName = "system_inspector", icon=icon("search-plus")),
    menuItem("All systems", tabName = "all_systems", icon=icon("list")),
    menuItem("Submit new system", tabName = "submit", icon=icon("plus"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "network_vis",
            box(title="Network visualisation",
                width=12,
                status="info",
                visNetworkOutput("network", width = "100%", height = "700px"))
    ),
    
    tabItem(tabName = "system_inspector",
            sidebarLayout(
              sidebarPanel(
                width=3,
                h2("Select a system"),
                actionButton("toggle_all", "Toggle All"), # Select/ deselect all
                
                make_CheckboxGroupInput("dept", "Department",{nodes %>% pull(DEPT) %>% levels}),
                actionButton("toggle_depts", "Toggle Depts"), # Select/ deselect all
                
                make_CheckboxGroupInput("hosting", "Hosting",{nodes %>% pull(HOSTING_MODEL) %>% levels}),
                actionButton("toggle_hosting", "Toggle Hosting"), # Select/ deselect all
                
                make_CheckboxGroupInput("classification", "Classification",{nodes %>% pull(CLASSIFICATION) %>% levels}),
                actionButton("toggle_class", "Toggle Classification"), # Select/ deselect all

                # Dropdown list of systems to select
                selectizeInput("system_select", "Select system", {nodes %>% make_choice_list()}),
                bsButton("choose_system", "Choose", style="primary"), ## Make sure to put in validation
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