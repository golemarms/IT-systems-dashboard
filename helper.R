
# setup -------------------------------------------------------------------

require(tidyverse)
require(visNetwork)
require(shiny)
require(searchable)
require(plotly)
require(shinydashboard)


class_colors <- c(chartreuse="Official (Open)",
                  `#98F5FF`="Official (Closed)",
                  orange="Restricted",
                  red="Confidential")

template_folder <- "templates"

nodes_raw <- read_csv(paste(template_folder, "systems.csv", sep="/"), col_types = cols(.default = "f"))
edges_raw <- read_csv(paste(template_folder, "links.csv", sep="/"), col_types = cols(.default = "f"))


nodes <- nodes_raw %>% 
  mutate(id=ID,
         label=Name,
         Classification=fct_relevel(Classification, c("Official (Open)",
                                                      "Official (Closed)",
                                                      "Restricted",
                                                      "Confidential")), 
         group=Classification) %>% 
  mutate(title=paste0("<b>Name:</b> ", Name,"<br>", 
                      "<b>Classification:</b> ", Classification,"<br>", 
                      "<b>Hosting:</b> ", Hosting,"<br>",
                      "<b>Department:</b> ", Department,"<br>",
                      "<b>IC:</b> ", IC,"<br>"))

edges <- edges_raw %>% 
  mutate(from=From,
         to=To,
         label=protocol,
         arrows="to") 


n_systems <- nodes %>% summarise(n=n_distinct(ID)) %>% unlist(use.name=F)

# functions ---------------------------------------------------------------

make_choice_list <- function(df_nodes) {
  df_nodes %>% 
  distinct(ID, Name) %>% 
  {split(as.character(.$ID), .$Name)} %>% 
  unlist()
}

filter_edges <- function(df_edge, id) {
  df_edge %>% 
    filter(from==id | to ==id)
}

filter_systems <- function(df_nodes, df_edges, id) {
  filtered_dfs <- list()
  filtered_dfs$edges <- df_edges %>% filter_edges(id)
  
  ids_to_keep <- filtered_dfs$edges %>% 
    select(from, to) %>% 
    as_vector() %>% 
    as.character() %>% 
    unique()
  
  # browser()
  filtered_dfs$nodes <- df_nodes %>% 
    filter(id %in% ids_to_keep)
  
  return(filtered_dfs)
}

tweak_graph <- function(vis_network) {
  vis_network %>% 
  visNodes(shape="database", width="100%") %>% 
  visGroups(groupname = "Official (Open)", color="chartreuse") %>% 
  visGroups(groupname = "Official (Closed)", color="#98F5FF") %>% 
  visGroups(groupname = "Restricted", color="orange") %>% 
  visGroups(groupname = "Confidential", color="red") 
}


vis_inspect <- function(df_nodes, df_edges, id) {
  filtered_dfs <- filter_systems(df_nodes, df_edges, id)
  
  .edges <- filtered_dfs$edges
  
  .nodes <- filtered_dfs$nodes %>% 
    mutate(font.size=ifelse(id==!!id, 20, 10),
           font.size=ifelse(id==!!id, 20, 10),
           level=case_when(id==!!id ~ 2,
                           id %in% !!.edges$from ~ 1,
                           TRUE ~ 3))
  
  
  visNetwork(nodes=.nodes, edges=.edges) %>% 
    tweak_graph() %>% 
    visHierarchicalLayout(direction="LR",
                          levelSeparation=300) %>% 
    visEdges(smooth=list(enabled=F)) %>% 
    visOptions(nodesIdSelection=list(enabled=T, selected=id))
}


plot_sys_class_count <- function(df_nodes) {
  
  df_nodes  %>% 
    count(Classification) %>% 
    ggplot(aes(x=1, y=n, fill=Classification)) +
    geom_col(color="black") + 
    geom_text(aes(label = n),
                  position = position_stack(vjust = 0.5),
              size=10) +
    scale_fill_manual(values=invert(class_colors)) + 
    # coord_flip() + 
    theme_minimal() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.line=element_blank(),
          panel.grid=element_blank(),
          axis.ticks=element_blank())
}
