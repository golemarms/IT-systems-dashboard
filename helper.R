
# setup -------------------------------------------------------------------

require(tidyverse)
require(visNetwork)
require(shiny)
require(searchable)
require(plotly)
require(shinydashboard)


class_colors <- c(chartreuse="Unclassified (Official Open)",
                  `#98F5FF`="Unclassified (Official Closed)",
                  orange="Restricted",
                  red="Confidential")

class_colors_df <- class_colors %>% enframe(name="color", value="Classification")

template_folder <- "templates"

nodes_raw <- read_csv(paste(template_folder, "systems_arun.csv", sep="/"),
                      col_types = cols(.default = "f"))
edges_raw <- read_csv(paste(template_folder, "links_arun.csv", sep="/"), col_types = cols(.default = "c"))



edges <- edges_raw %>% 
  mutate(from=FROM,
         to=TO,
         label=PROTOCOL,
         arrows="to") 


edge_systems <- edges %>% select(from, to) %>% unlist(use.names=F) %>% unique() 
edge_sys_df <- tibble(ID=edge_systems) 


nodes <- nodes_raw %>% 
  select(ID=(`System Name`), Name=`Project Name`, Classification, Department, Status, Hosting=`Hosting Model`, Owner=`System Owner`, IDTD_rep=`IDTD Rep`) %>% 
  mutate(ID=as.character(ID)) %>% 
  full_join(edge_sys_df) %>% 
  left_join(class_colors_df) %>% 
  mutate(id=ID,
         label=ID,
         color= replace_na(color, "grey"),
         Classification=fct_relevel(Classification, c("Unclassified (Official Open)",
                                                      "Unclassified (Official Closed)",
                                                      "Restricted",
                                                      "Confidential")), 
         group=Classification) %>% 
         mutate(title=paste0("<b>Short Name:</b> ", ID,"<br>", 
                             "<b>Full Name:</b> ", Name,"<br>", 
                             "<b>Classification:</b> ", Classification,"<br>", 
                             "<b>Hosting:</b> ", Hosting,"<br>",
                             "<b>Department:</b> ", Department,"<br>",
                             "<b>IDTD rep:</b> ", IDTD_rep,"<br>"))



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
  visNodes(shape="ellipse", width="100%") 
}


vis_inspect <- function(df_nodes, df_edges, id) {
  filtered_dfs <- filter_systems(df_nodes, df_edges, id)
  
  .edges <- filtered_dfs$edges
  
  .nodes <- filtered_dfs$nodes %>% 
    mutate(font.size=ifelse(id==!!id, 20, 15),
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
