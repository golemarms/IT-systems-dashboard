
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

class_colors_df <- class_colors %>% enframe(name="color", value="CLASSIFICATION")

template_folder <- "templates"



# import ------------------------------------------------------------------



nodes_raw <- read_csv(paste(template_folder, "systems_gen.csv", sep="/"),
                      col_types = cols(.default = "f"))
edges_raw <- read_csv(paste(template_folder, "links_gen.csv", sep="/"),
                      col_types = cols(.default = "c"))



# make edges --------------------------------------------------------------



edges <- edges_raw %>% 
  select(FROM,
         TO,
         PROTOCOL) %>% 
  rename(from=FROM,
         to=TO) %>% 
  mutate(arrows="to") 



# some systems are documented in the edge table but not the node table. Need to document these systems.
edge_systems <- edges %>% select(from, to) %>% unlist(use.names=F) %>% unique() 
edge_sys_df <- tibble(ID=edge_systems) 


# make nodes --------------------------------------------------------------


nodes <- nodes_raw %>% 
  select(ID,
         SHORT_NAME, 
         FULL_NAME,
         CLASSIFICATION,
         DEPT,
         STATUS,
         HOSTING_MODEL,
         DEPT_OWNER,
         IDTD_REP) %>% 
  mutate(ID=as.character(ID)) %>% 
  full_join(edge_sys_df) %>% #To make sure all system in edge df are listed in node df as well
  left_join(class_colors_df) %>% 
  mutate(id=ID,
         label=SHORT_NAME,
         color= replace_na(color, "grey"),
         CLASSIFICATION=fct_relevel(CLASSIFICATION, c("Unclassified (Official Open)",
                                                      "Unclassified (Official Closed)",
                                                      "Restricted",
                                                      "Confidential")), 
         group=CLASSIFICATION) %>% 
         mutate(title=paste0("<b>Short Name:</b> ", SHORT_NAME,"<br>", 
                             "<b>Full Name:</b> ", FULL_NAME,"<br>", 
                             "<b>Classification:</b> ", CLASSIFICATION,"<br>", 
                             "<b>Hosting:</b> ", HOSTING_MODEL,"<br>",
                             "<b>Department:</b> ", DEPT,"<br>",
                             "<b>DEPT OIC:</b> ", DEPT_OWNER,"<br>",
                             "<b>IDTD rep:</b> ", IDTD_REP,"<br>"))


n_systems <- nodes %>% summarise(n=n_distinct(id)) %>% unlist(use.name=F)

# functions ---------------------------------------------------------------

make_choice_list <- function(df_nodes) {
  df_nodes %>% 
  distinct(id, FULL_NAME) %>% 
  {split(as.character(.$id), .$FULL_NAME)} %>% 
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
    count(CLASSIFICATION) %>% 
    ggplot(aes(x=1, y=n, fill=CLASSIFICATION)) +
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
