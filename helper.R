
# setup -------------------------------------------------------------------

require(tidyverse)
require(visNetwork)
require(shiny)
require(shinyBS)
require(searchable)
require(plotly)
require(shinydashboard)
require(DT)


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
  mutate(from=FROM,
         to=TO,
         arrows="to") 



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

lnodes <- class_colors_df %>% 
  rename(label=CLASSIFICATION) %>% 
  mutate(shape="ellipse")


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


# graphs etc --------------------------------------------------------------

theme_set(theme_minimal())
theme_update(axis.title.y=element_blank(),
             axis.line=element_blank(),
             panel.grid=element_blank(),
             axis.ticks=element_blank())

## Create overall classification - hosting barchart

class_host_barchart <- function(.nodes=nodes) {
  .nodes %>%
    mutate(CLASSIFICATION= fct_recode(CLASSIFICATION,
                                      `Unclassified\n(Official Open)`="Unclassified (Official Open)",
                                      `Unclassified\n(Official Closed)`="Unclassified (Official Closed)")) %>% 
    count(HOSTING_MODEL, CLASSIFICATION) %>% 
    group_by(CLASSIFICATION) %>% 
    mutate(class_count = sum(n)) %>% 
    ggplot(aes(x=CLASSIFICATION, y=n)) +
    geom_col(aes(fill=HOSTING_MODEL), width=0.7, colour="black") +
    geom_text(aes(label=n, group=HOSTING_MODEL), position= position_stack(vjust = 0.5), size = 7) +
    geom_text(aes(label=class_count, y=class_count), position=position_nudge(y=0.5), size=10) +
    scale_fill_brewer(palette="PuBu") +
    theme(axis.text.y = element_blank(),
          axis.title.x.bottom = element_text(face="bold", margin=margin(t=0.5, unit="cm")))
}



dept_class_host_barchart <- function(.nodes=nodes) {
  class_count_df <- .nodes %>% 
    count(DEPT, CLASSIFICATION) 
  
  host_count_df <- .nodes %>% 
    count(DEPT, HOSTING_MODEL) 
  
  dept_count_df <- .nodes %>% 
    count(DEPT) %>% 
    mutate(label = paste(n, "Systems"))
  
  ggplot(mapping=aes(y=n)) +
    geom_col(aes(x=1, fill=CLASSIFICATION), data=class_count_df, colour="black", width=0.7) + 
    geom_text(aes(x=1, label = n, group=CLASSIFICATION), size = 7, data=class_count_df, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values=invert(class_colors)) + 
    ggnewscale::new_scale_fill() +
    geom_col(aes(x=2, fill=HOSTING_MODEL), data=host_count_df, colour="black", width=0.7) +
    geom_text(aes(x=2, label = n, group=HOSTING_MODEL), size = 7, data=host_count_df, position = position_stack(vjust = 0.5)) +
    geom_label(aes(x=1.5, label = label), size=8, data=dept_count_df, position=position_nudge(y=0.5)) +
    scale_fill_brewer(palette="PuBu") +
    scale_x_continuous(breaks=c(1,2), labels=c("CLASS", "HOST")) + 
    facet_wrap(~DEPT, scales="free_x") +
    theme_light() +
    theme(strip.text = element_text(size=15),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.text = element_text(size=10)
          )
}




## Create dept classification - hosting barchart

## Tweak network graph
tweak_graph <- function(vis_network) {
  vis_network %>% 
  visNodes(shape="ellipse", width="100%") 
}

## Create system inspector diagram
vis_inspect <- function(df_nodes, df_edges, id) {
  filtered_dfs <- filter_systems(df_nodes, df_edges, id) 
  
  .edges <- filtered_dfs$edges %>% mutate(label = PROTOCOL)
  
  .nodes <- filtered_dfs$nodes %>% 
    mutate(font.size=ifelse(id==!!id, 20, 15),
           level=case_when(id==!!id ~ 2,
                           id %in% !!.edges$from ~ 1,
                           TRUE ~ 3))
  
  visNetwork(nodes=.nodes, edges=.edges) %>% 
    tweak_graph() %>% 
    visHierarchicalLayout(direction="LR",
                          levelSeparation=300) %>% 
    visEdges(smooth=list(enabled=T),
             font=list(align="middle")) %>% 
    visOptions(nodesIdSelection=list(enabled=T, selected=id)) %>% 
    visLegend(useGroups=F, addNodes=lnodes)
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
          axis.ticks=element_blank(),
          legend.text = element_text(size=10)
          )
}


# ui elements -------------------------------------------------------------

make_CheckboxGroupInput <- function(inputId, label, choices, selected_init=TRUE){
  if (selected_init){
    selected <- choices
  }
  else{
    selected <- character(0)
  }
  return(checkboxGroupInput(inputId=inputId, label=label, choices = choices, selected=selected))
}


make_checkbox_updator <- function(session, inputId, choices) {
  ## checkbox updator function factory 
  
  select_state <-  FALSE
  
  update <- function() {
    if (select_state){
      selected <- choices
    }
    
    else{
      selected <- character(0)
    }
    
    updateCheckboxGroupInput(session=session, inputId=inputId, selected=selected)
    select_state <<- !select_state
  }
  
  return(update)
}

make_mult_updater <- function(updater_list) {
  
  mult_updater <- function() {
    updater_list %>% invoke_map()
  }
  
  return(mult_updater)
}