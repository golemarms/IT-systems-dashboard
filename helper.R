require(tidyverse)
require(visNetwork)
require(shiny)

class_colors <- c(chartreuse="Official (Open)",
                  `#98F5FF`="Official (Closed)",
                  orange="Restricted",
                  red="Confidential")



nodes_raw <- read_csv("systems.csv", col_types = cols(.default = "f"))
edges_raw <- read_csv("links.csv", col_types = cols(.default = "f"))


nodes <- nodes_raw %>% 
  mutate(id=ID,
         label=Name,
         group=Classification) %>% 
  mutate(title=paste0("<b>Name:</b> ", Name,"<br>", 
                      "<b>Classification:</b> ", Classification,"<br>", 
                      "<b>Location:</b> ", Location,"<br>",
                      "<b>Department:</b> ", Department,"<br>",
                      "<b>IC:</b> ", IC,"<br>"))

edges <- edges_raw %>% 
  mutate(from=From,
         to=To,
         label=protocol,
         arrows="to") 



