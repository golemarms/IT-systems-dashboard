visNetwork(nodes, edges, main="Network Visualization") %>% 
  tweak_graph() %>% 
  visOptions(selectedBy = list(variable="Department"))