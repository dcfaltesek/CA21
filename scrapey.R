library(rvest)

hole<-"https://en.wikipedia.org/wiki/List_of_Billboard_Hot_100_number_ones_of_2018"

hole %>% 
  read_html() %>% 
  html_elements(css = "table.wikitable:nth-child(13) > tbody:nth-child(1)")%>%
  html_table(header = TRUE)


yummi<-"https://en.wikipedia.org/wiki/Sushi"
tasty<-yummi %>% 
  read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[4]/tbody") %>% 
  html_table()
write.csv(tasty, "tasty.csv", row.names = FALSE)
