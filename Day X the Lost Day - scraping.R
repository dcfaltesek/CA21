#scraping for fun and profit
#you need data, right?
#websites have data, those jerks, we want it.

restaurant<-"https://en.wikipedia.org/wiki/List_of_countries_with_McDonald%27s_restaurants"

#parse the football data
food<-restaurant%>%
  read_html()%>%
  #this is the key - this is actually stored as a true table so we can easily take the xpath
  html_nodes(xpath='/html/body/div[3]/div[3]/div[5]/div[1]/table[1]')%>%
  html_table(header = TRUE)

library(tidyr)
L<-data.frame(food)
View(L)
colnames(L)[2]<-"Country"
L<-L%>%
  mutate(process_date = mdy(Date.of.first.store))

world <- map_data("world")
colnames(world)[5]<-"Country"
Zmapp<-left_join(world, L)

ggplot(data = Zmapp) + 
  geom_polygon(aes(x = long, y = lat, fill = year(process_date), group = group), color = "Orange") + scale_color_brooklyn99(type = "discrete") +
  coord_fixed(1.3)+
  theme_void()


coffee<-"https://www.silverdoorapartments.com/blog/which-country-has-the-most-starbucks-per-1000000-inhabitants/"

#parse the football data
latte<-coffee%>%
  read_html()%>%
  #this is the key - this is actually stored as a true table so we can easily take the xpath
  html_elements(css = "#sda-blog-post-content > table:nth-child(5)")%>%
  html_table(header = TRUE)

macchiato<-data.frame(latte)
cafeaulait<-left_join(world, macchiato)
frappy<-cafeaulait%>%
  mutate(breve = as.single(Number.of.Starbucks.Outlets))
ggplot(data = frappy) + 
  geom_polygon(aes(x = long, y = lat, fill = breve, group = group))+ 
  scale_fill_gradientn(colours=c("blue", "red"))+
  coord_fixed(1.3)+
  theme_void()

#we can diagnose...
anti_join(macchiato, world)


               