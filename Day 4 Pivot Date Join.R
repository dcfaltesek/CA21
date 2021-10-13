#day 4 learning objectives  
#pivot from longer to wider data
#use lubridate to interpret dates
#reprocess dates
#use a join to attach shapefiles to a data frame
#make a GGplot of that 

#pivot
#to start with, we should import weddings and state colors

library(tidyr)
library(dplyr)

#pivots are very useful so you can see your data in wide format
pivot_wider(state_colors, 
            id_cols = State, 
            names_from = Color1, 
            values_from = Color1, 
            values_fill = "Boring")

#if you want go ahead and store that in a variable, I DID NOT WRITE THAT IN THE CODE

#that is sort of hard to read too, right? All those borings. The problem is that we need a different way to encode the cells

#you all seem like you are ready for this, so here comes a big idea that we didn't plan for
#control structures. 
colors_2<-state_colors%>%
  #we get the idea of a mutate, you add a new column with a new name
  #in this case the colum we are adding uses an ifelse logic where the
  #left argument is the test, the middle argument is IF TRUE, the RIGHT argument is if false
  mutate(my_color = ifelse(Color1 == "None", "0", "1"))
#you can nest ifelse functions
#I wrote this mutate to use the == operator, can you rewrite it to use !=?

colors_3<-colors_2%>%
  pivot_wider(
    id_cols = State,
    names_from = Color1,
    values_from = my_color,
    values_fill= "0"
  )

View(colors_3)

#now let's do some weddings examples
#we will start by cooking up a unique identifier for each wedding 
w6<-unite(weddings, ID, 1:3, sep="-", remove=FALSE)
#IMPORTANT UNITE NOTE - if you don't say FALSE then it deletes the columns

#this data is just too dang wide, lets select some useful columns
w7<-w6%>%
  select(c(ID, State, Budget, Result, C1,C2,C3,C4, Season))

pivot_wider(w7, 
            id_cols = ID,
            names_from = State,
            values_from = Budget)

#here is a fun one...
w7 %>% 
  group_by(State)%>%
  summarise(woot=mean(Budget)) %>% 
  mutate(Review=if_else(woot > 25000, "Terrible life choice.", 
         if_else(woot < 25000 & woot > 10000, "A toyota camry>", "I guess you don't love each other?")))%>%
              pivot_wider(id_cols=State, names_from=Review, values_from=woot, values_fill=0)

#you can in fact pivot and summarize AT THE SAME TIME

w8<-w7%>%
  pivot_wider(
    id_cols = Season, 
    names_from=State,
    values_from=Budget, 
    values_fn=mean, 
    values_fill=0
  )

#pivoting back... Let's use this example to help see how it works...
#go ahead and load zords
#call the dataset
#let's go
zords%>%
  #the reference column is ranger and we want to make EVERY other column longer, so we do -
  pivot_longer(cols=-Ranger)

#and we can plot this
Z<-zords%>%
  #move down every column except Ranger
  pivot_longer(cols=-Ranger,
               #make the new values column Zord
               values_to = "Zord",
               #and those names to
               names_to = "Era",
               #kick all those NA values to the curb
               values_drop_na=TRUE)

ggplot(data = Z) +
  #geom_count is the only discreet, discreet method we have on hand. 
  geom_count(mapping = aes(x = Era, y = Ranger, colour=Ranger))

#that was just mean

#here, this might be more plesant
ggplot(Z, aes(Ranger, colour=Ranger))+geom_bar()


#Let's quick unpivot w9
#unpivot everyting EXCEPT season
w8%>%
  pivot_longer(-Season)

#you could now rejoin if needed OR plot the values...

#plotter
w8%>%
  pivot_longer(-Season)%>%
  filter(value > 500)%>%
  ggplot(aes(Season, value, colour=name))+geom_jitter()


#is their a relationship between season and value? 
#the easy way
w9<-w8%>%
  pivot_longer(-Season)%>%
  filter(value > 500)

#boom
cor.test(w9$Season, w9$value)

two_way<-aov(Season ~ value + name + value*name, data=w9)
summary(two_way)

#dates
library(lubridate)
#step one, figure out the format
wD<-weddings%>%
  mutate(lubridate = myd(weddings$Date))
#go check column 31 of your data...

#this is an expression of my care?
#yday on a polar coordinate plane
ggplot(wD, aes(yday(lubridate), Budget, colour=State))+geom_jitter()+coord_polar()

#that is not a thing
why_are_you_doing_this<-aov(yday(lubridate) ~ Budget + Result + Food, data=wD)
summary(why_are_you_doing_this)

#you can use any lubridate time call
ggplot(wD, aes(month(lubridate), Budget, colour=State))+geom_jitter()+coord_polar()

#NOW maps
#we need shape files
library(maps)
library(ggplot2)
states <- map_data("state")
head(states)
#ask yourself - how are they encoding state?

#let's make a version of our data that has lower case state names
w9<-weddings%>%
  mutate(state = tolower(weddings$State))
#confirm some shit

glimpse(states)

#lets rename and hit it...
colnames(states)[5]<-"state"
mappy<-left_join(w9, states)
mappy2<-right_join(w9, states)
#what do you notice about mappy?

#lets draw the shapes
ggplot(mappy, aes(long, lat)) + 
  geom_point(size = .25, show.legend = FALSE) +
  coord_quickmap()

library(tvthemes)
#This is a very old fashioned way to do this, but it is clearly better than geom_map or geom_sf
ggplot(data = mappy2) + 
  geom_polygon(aes(x = long, y = lat, fill = Budget, group = group), color = "Orange") + scale_color_brooklyn99(type = "continuous") +
  coord_fixed(1.3)+scale_fill_gradientn(colours=c("blue", "red"))+
  labs(title="Orange and Purple", subtitle = "Because I Like to Party?", fill="Mean Budget")+
  theme_void()

