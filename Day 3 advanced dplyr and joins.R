#day 3 - summary functions and joins
#learning objectives
#write a summary function using pipes
#use select and column tools 
#isolate a key column
#complete a basic inner_join
#distinguish uses of the types of joins

#Let's start with a throwback example
#how many television shows are from each type
library(ggplot2)
library(dplyr)
#load your TV dataset

#we need to write a function that will let us see how many shows
#the left hand is tv
TV %>% 
  count(Type) %>% 
  #and because we like clean outputs
  #why "n?" because its the number that was counted
  arrange(desc(n))


TV %>% 
  count(Type) %>% 
  #and because we like clean outputs
  #why "n?" because its the number that was counted
  arrange(desc(n))%>%
  ggplot(aes(n, colour=Type))+geom_histogram(bins=50)


#lets do one that is a little harder...
#what if we want to summarize a continuous
TV %>% 
  group_by(Type)%>%
  #notice that we are giving our new column a name
  summarize(average_rating=mean(Rating, na.rm=TRUE))%>%
  arrange(desc(average_rating))

#we might want to know when these happened, right?
TV %>% 
  group_by(Type)%>%
  #notice that we are giving our new column a name
  summarize(average_rating=mean(Rating, na.rm=TRUE), peak_year=mean(Year))%>%
  arrange(desc(average_rating))

#wait, this next thing is some fancy math called an ANOVA, don't panic
one_way<-aov(Rating ~ Year, data=TV)

#here is the summary
summary(one_way)
#interpretation - F value - higher is better, Pr - lower is better

#or visually
plot(one_way)

#we can also block it
two_way<-aov(Rating ~ Year + Type, data=TV)

#rival F and PR values...
summary(two_way)

plot(two_way)

#we could also get it in tabular form
TV%>%
  #pick two years
  filter(Year == 2008)%>%
  group_by(Type)%>%
  summarize(mean(Rating))

#you also might want to try an interaction model
interaction<-aov(Rating ~ Year*Type, data=TV)
summary(interaction)

#lets run it again for network
interaction<-aov(Rating ~ Year*Network, data=TV)
summary(interaction)

#we could isolate an alternative hypothesis...
level<-aov(Rating ~ Type, data=TV)
summary(level)

#maybe all those spinoffs are the key
interaction2<-aov(Rating ~ Year + Network + Spinoff + 
                    Type + Spinoff*Type + Network*Spinoff, data=TV)
summary(interaction2)
plot(interaction2)

#go ahead and write the code for a summary where you get the mean rating, per year, for Sports


#a few other functions
#sometimes you just want a row from a table

#if you want a vector...
TV_vector<-TV %>% 
  pull(Rating)

#or as a table...
TV_table<-TV %>% 
  select(Rating)

#Why is this distinction important?

#in a very WIDE dataset it gets annoying when your column is like 250 deep
relocate(TV, Rating, before = Rank)
#yes that did rename rank, and yes I think its an error that they will fix like next week...

#so we can filter, we can select, we can rename, we can rearrange
#are you starting to see that we are getting toward a kind of math called set theory?
#set theory, check it out
https://en.wikipedia.org/wiki/Set_theory

#isn't that some cool math?

#with our tools we can use filtering joins, but these are typically redundant with basic filters
#the deep power in the join game are mutating joins
#left, right, and inner_join.

#go ahead and load weddings
#join state colors
w2<-inner_join(weddings, state_colors)

#which state color has the most expensive weddings?
w2%>%
  group_by(Color1)%>%
  summarize(cost = mean(Budget))%>%
  arrange(desc(cost))

#now a filtering join
anti_join(state_economies, state_colors)
#WHY DIDN't THIS WORK?

#why does this work?
left_join(state_colors, state_populations)

#FIND THE KEY, CORRECT THE KEY
#Four Strategies to Correct to join...
#retains both
mutate()
#pure rename
rename()
#destructive BaseR 
colnames()[]
X<-data$var
Data2<-data.frame(data, "new name"= X)

#NOW MORE WEDDINGS!

#we should make a total column too
w3<-w2 %>% 
  mutate("Total" = Dress+Venue+Food+Experience)

w4<-w3%>%
  #the denomniator solves the missing bride problem
  mutate("meanness" = ((C1+C2+C3+C4)/3))%>%
  #this step in the algorithm uses the meanness recurisively to find zmean
  mutate("zmean" = (meanness-mean(meanness))/sd(meanness))

#now we really party
wed_effect<-aov(Total ~ Budget + 
                  Food + Dress + Venue + Experience + Bride + zmean,  data = w4)
summary(wed_effect)
