#Day Two - confirming our filter knowledge
#Learning Outcomes
#Confirm basic dplyr knowldege
#Produce plots quickly using ggplot2
#Use a pipe to filter a full dataset 
#Add labels 
#map colors for discrete and continuous
#summarize a more complex dataset

#First how do you load a library
library(dplyr)
library(ggplot2)

#these are the primary libraries we will be using today.
#later on we will load another library to style some things

#let's go ahead and load the TV dataset
#click on it in files

#To start with, let's make a quick plot of the data
#the LEFT hand argument is the dataset, the RIGHT hand are the X and Y axis
#we then ADD a geom
ggplot(TV, aes(Year, Rating))+geom_jitter()

#the grammar of graphics: we define the data, then the aesthetics, then we define styles
ggplot(TV, aes(Year, Rating))+geom_jitter()+coord_polar()

#Let's start with a few filters, for example: what is are the lowest five rated shows
slice_min(TV, Rating, n = 5)

#what if we wanted to slice randomly for a sample
slice_sample(TV, n=5)

#let's race...




#What if we want to COMBINE a FILTER and a GRAPHIC with out a lot of cumbersome code
#use a pipe, control+shift+m
#LEFT HAND
TV %>% 
  #right hand is a filter
  filter(Rating>10) %>% 
  #the LEFT HAND of this ggplot is the FILTERED data
  ggplot(aes(Year, Rating))+geom_jitter()

#NOW LETS have a GRAPHICAL RACE!



#we should take a step back and really think about our geoms, jitter is GREAT, but you might want something else
glimpse(TV)

#take a look at the data types, which are continuous, which are discrete?
#why would Program be discrete, but a BAD idea for use as anything except a label?

#let's go with a discrete to start, geom_bar
ggplot(TV, aes(Network))+geom_bar()

#and now 
ggplot(TV, aes(Rating))+geom_histogram()

#bar is for discrete, histogram is for continuous
#notice that bin warning? Bins are how wide each column is

#compare
ggplot(TV, aes(Rating))+geom_histogram(bins =100)
ggplot(TV, aes(Rating))+geom_histogram(bins = 5)

#bin width is really important, you might lose ENTIRE parts of a datastory

#let's try two continuous
ggplot(TV, aes(Rank, Rating))+geom_rug()

#one discrete and one continuous, and lets throw in a discrete color
ggplot(TV, aes(Type, Rating, colour=Network))+geom_col()

#those are some ugly colors alright, we have 5 of them
#lets install a package called tvthemes - time to learn how to install a package
#don't worry, lots of red stuff is about to happen, thats not bad. 
library(tvthemes)
library(ggthemes)
ggplot(TV, aes(Type, Rating, colour=Network))+geom_col()+scale_color_brooklyn99()

#go head and change this, use a DIFFERENT GEOM and switch your color as well, if you just delete brooklyn99..

#and now we give it a title...
ggplot(TV, aes(Type, Rating, colour=Network))+geom_col()+scale_color_brooklyn99()+labs(title="Noice!")

#ok and we add some more colors...
ggplot(TV, aes(Type, Network, colour=Rating))+geom_jitter()+scale_color_brooklyn99(type="continuous")

#we can also style it
library(ggthemes)
ggplot(TV, aes(Year, Rating))+geom_point(aes(colour=Network))+scale

#Let's move on to some harder, weirder data
#load the weddings data...
glimpse(weddings)

#this dataset has everything, categorial, discrete, location, names, and even different levels of measure

#two quick cleaning tasks, can you get rid of the row with the NAs?
#rename something

#be sure to use the storage arrow, tidy is non-destructive
weddings<-weddings%>%
  #new name first
  rename(Bride.Name = Bride.1)
  


#lets just have fun for a second
ggplot(weddings, aes(State, Budget, size = 1/Result, colour=Experience))+
  geom_jitter()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_gradient(low="green",high="darkgreen")+
  labs(title="When More is Less")

#now change that up a litte... take five minutes and chat with your pals...

#Now we need to learn some more advanced dplyr stuff, and for this you will need to use pipes
#what if we want to know some averages
weddings%>%
  group_by(Result)%>%
  summarize(mean(Experience), mean(Dress), mean(Food), mean(Venue))

#let's get rigorous, here is a correlation coefficient 
cor.test(weddings$Dress, weddings$Result)
cor.test(weddings$Food, weddings$Result)
#WHY AREN'T THESE WORKING?! MATH WHY DO YOU BETRAY ME?!

#We could take the recriprocal 
cor.test(weddings$Venue, 1/weddings$Result)

#that makes more sense
#so which one matters most to victory?

#we need a new column with the total score
w2<-weddings%>%
  mutate(Total = Dress+Venue+Food+Experience)

#now we can use w2

#let's have some fun, how much can you cram into a plot?





#this here is the fancy math
w2<-w2%>%
  #the denomniator solves the missing bride problem
  mutate("meanness" = ((C1+C2+C3+C4)/3))%>%
  #this step in the algorithm uses the meanness recurisively to find zmean
  mutate("zmean" = (meanness-mean(meanness))/sd(meanness))


#lets plot that
ggplot(w2, aes(zmean, Total, shape = as.factor(Result), colour=Budget))+geom_jitter()+scale_color_gradient(low="salmon4",high="salmon")

#or a rigorous test
cor.test(w2$Total, w2$zmean)

#it seems like we need to be less into central tendency and more into outliers
w2%>%
  #gave a bad score, but wasn't their own wedding
  filter(C4 < 5 & C4>0)%>%
  #what was the result
  count(Result)
  #rewrite the code for each bride

#We should look at that...
w3<-w2%>%
  filter(C1>0 & C1 <5)%>%
  arrange(C1)
View(w3)

#a rigorous test
cor.test(w2$zmean, 1/w2$Result)
cor.test(w2$zmean, w2$Budget)
cor.test(w2$zmean, w2$BudgetPerGues)
cor.test(w2$zmean, w2$Age)



