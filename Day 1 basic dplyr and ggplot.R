#These are the notes for the first big day of the class
#Learning Outcomes
#Connect Github to Rstudio
#Call Libraries, recognize ::
#Write filters including & | !=
#Use first level dplyr functions like slice_max
#ggplot2 basic graphic
#recognize use of pipes
#play with geoms and facets

#Last time we started by doing some basics, like sorting through flights

#we learned to call a library
library(nycflights13)

#We then could access the data by 
flights

#and to make our lives easier we can store it as a variable in our global environment
flights <- flights

#the arrow is the key, use that any time you want to add something to the global environment

#NYC flights has other stuff too
nycflights13::airlines
nycflights13::airports
nycflights13::planes
nycflights13::weather

#notice up there how there are :: ?
#those are how we call something WITHIN a package if we don't call it as a library
#in this case we already called nycflights, I just used :: to help us learn

#let's start with some basic dplyr stuff
library(dplyr)
#most people pronounce this de-plier, as in give me de pliers
#I often say dp-lyr like Doppeler 

#so look up your birthday
filter(flights, month ==11 & day ==1)
#this lookup had THREEE arguments, ONE on the left of the comma, two on the right
#this is a key idea - each function has a right and a left side

#you can also use other booleans
#every month EXCEPT February
filter(flights, month != 2)

#what if we want flights from Newark OR JFK? Note the ==
filter(flights, origin == "EWR" | origin == "JFK")

#let's race. Can you write a filter that gets every flight at JFK before 5 AM?

#so far we have only worked with filter, it has a partner called select which allows you to pick some columns
#there are some other super cool functions like across, which we will also deal with later
#you can also work with rows in some interesting and important ways - more on that next week

#here are a few more handy functions: 

#we just want the top ten most delayed flights
slice_max(flights, dep_delay, n=10)

#we just want the top 1% of arrival delays
slice_max(flights, arr_delay, prop = .01)

#slice min does the other thing, if you want ties 
slice_min(flights, arr_delay, prop=.01, with_ties=TRUE)

#make me a graphic of the top 500 delayed flights (WE)
late<-slice_max(flights, dep_delay, n= 500)

#make a graph
ggplot(late, aes(time_hour, dep_delay, colour= origin))+geom_jitter()

#OK team, give ol' Dan a challenge.....




#there is an error in windspeed
#don't panic, I am using a "pipe"
weather<-weather%>%filter(wind_speed < 150)

library(ggplot2)
ggplot(weather, aes(time_hour, wind_speed))+geom_jitter()

#left hand - weather
weather%>%
  #right hand filter
  filter(wind_speed > 1) %>% 
  #left is the filtered, right is another filter
  filter(precip > .1)%>%
  #left hand is double filtered, right hand is the graphic
  ggplot(aes(time_hour, wind_speed, colour= precip))+geom_jitter()


#Let's turn to a cultural question
#What happened to American Television?
#In 1988: 1. Cosby Show, 2. Roseanne, 3. Different World, 4. Cheers, 5. 60 Minutes
#6. Golden Girls, 7. Who's the Boss?, 8. Murder, She Wrote, 9. Empty Nest, 10. Anything but Love
#11. Dear John, 12. Matlock, 13. LA Law/Growing Pains, 15. ALF/Monday Night Feetball, 
#17. Unsolved Mysteries, 18. In the Heat of the Night, 19. Hunter, 20. Head of the Class, 
#21. Night Court, 22. Hogan Family/NBC Sunday Night Movie/Wonder Years, 25. Amen/NBC Monday Night Movie

#What do you notice? 
#Comedy Focus, Diverse Casts

#2019
#1. Sunday Night Feetball, 2. Big Bang Theory, 3. NCIS, 4. Thursday Night Feetball/Young Sheldon,
#6. This is Us, 7. Blue Bloods/FBI, 9. The Good Doctor/Manifest, 10. Chicago Fire/America's Got Talent,
#13. ChicagoMed/Bull 15. Chicago PD/New Amsterdam, 17. NCIS: NOLA, 18. 60 Minutes, 19. The Voice
#20. Grey's Anatomy, 21. Mom, 22. The Voice: Tuesday/Hawaii Five-O, NCIS: LA, 25. The Masked Singer

#Gebner's mean world hypothesis...

head(TV)
#what are the continuous variables here, which are discrete?
#what is that hole there? (we will fix it thursday...)

#what can we learn from the top?
slice_max(TV, Rating, n=10)

#a dense plot
ggplot(TV, aes(Year, Rating, colour=Type, shape=Spinoff))+geom_jitter()

#with smoothing
ggplot(TV, aes(Year, Rating, colour=Type))+geom_smooth()

#so we have a basic plot, we can go ahead and store that WITHOUT the geom
basic<-ggplot(TV, aes(Year, Rating, colour=Type))

#you can then add whatever you want for the geom
basic+geom_jitter()

#or add a facet
basic+geom_smooth()+facet_grid(~Network)

#how many game shows?

#tell the story of a network
TV%>%
  filter(Network == "FOX")

#What are the key moments?
ggplot(TV, aes(Year, Rating))+geom_jitter()+geom_vline(xintercept=2000)

#Telling Data Stories
#Central Tendencies
#Outliers
#First and Last Apperances of a Discrete
#returning to Beniger and Robyn (1976): why not use a table?


