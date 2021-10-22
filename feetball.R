#SOURCE THIS ENTIRE FILE!
#our football program
library(stringr)
library(rvest)
library(dplyr)

feetballer<-function(X){
J<<-paste("https://www.sports-reference.com/cfb/years/20",X,"-schedule.html", sep="")

#parse the football data
foot<-J%>%
  read_html()%>%
  #this is the key - this is actually stored as a true table so we can easily take the xpath
  html_nodes(xpath='//*[@id="schedule"]')%>%
  html_table(header = TRUE)

#make the data tidy
foot<-data.frame(foot)
print(foot)
foot<-filter(foot, Pts>0)
full_data<<-foot
#structure data 
football_data<<-rename(foot, "SOURCE" = "Loser")%>%
  rename("TARGET" = "Winner")%>%
  select(SOURCE, TARGET)

#now cleaning
A<-str_replace_all(football_data$SOURCE, "\\(", "")
B<-str_replace_all(A, "\\)", "")
C<-str_replace_all(B, "[:digit:]", "")
D<-str_trim(C, side="left")

G<-str_replace_all(football_data$TARGET, "\\(", "")
H<-str_replace_all(G, "\\)", "")
I<-str_replace_all(H, "[:digit:]", "")
J<-str_trim(I, side="left")

#combine and rename
clean_football<-data.frame(D, J)
clean_football<-rename(clean_football, "SOURCE" = "D")%>%
  rename("TARGET" = "J")
foot_meta<-select(foot, -Winner,-Loser)
foot_meta<-foot_meta%>%
  filter(Rk != "Rk")
View(clean_football)
#grab all but the rows that include "loser"
clean_football<<-filter(clean_football, SOURCE != "Loser")
}

