#install these
library(textfeatures)
library(tidytext)

#load these (and the ones above too)
library(ggplot2)
library(dplyr)
library(tidyr)

#for documentation see ??count_functions

features<-textfeatures(taylor_5$lyric, sentiment=FALSE)
featured_taylor<-inner_join(taylor_5, features)
View(featured_taylor)



#what can we infer....

#let's do some counting...
taylor_words <- taylor_5 %>%
  unnest_tokens(word, lyric) %>%
  count(album_name, word, sort = TRUE)

total_words <- taylor_words %>% 
  group_by(album_name) %>% 
  summarize(total = sum(n))

today_words <- inner_join(taylor_words, total_words, by="album_name")

beep<-today_words%>%
  mutate(index=n/total)

ggplot(today_words, aes(n, n/total, colour=album_name))+geom_text(aes(label=word), check_overlap = TRUE)+facet_wrap(~album_name)


library(tidytext)
today_bigrams<-taylor_5%>%
  #name the colmn, where do you get the text from, what token type is it, how many in the ngram
  unnest_tokens(bigram, lyric, token="ngrams", n=2)


#this is when we just do normal tidy stufff
today_bigrams%>%
  count(bigram, sort=TRUE)

library(tidyr)
#this is a godo chance ot check for negations
today_sep<-today_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#we can remove what are called stopwords
today_nostop<- today_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

View(today_nostop)

#we then can flag and mange negations, qualifiers, and modifiers
today_nostop %>%
  filter(word1== "no" | word1 == "never") %>%
  count(word1, word2, sort = TRUE)

#Zipf's law
#term frequency
freq_by_rank <- today_words %>% 
  group_by(album_name) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

#fairly clear plotting - notice the early truncation of the distrivution
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = album_name)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()

#and because we like to party
library(network)
library(ggnetwork)
TS<-today_nostop %>% 
  filter(word1 != "")

edgelist<-select(TS, c(word1,word2))
news<-network(j, directed = FALSE, loops=TRUE)
j<-news[-c(10,11,12,26,38,58),]


#detect the modularities
pulse<-as.matrix.network.adjacency(news)
l<-kcores(pulse)

#grab the detected modularities from 
news %v% "mod" <- l

#lay it out
plot<-ggnetwork(news, layout="fruchtermanreingold")

#now render that bad boy
ggplot(plot, aes(x, y, xend = xend, yend = yend)) +
  #just make the lines orange
  geom_edges(aes(color="pink"))+
  #set the other aesthetics as static values
  geom_nodetext(aes(colour = as.factor(louvain), label = vertex.names))+
  theme_blank()

