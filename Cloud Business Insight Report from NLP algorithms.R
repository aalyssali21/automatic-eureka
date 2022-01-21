###############################################################################
##############################   annual report   ##############################
###############################################################################

################# library #########################
library(tidytext)
library(tidyverse)
library(tidyr)
library(tidytuesdayR)
library(stringr)
library(textreadr)
library(pdftools)
library(textshape)
library(twitteR)
library(tm)
library(ggplot2)
library(scales)
library(magrittr)
library(dplyr)
library(gutenbergr)
library(Matrix)

library(reshape2)
library(wordcloud)
library(textdata)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)
library(gutenbergr)
library(quanteda)
library(quanteda.textmodels)
library(RColorBrewer)
library(rtweet)
library(httpuv)


library(pdftools) # we need this library to use pdf_text
setwd("/Users/lilialyssali/Downloads/Text Analytics/Financial Reports")
nm <- list.files(path="/Users/lilialyssali/Downloads/Text Analytics/Financial Reports")

google_data <- read_document(file=nm[1]) #This comes out as a vector, a list of strings
google_data_together <- paste(google_data, collapse = " ") # This will give us a concatenated vector, one string
google_text <- do.call(rbind, lapply(nm[1], function(x) paste(read_document(file=x), collapse = " "))) # each string for each file
google <- data.frame(google_text)
colnames(google) <- "text"

microsoft_data <- read_document(file=nm[3]) #This comes out as a vector, a list of strings
microsoft_data_together <- paste(microsoft_data, collapse = " ") # This will give us a concatenated vector, one string
microsoft_text <- do.call(rbind, lapply(nm[3], function(x) paste(read_document(file=x), collapse = " "))) # each string for each file
microsoft <- data.frame(microsoft_text)
colnames(microsoft) <- "text"

amazon_data <- read_document(file=nm[2]) #This comes out as a vector, a list of strings
amazon_data_together <- paste(amazon_data, collapse = " ") # This will give us a concatenated vector, one string
amazon_text <- do.call(rbind, lapply(nm[2], function(x) paste(read_document(file=x), collapse = " "))) # each string for each file
amazon <- data.frame(amazon_text)
colnames(amazon) <- "text"


##############################   tidy format   ##############################
tidy_google <- google %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word,sort = TRUE)

tidy_microsoft <- microsoft %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word,sort=TRUE)

tidy_amazon <- amazon %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word,sort = TRUE) 
##########################################
######## customize stop words ############
##########################################
custom_stop_words <- tribble(
  # Column names should match stop_words
  ~word, ~lexicon,
  # Add http, win, and t.co as custom stop words
  "http", "CUSTOM",
  "win", "CUSTOM",
  "t.co", "CUSTOM"
)

# Bind the custom stop words to stop_words
stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words)

# exclude common words in three datasets
inner_1 <- tidy_google %>% inner_join(tidy_microsoft, by = "word")
inner_2 <- inner_1 %>% inner_join(tidy_amazon,by="word")
tidy_google <- anti_join(tidy_google,inner_2,by="word")
tidy_amazon <- anti_join(tidy_amazon,inner_2,by="word")
tidy_microsoft <- anti_join(tidy_microsoft,inner_2,by="word")

##############################   term frequency   ##############################
# google
google_freq_hist <- tidy_google %>%
  mutate(word=reorder(word, n)) %>% # a factor column can include information about the 
  # order in which the words should appear. word is the column we want to reorder, n is 
  # the column we want to reorder by.
  filter(n>12) %>% 
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle("Term Frequency Histogram for Google") # add title to graph
print(google_freq_hist)
# microsoft
microsoft_freq_hist <- tidy_microsoft %>%
  mutate(word=reorder(word, n)) %>%
  filter(n>15) %>% 
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle("Term Frequency Histogram for Microsoft") # add title to graph
print(microsoft_freq_hist)
# amazon
amazon_freq_hist <- tidy_amazon %>%
  mutate(word=reorder(word, n)) %>%
  filter(n>10) %>% 
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle("Term Frequency Histogram for Amazon") # add title to graph
print(amazon_freq_hist)

###############################   correlograms   #############################
frequency <- bind_rows(mutate(tidy_google, author="google"),
                       mutate(tidy_microsoft, author= "microsoft"),
                       mutate(tidy_amazon, author="amazon")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  filter(!nchar(word)==1) %>% 
  filter(!nchar(word)==2) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `microsoft`, `amazon`)

ggplot(frequency, aes(x=proportion, y=`google`, 
                      color = abs(`google`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "google", x=NULL)

cor.test(data=frequency[frequency$author == "microsoft",],
         ~proportion + `google`)
cor.test(data=frequency[frequency$author == "amazon",],
         ~proportion + `google`)

############################   Ngrams   ##########################
# google
google_quadrograms <- google %>% 
  unnest_tokens(quadrograms,text,token="ngrams",n=4) %>% 
  separate(quadrograms,c("word1","word2","word3","word4"),sep = " ") %>% 
  filter(!word1 %in% stop_words) %>% 
  filter(!word2 %in% stop_words) %>%
  filter(!word3 %in% stop_words) %>%
  filter(!word4 %in% stop_words) 

google_quadrograms_counts <- google_quadrograms %>% 
  count(word1,word2,word3,word4,sort = TRUE)

google_quadrogram_graph <- google_quadrograms_counts %>% 
  filter(n>12) %>% 
  graph_from_data_frame()

ggraph(google_quadrogram_graph,layout="fr") + 
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name),vjust=1,hjust=1)
# microsoft
microsoft_quadrograms <- microsoft %>% 
  unnest_tokens(quadrograms,text,token="ngrams",n=4) %>% 
  separate(quadrograms,c("word1","word2","word3","word4"),sep = " ") %>% 
  filter(!word1 %in% stop_words) %>% 
  filter(!word2 %in% stop_words) %>%
  filter(!word3 %in% stop_words) %>%
  filter(!word4 %in% stop_words) 

microsoft_quadrograms_counts <- microsoft_quadrograms %>% 
  count(word1,word2,word3,word4,sort = TRUE)

microsoft_quadrogram_graph <- microsoft_quadrograms_counts %>% 
  filter(n>8) %>% 
  graph_from_data_frame()

ggraph(microsoft_quadrogram_graph,layout="fr") + 
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name),vjust=1,hjust=1)
# amazon
amazon_quadrograms <- amazon %>% 
  unnest_tokens(quadrograms,text,token="ngrams",n=4) %>% 
  separate(quadrograms,c("word1","word2","word3","word4"),sep = " ") %>% 
  filter(!word1 %in% stop_words) %>% 
  filter(!word2 %in% stop_words) %>%
  filter(!word3 %in% stop_words) %>%
  filter(!word4 %in% stop_words) 

amazon_quadrograms_counts <- amazon_quadrograms %>% 
  count(word1,word2,word3,word4,sort = TRUE)

amazon_quadrogram_graph <- amazon_quadrograms_counts %>% 
  filter(n>8) %>% 
  graph_from_data_frame()

ggraph(amazon_quadrogram_graph,layout="fr") + 
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name),vjust=1,hjust=1)


############################   tf-idf   ##########################
#we're grouping by the company
all_tokens <- bind_rows(mutate(tidy_google, author="google"),
                        mutate(tidy_microsoft, author= "microsoft"),
                        mutate(tidy_amazon, author="amazon")
) # after removing all the common words

total_words <- all_tokens %>%
  group_by(author) %>%
  summarize(total=sum(n))

words <- left_join(all_tokens, total_words)

print(words)

ggplot(words, aes(n/total, fill = author))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.003) +
  facet_wrap(~author, ncol=2, scales="free_y")  ##### left side stands for big business potential
#the tails represent extremely common words, but we are really interested in the not so common words. 

###################################################
################# TF_IDF ##########################
###################################################

company_words <- words %>%
  bind_tf_idf(word, author, n)  # why is "country" here? 

company_words # we get all the zeors because we are looking at stop words ... too common

arranged_idf <- company_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
company_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=author))+
  geom_col(show.legend=FALSE)+                # no legend
  labs(x=NULL, y="tf-idf")+         
  facet_wrap(~author, ncol=2, scales="free")+ # facet based on ~author, free_y means
  # y-axis can be different for each plot
  coord_flip()

#################################################################################
################# Latent Dirichlet Allocation algorithm #########################
#################################################################################
dtm <- words %>% cast_dtm(author,word,n)
ap_lda <- LDA(dtm, k=4, control=list(seed=123))
ap_lda

#now we are looking for the per topic per word probabilities
#beta - what is the probability that "this term" will be generated by "this topic"
ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics


top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1
#and words in topic 2

beta_spread <- ap_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >.001) %>%
  mutate(log_rate_google_cloud = log2(topic4/topic3),
         log_rate_amazon_cloud= log2(topic4/topic2)) # practice for more than 2 topics

google_cloud_insights <- beta_spread[,c("term","log_rate_google_cloud","log_rate_amazon_cloud")] %>% 
  filter(log_rate_google_cloud>-3,log_rate_google_cloud<3) %>% 
  arrange(desc(log_rate_google_cloud))

amazon_cloud_insights <- beta_spread[,c("term","log_rate_google_cloud","log_rate_amazon_cloud")] %>% 
  filter(log_rate_amazon_cloud>-6,log_rate_amazon_cloud<6) %>% 
  arrange(desc(log_rate_amazon_cloud))

gamma_topics <- tidy(ap_lda, matrix="gamma") #we created ap_lda in our LDA scripts 
gamma_topics #this will show the results in the console

###############################################################################
#################  Social media NLP: Tweeter as open source   #################
###############################################################################

tweet_google <- search_tweets("#google + #cloud", n = 10000, include_rts = FALSE,lang="en")
tweet_amazon <- search_tweets("#amazon + #cloud", n = 18000, include_rts = FALSE,lang="en")
tweet_microsoft <- search_tweets("#microsoft + #cloud", n = 10000, include_rts = FALSE, lang="en")


tweet_microsoft_text <- tweet_microsoft$text
tweet_microsoft_df <- data.frame(line=1:276,text=tweet_microsoft_text)

tweet_google_text <- tweet_google$text
tweet_google_df <- data.frame(line=1:173,text=tweet_google_text)

tweet_amazon_text <- tweet_amazon$text
tweet_amazon_df <- data.frame(line=1:93,text=tweet_amazon_text)

########################   Building tidy formate   ###########################
tidy_tweet_microsoft <- tweet_microsoft_df %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word,sort=TRUE)
tidy_tweet_google <- tweet_google_df %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word,sort=TRUE)
tidy_tweet_amazon <- tweet_amazon_df %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word,sort=TRUE)

####################   Building term frequency histogram   ###################
microsoft_freq_hist <- tidy_tweet_microsoft %>%
  mutate(word=reorder(word, n)) %>%
  filter(n>10) %>% 
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle("Term Frequency Histogram for Microsoft")
print(microsoft_freq_hist)

google_freq_hist <- tidy_tweet_google %>%
  mutate(word=reorder(word, n)) %>%
  filter(n>10) %>% 
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(google_freq_hist)

amazon_freq_hist <- tidy_tweet_amazon %>%
  mutate(word=reorder(word, n)) %>%
  filter(n>5) %>% 
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(amazon_freq_hist)

###############################   correlograms   #############################
frequency <- bind_rows(mutate(tidy_tweet_microsoft, author="microsoft"),
                       mutate(tidy_tweet_google, author= "google"),
                       mutate(tidy_tweet_amazon, author="amazon")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  filter(!nchar(word)==1) %>% 
  filter(!nchar(word)==2) %>% 
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `google`, `amazon`)

ggplot(frequency, aes(x=proportion, y=`microsoft`, 
                      color = abs(`microsoft`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "microsoft", x=NULL)

cor.test(data=frequency[frequency$author == "google",],
         ~proportion + `microsoft`)

cor.test(data=frequency[frequency$author == "amazon",],
         ~proportion + `microsoft`)

################   sentiment analysis: three lexicons comparison   #############
apple_afinn <- apple_frequency_tokens_nostop %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

apple_bing_and_nrc <- bind_rows(
  apple_frequency_tokens_nostop %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  apple_frequency_tokens_nostop %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(apple_afinn, apple_bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
# comparison in three in afinn
##############################################################
microsoft_afinn <- tidy_tweet_microsoft %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(company="microsoft")
google_afinn <- tidy_tweet_google %>% 
  inner_join(get_sentiments("afinn")) %>% 
  summarise(sentiment=sum(value)) %>% 
  mutate(company="google")
amazon_afinn <- tidy_tweet_amazon %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(company="amazon")
bind_rows(microsoft_afinn,google_afinn,amazon_afinn) %>%
  ggplot(aes(company, sentiment, fill=company))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~company, ncol =1, scales= "free_y")

##############################################################
# comparison in three in nrc
##############################################################
microsoft_nrc <- tidy_tweet_microsoft %>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment %in% c("positive", "negative"))) %>%
  mutate(method = "NRC") %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative) %>% 
  mutate(company="microsoft")

google_nrc <- tidy_tweet_google %>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment %in% c("positive", "negative"))) %>%
  mutate(method = "NRC") %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative) %>% 
  mutate(company="google")

amazon_nrc <- tidy_tweet_amazon %>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment %in% c("positive", "negative"))) %>%
  mutate(method = "NRC") %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative) %>% 
  mutate(company="amazon")

bind_rows(microsoft_nrc,google_nrc,amazon_nrc) %>%
  ggplot(aes(company, sentiment, fill=company))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~company, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################
##### nrc #######
nrc_counts <- tidy_tweet_microsoft %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word,sentiment,sort = T) %>% 
  ungroup()

nrc_counts

nrc_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

##### bing ######
bing_counts <- tidy_tweet_microsoft %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


tidy_tweet_microsoft %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 500)

tidy_tweet_google %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 800)

tidy_tweet_amazon %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 700)

############################   Ngrams   ##########################
# microsoft
microsoft_quadrograms <- tweet_microsoft_df %>% 
  unnest_tokens(quadrograms,text,token="ngrams",n=4) %>% 
  separate(quadrograms,c("word1","word2","word3","word4"),sep = " ") %>% 
  filter(!word1 %in% stop_words) %>% 
  filter(!word2 %in% stop_words) %>%
  filter(!word3 %in% stop_words) %>%
  filter(!word4 %in% stop_words) 

microsoft_quadrograms_counts <- microsoft_quadrograms %>% 
  count(word1,word2,word3,word4,sort = TRUE)

microsoft_quadrogram_graph <- microsoft_quadrograms_counts %>% 
  filter(n>3) %>% 
  graph_from_data_frame()

ggraph(microsoft_quadrogram_graph,layout="fr") + 
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name),vjust=1,hjust=1)
# google
google_quadrograms <- tweet_google_df %>% 
  unnest_tokens(quadrograms,text,token="ngrams",n=4) %>% 
  separate(quadrograms,c("word1","word2","word3","word4"),sep = " ") %>% 
  filter(!word1 %in% stop_words) %>% 
  filter(!word2 %in% stop_words) %>%
  filter(!word3 %in% stop_words) %>%
  filter(!word4 %in% stop_words) 

google_quadrograms_counts <- google_quadrograms %>% 
  count(word1,word2,word3,word4,sort = TRUE)

google_quadrogram_graph <- google_quadrograms_counts %>% 
  filter(n>3) %>% 
  graph_from_data_frame()

ggraph(google_quadrogram_graph,layout="fr") + 
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name),vjust=1,hjust=1)
# amazon
amazon_quadrograms <- tweet_amazon_df %>% 
  unnest_tokens(quadrograms,text,token="ngrams",n=4) %>% 
  separate(quadrograms,c("word1","word2","word3","word4"),sep = " ") %>% 
  filter(!word1 %in% stop_words) %>% 
  filter(!word2 %in% stop_words) %>%
  filter(!word3 %in% stop_words) %>%
  filter(!word4 %in% stop_words) 

amazon_quadrograms_counts <- amazon_quadrograms %>% 
  count(word1,word2,word3,word4,sort = TRUE)

amazon_quadrogram_graph <- amazon_quadrograms_counts %>% 
  filter(n>1) %>% 
  graph_from_data_frame()

ggraph(amazon_quadrogram_graph,layout="fr") + 
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name),vjust=1,hjust=1)


############################   tf-idf   ##########################
#we're grouping by the country this time
all_tokens <- bind_rows(mutate(tidy_tweet_microsoft, author="microsoft"),
                        mutate(tidy_tweet_google
                               , author= "google"),
                        mutate(tidy_tweet_amazon, author="amazon")
)
comparison_tokens <- all_tokens %>%
  count(author, word, sort=TRUE) %>%  #### count on the "country" level, each country each "doc"
  ungroup()

total_words <- all_tokens %>%
  group_by(author) %>%
  summarize(total=sum(n))

words <- left_join(all_tokens, total_words)

print(words)

ggplot(words, aes(n/total, fill = author))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.005) +
  facet_wrap(~author, ncol=2, scales="free_y")  ##### left side stands for big business potential
#the tails represent extremely common words, but we are really interested in the not so common words. 

###################################################
################# TF_IDF ##########################
###################################################

company_words <- words %>%
  bind_tf_idf(word, author, n)  

company_words # we get all the zeors because we are looking at stop words ... too common

arranged_idf <- company_words %>%
  arrange(desc(tf_idf))


#############
# looking at the graphical apprach:
company_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=author))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~author, ncol=2, scales="free")+
  coord_flip()








