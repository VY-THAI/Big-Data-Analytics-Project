install.packages("remotes")
library(remotes)

# install GitHub version of vosonSML 0.32.10 
install_github("vosonlab/vosonSML")

# install GitHub version of rtweet 1.1.0.9001
install_github("ropensci/rtweet")


# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(igraph)
library(tidyr)
library(tidytext)
library(stopwords)

# Set up Twitter authentication variables

my_app_name <- "7230ICT_App"
my_api_key <- "Pp3qPc4bTEN6Zeqou7T6rDOgZ"
my_api_secret <- "YNcVqWFGsemyXVnqkPdWFWapIhkFo39sj5ODydyHmRqTxqdovM"
my_access_token <- "1634080601425985536-l5TzqA8ulXGMA9pys2NyI1HCmI43re"
my_access_token_secret <- "1AJQZyvCVps4l1hMBgem8vXYbIIjmI0rB8YZSoB5PZJU3"


# Question 1.2: Collect data about your artist/band from Twitter

#Keyword: #TaylorSwift and #ErasTour

my_8000_tweets <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#TaylorSwift OR #ErasTour",
          searchType = "recent",
          numTweets = 8000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) 


# View collected Twitter data

View(twitter_data$tweets)
View(twitter_data$users)


# Question 1.3:	List the top 5 most influential users for your artist/band.

  # Create actor network and graph from the data, to find out the relationship between the tweets and users

twitter_actor_network <- twitter_data %>% Create("actor")
twitter_actor_graph <- twitter_actor_network %>% Graph()


  # Write graph to file

write.graph(twitter_actor_graph, file = "TwitterActor.graphml", format = "graphml")


  # Run Page Rank algorithm to find top 5 most influential users

rank_twitter_actor <- sort(page_rank(twitter_actor_graph)$vector, decreasing=TRUE)
head(rank_twitter_actor, n=5)


  # Overwrite the 'name' attribute in your graph with the 'screen name' attribute
  # to replace twitter IDs with more meaningful names,then run the Page Rank algorithm again

V(twitter_actor_graph)$name <- V(twitter_actor_graph)$screen_name
rank_twitter_actor <- sort(page_rank(twitter_actor_graph)$vector, decreasing = TRUE)
head(rank_twitter_actor, n = 5)


# Question 1.4: List the top 10 most important terms that appear together with your keyword(s) 
#related to your artist/band

# Create semantic network and graph from the data

twitter_semantic_network <- twitter_data %>% Create("semantic")
twitter_semantic_graph <- twitter_semantic_network %>% Graph()

# Write graph to file

write.graph(twitter_semantic_graph, file = "TwitterSemantic.graphml", format = "graphml")

# Run Page Rank algorithm to find important terms/hashtags

rank_twitter_semantic <- sort(page_rank(twitter_semantic_graph)$vector, decreasing = TRUE)
head(rank_twitter_semantic, n = 10)

# Question 1.5: Calculate how many unique user accounts there are in your dataset

  # install library(dplyr)

library(dplyr)

  # Use n_distinct function to calculate unique user accounts

n_distinct(twitter_data[["users"]][["id"]])

# Find the number of unique users
length(unique(twitter_data_results[["users"]][["screen_name"]]))

# Count number of tweets per screen_name
unique_df <- twitter_data_results[["users"]]%>% 
  group_by(screen_name) %>% 
  summarise(Count = n())

# Bar graph to show which users have tweeted more than twice
ggplot(subset(unique_df, Count > 2), aes(x = reorder(screen_name, Count), y = Count, fill = Count)) + 
  geom_bar(stat = "identity")+ 
  coord_flip()+ theme_gdocs()+ 
  ggtitle("Users who have replied, quote retweeted or retweeted tweets related to Taylor Swift") + 
  xlab("User")+ 
  ylab("Number of interactions")+
  theme( 
    plot.title = element_text(size = 16, hjust = 0.5), 
    panel.grid.major = element_blank(), 
    legend.title = element_blank(), 
    text=element_text(family="Arial"), 
    legend.position = "none"
  )+ 
  scale_fill_gradient(low = "#5E8BA5", high = "#04253A")
