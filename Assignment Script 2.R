
# Data Selection & Exploration

library(Rspotify)
library(spotifyr)
library(magrittr)
library(igraph)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggridges)
library(httpuv)

options(httr_oauth_cache = TRUE)

app_id <- "6ea975cba3824221bf0faf101395e90b"
app_secret <- "c734afa10aa2429186962615de0e2df6"
token <- "1"

keys <- spotifyOAuth(token, "6ea975cba3824221bf0faf101395e90b", "c734afa10aa2429186962615de0e2df6") 

# 2.1) Use the Spotify API to extract data about your artist/band.

# Get Spotify data on 'Taylor Swift'

find_my_artist <- searchArtist("Taylor Swift", token = keys)
View(find_my_artist)

# Retrieve information about artist

my_artist <- getArtist("06HL4z0CvFAxyc27GXpf02", token = keys)
View(my_artist)


# Retrieve album data of artist

albums <- getAlbums("06HL4z0CvFAxyc27GXpf02", token = keys)
View(albums)


# Find number of albums published
n_distinct(albums$name)

# Retrieve song data (from album 'Lover')

songs <- getAlbum("1NAmidJlEaVgA3MpcPFYGq", token = keys)
View(songs)


# Retrieve information about related artists

related_bm <- getRelated("Taylor Swift", token = keys)
View(related_bm)

# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = "6ea975cba3824221bf0faf101395e90b")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "c734afa10aa2429186962615de0e2df6")
access_token <- get_spotify_access_token()

# Get audio features for 'Taylor Swift'

audio_features <- get_artist_audio_features("Taylor Swift")
View(audio_features)

audio_features <- audio_features[!duplicated(audio_features$track_name), ]


# Plot happiness (valence) scores for each album

ggplot(audio_features, aes(x = valence, y = album_name)) +
  geom_density_ridges(fill = "lightblue", alpha = 0.5) +
  theme_ridges() +
  ggtitle("Happiness in Taylor Swift Albums",
          subtitle = "Based on valence from Spotify's Web API")

# Plot Danceability scores for each album

ggplot(audio_features, aes(x = danceability, y = album_name)) +
  geom_density_ridges(fill = "lightyellow", alpha = 0.5) +
  theme_ridges() +
  ggtitle("Danceability in Taylor Swift Albums",
          subtitle = "Based on Danceability from Spotify's Web API")


# Load packages required for Youtube into library

library(tuber)
library(vosonSML)
library(magrittr)
library(igraph)
library(httpuv)
library(ggpubr)

# Set up YouTube authentication variables 

api_key <- "AIzaSyB1HNEEhMBvI1oA8MET69M0rhKraecq2FQ"
client_id <- "209931985183-hf8kmlsrcs0mrbu1q9rhmad9fjp9bg9j.apps.googleusercontent.com"
client_secret <- "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"

# Authenticate to YouTube using the tuber package

yt_oauth(app_id = client_id, app_secret = client_secret)

# 2.2) Retrieve data relevant to your artist/band from YouTube

# Search YouTube

video_search <- yt_search("Taylor Swift")
View(video_search)

# Choose 10 videos and store their video IDs for which we want to collect and analyze data

video_ids <- list('b1kbLwvqugk','VuNIsY6JdUw','nfWlot6h_JM','e-ORhEE9VVg','3tmd-ClpJxA',
                  '8xg3vE8Ie_E','QcIy9NiNbmo','-BjZmE2gtdo','FuXNumBwDOM','IdneKLhsWOQ')

# Count Likes and Views for all 10 videos chosen

stats_10videos <- list()
for(i in video_ids){
  stats <- get_stats(video_id = i)
  df_stats <- data.frame(stats)
  
    for(i in video_ids){
    df_stats <- get_stats(video_id = i)}
    
  stats_10videos <- rbind(stats_10videos,data.frame(stats))
}
View(stats_10videos)
df_stats_10videos <- data.frame(stats_10videos)

df_stats_10videos1 <- mutate_all(df_stats_10videos, function(x) as.numeric(as.character(x)))

# Visualizing correlation using scatter plots by the Pearson formula

options(scipen = 999)
ggscatter(df_stats_10videos1, x = "viewCount", y = "likeCount", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Views", ylab = "Likes")

library(vosonSML)
library(magrittr)
library(tidyr)
library(tidytext)
library(stopwords)
library(textclean)
library(qdapRegex)
library(tm)
library(SnowballC)
library(ggplot2)

# 2.3)Perform text pre-processing and create a Term-Document Matrix for your Twitter data

# Collect more data of Taylor Swift

my_app_name <- "7230ICT_App"
my_api_key <- "Pp3qPc4bTEN6Zeqou7T6rDOgZ"
my_api_secret <- "YNcVqWFGsemyXVnqkPdWFWapIhkFo39sj5ODydyHmRqTxqdovM"
my_access_token <- "1634080601425985536-l5TzqA8ulXGMA9pys2NyI1HCmI43re"
my_access_token_secret <- "1AJQZyvCVps4l1hMBgem8vXYbIIjmI0rB8YZSoB5PZJU3"

#Keyword: #TaylorSwift, #ErasTour, #ErasEra, #Grammys.

my_8000_tweets <- Authenticate("twitter",
                                  appName = my_app_name,
                                  apiKey = my_api_key,
                                  apiSecret = my_api_secret,
                                  accessToken = my_access_token,
                                  accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#TaylorSwift OR #ErasTour OR #ErasEra OR #Grammys",
          searchType = "recent",
          numTweets = 8000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) 

# Clean the tweet text

clean_text <- my_8000_tweets$tweets$text %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_emoji() %>% 
  replace_emoticon()

# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

# Perform further pre-processing 

text_corpus <- text_corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords(kind = "SMART")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)

# Transform corpus into a Document Term Matrix

doc_term_matrix <- DocumentTermMatrix(text_corpus)

# Sort words by total frequency across all documents

dtm_df <- as.data.frame(as.matrix(doc_term_matrix))
View(dtm_df)

freq <- sort(colSums(dtm_df), decreasing = TRUE)

head(freq, n = 10)

# Plot word frequency

word_frequ_df <- data.frame(word = names(freq), freq)
View(word_frequ_df)

ggplot(subset(word_frequ_df, freq > 1150), aes(x = reorder(word, -freq), y = freq, fill = reorder(word, -freq))) +
  geom_bar(position = 'dodge', stat='identity') + 
  geom_text(aes(label=freq), position=position_dodge(width=0.9), vjust=-0.25)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")

# Social Network Analysis

library(vosonSML)
library(magrittr)
library(tidytext)
library(igraph)

# 2.4) Degree centrality, betweenness centralitY and closeness centrality analysis

  ###TAYLOR SWIFT

#Perform centrality analysis by detecting degree centrality, betweenness centrality, and closeness centrality 
# Create twomode (bimodal) network

twomode_network <- my_8000_tweets %>% Create("twomode", 
                                           removeTermsOrHashtags = c("#TaylorSwift"))
twomode_graph <- twomode_network %>% Graph()

# Write graph to file

write.graph(twomode_graph, file = "TwitterTwomode.graphml", format = "graphml")


# Inspect the graph object

length(V(twomode_graph))
V(twomode_graph)$name

# Find all maximum components that are weakly connected

twomode_comps <- components(twomode_graph, mode = c("weak"))

twomode_comps$no
twomode_comps$csize
head(twomode_comps$membership, n = 30)

# Get sub-graph with most members

largest_comp <- which.max(twomode_comps$csize)
largest_comp

twomode_subgraph <- twomode_graph %>% 
  induced_subgraph(vids = which(twomode_comps$membership == largest_comp))

# Display top 20 nodes from the sub-graph ordered by degree centrality

sort(degree(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]

# Display top 20 nodes from the sub-graph ordered by closeness centrality

sort(closeness(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(twomode_subgraph, directed = FALSE), decreasing = TRUE)[1:20]

  ###SELENA GOMEZ

# Collect data of related artist: SELENA GOMEZ

  #Keyword: #SelenaGomez, #MyMindAndMe, #SelenaAndChef, #HotelTransylvania, #OnlyMurders and #SephoraLovesRareBeauty.

my_8000_tweets_SG <- Authenticate("twitter",
                               appName = my_app_name,
                               apiKey = my_api_key,
                               apiSecret = my_api_secret,
                               accessToken = my_access_token,
                               accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#SelenaGomez OR #MyMindAndMe OR #SelenaAndChef OR #HotelTransylvania OR 
          #OnlyMurders OR #SephoraLovesRareBeauty",
          searchType = "recent",
          numTweets = 8000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) 

#Perform centrality analysis by detecting degree centrality, betweenness centrality, closeness centrality (SELENA GOMEZ)

# Create twomode (bimodal) network

twomode_network_SG <- my_8000_tweets_SG %>% Create("twomode", 
                                             removeTermsOrHashtags = c("#SelenaGomez"))
twomode_graph_SG <- twomode_network_SG %>% Graph()

# Write graph to file

write.graph(twomode_graph_SG, file = "TwitterTwomodeSG.graphml", format = "graphml")

# Inspect the graph object

length(V(twomode_graph_SG))
V(twomode_graph_SG)$name

# Find all maximum components that are weakly connected

twomode_comps_SG <- components(twomode_graph_SG, mode = c("weak"))

twomode_comps_SG$no
twomode_comps_SG$csize
head(twomode_comps_SG$membership, n = 30)

# Get sub-graph with most members

largest_comp_SG <- which.max(twomode_comps_SG$csize)
largest_comp_SG

twomode_subgraph_SG <- twomode_graph_SG %>% 
  induced_subgraph(vids = which(twomode_comps_SG$membership == largest_comp_SG))

# Display top 30 nodes from the sub-graph ordered by degree centrality

sort(degree(twomode_subgraph_SG, mode = "in"), decreasing = TRUE)[1:30]
sort(degree(twomode_subgraph_SG, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph_SG, mode = "total"), decreasing = TRUE)[1:30]

# Display top 30 nodes from the sub-graph ordered by closeness centrality

sort(closeness(twomode_subgraph_SG, mode = "in"), decreasing = TRUE)[1:30]
sort(closeness(twomode_subgraph_SG, mode = "out"), decreasing = TRUE)[1:30]
sort(closeness(twomode_subgraph_SG, mode = "total"), decreasing = TRUE)[1:30]

# Display top 30 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(twomode_subgraph_SG, directed = FALSE), decreasing = TRUE)[1:30]

  ###KATY PERRY 

# Collect data of related artist: KATY PERRY 

#Keyword: #katyperry, #idol, #disneynight, #shoesdaytuesday

my_8000_tweets_KP <- Authenticate("twitter",
                                  appName = my_app_name,
                                  apiKey = my_api_key,
                                  apiSecret = my_api_secret,
                                  accessToken = my_access_token,
                                  accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#katyperry OR #idol OR #disneynight OR #shoesdaytuesday",
          searchType = "recent",
          numTweets = 8000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) 

#Perform centrality analysis by detecting degree centrality, betweenness centrality, closeness centrality (KATY PERRY)
# Create twomode (bimodal) network

twomode_network_KP <- my_8000_tweets_KP %>% Create("twomode", 
                                                               removeTermsOrHashtags = c("#katyperry"))
twomode_graph_KP <- twomode_network_KP %>% Graph()

# Write graph to file

write.graph(twomode_graph_KP, file = "TwitterTwomodeKP.graphml", format = "graphml")

# Inspect the graph object

length(V(twomode_graph_KP))
V(twomode_graph_KP)$name

# Find all maximum components that are weakly connected

twomode_comps_KP <- components(twomode_graph_KP, mode = c("weak"))

twomode_comps_KP$no
twomode_comps_KP$csize
head(twomode_comps_KP$membership, n = 30)

# Get sub-graph with most members

largest_comp_KP <- which.max(twomode_comps_KP$csize)
largest_comp_KP

twomode_subgraph_KP <- twomode_graph_KP %>% 
  induced_subgraph(vids = which(twomode_comps_KP$membership == largest_comp_KP))

# Display top 30 nodes from the sub-graph ordered by degree centrality

sort(degree(twomode_subgraph_KP, mode = "in"), decreasing = TRUE)[1:30]
sort(degree(twomode_subgraph_KP, mode = "out"), decreasing = TRUE)[1:30]
sort(degree(twomode_subgraph_KP, mode = "total"), decreasing = TRUE)[1:30]

# Display top 30 nodes from the sub-graph ordered by closeness centrality

sort(closeness(twomode_subgraph_KP, mode = "in"), decreasing = TRUE)[1:30]
sort(closeness(twomode_subgraph_KP, mode = "out"), decreasing = TRUE)[1:30]
sort(closeness(twomode_subgraph_KP, mode = "total"), decreasing = TRUE)[1:30]


# Display top 30 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(twomode_subgraph_KP, directed = FALSE), decreasing = TRUE)[1:30]


#2.5) Perform community analysis with the Girvan-Newman (edge betweenness) and Louvain methods

  ### TAYLOR SWIFT

yt_data <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data)

yt_actor_network <- yt_data %>% Create("actor")
yt_actor_graph <- Graph(yt_actor_network)

# Transform into an undirected graph

undir_yt_actor_graph <- as.undirected(yt_actor_graph, mode = "collapse")


# Run Louvain algorithm

louvain_yt_actor <- cluster_louvain(undir_yt_actor_graph)

# See sizes of communities

sizes(louvain_yt_actor)

# Visualise the Louvain communities

plot(louvain_yt_actor, 
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 10,
     vertex.label.cex = 1.0)

# Use Gephi to visualise the Louvain communities

write.graph(undir_yt_actor_graph, file =  "undir_yt_actor_graph.graphml", format = "graphml")


# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor <- cluster_edge_betweenness(undir_yt_actor_graph)

# See sizes of communities

sizes(eb_yt_actor)


# Visualise the edge-betweenness communities

plot(eb_yt_actor,
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy

yt_actor_graph2 <- yt_actor_graph
V(yt_actor_graph2)$name <- V(yt_actor_graph2)$screen_name
undir_yt_actor_graph2 <- as.undirected(yt_actor_graph2, mode = "collapse")
eb_yt_actor2 <- cluster_edge_betweenness(undir_yt_actor_graph2)

is_hierarchical(eb_yt_actor2)
as.dendrogram(eb_yt_actor2)
plot_dendrogram(eb_yt_actor2)

#show only the first 20 labels
plot_dendrogram(eb_yt_actor2, mode = "dendrogram", xlim = c(1,20))


  ### SELENA GOMEZ

video_search_SG <- yt_search("Selena Gomez")
View(video_search_SG)

# Choose 10 videos and store their video IDs for which we want to collect and analyze data

video_ids_SG <- list('9h30Bx4Klxg','VY1eFxgRR-k','Z8eXaXoUJRQ','zlJDTxahav0',
                     '1TsVjvEkc4s','ij_0p_6qTss','tSIk1QvIM2E','cJTmumMot1U',
                     'NZKXkD6EgBk','STO4-8vkG0U')

  
yt_data_SG <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids_SG,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data_SG)

yt_actor_network_SG <- yt_data_SG %>% Create("actor")
yt_actor_graph_SG <- Graph(yt_actor_network_SG)

# Transform into an undirected graph

undir_yt_actor_graph_SG <- as.undirected(yt_actor_graph_SG, mode = "collapse")

# Run Louvain algorithm

louvain_yt_actor_SG <- cluster_louvain(undir_yt_actor_graph_SG)

# See sizes of communities

sizes(louvain_yt_actor_SG)

# Visualise the Louvain communities

plot(louvain_yt_actor_SG, 
     undir_yt_actor_graph_SG, 
     vertex.label = V(undir_yt_actor_graph_SG)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)

# Use Gephi to visualise the Louvain communities

write.graph(undir_yt_actor_graph_SG, file =  "undir_yt_actor_graph_SG.graphml", 
            format = "graphml")

# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor_SG <- cluster_edge_betweenness(undir_yt_actor_graph_SG)


# See sizes of communities

sizes(eb_yt_actor_SG)


# Visualise the edge-betweenness communities

plot(eb_yt_actor_SG,
     undir_yt_actor_graph_SG, 
     vertex.label = V(undir_yt_actor_graph_SG)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy

yt_actor_graph2_SG <- yt_actor_graph_SG
V(yt_actor_graph2_SG)$name <- V(yt_actor_graph2_SG)$screen_name
undir_yt_actor_graph2_SG <- as.undirected(yt_actor_graph2_SG, mode = "collapse")
eb_yt_actor2_SG <- cluster_edge_betweenness(undir_yt_actor_graph2_SG)

is_hierarchical(eb_yt_actor2_SG)
as.dendrogram(eb_yt_actor2_SG)
plot_dendrogram(eb_yt_actor2_SG)

plot_dendrogram(eb_yt_actor2_SG, mode = "dendrogram", xlim = c(1,20))

  ### KATY PERRY

video_search_KP <- yt_search("Katy Perry")
View(video_search_KP)

# Choose 10 videos and store their video IDs for which we want to collect and analyze data

video_ids_KP <- list('CevxZvSJLk8','F57P9C4SAW4','QGJuMBdaqIw','0KSOMA3QBU0','KlyXNRrsk4A',
                     'dPI-mRFEIH0','kTHNpusq654','Ahha3Cqe_fk','uuwfgXD8qV8','98WtmW-lfeE')
                     
yt_data_KP <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids_KP,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data_KP)

yt_actor_network_KP <- yt_data_KP %>% Create("actor")
yt_actor_graph_KP <- Graph(yt_actor_network_KP)

# Transform into an undirected graph

undir_yt_actor_graph_KP <- as.undirected(yt_actor_graph_KP, mode = "collapse")

# Run Louvain algorithm

louvain_yt_actor_KP <- cluster_louvain(undir_yt_actor_graph_KP)

# See sizes of communities

sizes(louvain_yt_actor_KP)

# Visualise the Louvain communities

plot(louvain_yt_actor_KP, 
     undir_yt_actor_graph_KP, 
     vertex.label = V(undir_yt_actor_graph_KP)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)

# Use Gephi to visualise the Louvain communities

write.graph(undir_yt_actor_graph_KP, file =  "undir_yt_actor_graph_KP.graphml", format = "graphml")

# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor_KP <- cluster_edge_betweenness(undir_yt_actor_graph_KP)

# See sizes of communities

sizes(eb_yt_actor_KP)


# Visualise the edge-betweenness communities

plot(eb_yt_actor_KP,
     undir_yt_actor_graph_KP, 
     vertex.label = V(undir_yt_actor_graph_KP)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy

yt_actor_graph2_KP <- yt_actor_graph_KP
V(yt_actor_graph2_KP)$name <- V(yt_actor_graph2_KP)$screen_name
undir_yt_actor_graph2_KP <- as.undirected(yt_actor_graph2_KP, mode = "collapse")
eb_yt_actor2_KP <- cluster_edge_betweenness(undir_yt_actor_graph2_KP)

is_hierarchical(eb_yt_actor2_KP)
as.dendrogram(eb_yt_actor2_KP)
plot_dendrogram(eb_yt_actor2_KP)

plot_dendrogram(eb_yt_actor2_KP, mode = "dendrogram", xlim = c(1,20))

# 2.6) Sentiment & Emotion Analysis 

# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(tidytext)
library(textclean)
library(qdapRegex)
library(syuzhet)
library(ggplot2)

# Clean the tweet text

clean_text <- my_8000_tweets$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()

# Assign sentiment scores to tweets

sentiment_scores <- get_sentiment(clean_text, method = "afinn") %>% sign()

sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)
View(sentiment_df)


# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df)


# Plot sentiment classification

ggplot(sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets")


# Assign emotion scores to tweets

emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

emo_scores_df <- data.frame(clean_text, emo_scores)
View(emo_scores_df)


# Calculate proportion of emotions across all tweets

emo_sums <- emo_scores_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_df) 

names(emo_sums)[1] <- "Proportion" 
View(emo_sums)


# Plot emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Tweets") +
  ggtitle("Emotion Analysis of Tweets")

# 2.7)  Decision Tree 

library(spotifyr)
library(C50)
library(caret)
library(e1071)
library(dplyr)

  # a) ALL SONGS from Taylor Swift vs Playlist TOP 100 SONGS
# Get songs from Taylor Swift and her audio features

TaylorSwift_features <- get_artist_audio_features("Taylor Swift")
View(TaylorSwift_features)

data.frame(colnames(TaylorSwift_features))

TaylorSwift_features_subset <- TaylorSwift_features[ , 9:20]
View(TaylorSwift_features_subset)

# Get top 100 songs and their audio features

top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
View(top100_features)

data.frame(colnames(top100_features))

top100_features_subset <- top100_features[ , 6:17]
View(top100_features_subset)

top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)


# Add the 'isTaylorSwift' column (class variable) to each data frame
# to indicate which songs are by Taylor Swift and which are not

top100_features_subset["isTaylorSwift"] <- 0
TaylorSwift_features_subset["isTaylorSwift"] <- 1


# Remove any songs by Taylor Swift that appear in the top 100
# and combine the two data frames into one dataset

top100_features_noTaylorSwift <- anti_join(top100_features_subset,
                                     TaylorSwift_features_subset,
                                     by = "track_id")
comb_data <- rbind(top100_features_noTaylorSwift, TaylorSwift_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isTaylorSwift' column into a factor
# and remove the 'track_id' column

comb_data$isTaylorSwift <- factor(comb_data$isTaylorSwift)
comb_data <- select(comb_data, -track_id)


# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]

# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]


# Train the decision tree model

dt_model <- train(isTaylorSwift~ ., data = training_set, method = "C5.0")


# Sample a single prediction 

prediction_row <- 1

if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}


# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$isTaylorSwift)

# Visualize the decision tree model

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

dt_model_v <- rpart(isTaylorSwift~., data = training_set, method = "class")
rpart.plot(dt_model_v)


    # IMPROVING DECISION TREE PERFORMANCE

  # b) Playlist 'THIS IS TAYLOR SWIFT' vs  Playlist 'SOFT POP HITS SONGS'

# Get Playlist 'THIS IS TAYLOR SWIFT' and their audio features

topTS_features <- get_playlist_audio_features("spotify", "37i9dQZF1DX5KpP2LN299J")
View(topTS_features)

data.frame(colnames(topTS_features))

topTS_features_subset <- topTS_features[ , 6:17]
View(topTS_features_subset)

# Get Playlist Soft Pop Hits songs and their audio features 

topPOPHITS_features <- get_playlist_audio_features("spotify", "37i9dQZF1DWTwnEm1IYyoj")
View(topPOPHITS_features)

data.frame(colnames(topPOPHITS_features))

topPOPHITS_features_subset <- topPOPHITS_features[ , 6:17]
View(topPOPHITS_features_subset)

# Add the 'isTaylorSwift' column (class variable) to each data frame
# to indicate which songs are by Taylor Swift and which are not

topPOPHITS_features_subset["isTaylorSwift"] <- 0
topTS_features_subset["isTaylorSwift"] <- 1

# Remove any songs by Taylor Swift that appear in the Soft Pop Hits playlist
# and combine the two data frames into one dataset

topPOPHITS_features_noTaylorSwift <- anti_join(topPOPHITS_features_subset,
                                               topTS_features_subset,
                                           by = "track.id")
comb_data1 <- rbind(topPOPHITS_features_noTaylorSwift, topTS_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isTaylorSwift' column into a factor
# and remove the 'track_id' column

comb_data1$isTaylorSwift <- factor(comb_data1$isTaylorSwift)
comb_data1 <- select(comb_data1, -track.id)


# Randomise the dataset (shuffle the rows)

comb_data1 <- comb_data1[sample(1:nrow(comb_data1)), ]

# Split the dataset into training and testing sets (80% training, 20% testing)

split_point1 <- as.integer(nrow(comb_data1)*0.8)
training_set1 <- comb_data1[1:split_point1, ]
testing_set1 <- comb_data1[(split_point1 + 1):nrow(comb_data1), ]


# Train the decision tree model

dt_model1 <- train(isTaylorSwift~ ., data = training_set1, method = "C5.0")


# Sample a single prediction 

prediction_row <- 1

if (tibble(predict(dt_model1, testing_set1[prediction_row, ])) ==
    testing_set1[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}


# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model1, reference = testing_set1$isTaylorSwift)

# Visualize the decision tree model

dt_model1_v <- rpart(isTaylorSwift~., data = training_set1, method = "class")
rpart.plot(dt_model1_v)


  # c) Playlist 'THIS IS TAYLOR SWIFT' vs  Playlist TOP 100 SONGS

# Get top 100 songs and their audio features

top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
View(top100_features)

data.frame(colnames(top100_features))

top100_features_subset1 <- top100_features[ , 6:17]
View(top100_features_subset1)

# Add the 'isTaylorSwift' column (class variable) to each data frame
# to indicate which songs are by Taylor Swift and which are not

top100_features_subset1["isTaylorSwift"] <- 0
topTS_features_subset["isTaylorSwift"] <- 1


# Remove any songs by Taylor Swift that appear in the Soft Pop Hits playlist
# and combine the two data frames into one dataset

top100_features_noTaylorSwift1 <- anti_join(top100_features_subset1,
                                               topTS_features_subset,
                                               by = "track.id")
comb_data2 <- rbind(top100_features_noTaylorSwift1, topTS_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isTaylorSwift' column into a factor
# and remove the 'track_id' column

comb_data2$isTaylorSwift <- factor(comb_data2$isTaylorSwift)
comb_data2 <- select(comb_data2, -track.id)


# Randomise the dataset (shuffle the rows)

comb_data2 <- comb_data2[sample(1:nrow(comb_data2)), ]

# Split the dataset into training and testing sets (80% training, 20% testing)

split_point2 <- as.integer(nrow(comb_data2)*0.8)
training_set2 <- comb_data2[1:split_point2, ]
testing_set2 <- comb_data2[(split_point2 + 1):nrow(comb_data2), ]


# Train the decision tree model

dt_model2 <- train(isTaylorSwift~ ., data = training_set2, method = "C5.0")


# Sample a single prediction 

prediction_row <- 1

if (tibble(predict(dt_model2, testing_set2[prediction_row, ])) ==
    testing_set2[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}


# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model2, reference = testing_set2$isTaylorSwift)

# Visualize the decision tree model

dt_model2_v <- rpart(isTaylorSwift~., data = training_set2, method = "class")
rpart.plot(dt_model2_v)



#2.8) Topic Modelling to identify some terms that are closely related to Taylor Swift

# Load packages required for this part

library(vosonSML)
library(magrittr)
library(tidytext)
library(textclean)
library(qdapRegex)
library(tm)
library(topicmodels)
library(slam)
library(Rmpfr)
library(dplyr)
library(ggplot2)
library(reshape2)

# Use clean_text dataset from 2.6) part
# Convert clean tweet vector into a document corpus (collection of documents)

text_corpus1 <- VCorpus(VectorSource(clean_text))

# Remove stop words

text_corpus1 <- text_corpus1 %>%
  tm_map(removeWords, stopwords(kind = "SMART")) 

# Transform corpus into a Document Term Matrix and remove 0 entries

doc_term_matrix1 <- DocumentTermMatrix(text_corpus1)
non_zero_entries = unique(doc_term_matrix1$i)
dtm1 = doc_term_matrix1[non_zero_entries,]

# Create LDA model with k topics (k=10)

lda_model <- LDA(dtm1, k = 10)


# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic

tweet_topics <- tidy(lda_model, matrix = "beta")


# Visualise the top 10 terms per topic

top_terms <- tweet_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#2.10 Visualisation with Tableau
# Convert R data to csv
write.csv(df_stats_10videos, "10videos.csv")

write.csv(sentiment_df, "sentiment.csv")

users <- select(my_8000_tweets$users, -c(entities,withheld_in_countries,withheld_scope))
write.csv(users, "users.csv")




