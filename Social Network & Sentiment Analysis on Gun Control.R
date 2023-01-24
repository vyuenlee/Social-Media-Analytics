#-----------------------------------------------------------------------------
#
# Social Media Analytics
# Title: Social Network & Sentiment Analysis on Gun Control
# By: Vivian Yuen-Lee
#
#-----------------------------------------------------------------------------

# Install and load all required R packages
library(twitteR)      # load package for Twitter data collection
library(stringr)      # load package for common string operations
library(igraph)       # load package for creating social network
library(dplyr)        # load package for data manipulation functions
library(syuzhet)      # load package for sentiment analysis


# API Keys and tokens
api_key <- "HIDDEN"
api_secret <- "HIDDEN"
access_token <- "HIDDEN"
access_token_secret <- "HIDDEN"

# Set up Twitter authorization with your keys and access tokens
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


##############################################################################
# Who are the key opinion leaders for “gun control”?
#  	  - Create a directed retweet network 
#     - Identify the number of followers for each user in the network
#     - Calculate the in-degree and eigenvector centrality values for each node
#     - Plot network graph with the vertex & label sizes proportional to the Eigenvector centrality of each vertex
# Is their message positive or negative?
#     - Compute a single polarity score for each original tweet
#     - Plot the retweet network again with two different colors to represent positive and negative sentiment in the messages
# Which opinion leaders have a positive sentiment on "gun control”?
#     - For all users showing a positive sentiment towards "gun control", compare their centrality values and follower count
# Which opinion leaders have a negative sentiment on "gun control"? 
#     - For all users showing a negative sentiment towards "gun control", compare their centrality values and follower count
# What are the main gun control communities?   
#     - Cluster analysis
#     - Plot communities network graph
##############################################################################


# Collect Twitter retweet data based on keywords "gun control".  
# Limit the number of tweets to 1000
CollectedTweets = searchTwitter("gun control", n=1000, lang="en")

# Get only the text portion of each tweet
CollectedTweets_txt = sapply(CollectedTweets, function(x) x$getText())

# Find retweets using the grep function and save the index of results to rt_patterns
# This line identifies retweets by matching "RT" or "via"
# "(?:)" are things that should exist but we do not capture them 
# "\\b\\W*@\\w+)+" allows us to make sure there is'@' plus a username after "RT" or "via"
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", 
                   CollectedTweets_txt,   # data to be processed
                   ignore.case=TRUE)      # ignore case of the text

# Store the actual text portion of the retweets in a variable
rt_text = grep("(RT|via)((?:\\b\\W*@\\w+)+)", CollectedTweets_txt, ignore.case=TRUE, value=TRUE)


#-----------------------------------------------------------------------------
# Q1:	Who are the key opinion leaders for “gun control”?

# Initialize two lists to store usernames and retweets
user_retweet = as.list(1:length(rt_patterns))
user_post = as.list(1:length(rt_patterns))
rt_content = as.list(1:length(rt_patterns))

# For each retweet stored in rt_patterns, look for and extract the original poster, the user who retweeted
# and the content of the tweet.  Store them in the respective lists.
for (i in 1:length(rt_patterns)) 
{ 
  # get tweet with retweet entity
  twit = CollectedTweets[[rt_patterns[i]]]
  # get retweet source 
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)") 
  # use unlist to convert poster to a single vector and remove ':'
  poster = gsub(":", "", unlist(poster))  # gsub looks for " and replace with ""
  # name of retweeted user - function returns username after 'RT @' or 'via @'
  user_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user
  user_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
  # get retweet content
  rt_content[[i]] = sub(".*: ", "", rt_text[[i]])   # store the text content in rt_content
}


# Convert the two lists to two vectors with the results
who_post = unlist(user_post)
who_retweet = unlist(user_retweet)

# For the network graph, combine the two vectors so each post author and retweeted user are paired together
retweeter_poster = cbind(who_post, who_retweet)
colnames(retweeter_poster)=c("Retweeter", "Poster")
rownames(retweeter_poster)=NULL

# Re-format this data so we will have one entry for each node in the network
# First, combine the who_post and rt_content
poster_content = cbind(who_post, rt_content)
# Next, combine who_retweet and rt_content
retweet_content = cbind(who_retweet, rt_content)
# Append the two matrices
user_content = rbind(poster_content, retweet_content)
# Remove rows with duplicate usernames to match the number of vertices in the network graph
user_content = user_content[!duplicated(user_content[ ,1]), ]
View(user_content)

# Convert the two lists to two dataframes with the results
who_postDF = as.data.frame(t(as.data.frame(user_post)[1,]))
who_retweetDF = as.data.frame(t(as.data.frame(user_retweet)[1,]))

# Set up a directed retweet network with each user pair as an edge of the graph
rt_graph = graph.edgelist(retweeter_poster, directed = TRUE)
# Simplify the network by removing self ties (i.e., people who retweeted themselves)
rt_graph = simplify(rt_graph)
length(V(rt_graph))

# Store the labels' name attribute of the vertices/nodes for the network graph
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

# Plot network graph with the vertex sizes proportional to the Eigenvector centrality of each vertex:

# Set graph background to white and margins of graph
par(bg="white", mar=c(1,1,1,1))  

# Plot the retweet network with the vertex & label sizes proportional to its Eigenvector centrality and identify the opinion leaders
set.seed(123)        # set seed to make graph reproducible
plot(rt_graph, 
     vertex.color='white', # Set vertex/node fill color to white
     vertex.size=evcent(rt_graph)$vector*5^1.5, # Vertex size proportional to Eigenvector centrality
     frame.color='black', # Vertex border color
     vertex.label=ver_labs, # Vertex label
     vertex.label.family="sans", # Vertex label font
     vertex.shape="sphere", # Vertex shape
     vertex.label.color='black', # Vertex label color
     vertex.label.cex=0.5, # Vertex font size proportional to Eigenvector centrality
     edge.arrow.size=0.2, # Edge arrow size
     edge.arrow.width=0.4, # Edge arrow width
     edge.width=1, # Edge width
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5),  # Edge color
     layout=layout.fruchterman.reingold)  # Set the layout of the graph to the Fruchterman & Reingold layout

title("Tweets with 'gun control':  Identifying the opinion leaders",
      cex.main=1, col.main="black") 

# Identify the number of followers for each user
# Combine the list of users who created the original post and those who retweeted
allusersDF = rbind(who_postDF, who_retweetDF)
allusers=unlist(as.list(allusersDF))
# Remove duplicate user names
users = unique(allusers)

# Lookup the number of followers for each user in this list
userInfo = lookupUsers(users)
userFollowers = data.frame(cbind(User=sapply(userInfo, function(x) x$screenName), "Followers"=sapply(userInfo, function(x) x$followersCount)))
rownames(userFollowers) = NULL    # remove rownames
colnames(userFollowers)=c('User', 'Followers')
userFollowers$Followers = as.numeric(userFollowers$Followers)
userFollowers = userFollowers[!duplicated(userFollowers),]

# Sort the data by follower count in descending order and extract the first 20
top20Followers <- userFollowers %>% arrange(desc(Followers)) %>% slice(1:20)

# Show barplot of users with top 20 follower counts
par(mar=c(6.5,4.5,3,1))
barplot(top20Followers$Followers, 
        names.arg = top20Followers$User, 
        col = "steel blue",
        main="Top 20 Users with Highest Follower Count", 
        xlab="", 
        ylab="# of Followers",
        las = 2,
        cex.axis = 0.8,
        cex.names = 0.7)

# Calculate in- and out-degree centrality 
rt_InDegree = degree(rt_graph, mode=("in"))
rt_OutDegree = degree(rt_graph, mode=("out"))

# Calculates eigenvector centrality
rt_Evector = evcent(rt_graph)$vector

# Combine all centrality measures in a single table & convert row name into the first column
userCentrality = data.frame(cbind(rt_InDegree, rt_OutDegree, rt_Evector))
users = rownames(userCentrality)
userCentrality = data.frame(cbind(users, userCentrality))
rownames(userCentrality) = NULL
colnames(userCentrality)=c('User', 'In-Degree', 'Out-Degree', 'Eigenvector')

# show the table of centrality measures, sorted by eigenvector centrality in descending order
View(userCentrality[order(-userCentrality$Eigenvector), ])   

# Sort the data by eigenvector centrality value in descending order and extract the first 20
top20Eigenvector <- userCentrality %>% arrange(desc(Eigenvector)) %>% slice(1:20)

# Show barplot of users with top 20 eigenvector centrality
par(mar=c(6.5,4.5,3,1))
barplot(top20Eigenvector$Eigenvector, 
        names.arg = top20Eigenvector$User, 
        col = "#69b3a2",
        main="Top 20 Users with Highest Eigenvector Centrality", 
        xlab="", 
        ylab="Eigenvector Centrality",
        las = 2,
        cex.axis = 0.8,
        cex.names = 0.7)



#-----------------------------------------------------------------------------
# Q2. Is their message positive or negative?
# Perform a sentiment analysis of the collected tweets and compute a single polarity score

# Begin data pre-Processing
# First, create a function that converts all cases to lower case
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

# Next, create a function to clean tweets by removing unnecessary characters and spaces
cleanTweets <- function(tweet){
  # Clean the tweet for sentiment analysis
  #  remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from the stored tweets (text)
  #tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  tweet = gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # Then we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # Finally we replace UTF-8 characters with ASCII ones
  tweet=iconv(tweet, "UTF-8", "ascii",sub='')
  # Next we'll convert all the word in lowar case. This makes uniform pattern.
  tweet = catch.error(tweet)      # apply the catch.error function
  tweet
}

# Call cleanTweetsAndRemoveNAs function to clean the tweets
gunsTweetsCleaned = cleanTweets(user_content[ ,2])
#gunsTweetsCleaned[1:10]       # check the first 10 processed tweet content

# Obtain an overall sentiment score for each tweet
# Note: +ve values indicate positive sentiment & -ve values indicate negative sentiment
TweetsPolarity = as.data.frame(get_sentiment(as.character(gunsTweetsCleaned)))

# Combine the post author and the tweet polarity score
userPolarity = cbind(as.character(user_content[ ,1]), TweetsPolarity)
# If the polarity score is positive assign "1", else assign "2" to a new column named "color"
for (j in 1:nrow(userPolarity)) {
  if (userPolarity[j,2] >= 0){
    userPolarity[j,3] = 1
  } else {
    userPolarity[j,3] = 2
  }
}
colnames(userPolarity) = c("User","Polarity","Color")
View(userPolarity)

# Add polarity color to the network graph
# First, copy the order of the vertex labels variable
sortVer = as.data.frame(ver_labs)
sortVer$row_num = seq.int(nrow(sortVer))   # include the row numbers for sorting

# Combine the user names, row numbers, polarity and color assignment code into one dataframe
vertexColor = merge(x=sortVer, y=userPolarity, by.x = "ver_labs", by.y = "User", all.x=TRUE)
# Sort this new dataframe by the order for the vertices
vertexColor = vertexColor[order(vertexColor$row_num),]
View(vertexColor)

# Assign coloring scheme to each vertex - Blue for positive sentiment & orange for negative
V(rt_graph)$color = c("steel blue","orange")[vertexColor$Color]

# Set graph background to white and margins of graph
par(bg="white", mar=c(1,1,1,1))  

# Plot the retweet network again with different vertex and label colors for positive and negative scores
set.seed(123)        # set seed to make graph reproducible

plot(rt_graph, 
     vertex.size=evcent(rt_graph)$vector*5^1.5, # Vertex size proportional to Eigenvector centrality
     frame.color='black', # Vertex border color
     vertex.label=ver_labs, # Vertex label
     vertex.label.family="sans", # Vertex label font
     vertex.shape="sphere", # Vertex shape
     vertex.label.color=c("blue","chocolate")[vertexColor$Color], # Same color coding as vertex color
     #vertex.label.cex = 0.1,
     vertex.label.cex=degree(rt_graph)/50, # Vertex font size proportional to Degree centrality
     edge.arrow.size=0.2, # Edge arrow size
     edge.arrow.width=0.3, # Edge arrow width
     edge.width=1, # Edge width
     edge.color="grey",  # Edge color
     layout=layout.fruchterman.reingold)  # Set the layout of the graph to the Fruchterman & Reingold layout

title("Retweets with 'gun control':  Opinion polarity (blue for positive, orange for negative)",
      cex.main=1, col.main="black") 

# Generate an interactive network plot
tkplot(rt_graph)


#-----------------------------------------------------------------------------
# Q3.  Which opinion leaders have a positive sentiment towards “gun control”?

# Merge the followers, centrality metrics and polarity data frames together 
userMetrics = merge(x=userCentrality, y=userFollowers, by="User", all.x=TRUE)
userMetrics = merge(x=userMetrics, y=userPolarity, by="User" )
userMetrics = unique(userMetrics)
View(userMetrics)

# select only the users with a positive sentiment towards "gun control" and more than 10,000 followers
posGunControl = subset(userMetrics, Color==1 & Followers>=10000)
# Sort this list by eigenvector centrality in descending order
posGunControl = posGunControl[order(-posGunControl$Eigenvector), ]
View(posGunControl)   # See the results

#-----------------------------------------------------------------------------
# Q4. Which opinion leaders have a negative sentiment towards “gun control”? 

# select only the users with a negative sentiment towards "gun control" and more than 10,000 followers
negGunControl = subset(userMetrics, Color==2 & Followers>=10000)
# Sort this list by eigenvector centrality in descending order
negGunControl = negGunControl[order(-negGunControl$Eigenvector), ]
View(negGunControl)  # See the results

#-----------------------------------------------------------------------------
# Q5.  What are the main gun control communities?   

# Identifying Clusters
# First, check how well the nodes are connected to other  nodes within the network
is.connected(rt_graph, mode=c("weak", "strong"))
# False means that not every node is connected to one another
# Identify number of clusters in the network, cluster size, and the cluster that each node belongs to
clusters(rt_graph, mode=c("weak", "strong"))

# Identifying Communities
# Identify communities using the walktrap algorithm
GCCommunity = walktrap.community(rt_graph)
# Calculate network modularity
# Modularity is a measure of how modular (or disconnected) communities are in the network
modularity(GCCommunity)
# Plot identified communities in network
set.seed(31)
plot(GCCommunity, rt_graph, vertex.size=5, vertex.label.cex=0.5, 
     vertex.label=NA, edge.arrow.size=0, edge.curved=TRUE, layout=layout.fruchterman.reingold,
     main="Retweets with 'gun control': Community Network")

