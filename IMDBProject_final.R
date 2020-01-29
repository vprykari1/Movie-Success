install.packages("httr")
install.packages("XML")

library(XML)
library(httr)

######################## IMDB WEB SCRAPE LOOP
finallist <- ""  #Start with an empty list of movies

url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2005,2015&view=simple&count=250&page="

#update url2 after each run of the loop-function

url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2005&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2006&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2007&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2008&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2009&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2010&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2011&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2012&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2013&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2014&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2015&view=simple&count=250&page="
url2 <- "http://www.imdb.com/search/title?sort=num_votes,desc&start=1&title_type=feature&year=2016&view=simple&count=250&page="

for (page in 1:40){ #page 40 is max on imdb, empty pages will just return null, used only 1:1 for the final set!
  urlInst <- paste(url2, page, sep="")
  print(page)
  movie.doc <- htmlParse(urlInst)
  movie <- xpathSApply(movie.doc, "//*/div[@class='col-title']", xmlValue)
  movie_g <- gsub("\n","",movie)
  movie_g <- gsub("  ","",movie_g)
  movie_g <- sub(",","",movie_g)
  movie_g <- sub("\\d*.","",movie_g)
  movie_g <- sub("\\(\\d\\d\\d\\d\\)","",movie_g)
  finallist <- append(finallist,movie_g)
  
}
show(finallist[1:50]) #Easy to check that the movielist is populated with all movies

#Write all movie titles to a separate list:
lapply(finallist, write, "movielist.txt", append=TRUE)

#Read textfile "movielist":
finallist <- scan("movielist.txt",what=character(), sep="\n")

###################### OMDB API LOOP: Takes a LONG time

URLAPI <- "http://www.omdbapi.com/?t="
dffin <- data.frame()
  #Title=NA,Year=NA,Rating=NA,ReleaseDate=NA, Director=NA, Writer=NA, Cast=NA, Plot=NA, Language=NA, Country=NA, Awards=NA, Poster=NA, Metascore=NA, IMDBRating=NA, numbervotes=NA, IMDbID=NA, Type=NA, Response=NA)
count <- 0
for (i in finallist){ 
  count <- count+1
  i <- gsub(" ","+",i)
  searchurl <- paste(URLAPI,i,sep="")
  print(paste(count, searchurl))
  movinfo <- GET(searchurl)
  result <- content(movinfo)
  df <- data.frame(matrix(unlist(result)),stringsAsFactors = FALSE)
  df <- t(df)
  dffin <- merge(dffin,df,all=TRUE)#, stringAsFactors = FALSE)
}
#Save the data file
write.csv(dffin, file = "2016_omdb.csv",row.names=FALSE)

######################################

#read the complete dataset from .csv:
extracted_omdb = read.csv("omdb_small_train.csv")
extracted_2016 = read.csv("omdb_small_test.csv")
extracted_omdb_all = read.csv("All_extracted.csv")

##################################
## CLEANING DATA

data_clean <- extracted_omdb
test_clean <- extracted_2016
all_clean <- extracted_omdb_all


#Removing missing values of imdb rating:
data_clean <- data_clean[data_clean$V16 != "N/A",]
data_clean <- data_clean[!(is.na(data_clean$V16)),]
data_clean$V16 <- as.character.factor(data_clean$V16)

test_clean <- test_clean[test_clean$V16 != "N/A",]
test_clean <- test_clean[!(is.na(test_clean$V16)),]
test_clean$V16 <- as.character.factor(test_clean$V16)

all_clean <- all_clean[all_clean$V16 != "N/A",]
all_clean <- all_clean[!(is.na(all_clean$V16)),]
all_clean$V16 <- as.character.factor(all_clean$V16)

#Pick successful movies from complete list tour smaller training set to make it more balanced

data_success <- subset(all_clean, V16 >= 7.5)
data_success <- data_success[1:2000,]           #pick first 2000 successful movies
data_clean <- merge(data_clean,data_success,all=TRUE)


#Only use the first genre the movie is listed in
data_clean$V6 <- as.character(data_clean$V6)
data_clean$V6 <- gsub(',.*', '' , data_clean$V6)
data_clean$V6 <- as.factor(data_clean$V6)
data_clean <- data_clean[!(is.na(data_clean$V6)),]


test_clean$V6 <- as.character(test_clean$V6)
test_clean$V6 <- gsub(',.*', '' , test_clean$V6)
test_clean$V6 <- as.factor(test_clean$V6)
test_clean <- test_clean[!(is.na(test_clean$V6)),]
#Remove "min" from length, and na 
data_clean <- data_clean[data_clean$V5 != "N/A",]
data_clean <- data_clean[!(is.na(data_clean$V5)),]
data_clean$V5 <- gsub('.{4}$', '', data_clean$V5)
data_clean$V5 <- as.numeric(data_clean$V5)

test_clean <- test_clean[test_clean$V5 != "N/A",]
test_clean <- test_clean[!(is.na(test_clean$V5)),]
test_clean$V5 <- gsub('.{4}$', '', test_clean$V5)
test_clean$V5 <- as.numeric(test_clean$V5)

#remove year and day from date field, also na values
data_clean$V4 <- gsub('.{5}$', '', data_clean$V4)
data_clean$V4 <- gsub('^.{3}?', '', data_clean$V4)
data_clean <- data_clean[data_clean$V4 != "N/A",]
data_clean <- data_clean[!(is.na(data_clean$V4)),]


test_clean$V4 <- gsub('.{5}$', '', test_clean$V4)
test_clean$V4 <- gsub('^.{3}?', '', test_clean$V4)
test_clean <- test_clean[test_clean$V4 != "N/A",]
test_clean <- test_clean[!(is.na(test_clean$V4)),]

#Only use the first actor
data_clean$V9 <- gsub(',.*', '' , data_clean$V9)
data_clean$V9 <- as.factor(data_clean$V9)

test_clean$V9 <- gsub(',.*', '' , test_clean$V9)
test_clean$V9 <- as.factor(test_clean$V9)

#first director
data_clean$V7 <- gsub(',.*', '' , data_clean$V7)
data_clean$V7 <- as.factor(data_clean$V7)

test_clean$V7 <- gsub(',.*', '' , test_clean$V7)
test_clean$V7 <- as.factor(test_clean$V7)

#create new column "successful" based on imdb rating of 7,5 and up
data_clean["Successful"] <- NA
data_clean$Successful <- c(0, 1)[ findInterval(data_clean$V16, c(0, 7.5, Inf)) ]
data_clean$Successful <- as.factor(data_clean$Successful)


test_clean["Successful"] <- NA
test_clean$Successful <- c(0, 1)[ findInterval(test_clean$V16, c(0, 7.5, Inf)) ]
test_clean$Successful <- as.factor(test_clean$Successful)


#Using first country
data_clean$V12 <- gsub(',.*', '' , data_clean$V12)
data_clean$V12 <- as.factor(data_clean$V12)

test_clean$V12 <- gsub(',.*', '' , test_clean$V12)
test_clean$V12 <- as.factor(test_clean$V12)




#####################Looking at data
install.packages("caret")
install.packages("e1071")

library(caret)
library(e1071)

#


#New balanced dataset:
summary(data_clean$Successful)
1921/3791


#same levels of categories in test set as training set
levels(test_clean$V3) <- levels(data_clean$V3)
levels(test_clean$V5) <- levels(data_clean$V5)
levels(test_clean$V6) <- levels(data_clean$V6)
levels(test_clean$V7) <- levels(data_clean$V7)
levels(test_clean$V9) <- levels(data_clean$V9)
levels(test_clean$V12) <- levels(data_clean$V12)

#####################################
#NAIVE BAYES model training using all  features

nb_model <- naiveBayes(Successful~V3+V6+V7+V9,data = data_clean)


#Make a prediction on the test set 
nb_prediction <- predict(nb_model, test_clean)

summary(nb_prediction)

#Confusion matrix
table(pred=nb_prediction, true=test_clean[,22])


-#Correct predictions, %:
mean(nb_prediction==test_clean[,22])


######################################
#SVM-model training with linear kernel

svm_model <- svm(Successful~V3+V6+V7+V9,data = data_clean, kernel="linear", cost = 0.1, scale=FALSE)
print(svm_model)
plot(svm_model, test_clean)


#Prediction:

svm_prediction <- predict(svm_model, type="class", newdata=test_clean)
svm_prediction
#Confusion matrix
table(pred=svm_prediction, true=test_clean[,22])


#Correct predictions, %:
mean(svm_prediction==test_clean[,22])


###################################
#Logistic regression model

glm_model <- glm(Successful ~ V7,family=binomial(link='logit'),data=data_clean)

glm_prediction <- predict(glm_model, type='response', newdata=test_clean)

glm_prediction
table(pred=glm_prediction >0.5, true=test_clean[,22])

mean(glm_prediction==test_clean[,22])
2143/2751
2137/2751


