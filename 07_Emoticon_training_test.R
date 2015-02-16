#-----------------------------------------------------------------------------------------#
#-----------> Assign emoticon clusters to message and build predictive model  <-----------#
#-----------------------------------------------------------------------------------------#

library(RMySQL) # MySQL for database I/O
library(class) # knn
library(randomForest) # random forest
library(stringi)


#-----------> 1. Pull database from MySQL <-----------#

drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="root", pass="",dbname="pvll")
df <- dbReadTable(conn = con, name = "pvll_trim_clean")
emoticons.groups <- dbReadTable(conn = con, name = "pvll_emoticons_in_groups")
dbDisconnect(con)


#-----------> 2. Identify messages with emojis  <-----------#

emoji_encoding <- df[,"garbledMsg"]
Encoding(emoji_encoding) <- "UTF-8"
f_contains.emoji <- Encoding(emoji_encoding)
f_contains.emoji[f_contains.emoji == "UTF-8"] <- 1
f_contains.emoji[f_contains.emoji == "unknown"] <- 0
f_contains.emoji <- as.numeric(f_contains.emoji)
rm(emoji_encoding)


#-----------> 3. Reduce dataframe to just messages with emoticons  <-----------#

# reduce dataset to just emojis
df<- df[f_contains.emoji == 1,]

# reduce dataset to just emojis that are represented in clusters clusters
full.emoji.list <- emoticons.groups[,"emoticon"]
emojies_used <- c()
for(i in 1:nrow(df)){
  emojies_used[i] <- sum(stri_count(df[i,"garbledMsg"],regex=full.emoji.list))
}
df <- df[emojies_used >= 1,]

#-----------> 4. Assign messages a unique identifier <-----------#

messages <- df[,"garbledMsg"]


#-----------> 5. Assign cluster group to specific messages  <-----------#

classify.message <- function(message){
  set <- c()
  for(i in 1:length(table(emoticons.groups[,"group"]))){
    set[i] <- sum(stri_count(message,regex=emoticons.groups[emoticons.groups[,"group"]==i,"emoticon"]))
  }
  names(set) <- c(1:length(set))
  if(sum(set) == 0){
    group <- NA
  }else{
    if(length(set[set==max(set)])!=1){
      group <- as.numeric(sample(names(set[set==max(set)]),1)) # modify this to be weighted by total proportions
    }else{
      group <- as.numeric(names(set[set==max(set)]))
    }
  }
  return(group)
}

message_group <- sapply(messages,classify.message,USE.NAMES=FALSE)
table(message_group)/sum(table(message_group))


#-----------> 6. Pull in feature data on messages  <-----------#

drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="root", pass="",dbname="pvll")
df.features <- dbReadTable(conn = con, name = "pvll_trim_clean_features")
dbDisconnect(con)


#-----------> 7. Trim feature data to be same length/structure as emoji labels  <-----------#

df.emojis <- df
df.features.emojis <- df.features[f_contains.emoji==1,]
df.features.emojis <- df.features.emojis[emojies_used >= 1,]

nrow(df.emojis) == nrow(df.features.emojis)


#-----------> 8. Set up training and testing set  <-----------#

# take 90% of data for training and 10% for testing, assign testing/training randomly #
n_train <- round(length(message_group)*0.9,0)
n_test <- length(message_group)-n_train
X <- sample(1:length(message_group),length(message_group),replace=FALSE)
train <- df.features.emojis[X[1:n_train],]
test <- df.features.emojis[X[n_train+1:n_test],]


#-----------> 9. Perform k-nearest neighbor on dataset  <-----------#

#http://blog.webagesolutions.com/archives/1164
set.k <- seq(1,60,2)
count <- 1
percent.correct <- c()
for(i in 1:length(set.k)){
  print(i)
  fit.knn <- knn(train, test, message_group[X[1:n_train]], k = set.k[i], l = 0, prob = FALSE, use.all = TRUE)
  summary(fit.knn)
  percent.correct[count] <- sum(fit.knn==message_group[X[n_train+1:n_test]])/length(fit.knn)
  count <- count+1
}

#-----------> 10. What to expect by chance? (re-weighted guess) <-----------#

permutations <- c()
for(i in 1:10000){
  true <- fit.knn
  sample.set <- sample(c(1:length(levels(true))),length(true),replace=TRUE,prob=c(as.numeric(table(true))/sum(table(true))))
  permutations[i] <- sum(true==sample.set)/length(fit.knn)
}
plot(density(permutations))
plot(set.k,percent.correct,type="l",col="black",lty=2,lwd=2,cex=3,cex.lab=1.5,main="k-NN classification",
     xlab="k",ylab="percent correct",ylim=c(0,0.5))
points(set.k,percent.correct,pch=16,cex=2)
rect(xleft=-10,xright=70,ybottom=as.numeric(quantile(permutations,0.01)),
     ytop=as.numeric(quantile(permutations,0.99)),col=rgb(0.5,0.5,0.5))
abline(h=mean(permutations),lwd=2,lty=2,col=rgb(0.1,0.1,0.1))
abline(h=(1/length(levels(fit.knn))),lwd=2,col="black",lty=3)
abline(h = max(table(message_group)/sum(table(message_group))), lwd = 2, lty = 3) 

# identify optimal k
names(percent.correct) <- set.k
optimal.k <- as.numeric(names(percent.correct)[percent.correct == max(percent.correct)])
abline(v=optimal.k,lwd=2,col="blue",lty=2)

print(max(percent.correct)) # best prediction using k-nn



#-----------> 11. Best prediction using random forest <-----------#

rf.values <- c()
for(i in 1:10){
  n_train <- round(length(message_group)*0.9,0)
  n_test <- length(message_group)-n_train
  X <- sample(1:length(message_group),length(message_group),replace=FALSE)
  train <- df.features.emojis[X[1:n_train],]
  test <- df.features.emojis[X[n_train+1:n_test],]

  fit.rf <- randomForest(x=train,y=as.factor(message_group[X[1:n_train]])) #takes longer then knn
  rf.prediction <- predict(fit.rf,test)

  rf.values[i] <- sum(rf.prediction == message_group[X[n_train+1:n_test]])/length(rf.prediction)
}
mean(rf.values) # mean is ~ 31.2
sd(rf.values) # sd ~ 0.0062
importance(fit.rf, type = 2) # V1, V2, and V10 are driving
partialPlot(fit.rf)


#-----------> 12. Run model from random forest across full message dataset <-----------#

# apply fully labeled emoticon dataset to build model #
fit.rf.full <- randomForest(x=df.features.emojis,y=as.factor(message_group)) #takes longer then knn
rf.prediction.full <- predict(fit.rf.full,df.features)

# importance of features
barplot(t(importance(fit.rf.full)),ylab = "Mean Decrease Gini",
        cex.lab=1.3,main="Feature Importance",xaxt="n")
axis(1,at=c(seq(0.5,11.5,by=(12/10))),labels=c(1:10))

# Save predictive model as .RData #
save(fit.rf.full,file="~/Desktop/Emoticon_prediction_model_randomforest.RData")


#-----------> 13. Frequency of predicted emoticon catagories in messages with and without emoticons <-----------#

# Predict emoticon catagory in messages with emoticons #
rf.prediction <- predict(fit.rf.full,df.features.emojis)

# partitioning of actual emoticon groups in messages with emoticons #
pie(table(message_group)/sum(table(rf.prediction)),cex=2,main=length(message_group))
# partitioning of predicted groups in untrained messages with emoticons #
pie(table(rf.prediction) /sum(table(rf.prediction)),main = length(rf.prediction),cex=2)
# partitioning of predicted groups in untrained messages with and without emoticons #
pie(table(rf.prediction.full) / sum(table(rf.prediction.full)),cex=2, main=length(rf.prediction.full))


#-----------> 14. Bind observations to data frame and save <-----------#

drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="root", pass="",dbname="pvll")
df <- dbReadTable(conn = con, name = "pvll_trim_clean")
dbDisconnect(con)

df.predicted.message.state <- cbind(df,rf.prediction.full)

save_data <- "Y"

if(save_data == "Y"){
  # reconnect to the database #
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv, user="root", pass="",dbname="pvll")
  dbWriteTable(con, "pvll_predicted_messages", as.data.frame(df.predicted.message.state),overwrite=TRUE)
  dbDisconnect(con)
}

#-----------------------------#
#-----------> END <-----------#
#-----------------------------#