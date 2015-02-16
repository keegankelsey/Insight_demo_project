#-------------------------------------------------------------------------#
#-----------> Create features for use in predictive modeling  <-----------#
#-------------------------------------------------------------------------#

library(RMySQL) # MySQL for database I/O
library(chron) # convert EPOCH time
library(stringi)


#-----------> 1. User input <-----------#

#Are you testing
testing_dataset = "N" # fill with Y/N

# Number of rows in test data #
n_rows_testing_dataset = 50000


#-----------> 1. Pull database from MySQL <-----------#

drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="root", pass="",dbname="pvll")
df <- dbReadTable(conn = con, name = "pvll_trim_clean")
dbDisconnect(con)

# Trim data set if testing to save time in testing #

if(testing_dataset == "Y"){
  df <- df[order(df[,"username"]),]
  df <- df[1:n_rows_testing_dataset,]
}


#-----------> 3. Build function for counting features <-----------#

# extract text features such as number of lowercase letters from a single message #
extract.feature <- function(line,feature){
  set <- attr(gregexpr(feature,line)[[1]],"match.length")
  if(set[1] == -1){
    return(0)
  }else{
    return(length(set))
  }
}

# function to extract features from multiple messages #
count.feature <- function(message,feature){
  full.set <- sapply(message,extract.feature,feature=feature,USE.NAMES=FALSE)
  return(full.set)
}


#-----------> 4. Assign messages to a unique variable <-----------#

messages <- df[,"garbledMsg"]


#-----------> 5. Build column for sender id <-----------#

sender_id <- c(1:nrow(df))
sender_id[df[,"sentByMe"]=="True"] <- df[df[,"sentByMe"]=="True","username"]
sender_id[df[,"sentByMe"]=="False"] <- df[df[,"sentByMe"]=="False","address"]


#-----------> 6. Build column for if user account sent or not <-----------#

sent_from_user_account <- df[,"sentByMe"]
sent_from_user_account[sent_from_user_account == "True"] <- 1
sent_from_user_account[sent_from_user_account == "False"] <- 0
sent_from_user_account <- as.numeric(sent_from_user_account)


#-----------> 7. Build column day message was sent <-----------#

z <- as.numeric(df[,"smsDate"])/1000
d <- as.POSIXct(z, origin="1970-01-01", tz="GMT")
date <- as.Date(d)


#-----------> 8. Determine average number of messages sent per day by sender <-----------#

# This is complicated as there may be users that have dropped from the app #
# To avoid this I will only count days where there were texts sent #
# So this feature should read; of days where a message was sent, what was #
# the daily sending rate #

count.dates <- function(x){
  length(unique(x))
}

# total number of messages sent #
total.messaage.counts <- tapply(rep(1,nrow(df)),sender_id,sum)
daily.messaage.counts <- tapply(date,cbind(sender_id),count.dates)
average_daily_texts_by.sender <- total.messaage.counts/daily.messaage.counts

match.values <- function(individual,group.values){
  text.average <- group.values[names(group.values) %in% individual]
  return(text.average)
}
average_daily_texts <- sapply(sender_id,match.values,group.values=average_daily_texts_by.sender,USE.NAMES=FALSE)


#-----------> 9. Identify messages with emojis <-----------#

emoji_encoding <- messages
Encoding(emoji_encoding) <- "UTF-8"
f_contains.emoji <- Encoding(emoji_encoding)
f_contains.emoji[f_contains.emoji == "UTF-8"] <- 1
f_contains.emoji[f_contains.emoji == "unknown"] <- 0
f_contains.emoji <- as.numeric(f_contains.emoji)


#-----------> 10. select various features from individual messages for clustering input <-----------#

# Create library of features to extract #
feature.library <- c("\\W+","[[:alpha:]]","[[:blank:]]","[[:digit:]]",
                     "[[:lower:]]","[[:punct:]]","[[:space:]]","[[:upper:]]",
                     "\\?","\\!")

names(feature.library) <- c("words","alphas","blanks","digits",
                            "lower","punct","space","upper","question","exclamation")

# Run function that assigns counts of feature to individual message #
f_n_words <- count.feature(messages,feature.library["words"])
f_n_alpha <- count.feature(messages,feature.library["alphas"])
f_n_blank <- count.feature(messages,feature.library["blanks"])
f_n_alpha_lower <- count.feature(messages,feature.library["lower"])
f_n_alpha_upper <- count.feature(messages,feature.library["upper"])
f_punct <- count.feature(messages,feature.library["punct"])
f_question <- count.feature(messages,feature.library["question"])
f_exclamation <- count.feature(messages,feature.library["exclamation"])
f_message_info_bytes <- sapply(messages,nchar,type="bytes",USE.NAMES=FALSE)
f_times_graphed <- df[,"times_graphed"]
f_initiated_conversation <- df[,"initiating"]
f_hours_elapsed <- df[,"hours_elapsed"]


#-----------> 11. Determine average number of words sent per message by sender <-----------#

f_n_words <- count.feature(messages,feature.library["words"])
individual.average.word.count <- tapply(f_n_words,sender_id,mean)
average.word.count <- sapply(sender_id,match.values,group.values=individual.average.word.count,USE.NAMES=FALSE)


#-----------> 12. Find difference in average number of words vs. word count of message for a message <-----------#

word.count.difference <- f_n_words - average.word.count

#-----------> NOTE: There is room here to build more features <-----------#


#-----------> 13. Build data frame of features <-----------#

f_df <- as.matrix(cbind(as.numeric(average_daily_texts),
           as.numeric(average.word.count),
           as.numeric(f_n_words),
           as.numeric(f_n_alpha),
           as.numeric(f_n_blank),
           as.numeric(f_n_alpha_lower),
           as.numeric(f_n_alpha_upper),
           as.numeric(f_punct),
           as.numeric(f_question),
           as.numeric(f_exclamation),
           as.numeric(f_message_info_bytes),
           as.numeric(f_times_graphed),
           as.numeric(f_initiated_conversation),
           as.numeric(f_hours_elapsed),
           as.numeric(word.count.difference)))


#-----------> 14. Check if features are correlated <-----------#

count <- c()
for(i in 1:ncol(f_df)){
  value <- cor(f_df[,1],f_df[,i])
  if(value >= 0.8){
    count <- c(count,i)
  }
}
heatmap(cor(f_df))
all.names <- c("average_daily_texts","average.word.count","f_n_words",
                   "f_n_alpha","f_n_blank","f_n_alpha_lower","f_n_alpha_upper",
                   "f_punct","f_question","f_exclamation","f_message_info_bytes",
                   "f_times_graphed","f_initiated_conversation","f_hours_elapsed",
                   "word.count.difference")
colnames(f_df) <- all.names


# iteratively remove correlated vectors in feature matrix # 
j = 1
while(j < ncol(f_df)){
  print(j)
  count <- c()
  for(i in (j+1):ncol(f_df)){
    value <- cor(f_df[,j],f_df[,i])
    if(value >= 0.8){
      count <- c(count,i)
    }
  }
  if(is.null(count)==TRUE){
    j = j+1
  }else{
    keep.names <- colnames(f_df)
    f_df <- f_df[,-count]
    colnames(f_df) <- keep.names[-count]
  }
}


#-----------> 15. Store Features in new table <-----------#

save_data <- "Y"

if(save_data == "Y"){
  # reconnect to the database #
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv, user="root", pass="",dbname="pvll")
  dbWriteTable(con, "pvll_trim_clean_features", as.data.frame(f_df),overwrite=TRUE)
  dbDisconnect(con)
}


#-----------------------------#
#-----------> END <-----------#
#-----------------------------#