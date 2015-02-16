#----------------------------------------------------------------#
#-----------> Exploratory analysis on clean dataset  <-----------#
#----------------------------------------------------------------#

library(RMySQL) # MySQL for I/O #
library(stringr) # library to deal with string and regex #


#-----------> 1. Query MySQL databases for databases  <-----------#

drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="root", pass="",dbname="pvll")

# Return tables in our dataset #
dbListTables(con)

# Return fields in our table #
dbListFields(con, "pvll_full_cleaned")

# Read in full table #
df <- dbReadTable(conn = con, name = "pvll_full_cleaned")


dbDisconnect(con)


#-----------> 2. Look at parameters of dataset <-----------#

dim(df)  # full data.frame = 3,291,840 rows and 10 columns #
colnames(df)


#-----------> 3. Remove odd dates <-----------#

plot(df[,"smsDate"],pch = 16) # this takes awhile #

# message 4 is the start of non-test values #
set.smsdate <- sort(as.numeric(df[,"smsDate"]))[4]

df.new <- df[as.numeric(df[,"smsDate"])>=set.smsdate,]

df <- df.new
rm(df.new)

#-----------> 4. Understand time in data <-----------#

# Timestamp is in milliseconds since Jan 1st 1990 #

# Identify min and max of time in dataset #
min.date <- min(as.numeric(df[,"smsDate"]))
max.date <- max(as.numeric(df[,"smsDate"]))

# number of weeks in dataset #
n_weeks <- (max.date-min.date) / 6.048e+9 # number of milliseconds in 1 week

# convert timestamp into human readable date #
# NOTE: The assumption here is that the timezone is GMT. This may or may not be correct. #
as.Date(as.POSIXct(min.date/1000, origin="1970-01-01", tz="GMT"))
as.Date(as.POSIXct(max.date/1000, origin="1970-01-01", tz="GMT"))


#-----------> 5. Plot total messages sent across time <-----------#

png("~/Desktop/Messages_across_time.png",width=800,height=500)
plot(sort(as.numeric(df[,"smsDate"])),c(1:nrow(df)),type="l",lwd=3,
     main="Total messages across time",xlab="time (weeks)",ylab="Total messages",xaxt="n",cex.axis=1.2,cex.lab = 1.5)
points(as.numeric(sort(df[df[,"sentByMe"]=="True","smsDate"])),c(1:nrow(df[df[,"sentByMe"]=="True",])),
       type = "l", col = "blue",lwd=3)
points(as.numeric(sort(df[df[,"sentByMe"]=="False","smsDate"])),c(1:nrow(df[df[,"sentByMe"]=="False",])),
       type = "l", col = "yellow",lwd=3,lty=2)
line_positions <- min.date
for(i in 2:floor(n_weeks)){
  line_positions[i] <-line_positions[i-1]+ one_week
}
# plot a line to show week-by-week segmentation #
abline(v=line_positions,col=rgb(0.5,0.5,0.5),lty=2)
abline(v=(line_positions[20]+one_week),col=rgb(0.5,0.5,0.5),lty=2)

dev.off()


#-----------> 6. Plot activity of a single user <-----------#

message_counts_by_user <- tapply(df[,"username"],df[,"username"],length)

# pick user and cycle plot message
user <- names(sort(message_counts_by_user,decreasing=TRUE)[20])

user.df <- df[df[,"username"] == user,]
dim(user.df)
min.time <- as.numeric(min(user.df[,"smsDate"]))
max.time <- as.numeric(max(user.df[,"smsDate"]))
as.Date(as.POSIXct(min.time/1000, origin="1970-01-01", tz="GMT"))
as.Date(as.POSIXct(max.time/1000, origin="1970-01-01", tz="GMT"))

plot(as.numeric(user.df[,"smsDate"]),rep(1,nrow(user.df)),pch=16,cex=0.5)
plot(sort(as.numeric(user.df[,"smsDate"])),as.numeric(user.df[,"smsDate"]),
     main="single user",xlab = "time", ylab= "message sent",pch=16,cex=0.5)


#-----------> 7. Identify total number of unique messages <-----------#

n_messages <- length(df[,"garbledMsg"])
n_unique_messages <- length(unique(df[,"garbledMsg"])) # 1,780,718 unique messages


#-----------> 8. Number of users <-----------#

n_users <- length(unique(df[,"username"]))


#-----------> 9. Distribution of messages by user <-----------#

plot(sort(table(factor(df[,"username"]))))


#-----------> 10. Number of receivers <-----------#

n_receivers <- length(unique(df[,"address"]))


#-----------> 11. Distribution of messages by receiver <-----------#

plot(sort(table(factor(df[,"address"]))))


#-----------> 12. Show overlap between app users and those that receive messages <-----------#

library(VennDiagram)
A <- 1:n_users
AB <- table(unique(df[,"username"]) %in% unique(df[,"address"]))["TRUE"]
B <- (n_users-AB):((n_users-AB)+n_receivers)
venn.diagram(list(A = A, B = B),fill = c("blue", "yellow"),
alpha = c(0.25, 0.25), cex = 2,cat.fontface = 4,lty =2, filename = "~/Desktop/ven_of_user.tiff")
dev.off()


#-----------> 11. How many people are using that stop using? <-----------#

# plot messages by user...how often do we see the line go flat to zero? #


#-----------> 12. Create new column indicating listing ID of who sent the message <-----------#

sender_id <- c(1:nrow(df))
sender_id[df[,"sentByMe"]=="True"] <- df[df[,"sentByMe"]=="True","username"]
sender_id[df[,"sentByMe"]=="False"] <- df[df[,"sentByMe"]=="False","address"]
length(unique(sender_id))


#-----------> 13. Identify if a message contains at least one emoji or not <-----------#

emoji_encoding <- df[,"garbledMsg"]
Encoding(emoji_encoding) <- "UTF-8"
f_contains.emoji <- Encoding(emoji_encoding)
f_contains.emoji[f_contains.emoji == "UTF-8"] <- 1
f_contains.emoji[f_contains.emoji == "unknown"] <- 0
f_contains.emoji <- as.numeric(f_contains.emoji)
rm(emoji_encoding)

# Percent of messages that contain at least one unicode emoji #
sum(f_contains.emoji)/length(df[,"garbledMsg"]) # 0.0388 % of messages contain emojies

# By user, count proportion of emojis sent compared to total messages #
fraction.function <- function(x){
  sum(x)/length(x)
}

# Identify fraction of messages sent that use emoticons, by individual #
fraction.messages.emojis <- tapply(f_contains.emoji,sender_id,fraction.function)
hist(fraction.messages.emojis)

# Identify the fraction of individuals that actually uses emoticons #
table(fraction.messages.emojis!=0)["TRUE"]/length(fraction.messages.emojis) # 37%


#-----------------------------#
#-----------> END <-----------#
#-----------------------------#