#---------------------------------------------------#
#-----------> Clean data from .csv file <-----------#
#---------------------------------------------------#

library(stringr) # library to deal with string and regex


#-----------> 1. Bring in data <-----------#

connection <- "~/Desktop/Insight/PVLL/PVLL_data_20150118/SMSMessage_Graphed_Anonymous_Processed.csv"

df <- readLines(con = connection, encoding = "UTF-8",skipNul = TRUE, warn = TRUE)
# Note, in the orginal file, there were 41 lines with an embedded nul: these were simply removed #
length(df)


#-----------> 2. Create unlist functions <-----------#

# NOTE: The "garbledMsg" may contain a column delimiting value,
# moditying the ability to separate data. Two functions are designed
# to evaluate if a) there are additional "pipes" and, if so, modify 
# for data splits, and b) return a list of lines that do not separate into
# the required number of columns.

count.number.of.pipes <- function(line){
  n_pipes <- str_count(line,"\\|")
  return(n_pipes)
}

count.parsed.number <- function(line){
  set <- unlist(strsplit(line,"|",fixed=TRUE,useBytes=TRUE))
  return(length(set))
}

parse.lines <- function(line){
  split_into_value <- 9
  set <- unlist(strsplit(line,"|",fixed=TRUE,useBytes=TRUE))
  
  # check number of data points
  if(length(set) == split_into_value){
    return(set)
  }else{
    n_pipes <- count.number.of.pipes(line)
    if(n_pipes != split_into_value-1){
      set <- unlist(strsplit(line,"|",fixed=TRUE,useBytes=TRUE))
      msg <- paste(set[6:(6+n_pipes-(split_into_value-1))],collapse="|")
      set <- c(set[1:5],msg,set[(length(set)-2):length(set)])
      if(length(set) == split_into_value){
        return(set)
      }
    }else{
      return(rep(NA,split_into_value))
    }
  }
}


#-----------> 3. Build new data frame <-----------#

X <- sapply(df[2:length(df)],parse.lines,USE.NAMES=FALSE)
X.names <- parse.lines(df[1])

rm(df)

new.X <- t(as.data.frame(X)) # this takes awhile
colnames(new.X) <- X.names
# confirm no NAs in dataset #
table(is.na(new.X[,1]))

X <- new.X


#-----------> 4. Remove full duplicates <-----------#

table(duplicated.matrix(new.X))

new.X <- new.X[duplicated.matrix(new.X) != TRUE,]


#-----------> 5. Remove duplicates where two messages duplicated pushed in same moment <-----------#

# For instance, there are some messages that were sent with the same time stamp
# yet show different "hours_elapsed" values, indicating this was an
# inappropriate push from the servicer

table(new.X[new.X[,9] == 0,9]) # 6,358 values

new.X <- new.X[new.X[,9] != 0,]


#-----------> 6. Replace "." values with 0 in "hours_elapsed" <-----------#

table(new.X[new.X[,9] == ".",9]) # 14,178 values...indicating there are 14,178 conversations

new.X[new.X[,9] == ".",9] <- "0"


#-----------> 7. check that smsDate == 0 <-----------#

table(new.X[,"smsDate"] == 0)


#-----------> 8. Find and replace leading \"\" <-----------#

new.X[,6] <- gsub(pattern="[:\"\":]",replacement="",new.X[,6])


#-----------> 9. Add a unique row identifier <-----------#

new.X <- cbind(c(1:nrow(new.X)),new.X)
colnames(new.X)[1] <- "uniqueRowID"


#-----------> 10. Write backup csv file <-----------#

write.table(new.X,"~/Desktop/Insight/PVLL/PVLL_data_20150118/SMSMessage_Graphed_Anonymous_Processed_cleaned.csv",row.names=FALSE)


#-----------------------------#
#-----------> END <-----------#
#-----------------------------#