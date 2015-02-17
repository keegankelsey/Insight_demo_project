#-----------------------------------------------------------------------------------------#
#-----------> Assign emoticon clusters to message and build predictive model  <-----------#
#-----------------------------------------------------------------------------------------#

# HMM http://gekkoquant.com/2014/09/07/hidden-markov-models-examples-in-r-part-3-of-4/
# HMM http://a-little-book-of-r-for-bioinformatics.readthedocs.org/en/latest/src/chapter10.html
# http://stats.stackexchange.com/questions/16564/hidden-markov-model-forward-algorithm-in-r

library(RMySQL) # MySQL for database I/O
library(HMM)


#-----------> 1. Pull database from MySQL <-----------#

drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="root", pass="",dbname="pvll")
df <- dbReadTable(conn = con, name = "pvll_predicted_messages")
dbDisconnect(con)


#-----------> 2. Assign number of conversation states to object <-----------#

conversation.states <- length(levels(as.factor(df[,"rf.prediction.full"])))


#-----------> 3. Determine initial transition probability based on observed conversations <-----------#

accounts <- unique(df[,"username"])

# Function that returns counts of transitions from one state to another state for a full conversation #
single.conversation.state.transition <- function(username,state.matrix){
  df.new <- df[df[,"username"]==username,]
  receivers <- unique(df.new[,"address"])
  conversation <- c()
  for(i in 1:length(receivers)){
    df.new.2 <- df.new[df.new[,"address"]==receivers[i],]
    for(i in 1:nrow(df.new.2)-1){
      state.matrix[df.new.2[i,"rf.prediction.full"],df.new.2[i+1,"rf.prediction.full"]] <- state.matrix[df.new.2[i,"rf.prediction.full"],df.new.2[i+1,"rf.prediction.full"]]+1
    }    
  }
  return(state.matrix)
}

# Function that returns counts of transitions from one state to another state for a multiple conversations #
multiple.conversation.state.transition <- function(individuals,n_states){
  state.matrix <- matrix(0,nrow=n_states,ncol=n_states)
  colnames(state.matrix) <- c(1:n_states)
  rownames(state.matrix) <- c(1:n_states)
  for(i in 1:length(individuals)){
    print(round(i/length(individuals),3))
    state.matrix <- single.conversation.state.transition(individuals[i],state.matrix)
  }
  state.matrix[state.matrix == 0] <- 1
  state.matrix <- state.matrix/rowSums(state.matrix)
  return(state.matrix)
}

# build transtion matrix #
obs.transition.matrix <- multiple.conversation.state.transition(accounts,conversation.states)

# save observed transtion matrix #
save_data <- "Y"
if(save_data == "Y"){
  # reconnect to the database #
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv, user="root", pass="",dbname="pvll")
  dbWriteTable(con, "pvll_HMM_observed_transtion_matrix", as.data.frame(obs.transition.matrix),overwrite=TRUE)
  dbDisconnect(con)
}

# read in table #
#obs.transition.matrix <- dbReadTable(conn = con, name = "pvll_HMM_observed_transtion_matrix")


#-----------> 4. Organize data into conversations and provide observed conversation states <-----------#

# pull all conversations from a single individual
single.conversation <- function(username){
  df.new <- df[df[,"username"]==username,]
  receivers <- unique(df.new[,"address"])
  conversation <- c()
  for(i in 1:length(receivers)){
    df.new.2 <- df.new[df.new[,"address"]==receivers[i],]
    individual.conversation <- paste(df.new.2[,"rf.prediction.full"],collapse=" ")
    conversation <- c(conversation,individual.conversation)
  }
  return(as.numeric(unlist(strsplit(conversation,split=" "))))
}
single.conversation(accounts[2]) # test

# organize all conversations into an array grouped by full conversations #
all.observed.states <- sapply(accounts,single.conversation,USE.NAMES=FALSE)
all.observed.states <- unlist(all.observed.states)
length(all.observed.states)
table(all.observed.states)
table(all.observed.states)/sum(table(all.observed.states))

#-----------> 5. Run Baum-Welch to infer optimal parameters of HMM <-----------#
#http://web.stanford.edu/class/stats366/hmmR2.html

# assign paramters to initialize HMM #
hidden.states <- as.character(c(1:conversation.states))
obs.states <- as.character(c(1:conversation.states))
transition.probability <- obs.transition.matrix
emission.probability <- matrix(rep(rep(1/length(hidden.states),length(hidden.states)),length(hidden.states)),length(hidden.states))
hmm = initHMM(hidden.states, obs.states, transProbs=as.matrix(transition.probability), emissionProbs=as.matrix(emission.probability))

# Estimate emmission probabilities ** NOTE ** THIS TAKES CONSIDERABLE TIME!! Test with a subset of data first #
hmm.bw <- baumWelch(hmm, all.observed.states, maxIterations=10,pseudoCount=1)


# Function to convert posterior probabilities of state into discrete catagories #
convert.posterior.prob <- function(posterior.prob){
  hidden.state <- names(as.array(posterior.prob[posterior.prob==max(posterior.prob)]))
  if(length(hidden.state) > 1){
    hidden.state <- sample(hidden.state,1)
  }
  return(hidden.state)
}

# Function to convert posterior probabilities of state using informed HMM #
extract.state <- function(hmm,observations){
  posterior.probabilities <- t(posterior(hmm,observations))
  X <- apply(posterior.probabilities,1,convert.posterior.prob)
  return(X)
}

# Extract conversation states given a list of observations #
observations <- all.observed.states
table(observations)

test <- extract.state(hmm.bw$hmm,observations)
table(test)

#------------------------------#
#-----------> End  <-----------#
#------------------------------#