# Insight Demo Project
# January 2015
# More details about the project may be found at http://www.keegankelsey.com/insightdemo/

A. Background and objective: This project was in partnership with a text messaging service and assigned during the January 2015 session of Insight in Palo Alto. The broad goal was to assign a, "flavor" to each conversation, for which I took to mean a type of sentiment or tone. A file of over 3 million text messages were received from the partnering company, however the messages were all anonymized and masked, i.e, language was stripped from the message along with any unique identifier (sex of user, age, etc). Sentence structure, punctuation, and emotions were still present in the messages, and I was able to use this information to build sentiment labels for each message and then assign a sentiment to the full conversation. For example, a messages the normally would read as, "I'll see you at 8 tonight, okay??" came to me as, "A'aa aaa aaa aa 0 aaaaaaa, aaaa??"

B. Overview of R script: Below are the descriptions, in detail, of each section of code. The overall concept was to build labels and features from unlabeled data, build a predictive model, and then use a subset of the data to validate the model. The product that was returned to the company was one that received a new message as input, automatically assigned sentiment based on the predictive model, and then updated the whole conversation sentiment using a hidden markov model and the new message information.

C. R scripts
  1. 01_Clean_data.R:
	The original .csv file sent by the company was cleaned and separated into respective columns of data.
  2. 02_Store_data_MySQL.R:
	The cleaned file received from the company was saved into a MySQL relational database.
  3. 03_Exploratory_analysis.R:
	Parameters of the dataset were explored.
  4. 04_Clean_data_round_2.R:
	The original file contained individuals 
  5. 05_Emoticon_library.R:
	A library of emoticons was created and clusters of related emoticons were grouped together using user’s messages.
  6. 06_Message_feature_detection.R:
	Features were extracted from each message and user profile.
  7. 07_Emoticon_training_test.R:
	A random forest was used to build a predictive model for assigning sentiment to individual messages.
  8. 08_Conversation_sentiment_build.R:
	A hidden markov model was used to assign a sentiment to each conversation.
  9. 09_Final_working_model.R:
	Here, I supplied the final working model to the company. This code contained the final random forest classifier and hidden markov model. This file is not included in the public Github repository.
