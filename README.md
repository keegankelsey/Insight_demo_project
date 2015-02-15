# Insight_demo_project January 2015

A. Background and objective: This project was in partnership with a text messaging serivice and assigned during the January 2015 session of Insight in Palo Alto. The broad goal was to assign a, "flavor" to each conversation, for which I took to mean a type of sentiment or tone. A file of over 3 million text messages were received from the partnering company, however the messages were all annonymized and masked, i.e, language was stripped from the message along with any unique identifier (sex of user, age, etc). Sentance structure, puncuation, and emotions were still present in the messages, and I was able to use this information to build sentiment labels for each message and then assign a sentiment to the full conversation. For example, a messages the normally would read as, "I'll see you at 8 tonight, okay??" came to me as, "A'aa aaa aaa aa 0 aaaaaaa, aaaa??"

B. Overview of R script: Below are the descriptions, in detail, of each section of code. The overall concept was to build labels and features from unlabeled data, build a predictive model, and then use a subset of the data to validate the model. The product that was returned to the company was one that received a new message as input, automatically assigned sentiment based on the predictive model, and then updated the whole conversation sentiment using a hidden markov model and the new message information.

C. R script
  1. 01_Clean_data.R
      Description: This file was used to clean the original .csv file sent by the company.
  2. 02
  3. 03
  4. 04
  5. 05
  6. 06
  7. 
