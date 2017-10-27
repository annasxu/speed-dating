#### Speed Dating Project Midterm Report
##### Marlene Berke and Anna Xu
###### I. The Data
The dataset we are using for this project was found on Kaggle.com. The data is on 552 participants during 21 speed dating events from 2002-2004. Participants who attended the same speed dating event (and thus filled out all the same surveys) are called “waves.” During the speed dating event, participants were asked to fill out questionnaires about themselves and their partner. Each speed dating participant rates themselves on a scale from 1 to 10 on five key attributes: Attractiveness, Sincerity, Intelligence, Fun, and Ambition. They also rate their date those same five key attributes, plus Shared Interests. They then rate how much they liked their partner (1-10 scale), and they give a yes-or-no decision as to whether they would like to see that partner again. Furthermore, participants filled out surveys about what they look for in the opposite sex, what they think the opposite sex looks for in their gender. Some waves answered these questions at different time intervals before and after the speed dating event.

The dataset also gives us demographic data on each participant, like their race, age, hometown, job, income, and hobbies.  In the original dataset, each row of the 8378 rows is an interaction between two participants. There are 551 participants total, and 195 features on each interaction (from demographic information about the participants to their assessment of their own attractiveness a couple of weeks after the event).
Not all of the waves were asked the same questions at the same time intervals. When a particular participant or wave did not answer a particular question, the is an NA. To explore a particular feature, we dropped the interactions for which there were NAs. After omitting all the rows that had NAs in any of the attributes or demographic information that we care about, we were left with 6740 interactions. 


![](https://github.com/annasxu/speed-dating/blob/master/Figure1.png)


