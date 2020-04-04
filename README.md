# Tweet Sentiment Extraction

*Extract support phrases for sentiment labels*

Project authors and participants:

* [Fellipe Gomes](https://github.com/gomesfellipe) (Statistics - UFF, Data Scientist - FGV IBRE / BRAZIL)
* [Rumenick Pereira da Silva](https://github.com/Rumenick) (PhD Statistics - UFMG, Data Scientist - FGV IBRE / BRAZIL)
* [Luiz Fernando Coelho Passos](https://github.com/luizfcp) (Statistics student - UFF, Data Scientist Intern - FGV IBRE / BRAZIL)

Text obtained in the project description:

*"My ridiculous dog is amazing." [sentiment: positive]* 

*With all of the tweets circulating every second it is hard to tell whether the sentiment behind a specific tweet will impact a company, or a person's, brand for being viral (positive), or devastate profit because it strikes a negative tone. Capturing sentiment in language is important in these times where decisions and reactions are created and updated in seconds. But, which words actually lead to the sentiment description? In this competition you will need to pick out the part of the tweet (word or phrase) that reflects the sentiment.*

*Help build your skills in this important area with this broad dataset of tweets. Work on your technique to grab a top spot in this competition. What words in tweets support a positive, negative, or neutral sentiment? How can you help make that determination using machine learning tools?*

*In this competition we've extracted support phrases from Figure Eight's Data for Everyone platform. The dataset is titled Sentiment Analysis: Emotion in Text tweets with existing sentiment labels, used here under creative commons attribution 4.0. international licence. Your objective in this competition is to construct a model that can do the same - look at the labeled sentiment for a given tweet and figure out what word or phrase best supports it.*

*Disclaimer: The dataset for this competition contains text that may be considered profane, vulgar, or offensive.*

## What am I predicting?

*You're attempting to predict the word or phrase from the tweet that exemplifies the provided sentiment. The word or phrase should include all characters within that span (i.e. including commas, spaces, etc.). The format is as follows:*

`<id>,"<word or phrase that supports the sentiment>`

For example:

    2, "very good"
    5, "I am neutral about this"
    6, "bad"
    8, "if you say so!"
    ...
    
### Files

  * train.csv - the training set
  * test.csv - the test set
  * sample_submission.csv - a sample submission file in the correct format

### Columns

  * textID - unique ID for each piece of text
  * text - the text of the tweet
  * sentiment - the general sentiment of the tweet
  * selected_text - [train only] the text that supports the tweet's sentiment