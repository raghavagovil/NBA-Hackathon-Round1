## Problem Statement
The goal of this project was to predict number of engagements on NBA's Instagram Posts from 2017 to 2019 based on some given training data

## Data Wrangling
Since the training data given was mostly clean, not much data wrangling needed to be done. I just had to convert the date in to date-type for R to work with. Then I extracted month, day, year and hour from the said date column to add to my features. For adding sentiments as my features post captions/descriptions were lemmatized to make it easier to find the underlying senitment

## Feature Engineering
The following features were added to improve my linear model:
- Year
- Month
- Day
- Hour
- Word Count: Number of words in the post description
- Tagged Count: Number of accounts tagged in the post (@)
- Hashtag Count: Number of hashtags
- Sentiments: Anger, Disgust, Fear, Happy, Sad, Joy, Surprise, Positive, Negative
- Event: Christmas Day, Off-season, Regular Season, Opening Weekend, Transfer Season etc.
- Weekend or Weekday
- Time of Day: Morning, Afternoon, Evening, Night

## Methodology
I followed the following steps to achieve the best model:
- Studied the Marginal Model plot to see how far off my data is from the linear model. The difference was not much to begin with
- Used the Inverse Reverse Plot to find that the Response variable needed to be log transformed
- Used the Box Cox transformation on all the numeric non-zero features
- Checked the Variable Inflation Factors to see if any of my independent variables were co-linear. Found that year and month (as expected) were highly collinear. Consequently, I decided to remove year, since it didn't really add much information to begin with
- Then I removed bad leverage based on Cook's Distance - around 212 data points out 7000+ were removed
- Then I used the Forward Subset regression method and BIC criterion to estimate the optimal number of independent variables

## Final Model
My final model had an R^2 of 89% and the final predictors were: Time of Day, Type of Post(Video or photo), Anger, Negative, Surprise, Event.

##### My test accuracy was 92%


This project was done as part of the NBA Hackathon 2019 First Round: https://hackathon.nba.com/
