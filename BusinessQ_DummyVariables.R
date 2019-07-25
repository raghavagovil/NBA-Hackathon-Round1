
#Reading the files
library(readr)
test <- read_csv("holdout_set.csv")
train <- read_csv("training_set.csv")
final_holdout <- test


#Converting Date to date format
train$Created <- strptime(train$Created, format='%Y-%m-%d  %H:%M:%S')
test$Created <- strptime(test$Created, format='%Y-%m-%d  %H:%M:%S')


#Extracting month, day and hour from date
library(lubridate)
train$Year <- year(train$Created)
train$Month <- month(train$Created)
train$Day <- weekdays(train$Created)
train$Hour <- hour(train$Created)
train$Created <- as.POSIXct(train$Created, format='%Y-%m-%d  %H:%M:%S')

test$Year <- year(test$Created)
test$Month <- month(test$Created)
test$Day <- weekdays(test$Created)
test$Hour <- hour(test$Created)
test$Created <- as.POSIXct(test$Created, format='%Y-%m-%d  %H:%M:%S')


#Count number of words, hashtags and mentions
train$WordCount <- sapply(strsplit(train$Description, " "), length)
train$TaggedCount <- sapply(strsplit(train$Description, "@"), length)
train$HashtagCount <- sapply(strsplit(train$Description, "#"), length)

test$WordCount <- sapply(strsplit(test$Description, " "), length)
test$TaggedCount <- sapply(strsplit(test$Description, "@"), length)
test$HashtagCount <- sapply(strsplit(test$Description, "#"), length)



#Extracting first hashtag and mention
library(stringr)
train$Tagged <- str_match(train$Description, "@(.*?) ")
train$Hashtag <- str_match(train$Description, "#(.*?) ")
train$Tagged[is.na(train$Tagged)] <- "No Tags"
train$Hashtag[is.na(train$Hashtag)] <- "No Hashtags"

test$Tagged <- str_match(test$Description, "@(.*?) ")
test$Hashtag <- str_match(test$Description, "#(.*?) ")
test$Tagged[is.na(test$Tagged)] <- "No Tags"
test$Hashtag[is.na(test$Hashtag)] <- "No Hashtags"


#Sentiments
library(tidyverse)
library(corpus)
library(tidytext)
library(stringi)
bing <- get_sentiments("nrc")#Calling NRC lexicon for sentiments
df <- data.frame(Description = train$Description)
df$Description <- str_replace_all(df$Description, "[^[:alnum:]]", " ")#Removing special characters
df$Description <- gsub('[[:digit:]]+', '', df$Description)#Removing digits
df$stem<- text_tokens(df$Description, stemmer = "en")#Stemming
df$ID <- seq.int(nrow(df))
df1 <- data.frame(ID = rep(df$ID, sapply(df$stem, length)), word = unlist(df$stem))
df_sentiment <- merge(df1, bing, by = "word")#Merging sentiment counts with main training dataset
df_sentiment <- as.data.frame(df_sentiment)
x <- df_sentiment %>% group_by(sentiment, ID) %>% summarise(Freq=n()) %>% spread(sentiment, Freq)
x[is.na(x)] <- 0
train$ID <- seq.int(nrow(train))
train <- merge(x = train, y = x, by = "ID", all.x = TRUE)

bing <- get_sentiments("nrc")#Calling NRC lexicon for sentiments
df <- data.frame(Description = test$Description)
df$Description <- str_replace_all(df$Description, "[^[:alnum:]]", " ")#Removing special characters
df$Description <- gsub('[[:digit:]]+', '', df$Description)#Removing digits
df$stem<- text_tokens(df$Description, stemmer = "en")#Stemming
df$ID <- seq.int(nrow(df))
df1 <- data.frame(ID = rep(df$ID, sapply(df$stem, length)), word = unlist(df$stem))
df_sentiment <- merge(df1, bing, by = "word")#Merging sentiment counts with main testing dataset
df_sentiment <- as.data.frame(df_sentiment)
x <- df_sentiment %>% group_by(sentiment, ID) %>% summarise(Freq=n()) %>% spread(sentiment, Freq)
x[is.na(x)] <- 0
test$ID <- seq.int(nrow(test))
test <- merge(x = test, y = x, by = "ID", all.x = TRUE)


#Filling NAs with 0
train$anger[is.na(train$anger)] <- 0
train$anticipation[is.na(train$anticipation)] <- 0
train$disgust[is.na(train$disgust)] <- 0
train$fear[is.na(train$fear)] <- 0
train$joy[is.na(train$joy)] <- 0
train$negative[is.na(train$negative)] <- 0
train$positive[is.na(train$positive)] <- 0
train$sadness[is.na(train$sadness)] <- 0
train$surprise[is.na(train$surprise)] <- 0
train$trust[is.na(train$trust)] <- 0

test$anger[is.na(test$anger)] <- 0
test$anticipation[is.na(test$anticipation)] <- 0
test$disgust[is.na(test$disgust)] <- 0
test$fear[is.na(test$fear)] <- 0
test$joy[is.na(test$joy)] <- 0
test$negative[is.na(test$negative)] <- 0
test$positive[is.na(test$positive)] <- 0
test$sadness[is.na(test$sadness)] <- 0
test$surprise[is.na(test$surprise)] <- 0
test$trust[is.na(test$trust)] <- 0


#Extracting only Date from the Data-Time column
train$Date <- as.Date(train$Created)
test$Date <- as.Date(test$Created)


#Creating new variables - Evet, TimeOfDay and WeekEndDay
train <- train %>% mutate(Event = case_when(Date >= as.Date("2019-02-15") & Date < as.Date("2019-02-17") ~ "All Star Weekend", 
                                            Date >= as.Date("2018-02-16") & Date < as.Date("2018-02-18") ~ "All Star Weekend",
                                            Date >= as.Date("2017-02-17") & Date < as.Date("2017-02-19") ~ "All Star Weekend",
                                            Date >= as.Date("2019-05-30") & Date < as.Date("2019-06-16") ~ "NBA Finals",
                                            Date >= as.Date("2018-05-31") & Date < as.Date("2018-06-17") ~ "NBA Finals",
                                            Date >= as.Date("2017-06-01") & Date < as.Date("2017-06-12") ~ "NBA Finals",
                                            Date >= as.Date("2019-04-13") & Date < as.Date("2019-05-29") ~ "Playoffs",
                                            Date >= as.Date("2018-04-14") & Date < as.Date("2018-05-30") ~ "Playoffs",
                                            Date >= as.Date("2017-04-15") & Date < as.Date("2017-05-31") ~ "Playoffs",
                                            Date >= as.Date("2019-12-25") & Date < as.Date("2019-12-26") ~ "Christmas Day",
                                            Date >= as.Date("2018-12-25") & Date < as.Date("2018-12-26") ~ "Christmas Day",
                                            Date >= as.Date("2017-12-25") & Date < as.Date("2017-12-26") ~ "Christmas Day",
                                            Date >= as.Date("2019-10-16") & Date < as.Date("2019-10-17") ~ "Opening Day",
                                            Date >= as.Date("2018-10-17") & Date < as.Date("2018-10-18") ~ "Opening Day",
                                            Date >= as.Date("2017-10-18") & Date < as.Date("2017-10-19") ~ "Opening Day",
                                            Date >= as.Date("2019-02-06") & Date < as.Date("2019-02-07") ~ "Trade Deadline",
                                            Date >= as.Date("2018-02-07") & Date < as.Date("2018-02-08") ~ "Trade Deadline",
                                            Date >= as.Date("2017-02-08") & Date < as.Date("2017-02-09") ~ "Trade Deadline",
                                            Date >= as.Date("2019-06-30") & Date < as.Date("2019-07-07") ~ "Free Agency",
                                            Date >= as.Date("2018-07-01") & Date < as.Date("2018-07-08") ~ "Free Agency",
                                            Date >= as.Date("2017-07-06") & Date < as.Date("2017-07-13") ~ "Free Agency",
                                            TRUE ~ "Regular Season"))

train <- train %>%  mutate(TimeOfDay = case_when(Hour >= 6 & Hour < 13 ~ "Morning",
                                                 Hour >= 13 & Hour < 17 ~ "Afternoon", 
                                                 Hour >= 17 & Hour < 20 ~ "Evening",
                                                 TRUE ~ "Night"))

train <- train %>%  mutate(WeekEndDay = case_when(Day == "Monday" | Day == "Tuesday" | Day == "Wednesday" | Day == "Thursday" | Day == "Friday" ~ "Weekday",
                                                  TRUE ~ "Weekend"))


test <- test %>% mutate(Event = case_when(Date >= as.Date("2019-02-15") & Date < as.Date("2019-02-17") ~ "All Star Weekend", 
                                          Date >= as.Date("2018-02-16") & Date < as.Date("2018-02-18") ~ "All Star Weekend",
                                          Date >= as.Date("2017-02-17") & Date < as.Date("2017-02-19") ~ "All Star Weekend",
                                          Date >= as.Date("2019-05-30") & Date < as.Date("2019-06-16") ~ "NBA Finals",
                                          Date >= as.Date("2018-05-31") & Date < as.Date("2018-06-17") ~ "NBA Finals",
                                          Date >= as.Date("2017-06-01") & Date < as.Date("2017-06-12") ~ "NBA Finals",
                                          Date >= as.Date("2019-04-13") & Date < as.Date("2019-05-29") ~ "Playoffs",
                                          Date >= as.Date("2018-04-14") & Date < as.Date("2018-05-30") ~ "Playoffs",
                                          Date >= as.Date("2017-04-15") & Date < as.Date("2017-05-31") ~ "Playoffs",
                                          Date >= as.Date("2019-12-25") & Date < as.Date("2019-12-26") ~ "Christmas Day",
                                          Date >= as.Date("2018-12-25") & Date < as.Date("2018-12-26") ~ "Christmas Day",
                                          Date >= as.Date("2017-12-25") & Date < as.Date("2017-12-26") ~ "Christmas Day",
                                          Date >= as.Date("2019-10-16") & Date < as.Date("2019-10-17") ~ "Opening Day",
                                          Date >= as.Date("2018-10-17") & Date < as.Date("2018-10-18") ~ "Opening Day",
                                          Date >= as.Date("2017-10-18") & Date < as.Date("2017-10-19") ~ "Opening Day",
                                          Date >= as.Date("2019-02-06") & Date < as.Date("2019-02-07") ~ "Trade Deadline",
                                          Date >= as.Date("2018-02-07") & Date < as.Date("2018-02-08") ~ "Trade Deadline",
                                          Date >= as.Date("2017-02-08") & Date < as.Date("2017-02-09") ~ "Trade Deadline",
                                          Date >= as.Date("2019-06-30") & Date < as.Date("2019-07-07") ~ "Free Agency",
                                          Date >= as.Date("2018-07-01") & Date < as.Date("2018-07-08") ~ "Free Agency",
                                          Date >= as.Date("2017-07-06") & Date < as.Date("2017-07-13") ~ "Free Agency",
                                          TRUE ~ "Regular Season"))

test <- test %>%  mutate(TimeOfDay = case_when(Hour >= 6 & Hour < 13 ~ "Morning",
                                               Hour >= 13 & Hour < 17 ~ "Afternoon", 
                                               Hour >= 17 & Hour < 20 ~ "Evening",
                                               TRUE ~ "Night"))

test <- test %>%  mutate(WeekEndDay = case_when(Day == "Monday" | Day == "Tuesday" | Day == "Wednesday" | Day == "Thursday" | Day == "Friday" ~ "Weekday",
                                                TRUE ~ "Weekend"))


#Correlation Plot
library(corrplot)
corr <- select_if(train, is.numeric)
M <- cor(corr, use = "complete.obs")
corrplot(M, method = "circle", type="upper")


#Train-test split
library(caret)
set.seed(123)
trainIndex <- createDataPartition(train$Engagements,p=0.75,list=FALSE)
train_split <- train[trainIndex, ]
test_split <- train[-trainIndex, ]


#First model
m1 <- lm(train$Engagements~train$`Followers at Posting` + train$TaggedCount + train$HashtagCount + as.factor(train$TimeOfDay) + as.factor(train$WeekEndDay) +  as.factor(train$Type) + train$Year + train$Month + as.factor(train$Day) + train$Hour + train$WordCount + train$anger + train$anticipation + train$disgust + train$fear + train$joy + train$negative + train$positive + train$sadness + train$surprise + train$trust + as.factor(train$Event), data = train)


#Marginal Model Plots
library(alr3)
mmp(m1)


#Inverse Reverse Plots
library(leaps)
library(car)
inverseResponsePlot(m1)


#Second Model after Response transformation
m2 <- lm((train$Engagements^(-0.07649186)) ~ train$`Followers at Posting` + train$TaggedCount + train$HashtagCount + as.factor(train$TimeOfDay) + as.factor(train$WeekEndDay) +  as.factor(train$Type) + train$Year + train$Month + as.factor(train$Day) + train$Hour + train$WordCount + train$anger + train$anticipation + train$disgust + train$fear + train$joy + train$negative + train$positive + train$sadness + train$surprise + train$trust + as.factor(train$Event), data = train)
summary(m2)


#Box-Cox
library(alr3)
summary(powerTransform(cbind(`Followers at Posting`, HashtagCount, Year, WordCount, Month) ~1, data=train))


#Third model after predictor transformations
m3 <- lm((train$Engagements^(-0.07649186)) ~ I(train$`Followers at Posting`^(-1)) + log(train$TaggedCount) + I(train$HashtagCount^(-1)) + as.factor(train$TimeOfDay) + as.factor(train$WeekEndDay) +  as.factor(train$Type) + train$Year + train$Month + as.factor(train$Day) + train$Hour + log(train$WordCount) + train$anger + train$anticipation + train$disgust + train$fear + train$joy + train$negative + train$positive + train$sadness + train$surprise + train$trust + as.factor(train$Event), data = train)
summary(m3)


#Fourth model for checking VIF
m4 <- lm((train$Engagements^(-0.07649186)) ~ I(train$`Followers at Posting`^(-1.21)) + I(train$TaggedCount^(0.33)) + I(train$HashtagCount^(-0.87)) + as.factor(train$TimeOfDay) + as.factor(train$WeekEndDay) +  as.factor(train$Type) + train$Year + train$Month + I(train$WordCount^(0.33)) + train$anger + train$anticipation + train$disgust + train$fear + train$joy + train$negative + train$positive + train$sadness + train$surprise + train$trust + as.factor(train$Event), data = train)
vif(m4)


#Fifth Model for checking VIF
m5 <- lm(log(train$Engagements) ~ I(train$`Followers at Posting`^(-1)) + log(train$TaggedCount) + I(train$HashtagCount^(-1)) + as.factor(train$TimeOfDay) + as.factor(train$WeekEndDay) +  as.factor(train$Type) + train$Month + log(train$WordCount) + train$anger + train$anticipation + train$disgust + train$fear + train$joy + train$negative + train$positive + train$sadness + train$surprise + train$trust + as.factor(train$Event), data = train)
vif(m5)


summary(m5)


#Removing bad leverage
d1 <- cooks.distance(m5)
r <- rstandard(m5)
a <- cbind.data.frame(train$ID, d1, r)
inf <- a[d1 > 4/(5826-(28+1)) & abs(r) > 2, ]
inf
dim(inf)
train2 <- train[-inf[,1],]


#Forward subset
require(leaps)
bestss<- regsubsets(log(Engagements) ~ I(`Followers at Posting`^(-1)) + log(TaggedCount) + I(HashtagCount^(-1)) + as.factor(TimeOfDay) + as.factor(WeekEndDay) + as.factor(Type) + Month + log(WordCount) + anger + anticipation + disgust + fear + joy + negative + positive + sadness + surprise + trust + as.factor(Event), data=train2, nvmax=28)
summary(bestss)


#BIC
bic <- summary(bestss)$bic
p <- length(bic)
plot(1:p, bic)
lines(1:p,bic)


boxplot(train$Engagements)


#Converting test variables
test$TimeOfDay <- as.factor((test$TimeOfDay))
test$WeekEndDay <- as.factor(test$WeekEndDay)
test$Type <- as.factor(test$Type)
test$Event <- as.factor(test$Event)


#Final model
final <- lm(log(Engagements) ~ TaggedCount + as.factor(TimeOfDay) + as.factor(WeekEndDay) + as.factor(Type) + anger + negative + surprise + as.factor(Event), data=train2)
summary(final)


#Prediction engagements
pred <- exp(predict(final, data.frame(test[,c(5, 12, 16, 21, 24, 27, 28, 29)])))
pred <- ceiling(pred)
final_holdout$Engagements <- pred


#Writing out final holdout csv
write.csv(final_holdout, file="holdout_set_DummyVariables.csv")

#y <- abs(((test$Engagements)-pred$pred)/(test$Engagements)) * 100
#mean(y)

plot(final)

anova(final)
