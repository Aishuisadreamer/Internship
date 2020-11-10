#TASK 1 -PREDICTION USING SUPERVISED ML(LEVEL- BEGINNER)
#REQUIRED LIBRARIES
library(tidyverse)
library(PredictionR)
library(ggpubr)
theme_set(theme_pubr())





#DATA
Hours<-c(2.5,5.1,3.2,8.5,3.5,1.5,9.2,5.5,8.3,2.7,7.7,5.9,4.5,3.3,1.1,8.9,2.5,1.9,6.1,7.4,2.7,4.8,3.8,6.9,7.8)
Scores<-c(21,47,27,75,30,20,88,60,81,25,85,62,41,42,17,95,30,24,67,69,30,54,35,76,86)
data1<-data.frame(Hours,Scores)
print(data1)

#DATA VISUALIZATION
windows(4,4)
ggplot(data1, aes(x = Hours, y = Scores)) +
  geom_point() +
  stat_smooth()
ggsave("plot1.png")
 

#The above graph shows an increasing relationship between hours and scores variables.
We can compute the correlation coefficient between the two variables using the R function cor()


cor(data1$Scores, data1$Hours)

#There is a high correlation between the two variable hours and scores

#Simple linear regression
The linear model euation is 
Scores = b0 + b1 * Hours
model <- lm(Scores ~ Hours , data = data1)
model
#The estimated regression line can be written as Scores = 2.484+9.776*Hours

#Regression line
windows(4,4)
ggplot(data1, aes(Hours, Scores)) +
  geom_point() +
  stat_smooth(method = lm)
ggsave("plot2.png")

summary(model)

#A statistically significant p value shows there is a relationship between the study hours 
contributing to the scores

#CONFIDENCE INTERVAL
confint(model)


#PREDICTED SCORE IF A STUDENT STUDIES 9.25 HOURS /DAY
input.Hours<-9.25
Scores.prediction<-as.numeric((model$coefficients["Hours"]*input.Hours)+model$coefficients[1])

Scores.prediction
predict(model, newdata=data1)
