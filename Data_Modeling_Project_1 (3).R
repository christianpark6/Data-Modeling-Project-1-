#Trying to find predictors to create model that will help us find Total Number of Followers on Congressmen's Social Media. 
library(tidyverse)
library(dplyr)
library(stringr)
library(leaps)
library(carData)
library(car)
library(alr3)
library(ggplot2)
#Cleaning the dataset removing messed up enteries from Party Column that aren't "R" or "D" as well as subsetting data with only enteries from specific time period.
clean_data = subset(congress,congress$Party %in% c("R","D")) ##Cleans data for enteries that may not be R or D as there were a few messed up enteries.
clean_data1= subset(clean_data,clean_data$Start.of.Term %in% c("2019-01-01"))
clean_data1= clean_data1[c("Start.of.Term", "Platform", "Party", "Bioguide.ID", "Number.of.Active.Accounts", "Max.Total.Followers",
                           "End.of.Term", "First.Post", "Last.Post", "Total.Posts", "Average.Post.Favorites.Reactions", "Average.Post.Retweets.Shares")]
#Delete rows
clean_data1= clean_data1[!(clean_data1$End.of.Term!=""),]
clean_data2= subset(clean_data1,clean_data$End.of.Term %in% c(""))
par(mfrow=c(1,1))
##Scatter Plot showing relationship between Max Total Followers and Total Posts
with(clean_data1,plot(Total.Posts, Max.Total.Followers, col=ifelse(Party == 'R', 'red','blue')))
with(clean_data1,plot(Average.Post.Favorites.Reactions, Average.Post.Retweets.Shares, col=ifelse(Party == 'R', 'red','blue')))

##Automated Stepwise Model Selection to find the best predictors for Total number of followers.
step(lm(Max.Total.Followers~1, data=clean_data1), scope=list(lower=lm(Max.Total.Followers~1, data=clean_data2) ,
upper=lm(Max.Total.Followers~Party + Number.of.Active.Accounts + Total.Posts + Average.Post.Favorites.Reactions + Average.Post.Retweets.Shares, data=clean_data1), direction="forward" ))
plot(clean_data1[,c('Max.Total.Followers','Average.Post.Favorites.Reactions','Average.Post.Retweets.Shares','Total.Posts','Number.of.Active.Accounts','Party')])
plot(clean_data1[,c('Party','Average.Post.Favorites.Reactions','Average.Post.Retweets.Shares','Total.Posts','Number.of.Active.Accounts')])
##Below is our first model given the predictors from the automated stepwise model selection process.
model1= lm(Max.Total.Followers ~ Average.Post.Favorites.Reactions + Average.Post.Retweets.Shares + 
Total.Posts + Number.of.Active.Accounts + Party, data=clean_data1)
summary(model1)
vif(model1)
par(mfrow=c(2,2))
plot(model1)

#Creation of Model 2 after looking at vif and plots.
model2= lm(Max.Total.Followers ~  Average.Post.Retweets.Shares + 
             Total.Posts + Number.of.Active.Accounts + Party, data=clean_data1)
vif(model2)
summary(model2)
plot(model2)
##Creation of the final model once undergoing power transformations.
powerTransform(clean_data1[,c('Max.Total.Followers','Average.Post.Retweets.Shares','Total.Posts','Number.of.Active.Accounts')])
model3= lm(log(Max.Total.Followers)~log(Average.Post.Retweets.Shares)+log(Total.Posts)+I(Number.of.Active.Accounts^2)+Party,data=clean_data1)
plot(model3)
vif(model3)
summary(model3)
par(mfrow=c(2,2))
with(clean_data1, plot(log(Max.Total.Followers)~log(Average.Post.Retweets.Shares),col=ifelse(Party == 'R', 'red','blue')))
length(model3)
#########
pred_r=predict(model3, data.frame(Party="R",Total.Posts=(mean(clean_data1$Total.Posts)), Number.of.Active.Accounts=(mean(clean_data1$Number.of.Active.Accounts)), Average.Post.Retweets.Shares=seq(2,10000,100)))
points(log(seq(2,10000,100)), pred_r, col="red", type="l")
pred_d=predict(model3, data.frame(Party="D",Total.Posts=(mean(clean_data1$Total.Posts)), Number.of.Active.Accounts=(mean(clean_data1$Number.of.Active.Accounts)), Average.Post.Retweets.Shares=seq(2,10000,100)))
points(log(seq(2,10000,100)), pred_d, col="blue", type="l")
##Interaction Model showing Interaction between Party and log(Average.Post.Retweets.Shares)
with(clean_data1, plot(log(Max.Total.Followers)~log(Average.Post.Retweets.Shares),col=ifelse(Party == 'R', 'red','blue')))
model4= lm(log(Max.Total.Followers)~log(Average.Post.Retweets.Shares)+log(Total.Posts)+I(Number.of.Active.Accounts^2)+Party+ Party*log(Average.Post.Retweets.Shares),data=clean_data1)
pred_rnew=predict(model4, data.frame(Party="R",Total.Posts=(mean(clean_data1$Total.Posts)), Number.of.Active.Accounts=(mean(clean_data1$Number.of.Active.Accounts)), Average.Post.Retweets.Shares=seq(2,10000,100)))
points(log(seq(2,10000,100)), pred_r, col="red", type="l")
pred_dnew=predict(model4, data.frame(Party="D",Total.Posts=(mean(clean_data1$Total.Posts)), Number.of.Active.Accounts=(mean(clean_data1$Number.of.Active.Accounts)), Average.Post.Retweets.Shares=seq(2,10000,100)))
points(log(seq(2,10000,100)), pred_d, col="blue", type="l")
points(seq(55,85,1), pred_female, col="red", type="l")
pred_female=predict(mod_htwt_gend2, data.frame(Gender="Female", Height=seq(55,85,1)))
##Testing Visualization 
ggplot(data =clean_data1, mapping = aes(y = log(Max.Total.Followers), x = log(Average.Post.Retweets.Shares), col=ifelse(Party == 'R', 'red','blue'))) +
geom_point(alpha = 0.3) +
geom_line(aes(y = log(Max.Total.Followers)), lwd = 1)
points(seq(1,14,.5),model3, type ='l', col ='red')
length(model3)
##Interaction
model4= lm(log(Max.Total.Followers)~log(Average.Post.Retweets.Shares)+log(Total.Posts)+I(Number.of.Active.Accounts^2)+Party+ Party*log(Average.Post.Retweets.Shares),data=clean_data1)
pred_prim = predict(model3, data.frame(Party="R",log(Average.Post.Retweets.Shares)=seq(0,10,1)))

pred_R2019=predict(model3, data.frame(Party='R',Average.Post.Retweets.Shares=seq(0,10,1)))
pred_D2019=predict(model3, data.frame(Party='D', log(Average.Post.Retweets.Shares)=seq(0,10,1)))
pred_Rnew2019=predict(model4, data.frame(Party='R',Average.Post.Retweets.Shares=seq(0,10,1)))
pred_Dnew2019=predict(model4, data.frame(Party='D', log(Average.Post.Retweets.Shares)=seq(0,10,1)))

#Best subsets regression for model 2 in order to look at BIC to reassure us 4 predictor model is the best. 

subs = regsubsets(log(Max.Total.Followers) ~ log(Average.Post.Retweets.Shares) + 
                    
log(Total.Posts) + I(Number.of.Active.Accounts^2) + Party, data=clean_data1)

summary(subs)

par(mfrow=c(1,1))

subsets(subs, statistic="bic")

subsets(subs, statistic="cp")

subsets(subs, statistic="adjr2")

