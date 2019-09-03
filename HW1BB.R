mydf = read.csv('tweets.album.success.csv')
names(mydf)

#relation between positive tweets and score
mymodel1 = lm(score~positive_tweets, data=mydf)
plot(mydf$positive_tweets,mydf$score)
abline(mymodel1, lwd = 3, col = "red")
coef(mymodel1)
cor.test(mydf$positive_tweets,mydf$score)

#relation between negative tweets and score
mymodel2 = lm(score~negative_tweets, data=mydf)
plot(mydf$negative_tweets ,mydf$score)
abline(mymodel2, lwd = 3, col = "red")
coef(mymodel2)
cor.test(mydf$negative_tweets,mydf$score)


#here we add  psitive tweets - negative tweets colum and positive tweets
# + negative tweets colum into mydf data frame
pminn <- mydf$positive_tweets - mydf$negative_tweets
paddn <- mydf$positive_tweets + mydf$negative_tweets
mydf <- cbind(mydf,pminn)
mydf <- cbind(mydf,paddn)
#View(mydf1)

#relation between positive - negative tweets and score
mymodel3 = lm(score~pminn, data=mydf)
plot(mydf$pminn ,mydf$score)
abline(mymodel3, lwd = 3, col = "red")
coef(mymodel3)
cor.test(mydf$pminn, mydf$score)



#relation between positive + negative tweets and score
mymodel4 = lm(score~paddn, data=mydf1)
plot(mydf$paddn ,mydf$score)
abline(mymodel4, lwd = 3, col = "red")
coef(mymodel4)
cor.test(mydf$paddn ,mydf$score)

#get p value (exact)
summary(mymodel1)$coefficient[,4]
summary(mymodel2)$coefficient[,4]
summary(mymodel3)$coefficient[,4]
summary(mymodel4)$coefficient[,4]

# I use this to find the R squared and std 
summary(mymodel1) 
summary(mymodel2) 
summary(mymodel3) 
summary(mymodel4) 


