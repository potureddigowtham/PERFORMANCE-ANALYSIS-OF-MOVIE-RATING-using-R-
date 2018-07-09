library("party")
library("rpart")
library("rpart.plot")
library("e1071")
library("randomForest")
library("ineq")
#add file 
setwd("C:/Users/Admin/Desktop/summer project/")
play_decision<-play_decision<-read.csv("C:/Users/Admin/Desktop/summer project/Book2.csv" ,header=TRUE, sep=",")
#show table
View(play_decision)
#null all the values
play1<-play_decision[complete.cases(play_decision),]
View(play1)

play1$rating<-cut(play1$rating,breaks=c(0,125,290),labels=paste("A",1:2,sep=""))
play2<-play1[complete.cases(play1),]
set.seed(1234)

play_decision <- play_decision[complete.cases(play_decision),]
summary(play_decision)

#divide values to train=70% and validate=30%
dec<-sample(2, nrow(play2), replace=TRUE, prob=c(0.7,0.3))
train<-play2[dec==1,]
validate<-play2[dec==2,]
table(train$rating)

#glm is used to fit generalized linear models, specified by giving a symbolic description of the linear predictor and a description of the error distribution.
fit.logit<-glm(rating~director_facebook_likes+actor_3_facebook_likes+actor_1_facebook_likes+num_voted_users+facenumber_in_poster+num_user_for_reviews+country+content_rating+actor_2_facebook_likes+imdb_score+aspect_ratio+movie_facebook_likes, data=train , family=binomial())
summary(fit.logit)


prob<- predict(fit.logit ,validate , type="response")
logit.pred<-factor(prob> .5, levels=c(FALSE,TRUE), labels=c("A1","A2"))
logit.perf<- table(validate$rating , logit.pred, dnn=c("Actual","Predicted"))
logit.perf


performance<- function(table,n=2) {
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 X 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("sensitivity = ",round(sensitivity, n),
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}
#performance analysis of glm
aglm<-performance(logit.perf)


#Recursive Partitioning And Regression Trees
fit<- rpart(rating~director_facebook_likes+actor_3_facebook_likes+imdb_score+aspect_ratio,data=train, method="class",parms=list(split="information"))
summary(fit)
fit$cptable

plotcp(fit)

#decision tree
dtree.pruned<-prune(fit, cp=.0125)
fit.pruned<-prune(fit, cp=.0125)
prp(fit.pruned, type=2, extra=104, fallen.leaves = TRUE, main="Decision Tree")
dtree.pred<-predict(fit.pruned, validate, type="class")
dtree.perf<-table(validate$rating, dtree.pred, dnn=c("Actual","Predicted"))
dtree.perf

+------------------------------------------------------------------------------------------------adtree<-performance(dtree.perf)

#Conditional Inference Trees
#Recursive partitioning for continuous, censored, ordered, nominal and multivariate response variables in a conditional inference framework.
fit.ctree<-ctree(rating~.,data=train)
plot(fit.ctree, main="Conditional Inference Tree")
ctree.pred<-predict(fit.ctree, validate, type="response")
ctree.perf<-table(validate$rating, ctree.pred, dnn=c("Actual","Predicted"))
ctree.perf

actree<-performance(ctree.perf)


#histogram
hist(play2$movie_facebook_likes, main="histogram of movie likes", xlab="movie_facebook_likes",border="blue",col="green",xlim=c(36,98),las=0,breaks=10)
hist(play2$aspect_ratio, main="histogram of aspect_ratio", xlab="aspect_ratio",border="red",col="orange",xlim=c(1,2.4),las=0,breaks=10)
hist(play2$imdb_score, main="histogram of imdb_score", xlab="imdb_score",border="blue",col="pink",xlim=c(2,8),las=0,breaks=10)
hist(play2$movie_facebook_likes, main="histogram of movie likes", xlab="movie_facebook_likes",border="blue",col="green",xlim=c(36,98),las=0,breaks=10)

#Random forest
fit.forest<-randomForest(rating~content_rating+aspect_ratio+imdb_score+language+country+director_facebook_likes, data=train, na.action=na.roughfix, importance=TRUE)
fit.forest
plot(fit.forest)
forest.pred<-predict(fit.forest,validate)
forest.perf<-table(validate$rating,forest.pred,dnn=c("Actual","Predicted"))
forest.perf
aforest<-performance(forest.perf)

#Simple vector machine
fit.svm<-svm(rating~.,data=train)
fit.svm
svm.pred<-predict(fit.svm, na.omit(validate))
svm.perf<-table(na.omit(validate)$rating,svm.pred,dnn=c("Actual","Predicted"))
svm.perf

asvm<-performance(svm.perf)

#################
#performance analysis
aglm<-performance(logit.perf)
adtree<-performance(dtree.perf)
actree<-performance(ctree.perf)
aforest<-performance(forest.perf)
asvm<-performance(svm.perf)
#################

# Gini index
g<-ineq(play2$director_facebook_likes, type="Gini")
g
plot(Lc(play2$director_facebook_likes), col="purple", lwd=2)
savehistory("~/C:/Users/Admin/Desktop/summer project/gini.Rhistory")

#green--content_rating
h<-ineq(play2$content_rating, type="Gini")
h
plot(Lc(play2$content_rating), col="green", lwd=2)

#blue--imdb_score
h<-ineq(play2$imdb_score, type="Gini")
h
plot(Lc(play2$imdb_score), col="blue", lwd=2)

#pink--country
i<-ineq(play2$country, type="Gini")
i
plot(Lc(play2$country), col="pink", lwd=2)

#orange--language
j<-ineq(play2$language, type="Gini")
j
plot(Lc(play2$language), col="orange", lwd=2)

#blue--imdb_score
k<-ineq(play2$imdb_score, type="Gini")
k
plot(Lc(play2$imdb_score), col="blue", lwd=2)

#blue--director_facebook_likes
a<-ineq(play2$director_facebook_likes, type="Gini") 
a
plot(Lc(play2$director_facebook_likes), col="purple", lwd=2)

#Entropy
#CONTENT RATING
b<-ineq(play2$content_rating, type="entropy")
b
plot(Lc(play2$content_rating), col="green", lwd=2)

#director facebook
a<-ineq(play2$director_facebook_likes, type="entropy")
a
plot(Lc(play2$content_rating), col="purple", lwd=2)

#country
c<-ineq(play2$country, type="entropy")
c
plot(Lc(play2$country), col="pink", lwd=2)

#language
d<-ineq(play2$language, type="entropy")
d
plot(Lc(play2$language, col="orange", lwd=2))

x1<-c(g,h,i,k)
x2<-c(a,b,c,d)

#plot
plot(x1, type="o", col="green",ylim=c(0.0,0.4))
par(new=TRUE)
plot(x2,type="o",col="red")
model<-naiveBayes(rating~.,data=train)

newdata<- data.frame(director_facebook_likes="0",actor_3_facebook_likes="530",actor_1_facebook_likes="895",num_voted_users="700",facenumber_in_poster="1",num_user_for_reviews="600",language="English",country="USA",content_rating="PG",actor_2_facebook_likes="89",imdb_score="7.5",aspect_ratio="2.35",movie_facebook_likes="8900",movie_title="Bahubali")
newdata
model<-naiveBayes(rating~.,data=train)
predict(model,newdata,type="class")

q<- play2[order(play2$aspect_ratio),c(2,5)]
q
