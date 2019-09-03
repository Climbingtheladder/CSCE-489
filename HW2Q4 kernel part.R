library(ISLR)    # data sets for the textbook 
library(tree)    # decision tree   
library(caret)   # caret: cross-validation and much more   
library(ggplot2) # nice plotting tool for R
library(gmodels) # for computing confidence intervals
library(cowplot) # multiple plots in a grid canvas
library(e1071)


set.seed(113)
x1 = runif(1000)-0.5
x2 = runif(1000)-0.5
y = ifelse( x1^2+x2^3 > median(x1^2+x2^3), "A", "B")
dataset = data.frame(x1, x2, y=as.factor(y))
ggplot(dataset, aes(x=x1, y=x2, colour=y, shape=y)) + geom_point(size=1)

N = nrow(dataset)

#10 fold cross validation for tree
K = 10
Folds = createFolds(dataset$y,k=K)

pre.yes = 
  rec.yes = 
  f1.yes  = 
  pre.no  =
  rec.no  = 
  f1.no   = NULL

for(fold in Folds){
  # current fold for testing
  # remainder for training
  
  training = dataset[-fold,]
  testing  = dataset[fold,]
  
  #what i did to test the best parameter and performance 
  #t.out=tune(svm,y~.,data=training,kernel="linear",ranges=list(cost=10^(-3:3)))
  #t.out=tune(svm,y~.,data=training,kernel="polynomial",ranges=list(cost=10^(-3:3)))
  #t.out=tune(svm,y~.,data=training,kernel="radial",ranges=list(cost=10^(-3:3)))
  #summary(t.out)
  #best=t.out$best.model
  #summary(best)
  #plot(best,training) 
  
  
  #what i did is change linear to polynomial then to radial with their best parameters
  svm.linear <- svm(y ~ ., data = training, kernel = "linear", cost = 0.01)
  #plot(svm.linear,training)
  #table(predict = predict(svm.linear, training), truth = training$y)
  pred = predict(svm.linear, testing)
  # test the tree (predict)
  #summary(svm.linear)
  
  
  # update the confusion matrix
  c.matrix = table(pred, testing$y)
  pre.yes = c(pre.yes, precision(c.matrix, relevant = "A"))
  rec.yes = c(pre.yes,    recall(c.matrix, relevant = "A"))
  f1.yes  = c(pre.yes,    F_meas(c.matrix, relevant = "A"))
  
  pre.no = c(pre.no, precision(c.matrix, relevant = "B"))
  rec.no = c(pre.no,    recall(c.matrix, relevant = "B"))
  f1.no  = c(pre.no,    F_meas(c.matrix, relevant = "B"))
}

results = data.frame(
  ci(pre.yes), 
  ci(rec.yes), 
  ci(f1.yes),
  ci(pre.no), 
  ci(rec.no), 
  ci(f1.no)
)

results = data.frame(t(results),row.names=NULL)

results = cbind(
  Class=c(rep("A",3),rep("B",3)),
  Metric=rep(c("Precision","Recall","F1"),2),
  results)

results1 = results
results1
#here i do results2 = results and results3 = results when polynomial and radial

results$Metric = factor(results$Metric, levels = c("Precision", "Recall","F1"))


#plot the results with what professor did in the examples

p3 =                                        # save the plot in p1,p2,p3
  ggplot(data=results, 
         aes(x=Metric,y=Estimate,fill=Class)) +  # specify the dataset and the columns
  ylim(0,1)                                   +  # y-axis range
  geom_bar(stat="identity",                      # plot the result as bars
           position=position_dodge())         +          
  geom_text(                                     # add the values with 2 digits
    aes(y=0.1, 
        label=round(Estimate,digits=2)),   
    vjust=1.6,
    color="white", 
    size=2.5,
    position =position_dodge(.9)) +       
  geom_errorbar(                                 # add the error bars 
    aes(ymin=CI.lower, ymax=CI.upper),           # with confidence intervals
    width=.1, 
    position=position_dodge(.9)) +
  ggtitle("10-Fold CV")                       +  # add a title   
  theme(text = element_text(size=12))

#i got my table from combining results after i do all 3 kernels
#results1
#results2
#results3

#bigresult = rbind(results1,results2,resuls3)
#rownames(bigresult)[c(1,7,13)]=c("Linear", "Polynomial", "Radical")
