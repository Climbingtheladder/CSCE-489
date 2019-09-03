library(ISLR)    # data sets for the textbook 
library(tree)    # decision tree   
library(caret)   # caret: cross-validation and much more   
library(ggplot2) # nice plotting tool for R
library(gmodels) # for computing confidence intervals
library(cowplot) # multiple plots in a grid canvas

set.seed(113)
x1 = runif(1000)-0.5
x2 = runif(1000)-0.5
y = ifelse( x1^2+x2^3 > median(x1^2+x2^3), "A", "B")
dataset = data.frame(x1, x2, y=as.factor(y))
ggplot(dataset, aes(x=x1, y=x2, colour=y, shape=y)) + geom_point(size=1)

#decision tree first 
head(dataset)

tree.dataset = tree(y~., dataset)
summary(tree.dataset)
plot(tree.dataset)
text(tree.dataset, pretty=0, cex=0.5)
N = nrow(dataset)
source("caret.10.fold.cv.R")


train = sample(1:n,500)
dataset.test = dataset[-train,]
y.test = y[-train]
tree.dataset =tree(y~., dataset, subset=train)
tree.pred=predict(tree.dataset, dataset.test, type="class")
conf.matrix = table(tree.pred,y.test)
precision(conf.matrix)
recall(conf.matrix)
F_meas(conf.matrix,beta=1)

cv.dataset=cv.tree(tree.dataset, FUN=prune.misclass)
names(cv.dataset)
cv.dataset

# finding the size of the best tree
index.best = which.min(cv.dataset$dev)
best.size = cv.dataset$size[index.best]


min(cv.dataset$dev)
prune.fory=prune.misclass(tree.dataset,best=best.size)
plot(prune.fory)

# pretty=0: the level names of a factor split attributes are used unchanged
text(prune.fory,pretty=0)



# Let us predict/classify the test subset 
# and compare with the groundtruth

tree.pred=predict(prune.fory, dataset.test, type="class")
conf.matrix = table(tree.pred,y.test)

precision(conf.matrix)
recall(conf.matrix)
F_meas(conf.matrix,beta=1)



