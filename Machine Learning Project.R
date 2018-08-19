###################
#   PREPARATION   #
###################
data = read.csv("newnewnewnew TMDB.csv",header=T)
set.seed(4510)
train = sample(nrow(data),0.7*nrow(data))

##############################################
#   CONVERT TO ONLY SHOW NUMERICAL NUMBERS   #
##############################################
columns <- c()
for (i in 1:dim(data)[2])
{
  if (is.numeric(data[, i]) || is.integer(data[, i]))
  {
    columns[i] = T
  }
  else
  {
    columns[i] = F
  }
}
temp = na.omit(data[, columns])
temp.train = temp[train,]
temp.test = temp[-train,]

#######################
#    STANDARDIZING    #
#######################
# For Non-CV Methods
#Actually, in many algorithms, scaling using std dev is enough. 
#The penalty in most models are affected by the std dev of the variables. The means centralisarion affect the effects of variables on intercept in some models.
temp.binary = temp[,c(7:28)]
temp.numeric = temp[,c(1:6)]
temp.numeric.mean = apply(temp.numeric[train,],2,mean)
temp.numeric.sd = apply(temp.numeric[train,],2,sd)
temp.binary.sd = apply(temp.binary[train,],2,sd)
temp.scaled.binary = data.frame(scale(x=temp.binary,center=FALSE,scale=temp.binary.sd))
temp.scaled.numeric = data.frame(scale(x=temp.numeric,center=temp.numeric.mean,scale=temp.numeric.sd))
temp.numeric.train = temp.scaled.numeric[train,]
temp.binary.train = temp.scaled.binary[train,]
temp.train.stand = cbind(temp.numeric.train,temp.binary.train)
temp.numeric.test = temp.scaled.numeric[-train,]
temp.binary.test = temp.scaled.binary[-train,]
temp.test.stand = cbind(temp.numeric.test,temp.binary.test)
temp.test.stand
train.mat = model.matrix(revenue ~ ., data = temp.train.stand)
test.mat = model.matrix(revenue ~ ., data = temp.test.stand)
# For CV Methods
set.seed(4510)
train1=as.data.frame(train)
pseudo.train = sample(nrow(train1),0.7*nrow(train1))
temp.pseudo.numeric = temp.numeric[train,]
temp.pseudo.binary = temp.binary[train,]
temp.pseudo.mean.numeric = apply(temp.numeric[pseudo.train,],2,mean)
temp.pseudo.sd.numeric = apply(temp.numeric[pseudo.train,],2,sd)
temp.pseudo.sd.binary = apply(temp.binary[pseudo.train,],2,sd)
temp.pseudo.scaled.numeric = data.frame(scale(x=temp.pseudo.numeric,center=temp.pseudo.mean.numeric,scale=temp.pseudo.sd.numeric))
temp.pseudo.scaled.binary = data.frame(scale(x=temp.pseudo.binary,center=FALSE,scale=temp.pseudo.sd.binary))
temp.pseudo.numeric.train = temp.pseudo.scaled.numeric[pseudo.train,]
temp.pseudo.binary.train = temp.pseudo.scaled.binary[pseudo.train,]
temp.pseudo.train.stand = cbind(temp.pseudo.numeric.train,temp.pseudo.binary.train)
temp.pseudo.numeric.test = temp.pseudo.scaled.numeric[-pseudo.train,]
temp.pseudo.binary.test = temp.pseudo.scaled.binary[-pseudo.train,]
temp.pseudo.test.stand = cbind(temp.pseudo.numeric.test,temp.pseudo.binary.test)


################################################
#   MULTIPLE LINEAR REGRESSION (INTERACTION)   #
################################################
lm.fit = lm(revenue~., data=temp.train.stand)
summary(lm.fit)
#Adjusted R-squared 0.4723
lm.fit.interaction = lm(revenue~. +(budget*Adventure), data=temp.train.stand)
summary(lm.fit.interaction)
#Adjusted R-squared 0.4764
# Predict on the test dataset
lm.pred = predict(lm.fit,temp.test.stand)
lm.pred1 = predict(lm.fit.interaction, temp.test.stand)
# Computing Test error
mean((temp.test.stand$revenue - lm.pred)^2)
mean((temp.test.stand$revenue - lm.pred1)^2)
# The test error for model without interaction model is 0.3726796
# The test error for model with interaction model is 0.3617262

###################
#   CORRELATION   #
###################
cor(temp.train.stand)
# Budget V.S Adventure correlation is 0.475370841

##################################
#   FORWARD STEPWISE SELECTION   #
##################################
library(leaps)
# According to adjusted r-squared
regfit.fwd <- regsubsets(revenue ~ ., data = temp.train.stand, nvmax = 27,
                         method = "forward")
(fwd.summary <- summary(regfit.fwd))
(fwd.max = which.max(fwd.summary$adjr2))
fwd.coef = coef(regfit.fwd, fwd.max)
fwd.pred <- test.mat[, names(fwd.coef)] %*% fwd.coef
mean((temp.test.stand$revenue - fwd.pred) ^ 2) # MSE
# The test error obtained is 0.3728298


# According to cp (AIC)
regfit.fwd <- regsubsets(revenue ~ ., data = temp.train.stand, nvmax = 27,
                         method = "forward")
(fwd.summary <- summary(regfit.fwd))
(fwd.max = which.min(fwd.summary$cp))
fwd.coef = coef(regfit.fwd, fwd.max)
fwd.pred <- test.mat[, names(fwd.coef)] %*% fwd.coef
mean((temp.test.stand$revenue - fwd.pred) ^ 2) # MSE
# The test error obtained is 0.3716237

# According to BIC.
regfit.fwd <- regsubsets(revenue ~ ., data = temp.train.stand, nvmax = 27,
                         method = "forward")
(fwd.summary <- summary(regfit.fwd))
(fwd.max = which.min(fwd.summary$bic))
fwd.coef = coef(regfit.fwd, fwd.max)
fwd.pred <- test.mat[, names(fwd.coef)] %*% fwd.coef
mean((temp.test.stand$revenue - fwd.pred) ^ 2) # MSE
# The test error obtained is 0.3681348

  
##############################
#   RIDGE REGRESSION MODEL   #
##############################
library(glmnet)
grid = 10^seq(-2,2,length=100)
set.seed(4510)
train.pseudo.mat = model.matrix(revenue ~ ., data = temp.pseudo.train.stand)
test.pseudo.mat = model.matrix(revenue ~ ., data = temp.pseudo.test.stand)
# Obtain best lambda
set.seed(4510)
ridge.mod = cv.glmnet(train.pseudo.mat,temp.pseudo.train.stand$revenue, alpha=0, lambda=grid)
ridge.bestlam = ridge.mod$lambda.min
ridge.bestlam
# Refit using the whole training dataset
set.seed(4510)
ridge.cvmod = glmnet(train.pseudo.mat, temp.pseudo.train.stand$revenue, alpha = 0, lambda = ridge.bestlam)
ridge.cvmod
ridge.coef = coef(ridge.cvmod)
ridge.coef
# Predict Test Set
ridge.pred = predict(ridge.cvmod, newx=test.pseudo.mat)
ridge.pred
mean((ridge.pred - temp.pseudo.test.stand$revenue)^2)
# The test error obtained is 0.3614558

#############
#   LASSO   #
#############
set.seed(4510)
lasso.mod = cv.glmnet(train.pseudo.mat,temp.pseudo.train.stand$revenue,alpha=1,lambda=grid)
lasso.bestlam = lasso.mod$lambda.min
lasso.bestlam

#Refit using the whole training data
lasso.cvmod = glmnet(train.pseudo.mat, temp.pseudo.train.stand$revenue, alpha = 1, lambda = lasso.bestlam)
lasso.coef = coef(lasso.cvmod)
lasso.coef

#Predict test set
lasso.pred = predict(lasso.cvmod, newx=test.pseudo.mat)
mean((lasso.pred - temp.pseudo.test.stand$revenue)^2)
# The test error obtained is 0.3659449

# The ridge regression has the lowest test error so we will use this model.
best.model.train = temp.train.stand
best.model.test = temp.test.stand
best.pseudo.model.train = temp.pseudo.train.stand
best.pseudo.model.test = temp.pseudo.test.stand
##########################
#  LOGISTIC REGRESSION   #
##########################
bins.columns = c()
bins.train = data[train,]
bins.train
bins.test = data[-train,]
bins.train1 = bins.train$Bins
bins.test1 = bins.test$Bins
bins.train2 = as.data.frame(bins.train1)
temp.bins.train= cbind(temp.train.stand, bins.train$Bins)
temp.bins.test = cbind(temp.test.stand, bins.test$Bins)
temp.bins.pseudo.train = temp.bins.train[pseudo.train,]
temp.bins.pseudo.test = temp.bins.train[-pseudo.train,]
colnames(temp.bins.train)[29] = "bins"
colnames(temp.bins.test)[29] = "bins"
library(nnet)
glm.fit = multinom(bins~., data=temp.bins.train)
summary(glm.fit)
glm.prob = predict(glm.fit,type="probs",newdata=temp.bins.test)
glm.pred = predict(glm.fit,type="class",newdata=temp.bins.test)
table(glm.pred, temp.bins.test$bins)
mean(glm.pred == temp.bins.test$bins)
#The overall fraction of correct prediction is 0.7555866
#From the confusion matrix, we see that the model makes (1-0.7555866 = 0.2444134) mistakes.

##########################
#     REGRESSION TREE    #
##########################
# Tree
set.seed(4510)
reg.tree <- tree(revenue ~ ., best.pseudo.model.train)
reg.tree
summary(reg.tree)
plot(reg.tree)
text(reg.tree)
tree.pred = predict(reg.tree, best.pseudo.model.test)
plot(tree.pred, best.pseudo.model.test$revenue)
abline(0, 1)
mean((best.pseudo.model.test$revenue - tree.pred) ^ 2)
# The test error is 0.4814516

# CV for Tree
set.seed(4510)
reg.tree <- tree(revenue ~ ., best.pseudo.model.train)
cv.reg.tree <- cv.tree(reg.tree, FUN = prune.tree, K=10)
cv.reg.tree
plot(cv.reg.tree$size, cv.reg.tree$dev, type = 'b')
(min.cv.reg <- cv.reg.tree$size[which.min(cv.reg.tree$dev)])

# Prune Tree
prune.reg.tree <- prune.tree(reg.tree, best = min.cv.reg)
plot(prune.reg.tree)
text(prune.reg.tree)

# Predict
prune.tree.pred <- predict(prune.reg.tree, best.pseudo.model.test)
plot(prune.tree.pred, best.pseudo.model.test$revenue)
abline(0, 1)
mean((best.pseudo.model.test$revenue - prune.tree.pred) ^ 2)

# The test error is 0.4814516. Apparently, pruning doesn't improve MSE.
# Bagging
set.seed(4510)
bag.reg = randomForest(revenue~., data=best.pseudo.model.train, mtry=27,ntree=500, importance=TRUE)
bag.reg
importance(bag.reg)
varImpPlot(bag.reg)
set.seed(4510)
bag.pred = predict(bag.reg, best.pseudo.model.test)
mean((best.pseudo.model.test$revenue - bag.pred)^2)
# MSE = 0.3786706
# The top 5 most important variables (based on MSE) are budget, top_companies, drama, release_month, runtime

# Random Forests
set.seed(4510)
rf = randomForest(revenue~., data=best.pseudo.model.train, mtry=floor(sqrt(27)), ntree=500, importance=TRUE)
rf
importance(rf)
varImpPlot(rf)
rf.pred = predict(rf,best.pseudo.model.test)
mean((best.pseudo.model.test$revenue - rf.pred)^2)
# MSE = 0.3184513
# The top 5 most important variables (based on MSE) are budget, runtime, family, animation, top_companies


# Boosting 
set.seed(4510)
boost.reg = gbm(revenue~., data=best.pseudo.model.train, distribution = "gaussian", n.trees= 10000, interaction.depth=1,shrinkage=0.01)
boost.reg
summary(boost.reg)
boost.pred = predict(boost.reg, best.pseudo.model.test, n.trees=10000)
mean((best.pseudo.model.test$revenue - boost.pred)^2)
# MSE = 0.4284014
# The top 5 most important variables are budget, runtime, release_year, animation, family.

##############################
#     CLASSIFICATION TREE    #
##############################
best.model.bins.train = cbind(best.model.train, bins.train$Bins)
best.model.bins.test = cbind(best.model.test, bins.test$Bins)
colnames(best.model.bins.train)[29] = "bins"
colnames(best.model.bins.test)[29] = "bins"
temp.bins.train1 <- best.model.bins.train[-3]
temp.bins.test1 <- best.model.bins.test[-3]
# Tree
cls.tree <- tree(bins ~ ., temp.bins.train1)
summary(cls.tree)
cls.tree
plot(cls.tree)
text(cls.tree)
set.seed(4510)
cls.pred <- predict(cls.tree, newdata = temp.bins.test1, type = "class")
table(cls.pred, temp.bins.test1$bins)
mean(cls.pred == temp.bins.test1$bins)
# The misclassification rate is (1-0.2304469 = 0.7695531)

# CV for Tree
set.seed(4510)
cv.cls.tree <- cv.tree(cls.tree, FUN = prune.misclass)
cv.cls.tree
plot(cv.cls.tree$size, cv.cls.tree$dev, type = 'b')
(min.cv.cls <- cv.cls.tree$size[which.min(cv.cls.tree$dev)])

# Prune Tree
prune.class.tree <- prune.tree(cls.tree, best = min.cv.cls)
plot(prune.class.tree)
text(prune.class.tree)

# Predict
set.seed(4510)
prune.cls.pred <- predict(prune.class.tree, newdata = temp.bins.test1, type = "class")
table(prune.cls.pred, temp.bins.test1$bins)
mean(prune.cls.pred == temp.bins.test1$bins)
# The misclassification rate is (1-0.2304469 = 0.7695531)

# Random Forests
set.seed(4510)
rf = randomForest(bins~., data=temp.bins.train1, mtry=floor(sqrt(27)), ntree=500, importance=TRUE)
rf
importance(rf)
varImpPlot(rf)
rf.pred = predict(rf,temp.bins.test1,type="class")
table(rf.pred, temp.bins.test1$bins)
mean(rf.pred == temp.bins.test1$bins)
# The misclassifiacction rate is (1-0.2472067 = 0.7527933)
# The top 5 most important variables (based on meandecreaseGini) are budget, runtime, release_year, release_month, top_companies

##########################
#     NAIVE BAYESIAN     #
##########################
library(naivebayes)
bayes.model = naive_bayes(bins~.,data = best.model.bins.train)
bayes.pred = predict(bayes.model, best.model.bins.test)
table(bayes.pred,best.model.bins.test$bins)
mean((bayes.pred == best.model.bins.test$bins))
# The misclassification rate is (1-0.4636872 = 0.5363128)

#############################
#       TEXT MINING         #       
#############################
library(rJava)
library(NLP) #NLP Functions
library(tm) #Text-mining
library(SnowballC) #Stemming
library(qdap) #Sentence Split
library(wordcloud) #Colours for WordCloud
library(stats) #clustering
library(readxl)
search = read_excel("Plot.xlsx",sheet="Sheet1")
str(search)
summary(search)
search_terms = search[,1]
n_search_terms = nrow(search_terms)
search_Sterms = list()
for (i in 1:n_search_terms){
  temp = sent_detect_nlp(search_terms[i,])
  search_Sterms[[i]] = temp}
search_Vterms = VCorpus(VectorSource(search_Sterms))
search_Vterms
as.character(search_Vterms[[1]])
search_Vterms = tm_map(search_Vterms,content_transformer(tolower))
search_Vterms = tm_map(search_Vterms, removeWords,stopwords("english"))
search_Vterms = tm_map(search_Vterms,removePunctuation)
search_Vterms = tm_map(search_Vterms,stripWhitespace)
newstopwords = c("and","by","to","for","with","make","must","soon","will","now","come","can","become","set","take","back","first","just","get")
search_Vterms = tm_map(search_Vterms,removeWords,newstopwords)
search_Vterms = tm_map(search_Vterms,PlainTextDocument)
search_Vterms = tm_map(search_Vterms,stemDocument)
search_Vterms
dtm_searchF = DocumentTermMatrix(((search_Vterms)))
dtm_searchF
dtm_searchF_ctrl = DocumentTermMatrix(search_Vterms, control=list(wordLenth=c(3,15),bounds=list(global=c(100,3000))))
dtm_searchF_ctrl
findFreqTerms(dtm_searchF_ctrl,100)
FFreq_Terms = colSums(as.matrix(dtm_searchF_ctrl))
FFreq_Terms
order_Ffreq_term = order(FFreq_Terms,decreasing=TRUE)
FFreq_Terms[order_Ffreq_term]
dtm_searchF_cluster = as.matrix(dtm_searchF_ctrl)
dtm_searchF_cluster

cluster1 = dtm_searchF_cluster[,c("agent")]
cluster2 = dtm_searchF_cluster[,c("dead")]
cluster3 = dtm_searchF_cluster[,c("discov")]
cluster4 = dtm_searchF_cluster[,c("famili")]
cluster5 = dtm_searchF_cluster[,c("fight")]
cluster6 = dtm_searchF_cluster[,c("film")]
cluster7 = dtm_searchF_cluster[,c("find")]
cluster8 = dtm_searchF_cluster[,c("group")]
cluster9 = dtm_searchF_cluster[,c("help")]
cluster10 = dtm_searchF_cluster[,c("home")]
cluster11 = dtm_searchF_cluster[,c("human")]
cluster12 = dtm_searchF_cluster[,c("kill")]
cluster13 = dtm_searchF_cluster[,c("lead")]
cluster14 = dtm_searchF_cluster[,c("learn")]
cluster15 = dtm_searchF_cluster[,c("life")]
cluster16 = dtm_searchF_cluster[,c("live")]
cluster17 = dtm_searchF_cluster[,c("love")]
cluster18 = dtm_searchF_cluster[,c("murder")]
cluster19 = dtm_searchF_cluster[,c("mysteri")]
cluster20 = dtm_searchF_cluster[,c("plan")]
cluster21 = dtm_searchF_cluster[,c("power")]
cluster22 = dtm_searchF_cluster[,c("save")]
cluster23 = dtm_searchF_cluster[,c("school")]
cluster24 = dtm_searchF_cluster[,c("secret")]
cluster25 = dtm_searchF_cluster[,c("son")]
cluster26 = dtm_searchF_cluster[,c("team")]
cluster27 = dtm_searchF_cluster[,c("time")]
cluster28 = dtm_searchF_cluster[,c("true")]
cluster29 = dtm_searchF_cluster[,c("war")]
cluster30 = dtm_searchF_cluster[,c("work")]
cluster31 = dtm_searchF_cluster[,c("world")]

Score = matrix(data=0, n_search_terms,31)
Score[,1] = as.matrix(cluster1)
Score[,2] = as.matrix(cluster2)
Score[,3] = as.matrix(cluster3)
Score[,4] = as.matrix(cluster4)
Score[,5] = as.matrix(cluster5)
Score[,6] = as.matrix(cluster6)
Score[,7] = as.matrix(cluster7)
Score[,8] = as.matrix(cluster8)
Score[,9] = as.matrix(cluster9)
Score[,10] = as.matrix(cluster10)
Score[,11] = as.matrix(cluster11)
Score[,12] = as.matrix(cluster12)
Score[,13] = as.matrix(cluster13)
Score[,14] = as.matrix(cluster14)
Score[,15] = as.matrix(cluster15)
Score[,16] = as.matrix(cluster16)
Score[,17] = as.matrix(cluster17)
Score[,18] = as.matrix(cluster18)
Score[,19] = as.matrix(cluster19)
Score[,20] = as.matrix(cluster20)
Score[,21] = as.matrix(cluster21)
Score[,22] = as.matrix(cluster22)
Score[,23] = as.matrix(cluster23)
Score[,24] = as.matrix(cluster24)
Score[,25] = as.matrix(cluster25)
Score[,26] = as.matrix(cluster26)
Score[,27] = as.matrix(cluster27)
Score[,28] = as.matrix(cluster28)
Score[,29] = as.matrix(cluster29)
Score[,30] = as.matrix(cluster30)
Score[,31] = as.matrix(cluster31)
Score
colnames(Score) = c("agent","dead","discov","famili","fight","film","find","group","help","home","human","kill","lead","learn","life","live","love","murder","mysteri","plan","power","save","school","secret","son","team","time","true","war","work","world")

# Combining and restandardizing
score.train = Score[train,]
score.test = Score[-train,]

score.train.sd = apply(score.train,2,sd)
score.scaled.train = data.frame(scale(x=score.train,center=FALSE,scale=score.train.sd))
score.train.stand = cbind(best.model.train,score.scaled.train)

score.test.sd = apply(score.test,2,sd)
score.scaled.test = data.frame(scale(x=score.test,center=FALSE,scale=score.test.sd))
score.test.stand = cbind(best.model.test, score.scaled.test)

# With Words
lm.fit.words= lm(revenue~., data=score.train.stand)
summary(lm.fit.words)
lm.pred.words = predict(lm.fit.words, score.test.stand)
# Adjusted R-squared 0.4799, which is a little better as compared to without words.

# Computing Test error
mean((score.test.stand$revenue - lm.pred.words)^2)
# The test error obtained is 0.3842086

# Without Words
lm.fit1 = lm(revenue~., data=best.model.train)
summary(lm.fit1)
# Adjusted R-squared 0.4723 without words.
lm.pred1 = predict(lm.fit1,best.model.train)
mean((best.model.train$revenue -lm.pred1)^2)
# The test error obtained is 0.518848

#################################
#     SUPPORT VECTOR MACHINE    #
#################################
library(e1071)

# Linear Kernels
set.seed(4510)
svm.tune <- tune(
  svm,
  bins ~ .,
  data = best.model.bins.train,
  kernel = "linear",
  ranges = list(cost = c(0.001 , 0.01, 0.1, 1, 5, 10, 100))
)
svm.tune
summary(svm.tune)
# Best paramters: Cost 100
names(svm.tune)
svm.lin <- svm.tune$best.model
svm.pred <- predict(svm.lin, best.model.bins.test)
table(svm.pred, best.model.bins.test$bins)
mean(svm.pred == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.9162011

# Polynomial
set.seed(4510)
svm.tune1 <- tune(
  svm,
  bins ~ .,
  data = best.model.bins.train,
  kernel = "polynomial",
  ranges = list(
    cost = c(0.1 , 1 , 10 , 100 , 1000),
    degree = c(2, 3, 4, 5)
  )
)
summary(svm.tune1)
# Best paramters: Cost 100 Degree 3
svm.poly <- svm.tune1$best.model
svm.pred1 <- predict(svm.poly, best.model.bins.test)
table(svm.pred1, best.model.bins.test$bins)
mean(svm.pred1 == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.3282123

# Radial Kernels
set.seed(4510)
svm.tune2 <- tune(
  svm,
  bins ~ .,
  data = best.model.bins.train,
  kernel = "radial",
  ranges = list(
    cost = c(0.1 , 1 , 10 , 100 , 1000),
    gamma = c(0.01, 0.1, 1, 10)
  )
)
summary(svm.tune2)
# Best paramters: Cost 1000  Gamma 0.01
svm.rad <- svm.tune2$best.model
svm.pred2 <- predict(svm.rad, best.model.bins.test)
table(svm.pred2, best.model.bins.test$bins)
mean(svm.pred2 == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.6759777

####################################
#     ARTIFICIAL NEURAL NETWORK    #
####################################
library(nnet)
set.seed(4510)

#Create a function that repeat nnet and find the weights that minimize the cost
nn.rep <- function(rep, ...) { # ... means the function takes any number of arguments
  v.min <- Inf                 # initialize v.min
  for (r in 1:rep) {           # repeat nnet
    nn.temp <- nnet(...)       # fit the first nnet
    v.temp <- nn.temp$value    # store the cost
    if (v.temp < v.min) {      # choose better weights
      v.min <- v.temp
      nn.min <- nn.temp
    }
  }
  return(nn.min)
}
set.seed(4510)
nn.rep = nn.rep(rep=100, bins~., data=best.model.bins.train, size=13, linout=FALSE, trace=FALSE)
summary(nn.rep)
nn.pred = predict(nn.rep, best.model.bins.test, type="class")
table(nn.pred, best.model.bins.test$bins)
mean(nn.pred == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.8645251

set.seed(4510)
nn.rep = nn.rep(rep=100, bins~., data=best.model.bins.train, size=10, linout=FALSE, trace=FALSE)
summary(nn.rep)
nn.pred = predict(nn.rep, best.model.bins.test, type="class")
table(nn.pred, best.model.bins.test$bins)
mean(nn.pred == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.8980447

set.seed(4510)
nn.rep = nn.rep(rep=100, bins~., data=best.model.bins.train, size=8, linout=FALSE, trace=FALSE)
summary(nn.rep)
nn.pred = predict(nn.rep, best.model.bins.test, type="class")
table(nn.pred, best.model.bins.test$bins)
mean(nn.pred == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.8756983

set.seed(4510)
nn.rep = nn.rep(rep=100, bins~., data=best.model.bins.train, size=11, linout=FALSE, trace=FALSE)
summary(nn.rep)
nn.pred = predict(nn.rep, best.model.bins.test, type="class")
table(nn.pred, best.model.bins.test$bins)
mean(nn.pred == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.8994413 (Best)

set.seed(4510)
nn.rep = nn.rep(rep=100, bins~., data=best.model.bins.train, size=12, linout=FALSE, trace=FALSE)
summary(nn.rep)
nn.pred = predict(nn.rep, best.model.bins.test, type="class")
table(nn.pred, best.model.bins.test$bins)
mean(nn.pred == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.8756983

set.seed(4510)
nn.rep = nn.rep(rep=100, bins~., data=best.model.bins.train, size=9, linout=FALSE, trace=FALSE)
summary(nn.rep)
nn.pred = predict(nn.rep, best.model.bins.test, type="class")
table(nn.pred, best.model.bins.test$bins)
mean(nn.pred == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.8868715

set.seed(4510)
nn.rep = nn.rep(rep=100, bins~., data=best.model.bins.train, size=15, linout=FALSE, trace=FALSE)
summary(nn.rep)
nn.pred = predict(nn.rep, best.model.bins.test, type="class")
table(nn.pred, best.model.bins.test$bins)
mean(nn.pred == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.8645251

set.seed(4510)
nn.rep = nn.rep(rep=100, bins~., data=best.model.bins.train, size=14, linout=FALSE, trace=FALSE)
summary(nn.rep)
nn.pred = predict(nn.rep, best.model.bins.test, type="class")
table(nn.pred, best.model.bins.test$bins)
mean(nn.pred == best.model.bins.test$bins)
# The overall fraction of correct prediction is 0.8589385