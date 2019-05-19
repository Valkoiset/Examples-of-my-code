

# Prediction of heart diseases with different types of Machine Learning models as the final project of Big Data subject at my
# uni programme.
# Models used in project: Logistic Regression, Random Forest, Decision Tree, Rpart and Neural Network.

library(dplyr)
library(caret)
library(DataExplorer)

# clear the workspace, plots and console
rm(list = ls())
if (!is.null(dev.list()))
  dev.off()
cat("\014")

# load the data
setwd("~/Desktop/SGH/2 semester/Big data/Project/Heart disease")
data <- read.csv("heart.csv")
   data <- rbind(data, data) # experiment with doubling the data to achieve better results

# check the number of each group occurences
length(data$target[data$target == 1])  # 165
length(data$target[data$target == 0])  # 138

          # ------- Data Exploration-------
library(ggplot2)
# Bar chart to show proportion of people with and without heart disease
ggplot(data, aes(x = target, fill = as.factor(target))) +
  geom_bar(stat = "count") + labs(x = "TARGET", title = "Number of people with and without heart disease") +
  geom_label(stat = "count", aes(label = ..count..), size = 5) + theme_grey(11) +
  scale_fill_manual("Presense of heart disease",values=c("lightblue","red"))
  

ggplot(data, aes(x = age, fill = target)) +
  geom_density(alpha = 0.7, aes(fill = factor(target))) + labs(title = "Age density") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + labs(x = "Age") +
  theme_grey() +
  scale_fill_manual("heart disease",values=c("lightblue","red"))

# Highly correlated numerical variables are identified using a plot of the 
# correlation matrix
numeric.var = sapply(data, as.numeric)
corr.matrix = cor(numeric.var)
plot_correlation(corr.matrix)

diag(corr.matrix) = 0
highly_correlated = findCorrelation(corr.matrix, cutoff = 0.90,
                                    names = TRUE)

    # ---------- Preparation -----------
  
# Transform some columns to factor
is_cat = c("target","thal","ca","restecg","fbs","cp","sex")
data[ , is_cat] = lapply(data[, is_cat], as.factor)

levels(data$target) <- make.names(c("No","Yes"))

    data2 = data
    # Deleting variables after considering correlation plot
    data2 = within(data2, {
      slope = NULL
      oldpeak = NULL
      exang = NULL 
      thalach = NULL
    })
    
# dividing data on training and validation datasets
set.seed(713)
rand <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[rand,]
valid <- data[-rand,]

    # dividing data2 on training and validation datasets
    set.seed(713)
    rand2 <- sample(1:nrow(data2), 0.7 * nrow(data2))
    train2 <- data2[rand2,]
    valid2 <- data2[-rand2,]

# check whether there are some NA values
any(is.na(data))   
any(is.na(data2))   
# -------------------- Modelling --------------------
                
# ------- Logistic regression -------
glm.mod = glm(
  target ~ age + sex + cp + trestbps + chol + fbs +
    restecg + thal + ca,
  data = train2,
  family = binomial(link = "logit")
)
  
summary(glm.mod)

linearPred <- predict(glm.mod, valid2, type = 'response')
linearPred <- ifelse(linearPred > 0.5, 1, 0)

cm = table(linearPred, valid2$target)

Accuracy = sum(diag(cm)) / sum(cm)
Accuracy   # 0.8791209

    # ---------- Random Forest ----------
train_Control = trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                             savePredictions = "final", classProbs = T, 
                             summaryFunction = twoClassSummary)

rfOut = train(target ~ ., data = train, method = "rf", trControl = train_Control,
              metric = "ROC") 
rfOut

rf.pred = predict(rfOut, valid)
confusionMatrix(data = rf.pred, reference = valid$target, mode = "everything")
# RF Accuracy: 0.8681

# ---------------------------------------------------------------
    # ------------ save point ------------
    # save preprocessed data to be able to work from this point next time
    setwd("~/Desktop/SGH/2 semester/Big data/Project/Heart disease")
    write.csv(data, file = "data.csv", row.names = FALSE)
    
    # ------ start from here if saved already ------
    # clear the workspace, plots and console
    rm(list = ls())
    if (!is.null(dev.list()))
      dev.off()
    cat("\014")
    
    # load the data after check point
    setwd("~/Desktop/SGH/2 semester/Big data/Project/Heart disease")
    data = read.csv("data.csv")
    head(data)
    str(data)
    
          # necessary transformation for next models!
          data$target = ifelse(data$target == "Yes", 1, 0)
    
# dividing data on training and validation datasets
set.seed(713)
rand <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[rand,]
valid <- data[-rand,]
# ---------------------------------------------------------------------------
    # ------------- Bagging ------------- 
library(ipred)

mod <- bagging(target ~ ., data = train, nbagg = 1000)   # nbagg - number of iterations
a <- predict(mod, newdata = valid)
a <- round(a, 0)
str(train)
    
# Calculate error and accuracy
wynik <- data.frame(a, valid$target)
error <- sum(abs(a - valid$target))
  error        # 14
  nrow(valid)  # 91
  acc <- (nrow(valid) - error) / nrow(valid)
  acc          
  err <- 1 - acc
  err          # 0.1538462   
  
# ------------ tree ------------   
library(tree)
# building tree
tree_data <- tree(target ~ ., data = train)
summary(tree_data)

# functions plot and text draws graphical representation of the tree
plot(tree_data)
text(tree_data)

tree2_data <-
  prune.tree(tree_data, best = 3) # prune.tree determines a nested sequence of 
                                  # subtrees of the supplied tree by recursively
                                  # “snipping” off the least important splits.
plot(tree2_data)
text(tree2_data)

# predict outcome in validation data
pred <- predict(tree2_data, newdata = valid)
pred <- round(pred, 0)
result <- data.frame(pred, valid$target)
error <- sum(abs(pred - valid$target)) / nrow(valid)
error      # 0.2197802
accuracy = 1 - error
accuracy   
summary(tree2_data)

# ------------- rpart ------------- 
library(rattle) # package for GUI in R
library(rpart)

RPART.1 <-
  rpart(target ~ . , data = train, cp = 0.022) # cp - complexity parameter. larger cp - simplier tree
plotcp(RPART.1)
printcp(RPART.1)  # Pruning Table
RPART.1           # Tree Leaf Summary

# draw plot with text
par(mar = c(1, 1, 1, 1))
plot(RPART.1)
text(RPART.1)

# To obtain more smart diagram the function fancyRpartPlot from rattle package can be used
fancyRpartPlot(RPART.1)

pred2 <- predict(RPART.1, newdata = valid)
pred2 <- round(pred2, 0)
result2 <- data.frame(pred2, valid$target)
error2 <- sum(abs(pred2 - valid$target)) / nrow(valid)
error2   # 0.1208791
Acc2 <- 1 - error2
Acc2     # 0.8791209

# ---------- Neural Network ----------    
  library(RcmdrPlugin.BCA)
  
NNET.2 <- Nnet(target ~ ., data = train, decay = 0.00000001, # decay is a 
                                            # regularization to avoid over-fitting
               size = 12) # size - number of hidden units in the neural network
  NNET.2$value # Final Objective Function Value
  summary(NNET.2)
  prediction <- predict(NNET.2, newdata = valid)
  prediction <- round(prediction, 0)
  result <- data.frame(prediction, valid$target)
  error <- sum(abs(prediction - valid$target)) / nrow(valid)
  error   # 0.1098901
  Acc3 <- 1 - error
  Acc3    # 0.9010989
  
