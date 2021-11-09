library(caret); library(gains);library(rpart); library(rpart.plot); library(pROC)


df <- read.csv("eBayAuctions.csv")

df$Duration <- as.factor(df$Duration)
set.seed(1234)
train_index <- sample(c(1:dim(df)[1]),dim(df)[1]*0.6)
train_data <- df[train_index,]
test_data <- df[-train_index,]


set.seed(1234)

full_tree <- rpart(Competitive. ~ ., data = train_data, method = "class",
                      cp=0, minsplit = 2, minbucket = 50,maxdepth =7)

summary(full_tree)
options(scipen = 9999)
printcp(full_tree)
pruned_tree <- prune(full_tree, cp=0.024528)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)


predict_data<-predict(pruned_tree,newdata = test_data,type = "class")

confusionMatrix(predict_data,as.factor(test_data$Competitive))

rpart.rules(pruned_tree)
plot(pruned_tree)
#---------------------------------------------------------------

#Using Neural Network
df <- read.csv("eBayAuctions.csv")

df$Competitive. <- as.factor(df$Competitive.)
C <-df$Competitive.
library(neuralnet); library(nnet); library(caret); library(e1071)

numdata_df <- df[,-8]

library(fastDummies)
numdata_df <- dummy_cols(numdata_df,select_columns = "Category");
numdata_df <- dummy_cols(numdata_df,select_columns = "currency");
numdata_df <- dummy_cols(numdata_df,select_columns = "endDay");

select_columns <- setdiff(colnames(numdata_df),c("Category","currency","endDay"))
numdata_df <- subset(numdata_df,select = select_columns)
names(numdata_df)<- gsub("/","_",names(numdata_df)) 

max = apply(numdata_df,2,max)
min = apply(numdata_df,2,min)
scaled_df<- as.data.frame(scale(numdata_df, center = min, scale = max-min)) 


auc_scaled.df <- cbind(scaled_df, C)
set.seed(1234)

train.index <- sample(c(1:dim(auc_scaled.df)[1]), dim(auc_scaled.df)[1]*0.6)  
train.df <- auc_scaled.df[train.index, ]
valid.df <- auc_scaled.df[-train.index, ]
names(train.df)[33]<- "Comp HU"
names(valid.df)[33]<- "Comp HU"
nn <- neuralnet(`Comp HU`~., data=train.df, act.fct = "logistic",hidden = 3,threshold = 0.1, linear.output = FALSE)
nn$weights
op <- compute(nn,rep=1, valid.df)
p1 <- op$net.result # get the probabilities
pred1 <- ifelse(p1>0.5,1,0) # convert probabilities into classification
pred_values<- pred1[,2]
conf_matrix <- table(pred_values, valid.df$`Comp HU`)
conf_matrix
error1 <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
error1







