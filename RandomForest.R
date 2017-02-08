library(randomForest)
ratingtest<-read.csv(file="ratings_firstmillion.csv",header = T)
ratingtest=ratingtest[1:100000,]
names(ratingtest)

ratingtest$rating <- as.factor(ratingtest$rating)

sample.ind <- sample(2, 
                     nrow(ratingtest),
                     replace = T,
                     prob = c(0.8,0.2))
rating.dev <- ratingtest[sample.ind==1,]
rating.val <- ratingtest[sample.ind==2,]

table(rating.dev$rating)/nrow(rating.dev)


table(rating.val$rating)/nrow(rating.val)


varNames <- names(rating.dev)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("rating")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("rating", varNames1, sep = " ~ "))



ratings.rf <- randomForest(rf.form,
                              rating.dev,
                              ntree=500,
                              importance=T)

plot(ratings.rf)

varImpPlot(ratings.rf)

rating.dev$predicted.response <- predict(ratings.rf ,rating.dev)


library(e1071)
library(caret)
confusionMatrix(data=rating.dev$predicted.response,
                reference=rating.dev$rating,
                positive='yes')

rating.val$predicted.response <- predict(ratings.rf ,rating.val)

confusionMatrix(data=rating.val$predicted.response,
                reference=rating.val$rating,
                positive='yes')

