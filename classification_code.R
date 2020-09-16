### MATH 445: Final Project ###
### Adam Rockett, Caelin Schaefer, Fiona Cleary, Elias Peters###


data = read.csv("Drinks5.csv", header = TRUE)
attach(data)

# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(data)/2)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(data), size = data_set_size)

# Assign the data to the correct sets
training <- data[indexes,]
validation1 <- data[-indexes,]


#import the package
library(rpart)

#constructing a tree
rpart.tree <- rpart(Region ~ beer_servings + spirit_servings + wine_servings + total_litres_of_pure_alcohol, data=training)
plot(rpart.tree, uniform=TRUE, branch=0.5, margin=0.4)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Training Set's Classification Tree")

predictions <- predict(rpart.tree, validation1, type="class")
table(predictions, validation1$Region)



### Random Forest

#import the package
library(randomForest)
set.seed(1)

# Perform training:
rf_classifier = randomForest(Region ~ beer_servings + spirit_servings + wine_servings + total_litres_of_pure_alcohol, data=training, ntree=300, mtry=2, importance=TRUE)
rf_classifier
varImpPlot(rf_classifier)
rf_classifier$err.rate[,1][200]

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier, validation1[,-6])
table(predicted=prediction_for_table, observed=validation1[,6])

# Validation set assessment #2: ROC curves and AUC (optional)

# Needs to import ROCR package for ROC curve plotting:
library(ROCR)

# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier, validation1[,-6], type="prob")

# Use pretty colours:
pretty_colours <- c("red","blue","green","purple","yellow","orange","cyan","grey","magenta")
# Specify the different classes 
classes <- levels(validation1$Region)
# For each class
for (i in 1:9) {
  # Define which observations belong to class[i]
  true_values <- ifelse(validation1[,6]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  if (mean(true_values) != 0) {
    pred <- prediction(prediction_for_roc_curve[,i],true_values)
    perf <- performance(pred, "tpr", "fpr")
    if (i==1)
    {
      plot(perf,main="ROC Curve",col=pretty_colours[i]) 
    }
    else
    {
      plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
    }
    # Calculate the AUC and print it to screen
    auc.perf <- performance(pred, measure = "auc")
    print(auc.perf@y.values)
  }
  
}


legend(0.55, 0.65, classes, col = pretty_colours, lty = 1, lwd = 1, cex = 0.8, fill = NULL, border = "white")


