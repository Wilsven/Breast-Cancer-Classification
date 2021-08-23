# Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caret)

# Read data
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

col_names <- names <- c('id_number', 'diagnosis', 'radius_mean', 
                        'texture_mean', 'perimeter_mean', 'area_mean', 
                        'smoothness_mean', 'compactness_mean', 
                        'concavity_mean','concave_points_mean', 
                        'symmetry_mean', 'fractal_dimension_mean',
                        'radius_se', 'texture_se', 'perimeter_se', 
                        'area_se', 'smoothness_se', 'compactness_se', 
                        'concavity_se', 'concave_points_se', 
                        'symmetry_se', 'fractal_dimension_se', 
                        'radius_worst', 'texture_worst', 
                        'perimeter_worst', 'area_worst', 
                        'smoothness_worst', 'compactness_worst', 
                        'concavity_worst', 'concave_points_worst', 
                        'symmetry_worst', 'fractal_dimension_worst')

data <- read.csv(file = url, header = FALSE,
                 col.names = col_names)
str(data)

rm(url, col_names, GCtorture, names)

# Check for missing values
sum(is.na(data))

# Remove id column and change diagnosis column to factor
data <- data %>%
  select(-id_number) %>%
  mutate(diagnosis = as.factor(diagnosis))

summary(data)

library(kableExtra)

# See proportions of diagnosis
kable(prop.table(table(data$diagnosis)),
      col.names = c("Diagnosis", "Proportions")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Diagnosis countplot
data %>%
  ggplot(aes(diagnosis, fill = diagnosis)) +
  geom_bar() +
  geom_text(stat = "Count", aes(label = ..count..), vjust = 2) +
  scale_x_discrete(breaks = c(0, 1),
                   labels = c("Benign", "Malignant")) +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(),
        legend.position = "right",
        legend.direction = "vertical") +
  scale_fill_fivethirtyeight(name = "Diagnosis",
                             labels = c("Benign", "Malignant")) +
  labs(y ="Count", title = "No. of Benign & Malignant Cases")

# Distribution cases and radius mean
data %>%
  ggplot(aes(radius_mean, fill = diagnosis)) +
  geom_histogram(bins = 30) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        legend.position = "right",
        legend.direction = "vertical") +
  scale_fill_fivethirtyeight(name = "Diagnosis",
                             labels = c("Benign", "Malignant")) +
  labs(x = "Radius mean", y = "Count", 
       title = "Distribution of Cases based on Radius Mean")

# Boxplot of cases and radius mean
data %>%
  ggplot(aes(diagnosis, radius_mean)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text()) +
  labs(y = "Radius mean", 
       title = "Boxplot of Cases based on Radius Mean")

# Boxplot of cases and perimeter/area mean
x <- data %>%
  ggplot(aes(diagnosis, perimeter_mean)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text()) +
  labs(y = "Perimeter mean", 
       title = "Boxplot of Cases based on Perimeter Mean")

y <- data %>%
  ggplot(aes(diagnosis, area_mean)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text()) +
  labs(y = "Area mean", 
       title = "Boxplot of Cases based on Area Mean")

library(gridExtra)

grid.arrange(x, y, ncol = 1)
rm(x, y)

# How Cases relate to Radius and Texture
data %>%
  ggplot(aes(radius_mean, texture_mean, colour = diagnosis)) +
  geom_point() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        legend.position = "right",
        legend.direction = "vertical") +
  scale_colour_fivethirtyeight(name = "Diagnosis",
                               labels = c("Benign", "Malignant")) +
  labs(x = "Radius mean", y = "Texture Mean", 
       title = "How Cases relate to Radius and Texture")

# Correlation of only the independent variables, exclude diagnosis
corrplot(cor(data[, -1]), type = "lower", tl.srt = 90, tl.cex = .7)

# Select highly correlated variables}
highlyCor <- colnames(data)[findCorrelation(cor(data[, -1]), cutoff = 0.9, verbose = TRUE)]

# Print out highly correlated variable names}
highlyCor

# Split data
set.seed(1, sample.kind = "Rounding")

train_index <- createDataPartition(data$diagnosis, 
                                   times = 1, p = .8, list = FALSE)
train <- data[train_index,]
test <- data[-train_index,]

# Check proportion table of Train
kable(prop.table(table(train$diagnosis)),
      col.names = c("Diagnosis", "Proportion")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# CHeck proportion of Test
kable(prop.table(table(test$diagnosis)),
      col.names = c("Diagnosis", "Proportion")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Cumulative Variance of top 10 PCs
cov_table %>%
  ggplot(aes(PC, Variance)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(min(cov_table$PC), 
                                  max(cov_table$PC), 
                                  by = 1)) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        legend.position = "right",
        legend.direction = "vertical") +
  labs(x = "PC", y = "Variance", 
       title = "Variance Explained by Top 10 PCs")

# Box plot for Top 10 PCs
data.frame(pca$x[, 1:10], Diagnosis = train$diagnosis) %>%
  gather(key = "PC", value = "Value", - Diagnosis) %>%
  ggplot(aes(PC, Value, fill = Diagnosis)) + 
  geom_boxplot() +
  scale_fill_fivethirtyeight(name = "Diagnosis",
                             labels = c("Benign", "Malignant")) +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(),
        legend.position = "right",
        legend.direction = "vertical") +
  labs(y = "Value", 
       title = "Box plot of Top 10 PCs")

# Scatter plot of PC2 vs PC1
data.frame(pca$x[, 1:2], Diagnosis = train$diagnosis) %>%
  ggplot(aes(PC1, PC2, colour = Diagnosis)) +
  geom_point() +
  stat_ellipse() +
  scale_colour_fivethirtyeight(name = "Diagnosis",
                               labels = c("Benign", "Malignant")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        legend.position = "right",
        legend.direction = "vertical") +
  labs(x = paste("PC1: ", round(cov[1]*100, 1), "%"), 
       y = paste("PC2: ", round(cov[2]*100, 1), "%"), 
       title = "Scatter plot of PC2 vs PC1")

# Train control
# Define train control parameters for appropriate models
fitControl <- trainControl(method = "repeatedcv",
                           number = 10, # 10-fold cross-validation
                           repeats = 10, # repeat each cross-validation 10 times
                           classProbs = TRUE, # class probabilities computed
                           returnResamp = "final", # only save the final resampled summary metrics
                           savePredictions = "final") # only save the final predictions for each resample

# Naive Bayes
set.seed(1, sample.kind = "Rounding")

nb <- train(train[-1], train$diagnosis, 
            method = "nb", trControl = fitControl)

nb_pred <- predict(nb, test[-1])

# Confusion Matrix
cm_nb <- confusionMatrix(nb_pred, test$diagnosis, positive = "M")

kable(cm_nb$table, caption = "Confusion Matrix for Naive Bayes") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Store results Naive Bayes
nb_res <- round(data.frame("Accuracy" = cm_nb$overall["Accuracy"],
                           "Senstivity" = cm_nb$byClass["Sensitivity"],
                           "Specificity" = cm_nb$byClass["Specificity"],
                           "F1" = cm_nb$byClass["F1"],
                           "False Neg. Rate" = 1-cm_nb$byClass["Sensitivity"],
                           "False Pos. Rate" = 1-cm_nb$byClass["Specificity"],
                           row.names = "Naive Bayes"), 2)

# Logistic Regression
set.seed(1, sample.kind = "Rounding")

glm <- train(train[-1], train$diagnosis,
             method = "glm", 
             trControl = fitControl)

glm_pred <- predict(glm, test[-1])

# Confusion Matrix
cm_glm <- confusionMatrix(glm_pred, test$diagnosis, positive = "M")

kable(cm_glm$table, caption = "Confusion Matrix for Logistic Regression") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Store results Logistic Regression
glm_res <- round(data.frame("Accuracy" = cm_glm$overall["Accuracy"],
                            "Senstivity" = cm_glm$byClass["Sensitivity"],
                            "Specificity" = cm_glm$byClass["Specificity"],
                            "F1" = cm_glm$byClass["F1"],
                            "False Neg. Rate" = 1-cm_glm$byClass["Sensitivity"],
                            "False Pos. Rate" = 1-cm_glm$byClass["Specificity"],
                            row.names = "Logistic Regression"), 2)

# Logistic Regression PCA
set.seed(1, sample.kind = "Rounding")

glmPCA <- train(train[-1], train$diagnosis,
                method = "glm", 
                trControl = fitControl,
                preProcess = c("center", "scale", "pca"))

glmPCA_pred <- predict(glmPCA, test[-1])

# Confusion Matrix
cm_glmPCA <- confusionMatrix(glmPCA_pred, test$diagnosis, positive = "M")

kable(cm_glmPCA$table, 
      caption = "Confusion Matrix for Logistic Regression (PCA)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Store results Logistic Regression PCA
glmPCA_res <- round(data.frame("Accuracy" = cm_glmPCA$overall["Accuracy"],
                               "Senstivity" = cm_glmPCA$byClass["Sensitivity"],
                               "Specificity" = cm_glmPCA$byClass["Specificity"],
                               "F1" = cm_glmPCA$byClass["F1"],
                               "False Neg. Rate" = 1-cm_glmPCA$byClass["Sensitivity"],
                               "False Pos. Rate" = 1-cm_glmPCA$byClass["Specificity"],
                               row.names = "Logistic Regression (PCA)"), 2)

# Decision Tree
set.seed(1, sample.kind = "Rounding")

fit.Control <- trainControl(method = "repeatedcv",
                            number = 10, ## 10-fold CV
                            repeats = 10,## repeated three times
                            # USE AUC
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE)

grid.Control <- expand.grid(maxdepth = 2:10)

tree <- train(train[-1], train$diagnosis,
              method = "rpart2",
              tuneGrid = grid.Control, 
              trControl = fit.Control,
              metric = "ROC")

# ROC vs Max Tree Depth
plot(tree)

# Plot Decision Tree
library(rattle)

fancyRpartPlot(tree$finalModel)

# Predict and Confusion Matrix Decision Tree
tree_pred <- predict(tree, test[-1])

# Confusion Matrix
cm_tree <- confusionMatrix(tree_pred, test$diagnosis, positive = "M")

kable(cm_tree$table, caption = "Confusion Matrix for Decision Tree") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Store results Decision Tree
tree_res <- round(data.frame("Accuracy" = cm_tree$overall["Accuracy"],
                             "Senstivity" = cm_tree$byClass["Sensitivity"],
                             "Specificity" = cm_tree$byClass["Specificity"],
                             "F1" = cm_tree$byClass["F1"],
                             "False Neg. Rate" = 1-cm_tree$byClass["Sensitivity"],
                             "False Pos. Rate" = 1-cm_tree$byClass["Specificity"],
                             row.names = "Decision Tree"), 2)

# Random Forest
set.seed(1, sample.kind = "Rounding")

rf <- train(train[-1], train$diagnosis,
            method = "rf",
            tuneGrid = data.frame(mtry = seq(3, 15, 2)),
            trControl = fitControl,
            importance = TRUE)

rf_pred <- predict(rf, test[-1])

# Confusion Matrix
cm_rf <- confusionMatrix(rf_pred, test$diagnosis, positive = "M")

kable(cm_rf$table, caption = "Confusion Matrix for Random Forest") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Store results Random Forest
rf_res <- round(data.frame("Accuracy" = cm_rf$overall["Accuracy"],
                           "Senstivity" = cm_rf$byClass["Sensitivity"],
                           "Specificity" = cm_rf$byClass["Specificity"],
                           "F1" = cm_rf$byClass["F1"],
                           "False Neg. Rate" = 1-cm_rf$byClass["Sensitivity"],
                           "False Pos. Rate" = 1-cm_rf$byClass["Specificity"],
                           row.names = "Random Forest"), 2)


# Combine results
final_res <- rbind(nb_res,
                   glm_res,
                   glmPCA_res,
                   tree_res,
                   rf_res)

kable(final_res, caption = "Final Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(5, bold = TRUE, color = "white", background = "#d88277")