knitr::opts_chunk$set(
  echo = FALSE, warning = FALSE, error = FALSE, message = FALSE,
  tidy.opts = list(width.cutoff = 55)
)

# libraries

# If the package is not installed then it will be installed
if (!require("rpart")) install.packages("rpart")
if (!require("gbm")) install.packages("gbm")
if (!require("readr")) install.packages("readr")
if (!require("knitr")) install.packages("knitr")
if (!require("caret")) install.packages("caret")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("ggplot2")) install.packages("ggplot2")

require("readr")
require("tidyverse")
require("caret")
require("gridExtra")
require("ggplot2")
require("gbm")

# data parsing
soldat <- read_csv("soldat.csv")
soldat$y <- factor(soldat$y, levels = c(-1, 1), labels = c("insoluble", "soluble"))
n <- nrow(soldat)
p_start <- ncol(soldat)

##################################
#  Exploratory data analysis
#################################

# class frequency
table(soldat$y)


# predictors variance
bxp1 <- soldat %>%
  select(-y) %>%
  select(num_range("x", c(1:18))) %>%
  stack() %>%
  ggplot(aes(x = ind, y = values)) +
  geom_boxplot() +
  xlab("")

bxp2 <- soldat %>%
  select(-y) %>%
  select(num_range("x", c(19:34))) %>%
  stack() %>%
  ggplot(aes(x = ind, y = values)) +
  geom_boxplot() +
  xlab("")

bxp3 <- soldat %>%
  select(-y) %>%
  select(num_range("x", c(35:50))) %>%
  stack() %>%
  ggplot(aes(x = ind, y = values)) +
  geom_boxplot() +
  xlab("")

bxp4 <- soldat %>%
  select(-y) %>%
  select(num_range("x", c(55:72))) %>%
  stack() %>%
  ggplot(aes(x = ind, y = values)) +
  geom_boxplot() +
  xlab("")

grid.arrange(bxp1, bxp2, bxp3, bxp4, ncol = 2)


# predictors granularity / number of unique values
nb_unique_val <- function(x) {
  return(length(unique(x)))
}

nb_unique_val_pred <- tibble(nb = sapply(soldat %>% select(-y), nb_unique_val), predictor = colnames(soldat)[colnames(soldat) != "y"])

ggplot(nb_unique_val_pred) +
  geom_histogram(aes(x = nb), col = "grey", bins = 100) +
  geom_vline(xintercept = 300, col = "red") +
  xlab("Number of unique values")


# predictors correlation heatmap
v <- cor(soldat %>% select(-y), use = "pairwise.complete.obs")
col <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(v, col = col, symm = TRUE, Colv = "Rowv")

# filtering of highly correlated predictors
high_corr <- colnames(soldat)[findCorrelation(v, cutoff = 0.95)]
soldat <- soldat %>% select(-all_of(high_corr))

# covariation of outcome and selected predictors
p <- list()
i <- 1
for (v in c("x35", "x36", "x37", "x41")) {
  p[[i]] <- ggplot(data = soldat) +
    geom_boxplot(aes_string(x = "y", y = v))
  i <- i + 1
}
do.call(grid.arrange, list(grobs = p, ncol = 2))

# number of missing data
cat("Missing data per predictor\n")
apply(is.na(soldat), 2, sum)

##################################
#  data partitioning
#################################

set.seed(1234)
inTest <- createDataPartition(soldat$y, p = 0.5, list = FALSE)[, 1]
test <- soldat[inTest, ]
training <- soldat[-inTest, ]

# check for balance training and test sets
cat("Training set class distribution\n")
round(table(training[, "y"]) / nrow(training), 2)
cat("Test set class distribution\n")
round(table(test[, "y"]) / nrow(test), 2)

##################################
#  CART model
#################################

# fitting
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
CART <- train(y ~ ., data = training, method = "rpart1SE", trControl = ctrl, metric = "ROC")

# training error
pred_train_CART <- predict.train(CART, type = "raw")
confusionMatrix(data = pred_train_CART, reference = training$y)

# test error
pred_CART <- predict(CART, newdata = test)

confusionMatrix(data = pred_CART, reference = test$y)

df <- data.frame(obs = test$y, pred = pred_CART, predict(CART, newdata = test, type = "prob"))

## ##################################
## #  Random forest
## #################################
## 
## # fitting (not evaluated inside R markdown but in seperate colab 'Parallel Random Forest')
## control <- trainControl(
##   method = "repeatedcv",
##   number = 5,
##   repeats = 3,
##   search = "grid",
##   classProbs = TRUE,
##   allowParallel = TRUE,
##   summaryFunction = twoClassSummary
## )
## 
## metric <- "ROC"
## tunegrid <- expand.grid(mtry = seq(2, 45, 5)) # then seq(2,12,2)
## rf_models <- list()
## 
## 
## i <- 1
## for (ntree in c(1000, 1500, 2000, 2500)) {
##   cat(paste0("Training with ", ntree, " trees ..."))
##   rf_models[[i]] <- train(y ~ .,
##     data = training,
##     method = "rf",
##     metric = metric,
##     tuneGrid = tunegrid,
##     trControl = control,
##     ntree = ntree
##   )
##   i <- i + 1
## }
## save(rf_models, file = "rf_models.Rdata")

# loading 'Parallel random forest' output
load("rf_models.Rdata")

# parameters turning
n_trees <- c(1000, 1500, 2000, 2500)
p <- list()
for (i in 1:4) {
  p[[i]] <- ggplot(rf_models[[i]]) +
    ggtitle(paste("N. trees:", n_trees[i])) +
    theme(text = element_text(size = 10))
}
do.call(grid.arrange, list(grobs = p, ncol = 2))

df <- rbind(
  rf_models[[1]]$results[, 1:2],
  rf_models[[2]]$results[, 1:2],
  rf_models[[3]]$results[, 1:2],
  rf_models[[4]]$results[, 1:2]
)
df$n_trees <- c(rep(1000, 6), rep(1500, 6), rep(2000, 6), rep(2500, 6))

df <- df[, c(1, 3, 2)] %>%
  arrange(-ROC) %>%
  slice(1:10)
kable(df,
  digits = c(0, 0, 4),
  col.names = c("mtry", "Number of trees", "ROC"),
  align = "c",
  caption = "Top 10 pairs of tuning parameters"
)

RF <- rf_models[[2]]

# training error
pred_train_RF <- predict.train(RF, type = "raw")
confusionMatrix(data = pred_train_RF, reference = training$y)

# test error
pred_RF <- predict(RF, newdata = test)

confusionMatrix(data = pred_RF, reference = test$y)

df <- data.frame(obs = test$y, pred = pred_RF, predict(RF, newdata = test, type = "prob"))

# variable importance
varImp(RF)

##################################
#  Comparison CART and random forest
#################################

# boxplot performance on training set
resamps <- resamples(list(
  SingleTree = CART,
  RandomForest = RF
))

theme1 <- trellis.par.get()
theme1$plot.symbol$col <- rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch <- 16
theme1$plot.line$col <- rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))

# confidence intervals for performance metrics on training set
metric <- c("ROC", "Spec", "Sens")
p <- list()
for (i in 1:3) {
  p[[i]] <- dotplot(resamps, metric = metric[i])
}
do.call(grid.arrange, list(grobs = p, ncol = 3))

# pvalue for difference in perf. metrics on training set

diff_rf <- diff(resamps)
summary(diff_rf)

## ##################################
## #  Adaboost with stumps
## #################################
## 
## # fitting (not evaluated inside R markdown but in seperate colab 'Parallel Boosting Trees')
## control <- trainControl(
##   method = "repeatedcv",
##   number = 5,
##   repeats = 3,
##   returnResamp = "all",
##   classProbs = TRUE
## )
## 
## metric <- "Accuracy"
## 
## grid <- expand.grid(
##   .interaction.depth = 1,
##   .n.trees = c(
##     1, 10, 100, 250, 500, 750, 1000, 1250, 1500,
##     1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500
##   ),
##   .shrinkage = c(.0001, .001, .01, .1, 1),
##   .n.minobsinnode = 10
## )
## 
## stumps_2000_model <- train(y ~ .,
##   data = training,
##   method = "gbm",
##   bag.fraction = 0.5,
##   distribution = "adaboost",
##   trControl = control,
##   tuneGrid = grid,
##   verbose = FALSE,
##   metric = metric
## )
## 
## save(stumps_2000_model, file = "stumps_2000_model.Rdata")

# loading output of 'Parallel Boosting Trees'
load("stumps_2000_model.Rdata")

# tuning parameter grid search plot
ggplot(stumps_2000_model)

# training missclassification rate as a function of n.trees
df_res_ada_train <- stumps_2000_model$results %>% filter(shrinkage == 0.1)
df_res_ada_train$missclassification <- 1 - df_res_ada_train$Accuracy

ggplot(df_res_ada_train) +
  geom_line(aes(x = n.trees, y = missclassification)) +
  geom_point(aes(x = n.trees, y = missclassification)) +
  xlab("# Boosting Iterations") +
  ylab("Missclassification rate (Cross-Validation")

## # test missclassification rate as a function of n.trees
## # not evaluated in the Rmarkdown
## boosting_iter <- c(1, 10, 100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500)
## miss_rate <- c()
## for (i in 1:length(boosting_iter)) {
##   model <- update(stumps_2000_model, list(
##     interaction.depth = 1,
##     n.trees = boosting_iter[i],
##     shrinkage = .1,
##     n.minobsinnode = 10
##   ))
##   pred_adaboost <- predict(model, newdata = test)
##   miss_rate <- c(miss_rate, mean(pred_adaboost != test$y))
## }
## 
## df_res_ada_test <- data.frame(n.trees = boosting_iter, missclassification = miss_rate)
## 
## df_res_ada <- rbind(
##   df_res_ada_train[, c("n.trees", "missclassification")],
##   df_res_ada_test
## )
## df_res_ada$Set <- c(
##   rep("Training", nrow(df_res_ada_train), ),
##   rep("Test", nrow(df_res_ada_test))
## )
## save(df_res_ada, file = "df_res_ada.Rdata")

load("df_res_ada.Rdata")
ggplot(df_res_ada) +
  geom_line(aes(x = n.trees, y = missclassification, col = Set)) +
  geom_point(aes(x = n.trees, y = missclassification, col = Set, pch = Set)) +
  xlab("# Boosting Iterations") +
  ylab("Missclassification rate (Cross-Validation")

## #### Train and test missclassification rates as a function of n.trees with gbm.more
## 
## # not evaluated in Rmarkdown
## 
## # recodifying y for gbm adaboost
## train_gbm <- training
## train_gbm$y <- as.numeric(training$y)
## train_gbm <- train_gbm %>% mutate(y = case_when(y == 2 ~ 0, TRUE ~ 1))
## 
## test_gbm <- test
## test_gbm$y <- as.numeric(test$y)
## test_gbm <- test_gbm %>% mutate(y = case_when(y == 2 ~ 0, TRUE ~ 1))
## 
## # first gbm with 10 iterations
## b_start <- 10
## ada <- gbm(y ~ .,
##   distribution = "adaboost", data = train_gbm,
##   n.trees = 10, interaction.depth = 1, n.minobsinnode = 10,
##   shrinkage = 0.1, cv.folds = 15
## )
## pred_ada_test <- as.numeric(predict.gbm(ada, newdata = test_gbm, n.trees = ada$n.trees) > 0.5)
## miss_rate_test <- mean(pred_ada_test != test_gbm$y)
## 
## pred_ada_train <- as.numeric(predict.gbm(ada, newdata = train_gbm, n.trees = ada$n.trees) > 0.5)
## miss_rate_train <- mean(pred_ada_train != train_gbm$y)
## 
## # updating gbm with more iterations
## for (diff_b in diff(boosting_iter)[-1]) {
##   ada <- gbm.more(ada, n.new.trees = diff_b, data = train_gbm)
##   pred_ada_test <- as.numeric(predict.gbm(ada, newdata = test_gbm, n.trees = ada$n.trees) > 0.5)
##   miss_rate_test <- c(miss_rate_test, mean(pred_ada_test != test_gbm$y))
## 
##   pred_ada_train <- as.numeric(predict.gbm(ada, newdata = train_gbm, n.trees = ada$n.trees) > 0.5)
##   miss_rate_train <- c(miss_rate_train, mean(pred_ada_train != train_gbm$y))
## }
## 
## # data frame of missclassification rate
## df <- data.frame(
##   n.trees = rep(boosting_iter[-1], 2),
##   Set = c(rep("Training", length(boosting_iter[-1])), rep("Test", length(boosting_iter[-1]))),
##   missclassification = c(miss_rate_train, miss_rate_test)
## )
## save(df, file = "df_res_ada_incre.Rdata")

load("df_res_ada_incre.Rdata")
# plot
ggplot(df) +
  geom_line(aes(x = n.trees, y = missclassification, col = Set)) +
  geom_point(aes(x = n.trees, y = missclassification, col = Set, pch = Set)) +
  xlab("# Boosting Iterations") +
  ylab("Missclassification rate (Cross-Validation")

## ##################################
## #  Adaboost with larger interaction depth
## #################################
## 
## # fitting (not evaluated inside R markdown but in seperate colab 'Parallel Boosting Trees')
## grid <- expand.grid(
##   .interaction.depth = c(1, 4, 8, 16),
##   .n.trees = c(
##     1, 10, 100, 250, 500, 750, 1000, 1250, 1500,
##     1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500
##   ),
##   .shrinkage = c(.0001, .001, .01, .1, 1),
##   .n.minobsinnode = 10
## )
## 
## multi_lvl_model <- train(y ~ .,
##   data = training,
##   method = "gbm",
##   bag.fraction = 0.5,
##   distribution = "adaboost",
##   trControl = control,
##   tuneGrid = grid,
##   verbose = FALSE,
##   metric = metric
## )
## 
## multi_lvl_model
## 
## save(multi_lvl_model, file = "multi_lvl_model.Rdata")

# loading output from 'Parallel Boosting Trees'
load("multi_lvl_model.Rdata")

# tuning parameters grid search plot
ggplot(multi_lvl_model)

## # test missclassification rate as a function of n.trees
## # not evaluated in the Rmarkdown
## boosting_iter <- c(
##   1, 10, 100, 250, 500, 750, 1000, 1250, 1500,
##   1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500
## )
## depth <- c(1, 4, 8, 16)
## miss_rate <- c()
## depth_ord <- c()
## boosting_iter_ord <- c()
## 
## for (i in 1:length(boosting_iter)) {
##   for (j in 1:length(depth)) {
##     model <- update(multi_lvl_model, list(
##       interaction.depth = depth[j],
##       n.trees = boosting_iter[i],
##       shrinkage = .01,
##       n.minobsinnode = 10
##     ))
##     pred_adaboost <- predict(model, newdata = test)
##     miss_rate <- c(miss_rate, mean(pred_adaboost != test$y))
##     boosting_iter_ord <- c(boosting_iter_ord, boosting_iter[i])
##     depth_ord <- c(depth_ord, depth[j])
##   }
## }
## 
## df_res_ada_test_depth <- data.frame(
##   n.trees = boosting_iter_ord,
##   missclassification = miss_rate,
##   depth = depth_ord
## )
## 
## df_res_ada_test_depth$depth <- as.factor(df_res_ada_test_depth$depth)
## save(df_res_ada_test_depth, file = "df_res_ada_test_depth.Rdata")

load("df_res_ada_test_depth.Rdata")

ggplot(df_res_ada_test_depth) +
  geom_line(aes(x = n.trees, y = missclassification, col = depth)) +
  geom_point(aes(x = n.trees, y = missclassification, col = depth, pch = depth)) +
  xlab("# Boosting Iterations") +
  ylab("Missclassification rate (Cross-Validation") +
  labs(col = "Depth", pch = "Depth")

## NA
