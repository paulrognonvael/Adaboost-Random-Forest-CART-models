control <- trainControl(method = "cv", 
                        number = 5, 
                        returnResamp = "all",
                        classProbs = TRUE)

metric <- "Accuracy"

grid <- expand.grid(.interaction.depth = 1, 
                    .n.trees = c(1, 10, 100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000), 
                    .shrinkage = c(.0001,.001,.01,.1,1),  
                    .n.minobsinnode = 10
                    )

stumps_2000_model <- train(y ~. , 
                           data = training, 
                           method = "gbm", 
                           bag.fraction = 0.5,
                           distribution = "adaboost",
                           trControl = control,
                           tuneGrid = grid,
                           verbose = FALSE,
                           metric = metric)

stumps_2000_model

save(stumps_2000_model,file="stumps_2000_model.Rdata")

################################################################################


control <- trainControl(method = "cv", 
                        number = 5, 
                        returnResamp = "all",
                        classProbs = TRUE)

metric <- "Accuracy"

grid <- expand.grid(.interaction.depth = c(1,4,8,16), 
                    .n.trees = c(1, 10, 100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000), 
                    .shrinkage = c(.0001,.001,.01,.1,1),  
                    .n.minobsinnode = 10
)

multi_lvl_model <- train(y ~. , 
                         data = training, 
                         method = "gbm", 
                         bag.fraction = 0.5,
                         distribution = "adaboost",
                         trControl = control,
                         tuneGrid = grid,
                         verbose = FALSE,
                         metric = metric)

multi_lvl_model

save(multi_lvl_model,file="multi_lvl_model.Rdata")

