#12
#setting up random forest for use with Ranger
rf_model <- rand_forest(mtry=tune(),
                        trees = 200) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")
#Creating Workflow
rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_formula(above_mv ~ .)
folds<- vfold_cv(train_data, v=5) #splits into 5 different fold

#Tuning Forest
set.seed(123)
rf_tuned <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = tibble(mtry = seq(from = 1, to = 13, by = 3)),
  metrics = metric_set(roc_auc)
)

#Looking at individual AUCs for later
rf_results <- rf_tuned %>%
  collect_metrics()
#make a QUALITY plot
ggplot(data=rf_results) + #not training
  geom_line(aes(x=mtry, y=mean))+
  labs(title = "AUC Performance Across mtry Values", x="mtry (Variables sampled per split)", y = "AUC")+
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 13, by = 3))
#AUC was maximized under m = 4 to AUC = 0.817
#Automating ^
best_params <- select_best(rf_tuned, metric = "roc_auc")

#Building final forest with optimal information
set.seed(123)
finalforest <- ranger(
  above_mv ~ .,
  data = train_data,
  num.trees = 200,
  mtry = best_params$mtry,
  importance = "impurity",
  probability = TRUE
)

#pi_hat using test_data
pi_hat <- predict(finalforest, test_data)$predictions[, "1"]
rocCurve <- roc(response = test_data$above_mv,
                predictor = pi_hat,
                levels = c("0","1"))
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)
#Automating optimal threshold
opt <- coords(rocCurve, "best", ret = "threshold")
opt <- as.numeric(opt)

#AUC = 0.818, which is alright, pi_star = 0.667, predict if threshold = 0.667, specificity(true negative) = 0.788, senstivity(true positive) = 0.710
#We correctly predict a house selling over market value 71% of the time, and under 79% of the time, within the training data.
rf_pred_class <- ifelse(pi_hat > opt, 1, 0)
table(Predicted = rf_pred_class, Actual = test_data$above_mv)
#When using the testing dataset, our true specificity is 78.8%, and true sensitivty is 70.9%, which lines up well with our training,
#and out accuracy is 74%, which is very good. Our false positive rates are 21.2%
