# 1. Create x and y for glmnet
x.train <- model.matrix(above_mv ~ ., data = train_data)[,-1]  # remove intercept
y.train <- as.vector(as.numeric(as.character(train_data$above_mv)))  # vectorized
x.test  <- model.matrix(above_mv ~ ., data = test_data)[,-1]
y.test  <- as.vector(as.numeric(as.character(test_data$above_mv)))

# 2. Fit cross-validated LASSO and Ridge logistic regression
set.seed(123)
lr_lasso_cv <- cv.glmnet(x.train, y.train, family = "binomial", alpha = 1)
set.seed(123)
lr_ridge_cv <- cv.glmnet(x.train, y.train, family = "binomial", alpha = 0)

# 3. Plot cross-validation results
plot(lr_lasso_cv, main = "Cross Validation Curve Lasso Penalized Regression")
plot(lr_ridge_cv,, main = "Cross Validation Curve Ridge Penalized Regression")

# 4. Extract best lambda
best_lasso_lambda <- lr_lasso_cv$lambda.min
best_ridge_lambda <- lr_ridge_cv$lambda.min

# 5. Coefficients from best models
lr_lasso_coefs <- coef(lr_lasso_cv, s = best_lasso_lambda)
lr_ridge_coefs <- coef(lr_ridge_cv, s = best_ridge_lambda)

# 6. Fit final models at best lambda
final_lasso <- glmnet(x.train, y.train, family = "binomial", alpha = 1, lambda = best_lasso_lambda)
final_ridge <- glmnet(x.train, y.train, family = "binomial", alpha = 0, lambda = best_ridge_lambda)

# 7. Predict on test set
test.df.preds <- test_data %>%
  mutate(
    lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
    ridge_pred = predict(final_ridge, x.test, type = "response")[,1]
  )

# 8. ROC curves
lasso_rocCurve <- roc(response = y.test, predictor = test.df.preds$lasso_pred)
ridge_rocCurve <- roc(response = y.test, predictor = test.df.preds$ridge_pred)

# 9. Prepare data for plotting
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = as.numeric(lasso_rocCurve$auc)
)

ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = as.numeric(ridge_rocCurve$auc)
)

roc_data <- rbind(lasso_data, ridge_data)

# 10. Plot ROC curves
ggplot(roc_data) +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model)) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1),
            aes(x = 0.75, y = c(0.65, 0.55), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(title = "Lasso and Ridge AUC Curves",x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()