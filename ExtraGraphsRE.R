df_bar <- dre %>%
  group_by(Remarks) %>%
  summarise(MeanMarketValue = mean(Market.Value, na.rm = TRUE))

ggplot(df_bar, aes(x = factor(Remarks), y = MeanMarketValue)) +
  geom_col(fill = "grey") +
  labs(
    title = "Average Market Value by Assessor Remarks",
    x = "Remarks (0 = No, 1 = Yes)",
    y = "Average Market Value"
  ) +
  theme_bw()

hdre <- dre
head(hdre)
hart <- hdre %>%
  filter(Town == "Hartford")

ggplot(hart, aes(x = above_mv)) +
  geom_bar(fill = "grey") +
  labs(
    title = "Distribution of Hartford Properties",
    x = "Above Market Value Count",
    y = "Count"
  ) +
  theme_bw()

glm_probs <- predict(m9, test_data, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
table(Predicted = glm_pred, Actual = test_data$above_mv)

lasso_class <- ifelse(test.df.preds$lasso_pred > 0.5, 1, 0)
ridge_class <- ifelse(test.df.preds$ridge_pred > 0.5, 1, 0)

table(Predicted = lasso_class, Actual = y.test)
table(Predicted = ridge_class, Actual = y.test)