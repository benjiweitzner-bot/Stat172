#Ranger requires a different vi - format was obtained from ChatGPT
vi <- data.frame(
  Variable = names(finalforest$variable.importance),
  Importance = as.numeric(finalforest$variable.importance)
)
#Vi graph for executive
ggplot(vi, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Variable", y = "Impurity Decrease",title = "Random Forest Variables"
  ) +
  theme_bw()

#GLM Models:
m0 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end,
          data = train_data, family = binomial(link = "logit"))
BIC(m0)
#86386.12
m1 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + List.Year,
          data = train_data, family = binomial(link = "logit"))
BIC(m1)
#86255.92
m2 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + List.Year + PCE,
          data = train_data, family = binomial(link = "logit"))
BIC(m2)
#79723.73
m3 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + PCE + List.Year + GDP,
          data = train_data, family = binomial(link = "logit"))
BIC(m3)
#78721.74
m4 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + PCE + List.Year + GDP + NASDAQCOM,
          data = train_data, family = binomial(link = "logit"))
BIC(m4)
#76379.17
m5 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + PCE + List.Year + NASDAQCOM + GDP + Town,
          data = train_data, family = binomial(link = "logit"))
BIC(m5)
#75984.88
m6 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + PCE + List.Year + NASDAQCOM + Town + GDP + REITTMA,
          data = train_data, family = binomial(link = "logit"))
BIC(m6)
#74420.35
m7 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + PCE + List.Year + NASDAQCOM + Town + GDP + REITTMA + Remarks,
          data = train_data, family = binomial(link = "logit"))
BIC(m7)
#72981.78
m8 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + PCE + List.Year + NASDAQCOM + Town + GDP + UNRATE + Remarks
          +REITTMA,
          data = train_data, family = binomial(link = "logit"))
BIC(m8)
#72823.76
m9 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + PCE + List.Year + NASDAQCOM + Town + GDP + UNRATE + Remarks
          +REITTMA+Property.Type,
          data = train_data, family = binomial(link = "logit"))
BIC(m9)
#72277.18
m10 <- glm(above_mv ~ Market.Value + MVMTD027MNFRBDAL + date_q_end + PCE + List.Year + NASDAQCOM + Town + GDP + UNRATE + Remarks
           +REITTMA+Property.Type + Residential.Type,
           data = train_data, family = binomial(link = "logit"))
BIC(m10)
#72283.31
# BIC increased at m10, so I will keep m9 as my final model
beta <- coef(m9)
exp(beta)
#Roc Curve for best model
glm_pred_prob <- predict(m9, newdata = test_data, type = "response")

glm_roc <- roc(response = test_data$above_mv,
               predictor = glm_pred_prob,
               levels = c("0","1"))

plot(glm_roc,main = "AUC Curve for Logistic Regression Model m9", print.auc = TRUE)