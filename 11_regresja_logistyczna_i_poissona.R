# Zajêcia 11 ####

load(url("http://ls.home.amu.edu.pl/data_sets/liver_data.RData"))

# zadanie 1 ---------------------------------------------------------------
# 1.
model_1 <- glm(
  formula=condition ~ bilirubin + ldh, 
  family = "binomial", 
  data=liver_data
)
model_1

# 2.
summary(model_1)

# 3.
step(model_1)

# 4.
bilirubin <- exp(coef(model_1[1]))[2]

ldh <- exp(coef(model_1[1]))[3]

# 5.
library(ROCR)
pred_1 <- prediction(
  model_1$fitted.values, 
  liver_data$condition
)

plot(
  performance(
    pred_1, 
    'tpr', 
    'fpr', 
    main="Model 1"
  )
)

# auc
performance(pred_1, 'auc')@y.values

# 6.

liver_data_new <- data.frame(
  bilirubin = c(0.9, 2.1, 3.4),
  ldh = c(100, 200, 300)
)

predict_glm <- predict(
  model_1, 
  liver_data_new,
  type = 'response'
)

model_1_hat <- coef(model_1)[1] + coef(model_1)[2] * liver_data$bilirubin + coef(model_1)[3] * liver_data$ldh
model_1_temp <- seq(min(model_1_hat) - 1, max(model_1_hat) + 2.5, length.out = 100)
condition_temp <- exp(model_1_temp) / (1 + exp(model_1_temp))

plot(
  model_1_temp, 
  condition_temp, 
  type = "l", 
  xlab = "X beta", 
  ylab = "condition", 
  xlim = c(-6, 9), 
  ylim = c(-0.1, 1.1)
)

# górne czarne kropki
points(
  model_1_hat, 
  liver_data$condition, 
  pch = 16
)

# czerwone kropki
points(
  coef(model_1)[1] + coef(model_1)[2] * liver_data_new$bilirubin + coef(model_1)[3] * liver_data_new$ldh, 
  predict_glm, 
  pch = 16, 
  col = "red"
)

# 7.
# 7.1
# 7.2
# 7.3
# 7.4
# 7.5
# 7.6

# zadanie 2 ---------------------------------------------------------------
# 1.

# 2.

# 3.

# 4.

# 5.
# 5.1
# 5.2
# 5.3
# 5.4
