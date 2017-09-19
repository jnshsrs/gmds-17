# model creation 
# evaluation of
# pedis-assesment

library(magick)
library(tidyverse)
library(caret)
library(dplyr)
library(RSQLite)
library(pROC)
library(officer)
library(Epi)

# read data
con <- RSQLite::dbConnect(SQLite(), "../pedis-algorithms/pedis-app-database.sqlite")
data <- RSQLite::dbReadTable(con, "tbl_pedis") %>% tbl_df
data$amputation <- data$amputation_major
data <- data %>% rename(beobachtungsintervall = endpoint_factor)
dbDisconnect(con)

# Are the samples independent?
data %>%
  ggplot(aes(x = id, y = pedis_log)) +
  geom_line() +
  geom_smooth() +
  scale_y_continuous(limits = c(0, 20))

# Follow Up Status of all patients in database
# How many patients reached the endpoint
data %>% 
  count(beobachtungsintervall)

# filter patients that reached endpoint 
data <- data %>%
  filter(!grepl(x = beobachtungsintervall, pattern = "reached")) %>%
  filter(!is.na(amputation))

data %>%
  select(amputation, beobachtungsintervall) %>%
  group_by(amputation) %>%
  tally

# Plot pedis score as a Function of Amputation
data %>%
  ggplot(aes(x = pedis_log, fill = amputation)) +
  geom_bar(position = "stack")

# descriptive analysis
data %>% 
  group_by(amputation_major) %>% 
  summarise(mean_pedis = mean(pedis), sd_pedis = sd(pedis))

data %>% count(geschlecht)

# modeling
# logistic regression with caret

X <- data %>% select(amputation, p,e,d,i,s, alter, crp, lokalisation, dialyse) 
X <- data %>% select(amputation, p,e,d,i, alter, crp, lokalisation) 

options(na.action = na.pass)
pre_processing_parameters <- preProcess(X, method = "medianImpute", "center", "scale")
X <- predict(pre_processing_parameters, newdata = X)
y <- X$amputation
x <- model.matrix(amputation ~., data = X, na.action = na.pass)

# Define training process of the model
train_control <- trainControl(
  summaryFunction = twoClassSummary,
  method = "cv",
  number = 5,
  classProbs = TRUE,
  returnData = TRUE)

# Create the tuning grid with hyperparameter for LASSO Regularization

tune_grid <- expand.grid(alpha = seq(0, 1, .1), lambda = seq(0, 10, length.out = 15))

# Model Training
set.seed(1123)
model <- train(x = x, 
               y = y, 
               method = "glmnet", 
               family = "binomial", 
               preProcess = c("zv", "center", "scale"),
               metric = "ROC", 
               trControl = train_control,  
               tuneGrid = tune_grid)

plot(model)

final_model <- model$finalModel
plot(final_model)

# Visualize Hyperparamter
ggplot(data = model) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, .1)) +
  theme_minimal()

# Predict Values
df_predictors <- predict(model, type = "prob") %>% 
  tbl_df %>% 
  select(yes) %>% 
  rename(predictor = yes) %>% 
  mutate(response = y) %>% 
  mutate(predictor = round(predictor, 3))

# Visualize Riskscore vs. reference status
df_predictors %>% 
  ggplot(aes(response, predictor)) +
  geom_point(position = position_jitter(width = .1)) +
  theme_minimal()

roc_eval <- pROC::roc(response = df_predictors$response, predictor = df_predictors$predictor)
auc_eval <- pROC::auc(roc = roc_eval)


# predict class
df_predictors <- df_predictors %>% 
  mutate(predicted_class = if_else(predict(model, type = "prob")[2] < .15, "no", "yes"))

conf_matr_risk_model <- confusionMatrix(df_predictors$response, df_predictors$predicted_class, mode = "everything")

# PEDIS Skala
df_predictors <- df_predictors %>% mutate(pedis = data %>% pull(pedis))
roc_pedis <- pROC::roc(response ~ pedis, data = df_predictors)
auc_pedis <- pROC::auc(response ~ pedis, data = df_predictors)
print(auc_pedis)
ci.auc(roc_pedis)

# plot both AUC Curves

pROC::ggroc(list(Sumscore = roc_pedis)) + coord_equal() + theme_minimal() +
  scale_color_manual(values = "blue", "Models") +
  scale_x_continuous("Specificity", trans = "reverse") +
  scale_y_continuous("Sensitivity") +
  scale_linetype_manual(values = 2) +
  theme(legend.title.align = .5) +
  geom_abline(slope = 1, intercept = 1, alpha = .4, linetype = 2) +
  annotate(geom = "text", x = .65, y = .75, label = "AUC = .90", col = "blue")  +
  ggsave(filename = "./img/plot-auc-sumscore.png", width = unit(6, "cm"), height = unit(4, "cm"), dpi = 600)

pROC::ggroc(list(Riskscore = roc_eval, Sumscore = roc_pedis)) + coord_equal() + theme_minimal() +
  scale_color_manual(values = c("red", "blue"), "Models") +
  scale_x_continuous("Specificity", trans = "reverse") +
  scale_y_continuous("Sensitivity") +
  scale_linetype_manual(values = 2) +
  theme(legend.title.align = .5) +
  geom_abline(slope = 1, intercept = 1, alpha = .4, linetype = 2) +
  annotate(geom = "text", x = .65, y = .985, label = "AUC = .93", col = "red") +
  annotate(geom = "text", x = .65, y = .75, label = "AUC = .90", col = "blue")  +
  ggsave(filename = "./img/plot-auc-both-models.png", width = unit(6, "cm"), height = unit(4, "cm"), dpi = 600)

# post editing plots 
list("./img/plot-auc-both-models.png", "./img/plot-auc-sumscore.png") %>% 
  tibble(path = .) %>% 
  mutate(image = path %>% map(.f = image_read)) %>% 
  mutate(image = image %>% map(image_trim)) %>% 
  pwalk(.f = image_write)

# plot riskscore vs response 
df_predictors %>% 
  ggplot(aes(x = response, pedis)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = .1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  geom_hline(yintercept = 27, linetype = 3) +
  theme_minimal()
