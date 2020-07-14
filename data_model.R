#### PRIMARY FEATURES
## EXANG
## SEX
## OLD PEAK
## Age


#### SECONDARY FEATURES
## Thalach
## CP
## CA

library(rstanarm)
library(bayestestR)
library(ROCR)


## change sex to numeric
bayesData <- data %>% mutate(sex = as.factor(sex))


# Split Data
splitIndex <- caret::createDataPartition(bayesData$target, p = .7, list = FALSE)
trainData <- bayesData[splitIndex,]
testData <- bayesData[-splitIndex,]


# Using default weakly informative priors 
# No transformation to oldpeak
options(mc.cores = parallel::detectCores())
set.seed(456)
mlm <- stan_glmer(target ~ 1 + oldpeak + age + sex + (1|exang),
                  data = trainData,
                  family = binomial("logit"))


# evaluate
pp_check(mlm) # posterior simulations follow same distribution as observed data

plot(mlm, "trace") # We have convergence from all 4 chains

summary(mlm)



# Variable inference
r <- p_direction(mlm)
plot(r, effects = "all")


invlogit <- function(x) {
  exp(x)/(1+exp(x))
}


p_sim <- data.frame(mlm) %>% 
  rename(b0 = X.Intercept.,
         b0_exang0 = b..Intercept..exang.0.,
         b0_exang1 = b..Intercept..exang.1.) %>%
  mutate_at(vars(b0:b0_exang1), invlogit)


p_sim %>%
  ggplot(aes(x = b0)) +
  geom_histogram(fill = "navy blue", alpha = .6) +
  xlab("Intercept") +
  xlim(0,1) +
  theme_minimal()


p_sim %>%
  ggplot() +
  geom_histogram(aes(x = b0_exang0), fill = "navy blue", alpha = .6) +
  geom_histogram(aes(x = b0_exang1), fill = "orange", alpha = .6) +
  xlim(0,1) +
  theme_minimal()



p_sim %>%
  ggplot() +
  geom_histogram(aes(x = age), fill = "navy blue", alpha = .6) +
  xlim(0,1) +
  theme_minimal()

p_sim %>%
  ggplot() +
  geom_histogram(aes(x = sex1), fill = "orange", alpha = .6) +
  xlim(0,1) +
  theme_minimal()


# Prediction
pred <- posterior_predict(mlm, testData)
pred <- colMeans(pred, dims = 1)

table(testData$target, pred > .5)

TP = 35
TN = 29
FP = 16
FN = 10

# Accuracy
(TP + TN)/(TP+TN+FP+FN)



ROCpred <- prediction(pred, testData$targe)
ROCperf <- performance(ROCpred, 'tpr','fpr')
plot(ROCperf, colorize = TRUE, text.adj = c(-.2,1.7))



#### Add Thalach ####
updated <- update(mlm, ~. + thalach)

pp_check(updated)

plot(updated, "trace")

summary(updated)

## Inference
p_sim_update <- data.frame(updated) %>% 
  rename(b0 = X.Intercept.,
         b0_exang0 = b..Intercept..exang.0.,
         b0_exang1 = b..Intercept..exang.1.) %>%
  mutate_at(vars(b0:b0_exang1), invlogit)


p_sim_update %>%
  ggplot(aes(x = b0)) +
  geom_histogram(fill = "navy blue", alpha = .6) +
  xlab("Intercept") +
  xlim(0,1) +
  theme_minimal()


p_sim_update %>%
  ggplot() +
  geom_histogram(aes(x = b0_exang0), fill = "navy blue", alpha = .6) +
  geom_histogram(aes(x = b0_exang1), fill = "orange", alpha = .6) +
  xlim(0,1) +
  theme_minimal()



p_sim_update %>%
  ggplot() +
  geom_histogram(aes(x = thalach), fill = "navy blue", alpha = .6) +
  geom_histogram(aes(x = sex1), fill = "orange", alpha = .6) +
  xlim(0,1) +
  theme_minimal()


# Prediction
pred_update <- posterior_predict(updated, testData)
pred_update <- colMeans(pred_update, dims = 1)

table(testData$target, pred_update > .5)

TP = 39
TN = 30
FP = 15
FN = 6

# Accuracy
(TP + TN)/(TP+TN+FP+FN)



# *** Accuracy was included as part of the project, but Bayesian inference often doesn't need train/test splits 
# *** which is one of the advantages when working with smaller data sets



