library(ggplot2)
library(corrplot)
library(tidyr)

### View all numeric variables correlation 
corr <- round(cor(data %>% select_if(is.numeric)),1)
cor <- round(cor(dataRaw),1) # visualize all variables as numeric if needed
corrplot(cor, method = "number")
corrplot(corr, method = "number")


### oldpeak, ca, age, slope and thalach seem to be the best predictors with minimal collinearity


### View all factor variables with target heart attack
data %>%
  ggplot(aes(x = cp, y = target)) +
  geom_bar(stat = "summary", fill = 'light blue', color = "navy blue") +
  theme_bw()


data %>%
  ggplot(aes(x = sex, y = target)) +
  geom_bar(stat = "summary", fill = 'light blue', color = "navy blue") +
  theme_bw()

data %>%
  ggplot(aes(x = fbs, y = target)) +
  geom_bar(stat = "summary", fill = 'light blue', color = "navy blue") +
  theme_bw()
## ------- fbs shows no sign of influence on heart attack

data %>%
  ggplot(aes(x = restecg, y = target)) +
  geom_bar(stat = "summary", fill = 'light blue', color = "navy blue") +
  theme_bw()
## ------ leave restecg out of first model


data %>%
  ggplot(aes(x = exang, y = target)) +
  geom_bar(stat = "summary", fill = 'light blue', color = "navy blue") +
  theme_bw()

data %>%
  ggplot(aes(x = thal, y = target)) +
  geom_bar(stat = "summary", fill = 'light blue', color = "navy blue") +
  theme_bw()
### leave thal out of first model




### Features left:
## oldpeak
## age
## slope
## ca
## thalach
## sex
## cp
## exang


#### Now visualize continuous variables relationship to categorical


### SEX
data %>%
  ggplot(aes(x = sex, y = oldpeak)) +
  geom_bar(stat = "summary", fill = "firebrick1", color = "black") +
  theme_minimal()

data %>%
  ggplot(aes(x = sex, y = ca)) +
  geom_bar(stat = "summary", fill = "firebrick1", color = "black") +
  theme_minimal()
## ------ Sex may capture ca values

data %>%
  ggplot(aes(x = sex, y = thalach)) +
  geom_bar(stat = "summary", fill = "firebrick1", color = "black") +
  theme_minimal()




### CP
data %>%
  ggplot(aes(x = cp, y = oldpeak)) +
  geom_bar(stat = "summary", fill = "firebrick1", color = "black") +
  theme_minimal()
## ------- cp may be captured by oldpeak


data %>%
  ggplot(aes(x = cp, y = ca)) +
  geom_bar(stat = "summary", fill = "firebrick1", color = "black") +
  theme_minimal()
## ------- cp may be captured by ca

data %>%
  ggplot(aes(x = cp, y = thalach)) +
  geom_bar(stat = "summary", fill = "firebrick1", color = "black") +
  theme_minimal()


### EXANG
data %>%
  ggplot(aes(x = exang, y = oldpeak)) +
  geom_bar(stat = "summary", fill = "firebrick1", color = "black") +
  theme_minimal()
## ------- exang may be captured by oldpeak

data %>%
  ggplot(aes(x = exang, y = ca)) +
  geom_bar(stat = "summary", fill = "firebrick1", color = "black") +
  theme_minimal()


data %>%
  ggplot(aes(x = exang, y = thalach)) +
  geom_bar(stat = "summary", fill = "firebrick1", color = "black") +
  theme_minimal()



### Thalach is captured in no categorical variable, so it needs to stay
### Exang was significant in predicting target and looks to be related to old peak so leave peak out
### Sex captures ca so leave ca out
### Possible CP is captured in exang and thalach so leave it out initially


#### PRIMARY FEATURES
## EXANG
## AGE
## SEX
## OLD PEAK


#### SECONDARY FEATURES
## THALCH
## CP
## CA



# View Distributions
data %>%
  ggplot(aes(x = thalach)) +
  geom_histogram(fill = "red", alpha = .6) +
  theme_minimal()


data %>%
  ggplot(aes(x = oldpeak)) +
  geom_histogram(fill = "red", alpha = .6) +
  theme_minimal()
# ------- may need to transform old peak


data %>%
  ggplot(aes(x = age)) +
  geom_histogram(fill = "red", alpha = .6) +
  theme_minimal()

data %>%
  ggplot(aes(x = cp)) +
  geom_histogram(fill = "red", alpha = .6) +
  theme_minimal()



