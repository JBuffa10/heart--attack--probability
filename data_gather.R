library(dplyr)
dataRaw <- read.csv("heart_attack_data.csv")


# view
head(dataRaw)


# dimensions
dim(dataRaw)

### 303 observations and 14 features


# summary and data types
summary(dataRaw)
str(dataRaw)


### Change sex, cp, fbs, restecg, exang, thal to factors
data <-
  dataRaw %>%
  rename(age = ï..age) %>%
  mutate_at(vars(sex, cp, fbs, restecg, exang, thal), as.factor)
