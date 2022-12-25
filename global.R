rm(list = ls())

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(wesanderson)
library(corrplot)
library(caret)
library(ROCR)
library(equatiomatic)

df <- read.csv('dataset.csv')
# remove outlier values
df_clean <- df %>% filter(df$gender != 'Other')
df_clean = df_clean %>% filter(df_clean$bmi != 'N/A')

df_clean = na.omit(df_clean)

df_copy <- df_clean
# convert chr to factors
df_copy$gender = as.factor(df_copy$gender)
df_copy$ever_married = as.factor(df_copy$ever_married)
df_copy$work_type = as.factor(df_copy$work_type)
df_copy$Residence_type = as.factor(df_copy$Residence_type)
df_copy$smoking_status = as.factor(df_copy$smoking_status)
df_copy$bmi = as.numeric(df_copy$bmi)


#Modal
#Data preprocessing
df_num <- data.frame(df_copy)
df_num$ever_married = str_replace_all(df_num$ever_married, c("Yes" = "1", "No" =
                                                               "0"))
df_num$ever_married = as.numeric(df_num$ever_married)

df_num$gender = str_replace_all(df_num$gender, c("Male" = "1", "Female" =
                                                   "2"))
df_num$gender = as.numeric(df_num$gender)

df_num$work_type = str_replace_all(
  df_num$work_type,
  c(
    "Never_worked" = "0",
    "children" = "1",
    "Private" = "2",
    "Self-employed" = "3",
    "Govt_job" = "4"
  )
)
df_num$work_type = as.numeric(df_num$work_type)

df_num$Residence_type = str_replace_all(df_num$Residence_type, c("Rural" =
                                                                   "1", "Urban" = "2"))
df_num$Residence_type = as.numeric(df_num$Residence_type)

df_num$stroke = as.numeric(as.character(df_num$stroke))

df_num$smoking_status = as.numeric(df_num$smoking_status)

drop <- c("id")
df_num = df_num[, !(names(df_num) %in% drop)]

# Splitting our dataset into 20%, 80% testing and training
set.seed(42)
to_take <- floor(0.8 * nrow(df_num))
train_idx_1 <- sample(seq_len(nrow(df_num)), size = to_take)

# split Train data and test data
train <- df_num[train_idx_1, ]
test <- df_num[-train_idx_1, ]

# apply model
model <-
  glm(stroke ~ ., family = binomial(link = 'logit'), data = train)

pred_test <- predict(model, test, type = 'response')

pred_glm = as.factor(ifelse(pred_test > 0.5, "1", "0"))
