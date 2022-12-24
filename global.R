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

# Gender
g1_gender <-
  ggplot(data = df_copy, aes(x = gender, fill = gender)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
  scale_fill_manual(values = c(
    wes_palette("GrandBudapest2")[2],
    wes_palette("GrandBudapest2")[3]
  )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Gender", title = "Gender Distribution")

# Residence type
g1_residence <-
  ggplot(df_copy, aes(x = Residence_type, fill = Residence_type)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
  scale_fill_manual(values = c(
    wes_palette("GrandBudapest2")[2],
    wes_palette("GrandBudapest2")[3]
  )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Residence Type", title = "Residence Type Distribution")

# Ever married
g1_married <-
  ggplot(data = df_copy, aes(x = ever_married, fill = ever_married)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
  scale_fill_manual(values = c(
    wes_palette("GrandBudapest2")[2],
    wes_palette("GrandBudapest2")[3]
  )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Ever Married", title = "Ever Married Distribution")

# Work Type: collapse into two cateogies: Child/never worked and all others
g1_work <-
  ggplot(data = df_copy, aes(x = work_type, fill = work_type)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
  scale_fill_manual(values = c(
    wes_palette("GrandBudapest2")[1],
    wes_palette("GrandBudapest2")[2],
    wes_palette("GrandBudapest2")[3],
    wes_palette("GrandBudapest2")[4],
    wes_palette("GrandBudapest2")[5]
  )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Work Type", title = "Work Type Distribution")

# Hypertension
g1_hyper <-
  ggplot(df_copy, aes(as.factor(hypertension), fill = as.factor(hypertension))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
  scale_fill_manual(values = c(
    wes_palette("GrandBudapest2")[2],
    wes_palette("GrandBudapest2")[3]
  )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Hypertension", title = "Hypertension Distribution")

# Heart Disease
g1_heart <-
  ggplot(df_copy, aes(as.factor(heart_disease), fill = as.factor(heart_disease))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
  scale_fill_manual(values = c(
    wes_palette("GrandBudapest2")[2],
    wes_palette("GrandBudapest2")[3]
  )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Heart Dsiease", title = "Heart Disease Distribution")

# Smoking Status
g1_smoking <-
  ggplot(df_copy, aes(x = smoking_status, fill = smoking_status)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
  scale_fill_manual(values = c(
    wes_palette("GrandBudapest2")[1],
    wes_palette("GrandBudapest2")[2],
    wes_palette("GrandBudapest2")[3],
    wes_palette("GrandBudapest2")[4]
  )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Smoking Status", title = "Smoking Status Distribution")

# Stroke
g1_stroke <-
  ggplot(df_copy, aes(as.factor(stroke), fill = as.factor(stroke))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
  scale_fill_manual(values = c(
    wes_palette("GrandBudapest2")[2],
    wes_palette("GrandBudapest2")[3]
  )) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Stroke", title = "Stroke Distribution")

g1_age <- ggplot(df_copy) +
  geom_histogram(
    data = df_copy,
    aes(x = age),
    fill = wes_palette("IsleofDogs1")[1],
    color = "gray"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  labs(y = "Count", x = "Age (years)", title = "Age Distribution")

g1_bmi <- ggplot(df_copy) +
  geom_histogram(
    data = df_copy,
    aes(x = bmi),
    fill = wes_palette("IsleofDogs1")[1],
    color = "gray"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  labs(y = "Count", x = "BMI", title = "BMI Distribution")

g1_glu <-
  ggplot(df_copy) +
  geom_histogram(
    data = df_copy,
    aes(x = avg_glucose_level),
    fill = wes_palette("IsleofDogs1")[1],
    color = "gray"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  labs(y = "Count", x = "Average Glucose Level", title = "Average Glucose Level Distribution")


tbg2_gender <-
  df_copy %>% group_by(gender) %>% count(stroke) %>% mutate(pct = prop.table(n))
g2_gender <-
  ggplot(tbg2_gender,
         aes(
           x = gender,
           y = pct,
           fill = as.factor(stroke),
           label = scales::percent(pct)
         )) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",
       x = "Gender",
       title = "Gender Distribution by Stroke Status",
       fill = "Stroke") +
  scale_y_continuous(labels = scales::percent)

tbg2_hyper <-
  df_copy %>% group_by(hypertension) %>% count(stroke) %>% mutate(pct = prop.table(n))
g2_hyper <-
  ggplot(tbg2_hyper,
         aes(
           x = as.factor(hypertension),
           y = pct,
           fill = as.factor(stroke),
           label = scales::percent(pct)
         )) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",
       x = "Hypertension",
       title = "Hypertension Distribution by Stroke Status",
       fill = "Stroke") +
  scale_y_continuous(labels = scales::percent)

tbg2_heart <-
  df_copy %>% group_by(heart_disease) %>% count(stroke) %>% mutate(pct = prop.table(n))
g2_heart <-
  ggplot(tbg2_heart,
         aes(
           x = as.factor(heart_disease),
           y = pct,
           fill = as.factor(stroke),
           label = scales::percent(pct)
         )) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",
       x = "Heart Disease",
       title = "Heart Disease Distribution by Stroke Status",
       fill = "Stroke") +
  scale_y_continuous(labels = scales::percent)

tbg2_married <-
  df_copy %>% group_by(ever_married) %>% count(stroke) %>% mutate(pct = prop.table(n))
g2_married <-
  ggplot(tbg2_married,
         aes(
           x = ever_married,
           y = pct,
           fill = as.factor(stroke),
           label = scales::percent(pct)
         )) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",
       x = "Ever Married",
       title = "Ever Married Distribution by Stroke Status",
       fill = "Stroke") +
  scale_y_continuous(labels = scales::percent)

tbg2_smoking <-
  df_copy %>% group_by(smoking_status) %>% count(stroke) %>% mutate(pct = prop.table(n))
g2_smoking <-
  ggplot(
    tbg2_smoking,
    aes(
      x = smoking_status,
      y = pct,
      fill = as.factor(stroke),
      label = scales::percent(pct)
    )
  ) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",
       x = "Smoking Status",
       title = "Smoking Status Distribution by Stroke Status",
       fill = "Stroke") +
  scale_y_continuous(labels = scales::percent)

tbg2_work <-
  df_copy %>% group_by(work_type) %>% count(stroke) %>% mutate(pct = prop.table(n))
g2_work <-
  ggplot(tbg2_work,
         aes(
           x = work_type,
           y = pct,
           fill = as.factor(stroke),
           label = scales::percent(pct)
         )) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",
       x = "Work Type",
       title = "Work Type Distribution by Stroke Status",
       fill = "Stroke") +
  scale_y_continuous(labels = scales::percent)

tbg2_residence <-
  df_copy %>% group_by(Residence_type) %>% count(stroke) %>% mutate(pct = prop.table(n))
g2_residence <-
  ggplot(
    tbg2_residence,
    aes(
      x = Residence_type,
      y = pct,
      fill = as.factor(stroke),
      label = scales::percent(pct)
    )
  ) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",
       x = "Residence Type",
       title = "Residence Type Distribution by Stroke Status",
       fill = "Stroke") +
  scale_y_continuous(labels = scales::percent)


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
df_num = df_num[,!(names(df_num) %in% drop)]

# Splitting our dataset into 20%, 80% testing and training
set.seed(42)
to_take <- floor(0.8 * nrow(df_num))
train_idx_1 <- sample(seq_len(nrow(df_num)), size = to_take)

# split Train data and test data
train <- df_num[train_idx_1,]
test <- df_num[-train_idx_1,]

# apply model
model <-
  glm(stroke ~ ., family = binomial(link = 'logit'), data = train)

pred_test <- predict(model, test, type = 'response')

pred_glm = as.factor(ifelse(pred_test > 0.5, "1", "0"))
