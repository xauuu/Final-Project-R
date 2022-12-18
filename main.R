library(dplyr)
library(tidyverse)
library(gridExtra)

df <- read.csv('dataset.csv')
str(df)
summary(df)

table(df$gender)
table(df$smoking_status)
table(df$work_type)
length(df$bmi[df$bmi == "N/A"])

# remove outlier values
df_clean <- df %>% filter(df$gender != 'Other')
df_clean = df_clean %>% filter(df_clean$bmi != 'N/A')

df_clean = na.omit(df_clean)

# convert chr to factors
df_clean$gender = as.factor(df_clean$gender)
df_clean$ever_married = as.factor(df_clean$ever_married)
df_clean$work_type = as.factor(df_clean$work_type)
df_clean$Residence_type = as.factor(df_clean$Residence_type)
df_clean$smoking_status = as.factor(df_clean$smoking_status)
df_clean$bmi = as.numeric(df_clean$bmi)
df_clean$hypertension <- as.factor(df_clean$hypertension)
df_clean$heart_disease <- as.factor(df_clean$heart_disease)
df_clean$stroke <- as.factor(df_clean$stroke)

# Description
g1 <-
  ggplot(data = df_clean, aes(x = gender, fill = gender)) + geom_bar()
g2 <-
  ggplot(data = df_clean, aes(x = age)) + geom_histogram(binwidth = 20)
g3 <-
  ggplot(data = df_clean, aes(x = ever_married, fill = ever_married)) + geom_bar()
g4 <-
  ggplot(data = df_clean, aes(x = work_type, fill = work_type)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 45))
grid.arrange(g1, g2, g3, g4, ncol = 2)

g5 <-
  ggplot(df_clean, aes(hypertension, fill = hypertension)) + geom_bar()
g6 <-
  ggplot(df_clean, aes(heart_disease, fill = heart_disease)) + geom_bar()
g7 <- ggplot(df_clean, aes(stroke, fill = stroke)) + geom_bar()
grid.arrange(g5, g6, g7, ncol = 3)

# check gender vs stroke

## check by graph
ggplot(df_clean, aes(x = gender, fill = stroke)) +
  geom_bar(aes(y = ..count.. / tapply(..count.., ..x.., sum)[..x..]), position = "dodge") +
  geom_text(
    aes(
      y = ..count.. / tapply(..count.., ..x.. , sum)[..x..],
      label = scales::percent(..count.. / tapply(..count.., ..x.. , sum)[..x..])
    ),
    stat = "count",
    position = position_dodge(0.9),
    vjust = -0.5
  ) +
  ylab('Percent of Gender Group(%)') +
  xlab('Gender') +
  scale_y_continuous(labels = scales::percent)

# check hypertesion vs stroke
ggplot(df_clean, aes(x = hypertension, fill = stroke)) +
  geom_bar(aes(y = ..count.. / tapply(..count.., ..x.., sum)[..x..]), position = "dodge") +
  geom_text(
    aes(
      y = ..count.. / tapply(..count.., ..x.. , sum)[..x..],
      label = scales::percent(..count.. / tapply(..count.., ..x.. , sum)[..x..])
    ),
    stat = "count",
    position = position_dodge(0.9),
    vjust = -0.5
  ) + ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))


# check heart_disease vs stroke
ggplot(df_clean, aes(x = heart_disease, fill = stroke)) +
  geom_bar(aes(y = ..count.. / tapply(..count.., ..x.., sum)[..x..]), position = "dodge") +
  geom_text(
    aes(
      y = ..count.. / tapply(..count.., ..x.. , sum)[..x..],
      label = scales::percent(..count.. / tapply(..count.., ..x.. , sum)[..x..])
    ),
    stat = "count",
    position = position_dodge(0.9),
    vjust = -0.5
  ) +
  ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))

# check ever_married vs stroke
ggplot(df_clean, aes(x=ever_married, fill=stroke))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent)

# check worktye vs stroke
ggplot(df_clean, aes(x=work_type, fill=stroke))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent)

# check smoke vs stroke
ggplot(df_clean, aes(x=smoking_status, fill=stroke))+
  geom_bar(aes(y=..count../tapply(..count.., ..x.., sum)[..x..]), position = "dodge")+
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_dodge(0.9), vjust=-0.5)+ylab('Percent of Hypertension Group, %') +
  scale_y_continuous(labels = scales::percent)

#check avg_glucose_level vs stroke
ggplot(df_clean, aes(x=stroke, y=avg_glucose_level, fill=stroke)) + 
  geom_boxplot() + 
  labs(subtitle="avg_glucose_level by group stroke")
lm.stroke_glucose <- lm(as.numeric(df_clean$stroke) ~ df_clean$avg_glucose_level)
summary(lm.stroke_glucose)

#check age vs stroke
ggplot(df_clean, aes(x=as.factor(stroke), y=age, fill=as.factor(stroke))) + 
  geom_boxplot() + 
  labs(subtitle="age by group stroke")
lm.stroke_age <- lm(as.numeric(df_clean$stroke) ~ df_clean$age)
summary(lm.stroke_age)

#check bmi vs stroke
ggplot(df_clean, aes(x=as.factor(stroke), y=bmi, fill=as.factor(stroke))) + 
  geom_boxplot() + 
  labs(subtitle="bmi by group stroke")
lm.stroke_bmi <- lm(as.numeric(df_clean$stroke) ~ df_clean$bmi)
summary(lm.stroke_bmi)
