#load necessary libraries
library(RODBC)
library(tidyverse)
library(psych)
library(dplyr)
library(tidyr)

#Connect to server
dbsu <- odbcConnect("surveys")

Variable <-sqlTables(dbsu)
view(Variable)

Hours1 <-sqlFetch(dbsu, "wkload24_q1q2.demo") %>%
  filter(valid == 1)
Hours2 <-sqlFetch(dbsu, "wkload24_q1q2.question_meta")
Hours3 <-sqlFetch(dbsu, "wkload24_q1q2.question_choices")
Hours4 <-sqlFetch(dbsu, "wkload24_q1q2.resp_text")
Hours5 <-sqlFetch(dbsu, "wkload24_q1q2.response") 
Hours6 <-sqlFetch(dbsu, "wkload24_q1q2.response")


# Create DF with all well necessary question data (All questions here are single select)
Productivity <-sqlFetch(dbsu,"wkload24_q1q2.response") %>%
  filter(rid %in% Hours1$rid) %>%
  mutate(
    response = as.numeric(response)
  ) %>%
  filter(question %in% c("QID45", "QID66", "QID26_1", "QID26_2", "QID29_1", "QID29_2")) %>%
  pivot_wider(names_from = question, values_from = response) %>%
  group_by(rid) %>%
  ungroup()

#Calculate mean hours worked - grouped by Age x Working Arrangement
hours_worked <-Productivity %>%
  group_by(QID45, QID66) %>%
  summarise(mean_sum = mean(QID26_1 + QID26_2)/2)

base <- Productivity %>%
  group_by(QID45, QID66) %>%
  summarise(distinct_respondents = n_distinct(rid))

#Calculate mean hours billed - grouped by Age x Working Arrangement
Productivity2 <-na.omit(Productivity)

billed_hours <-Productivity2 %>%
  group_by(QID45, QID66) %>%
  summarise(mean_sum = mean(QID29_1 + QID29_2)/2)

base2 <- Productivity2 %>%
  group_by(QID45, QID66) %>%
  summarise(distinct_respondents = n_distinct(rid))







# hours_billed <-Productivity %>%
#   group_by(QID45, QID66) %>%
#   summarise(mean_sum = mean((QID29_1 + QID29_2)/2), na.rm = TRUE)
# 
# hours_billed <-Productivity %>%
#   mutate(sum_col = QID29_1 + QID26_2) %>%
#   group_by(QID45, QID66) %>%
#   summarise(mean_sum = mean(sum_col), na.rm = TRUE)