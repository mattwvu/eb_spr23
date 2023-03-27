# set working directory

setwd("C:/Users/Matt/Documents/Rprojects/eb_spr23")

# install packages

install.packages("tidyverse")
install.packages("Hmisc")
install.packages("psych")
install.packages("data.table")


# load packages

library(tidyverse)
library(Hmisc)
library(psych)
library(data.table)

#read dataset

df <- readxl::read_xlsx("data.xlsx", sheet = 1)
view(df)

df <- df %>% select(-`Emotional Empathy Questionaire`)
df

# read regression dataset

df_reg <- read_csv("data_regression.csv")
view(df_reg)
str(df_reg)

# create new variables

df_reg <- df_reg %>% 
  mutate(sex_desc = recode_factor(sex, "0" = "Male", "1" = "Female")) %>% 
  mutate(mani_desc = recode_factor(manipulation, "0" = "Polite", "1" = "Rude"))

# Number and type of interactions

  #by manipultation variable

df_count <- df_reg %>% 
  group_by(mani_desc) %>% 
  summarise(N = n())
df_count
  
  #by sex variable

filter_female <-  df_reg %>% 
  filter(sex_desc == "Female")
filter_female

# Number of and percentage of gender

  # n by sex
  
df_gender <- df_reg %>% 
  group_by(sex_desc) %>% 
  summarise(number = n())
  
df_gender


  # by manipulation and gender

df_female <- df_reg %>% 
  filter(sex_desc == "Female") %>% 
  group_by(mani_desc, sex_desc) %>% 
  summarise(number = n())

df_female

  # percentage by manipulation

percent_female <- df_female$number/df_count$N 

df_female$percent = percent_female * 100    #create new variable

df_female


#rename correlation variable (anagram, fluency, creativity, flexibility)

colnames(df_reg)

df_reg = data.frame(anagram_score = , brick_flexibility = , brick_use_score = , brick_creativity = )


# Get mean and sd and min max for emotional empathy
  
df_vars <- df_reg %>% 
  group_by(mani_desc) %>% 
  summarise(n = n(),
            mean_emp = mean(empathy_raw_score), 
            sd_emp = sd(empathy_raw_score), 
            min_emp = min(empathy_raw_score), 
            max_emp = max(empathy_raw_score)
            ) 

df_emp

# repeat for anagram, fluency, creativity, flexibility


#join the female data frame with the m/sd/min/max data frames

df_joined <- df_female %>%  left_join(df_vars, by ="mani_desc")
df_joined

write.csv(df_joined, "descriptives.csv")

# get the total statistics

describe(df_reg)

summary_stats <- describe(df_reg)
summary_stats

write_csv(summary_stats, "totals.csv")

# total N

total_N <- nrow(df_reg)


# total n female

total_female <- nrow(filter_female)

# mean  percent female

mean_percent_female <- mean(df_female$percent)
mean_percent_female     #not sure if this a mean total

# mean emotional empathy

mean_emp <- mean(df_reg$empathy_raw_score)


# sd emotional empathy

sd_emp <- sd(df_reg$empathy_raw_score)

# min emotional empathy

min_emp <- min(df_reg$empathy_raw_score)

# max emotional empathy

max_emp <- max(df_reg$empathy_raw_score)





####

sum_list <-  data.table("Total",
                   "Female",
                   total_female, 
                   mean_percent_female,
                   total_N,
                   mean_emp,
                   sd_emp,
                   min_emp,
                   max_emp)

sum_list

df_test <- rbindlist(df_joined, sum_list)



colnames(df_joined)

ncol(df_joined)
length(sum_stats)

sum_stats <- data.table(mani_desc = "Total", 
                       sex_desc = "Female", 
                       number = total_female, 
                       percent = mean_percent_female,
                       n =total_N, 
                       mean_emp = mean_emp,
                       sd_emp = sd_emp,
                       min_emp = min_emp,
                       max_emp = max_emp)
sum_stats


df_final <- rbind(df_joined, sum_stats)
df_final

# join the data_sets
colnames(df_joined)
ncol(df_joined)
length(sum_stats)


df_final <- cbind(df_joined, sum_stats)


# select anagram, fluency, creativity, flexibility variables

colnames(df_reg)

df_corr <- df_reg %>% select(anagram_score, brick_use_score, brick_creativity, brick_flexibility)

# correlation table using rcorr from Hmisc package

corr_matrix <- rcorr(as.matrix(df_corr))            
corr_matrix

# plotting the data

colnames(df_reg)

view(df_reg)


emp_ana_corr <- cor(df_reg$empathy_raw_score, df_reg$anagram_score)
emp_ana_corr

?geom_smooth

emp_ana_plot <- ggplot(df_reg, aes(empathy_raw_score, anagram_score, color = mani_desc))

emp_ana_plot + geom_point() +
  theme_classic() + geom_smooth(method=lm, se = FALSE) +
  labs (title = "p =",
        subtitle = "A",
        x = "Emotional Empathy",
        y = "Anagrams Solved")


#----

#from psych package - describe

?psych::describe
?psych::describeBy

colnames(df_reg)

psych_condi <- describeBy(df_reg$manipulation_desc, df_reg$empathy_raw_score + df_reg$brick_use_score)


describeBy(df_reg)