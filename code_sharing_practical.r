# Code sharing practical exercise at the ADR UK PhD gathering, 27 January 2026

# In the following example code, find examples of how the code could be
# improved for sharing, using the principles introduced in the talk.



############################################################
# R SCRIPT FOR ANALYZING A DATASET
# Written by: Share Eagerly
# Date: 2026-01-27
############################################################

######################## INTRODUCTION ######################

# This script loads the clean dataset, and the income groupings dataset,
# combines them, and calculates mean incomes


########################
# LOAD LIBRARIES
########################

library(dplyr)     # for data manipulation
library(ggplot2)
library(tidyr)     # might need this
library(dplyr)     # TODO: script only works if this is loaded twice?!


########################
# LOAD DATA
########################

# load the final, clean dataset
# TODO: fix issues in the final version of the dataset
mydata<-read.csv("/project/clean_data/DO_NOT_USE_THIS/data.csv")
# mydata<-read.csv("/project/clean_data/final_clean_data/data2.csv")


# Let's call it Data from now on
Data <- mydata


########################
# INITIAL EXPLORATION
########################

# Look at the structure of the data
str(Data)

# Print first few rows
head(mydata)

# Summary statistics
summary(mydata)

# Number of rows
nrow(Data)

# number of individuals, assume they are the same
n_distinct(Data$personID)

# Number of columns
ncol(Data)

# Check column names
names(Data)

# Just checking again
head(mydata)

# alcohol consumption
unique(Data$alcgp)

# tobacco consumption
unique(mydata$tobgp)


########################
# DERIVE NEW VARIABLES
########################

# Create age in months
# assuming age is in years
Data$age_months <- Data$age * 12

# Create a binary variable for high income
# income > 50000 is considered high
Data$HighIncome<-ifelse(Data$income>50000,1,0)

# Create another variable but using a different name style
Data$income_group = ifelse(Data$income < 30000, "low",
                           ifelse(Data$income < 70000, "medium", "HIGH"))

# This variable should probably have been created earlier
mean_income <- mean(Data$income, na.rm=TRUE)

# Center income (but using a new dataset name)
data2 <- Data
data2$incomeCentered <- data2$income - mean_income

# Recalculate mean again instead of reusing it
mean_income2 <- mean(data2$income, na.rm = TRUE)

# Another derived variable using a magic number
data2$is_old <- ifelse(data2$age > 47, TRUE, FALSE)



########################
# PLOT INCOME DISTRIBUTION
########################

# Plot income distribution
ggplot(data2, aes(x=income)) +
  geom_histogram(bins=30)

# Plot income by age
ggplot(Data, aes(x=age,y=income))+
  geom_point()

# Check summary again
summary(data2)


########################
# LOAD INCOME GROUP
########################

incomeGroup=read.csv("/project/clean_data/income_group_incomplete.csv")
# merge income group to data - this takes all individuals in data2 and finds the
# same individual in income group and adds the columns from income group to the
# data
data2=left_join(data2, incomeGroup, join_by=c("personID"="id_person"))
# remove entries that aren't also in income group data
data2=data2 %>%
  filter(personID %in% incomeGroup)

########################
# DESCRIPTIVE STATISTICS TABLES
########################

# Mean income
mean(data2$income, na.rm = TRUE)

# Median income
median(data2$income, na.rm = TRUE)

# SD income
sd(data2$income, na.rm = TRUE)

# Mean age
mean(data2$age, na.rm = TRUE)

# Median age
median(data2$age, na.rm = TRUE)

# SD age
sd(data2$age, na.rm = TRUE)

# Mean income by income group
mean(data2$income[data2$income_group=="low"], na.rm=TRUE)
mean(data2$income[data2$income_group=="medium"], na.rm=TRUE)
mean(data2$income[data2$income_group=="HIGH"], na.rm=TRUE)

# Age summaries by group
mean(data2$age[data2$income_group=="low"], na.rm=TRUE)
mean(data2$age[data2$income_group=="medium"], na.rm=TRUE)
mean(data2$age[data2$income_group=="HIGH"], na.rm=TRUE)

# Create a basic table
table(data2$income_group)

# Another table with slightly different variable name
table(Data$HighIncome)


########################
# RISK FACTOR COMPUTATION
########################

# copied algorithm from Prestigious & Worthy, 2023
tmp1 <- data2 %>%
  filter(!is.na(income) & !is.na(age)) %>%
  mutate(x1 = income / (age + 1),
         x2 = log(abs(x1) + 0.0001),
         x3 = ifelse(x2 > quantile(x2, 0.73), 1,
                     ifelse(x2 < quantile(x2, 0.21), -1, 0))) %>%
  group_by(income_group) %>%
  summarise(z = sum(x3 * rank(x2)) / length(x2))

data2$riskFactor <- tmp1$z[match(data2$income_group, tmp1$income_group)]


########################
# DESCRIPTIVE STATISTICS TABLES
########################

data2 %>%
  group_by(riskFactor, income_group) %>%
  summarise(mean = mean(income))


########################
# EXPORT
########################

