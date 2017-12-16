# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

#R is always pointed at a directory on your computer. 
#You can find out which directory by running the getwd() function
#use setwd() and specify the path to the desired folder

# read file and do some basic setup
HealthCare<- read.csv(".\\data\\usa_state_healtcare.csv", stringsAsFactors = FALSE)

colnames(HealthCare) <- vector("State", 
                               "Uninsured_Rate_2010", 
                               "Uninsured_Rate_2015",
                               "Uninsured_Rate_Ch_2010_2015", 
                               "Health_Ins_Cov_Ch_2010_2015",
                               "Empl_Ins_Cov_Ch_2010_2015", 
                               "Mktpl_Ins_Cov_Ch_2010_2015",
                               "Mktpl_Tx_Credits", 
                               "Avg_Mo_Tx_Credit", 
                               "State_Medicaid_Exp", 
                               "Medicaid_Enroll_2013", 
                               "Medicaid_Enroll_2016",
                               "Medicaid_Enroll_Ch_2013_2016", 
                               "Medicare_Enroll_2016")

# convert character percentage rates to numeric

HealthCare$Uninsured_Rate_2010 <- as.numeric(sub("%", "", HealthCare$Uninsured_Rate_2010))/100
HealthCare$Uninsured_Rate_2015 <- as.numeric(sub("%", "", HealthCare$Uninsured_Rate_2015))/100
HealthCare$Uninsured_Rate_Ch_2010_2015 <- as.numeric(sub("%", "", HealthCare$Uninsured_Rate_Ch_2010_2015))/100

# convert $ to numeric
HealthCare$Avg_Mo_Tx_Credit <- as.numeric(gsub("[^0-9]", "", HealthCare$Avg_Mo_Tx_Credit))

# gsub() function replaces all matches of a string [0-9]-All Digit(Regular Experession Syntax)

# convert state to a factor

HealthCare$State <- as.factor(HealthCare$State)

# the value for the US in Uninsured_Rate_Ch_2010_2015 is incorrect.  we'll fix it
HealthCare$Uninsured_Rate_Ch_2010_2015 <- HealthCare$Uninsured_Rate_2015 - HealthCare$Uninsured_Rate_2010

# create a data frame with just the US summary info and a set with just the states
HealthCare_state <- HealthCare[!HealthCare$State == "United States", ]
HealthCare_US <- HealthCare[HealthCare$State == "United States", ]

# drop unused factor levels (not really necessary, but I prefer it)

#MyData_state$State <- factor(MyData_state$State)
#MyData_US$State <- factor(MyData_US$State)


# Summarize US Stats (just print it)
#MyData_US

 
# create a new State vector thats ordered by Uninsured Rate Change for better plotting
library(ggplot2)

HealthCare_state$State <- factor(HealthCare_state$State, levels =HealthCare_state[order(HealthCare_state$Uninsured_Rate_Ch_2010_2015), "State"])
ggplot(HealthCare_state, aes(State, Uninsured_Rate_Ch_2010_2015)) + 
  geom_bar(stat="identity", fill = "firebrick") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  ggtitle("Uninsured Rate Change 2010 - 2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Uninsured Rate Change 2010-2015") + 
  geom_hline(yintercept = HealthCare_US$Uninsured_Rate_Ch_2010_2015, color = "blue", linetype = "dotdash") # add national average



# medicaid expansion

# 2 missing values

sum(is.na(HealthCare_state$Medicaid_Enroll_2013)) 
aca_state2 <- HealthCare_state[complete.cases(HealthCare_state), ] # we'll just eliminate them for this exercise
aca_state2$State2 <- factor(aca_state2$State, levels =aca_state2[order(aca_state2$Medicaid_Enroll_Ch_2013_2016), "State"])

#plot
ggplot(aca_state2, aes(State2, Medicaid_Enroll_Ch_2013_2016, fill=State_Medicaid_Exp)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Medicaid Enrollment Change 2013-2016") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Medicaid Enrollment Change 2013-2016") +
  scale_fill_discrete(name="Medicaid\nExpansion") +
  geom_hline(yintercept = mean(aca_state2$Medicaid_Enroll_Ch_2013_2016), color = "blue", linetype = "dotdash") # add national average



# Look at Health Insurance Coverage Change 
# this would be more useful on a per capita basis

HealthCare_state$State2 <- factor(HealthCare_state$State, levels =HealthCare_state[order(HealthCare_state$Health_Ins_Cov_Ch_2010_2015), "State"])
ggplot(HealthCare_state, aes(State2, Health_Ins_Cov_Ch_2010_2015)) + 
  geom_bar(stat="identity", fill = "firebrick") +
  coord_flip() +
  ggtitle("Health Insurance Change 2013-2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Health Insurance Change 2013-2015") + 
  geom_hline(yintercept = mean(HealthCare_state$Health_Ins_Cov_Ch_2010_2015), color = "blue", linetype = "dotdash") # add national average


# Marketplace Insurance Coverage Change 
# this would be more useful on a per capita basis

HealthCare_state$State2 <- factor(HealthCare_state$State, levels =HealthCare_state[order(HealthCare_state$Mktpl_Ins_Cov_Ch_2010_2015), "State"])
ggplot(HealthCare_state, aes(State2, Mktpl_Ins_Cov_Ch_2010_2015)) + 
  geom_bar(stat="identity", fill = "firebrick") +
  coord_flip() +
  ggtitle("Marketplace Health Insurance Change 2013-2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Marketplace Health Insurance Change 2013-2015") + 
  geom_hline(yintercept = mean(HealthCare_state$Mktpl_Ins_Cov_Ch_2010_2015), color = "blue", linetype = "dotdash") # add national average


# Avg Monthly Tax Credit
# this would be more useful on a per capita basis

HealthCare_state$State2 <- factor(HealthCare_state$State, levels =HealthCare_state[order(HealthCare_state$Avg_Mo_Tx_Credit), "State"])
ggplot(HealthCare_state, aes(State2, Avg_Mo_Tx_Credit)) + 
  geom_bar(stat="identity", fill = "red") +
  coord_flip() +
  ggtitle("Avg Monthly Tax Credit") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") +
  ylab("Avg Monthly Tax Credit") + 
  geom_hline(yintercept = mean(HealthCare_state$Avg_Mo_Tx_Credit), color = "blue", linetype = "dotdash") # add national average

