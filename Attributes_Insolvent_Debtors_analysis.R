# 0. SETUP ENVIRONMENT ----------------------------------------------------

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")

# 1. GET DATA -------------------------------------------------------------

Data_1 <- list()

Data_1$Attributes_Insolvent_Debtors_data <- read_csv("data/Attributes_Insolvent_Debtors_data.csv")

# 2. CLEAN DATA ------------------------------------------------------------

Data_2 <- Data_1

# 3. TIDY DATA ------------------------------------------------------------

Data_3 <- Data_2

# 4. VISUALISE DATA -------------------------------------------------------

Data_4 <- Data_3

if(exists("Analysis") != TRUE) {
  Analysis <- list()
}

# get table analysis data
Analysis$Tables$Attributes_Insolvent_Debtors_data <- Data_4$Attributes_Insolvent_Debtors_data

# analysis state level data
Analysis$Plots$Attributes_Insolvency_by_State <- Analysis$Tables$Attributes_Insolvent_Debtors_data %>%
  group_by(State = `State of Debtor`) %>%
  count() %>%
  filter(State != "Unknown") %>%
  ggplot(aes(x = State, y = n)) +
  geom_bar(stat = 'identity') +
  labs(title = "Number of Insolvency Reports by State", y = "Number of Reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

Analysis$Plots$Attributes_Insolvency_by_State_n_Assets <- Analysis$Tables$Attributes_Insolvent_Debtors_data %>%
  group_by(State = `State of Debtor`, `Value of Assets`) %>%
  count() %>%
  filter(State != "Unknown") %>%
  ggplot(aes(x = State, y = n, fill = `Value of Assets`)) +
  geom_bar(stat = 'identity') +
  labs(title = "Number of Insolvency Reports by State", y = "Number of Reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

Analysis$Plots$Attributes_Insolvency_by_State_n_Debts <- Analysis$Tables$Attributes_Insolvent_Debtors_data %>%
  group_by(State = `State of Debtor`, `Unsecured Debts`) %>%
  count() %>%
  filter(State != "Unknown") %>%
  ggplot(aes(x = State, y = n, fill = `Unsecured Debts`)) +
  geom_bar(stat = 'identity') +
  labs(title = "Number of Insolvency Reports by State", y = "Number of Reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

Analysis$Plots$Attributes_Insolvency_by_State_n_Gender <- Analysis$Tables$Attributes_Insolvent_Debtors_data %>%
  group_by(State = `State of Debtor`, `Sex of Debtor`) %>%
  count() %>%
  filter(State != "Unknown") %>%
  ggplot(aes(x = State, y = n, fill = `Sex of Debtor`)) +
  geom_bar(stat = 'identity') +
  labs(title = "Number of Insolvency Reports by State", y = "Number of Reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

Analysis$Plots$Attributes_Insolvency_by_State_n_Situation <- Analysis$Tables$Attributes_Insolvent_Debtors_data %>%
  group_by(State = `State of Debtor`, `Family Situation`) %>%
  count() %>%
  filter(State != "Unknown") %>%
  ggplot(aes(x = State, y = n, fill = `Family Situation`)) +
  geom_bar(stat = 'identity') +
  labs(title = "Number of Insolvency Reports by State", y = "Number of Reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# analysis SA3 level data
Analysis$Plots$Attributes_Insolvency_by_SA3 <- Analysis$Tables$Attributes_Insolvent_Debtors_data %>%
  group_by(`SA3 of Debtor`) %>%
  count() %>%
  filter(`SA3 of Debtor` != "Unknown") %>%
  filter(n > 2500) %>%
  ggplot(aes(x = `SA3 of Debtor`, y = n)) +
  geom_bar(stat = 'identity') +
  labs(title = "SA3 Locations with over 2500 Insolvency Reports", y = "Number of Reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

Analysis$Plots$Attributes_Insolvency_by_SA3_n_State <- Analysis$Tables$Attributes_Insolvent_Debtors_data %>%
  group_by(`SA3 of Debtor`, `State of Debtor`) %>%
  count() %>%
  filter(`SA3 of Debtor` != "Unknown") %>%
  filter(n > 2500) %>%
  ggplot(aes(x = `SA3 of Debtor`, y = n, fill = `State of Debtor`)) +
  geom_bar(stat = 'identity') +
  labs(title = "SA3 Locations with over 2500 Insolvency Reports", y = "Number of Reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# analsys other relevant attributes

Analysis$Plots$Attributes_Insolvency_by_Job_n_State <- Analysis$Tables$Attributes_Insolvent_Debtors_data %>%
  group_by(`Debtor Occupation Name (ANZSCO)`, `State of Debtor`) %>%
  count() %>%
  filter(`Debtor Occupation Name (ANZSCO)` != "NA") %>%
  filter(n > 2500) %>%
  ggplot(aes(x = `Debtor Occupation Name (ANZSCO)`, y = n, fill = `State of Debtor`)) +
  geom_bar(stat = 'identity') +
  labs(title = "Jobs with over 2500 Insolvency Reports", y = "Number of Reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

Analysis$Plots$Attributes_Insolvency_by_Year_n_State <- Analysis$Tables$Attributes_Insolvent_Debtors_data %>%
  group_by(`Calendar Year of Insolvency`, `State of Debtor`) %>%
  count() %>%
  filter(`State of Debtor` != "Unknown") %>%
  ggplot(aes(x = `Calendar Year of Insolvency`, y = n, fill = `State of Debtor`)) +
  geom_bar(stat = 'identity') +
  labs(title = "Number of Insolvency Reports Over Time by State", y = "Number of Reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(from = 2007, to = 2018, by = 1))

