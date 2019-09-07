# 0. SETUP ENVIRONMENT ----------------------------------------------------

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")

# 1. GET DATA -------------------------------------------------------------

Data_1 <- list()

Data_1$DSS_Welfare_Payments_March2014 <- read_csv("data/DSS_Welfare_Payments_March2014.csv")

# 2. CLEAN DATA ------------------------------------------------------------

Data_2 <- Data_1

# 3. TIDY DATA ------------------------------------------------------------

Data_3 <- Data_2


# 4. VISUALISE DATA -------------------------------------------------------

Data_4 <- Data_3

if(exists("Analysis") != TRUE) {
Analysis <- list()
}

Analysis$Tables$DSS_Welfare_Payments_State_Summary_YouthAllowance <- Data_4$DSS_Welfare_Payments_March2014 %>%
  group_by(State = `state_of_commonwealth_electoral_`) %>%
  summarise(Youth_Allowance_Payments = sum(`youth_allowance__student_`)) %>%
  filter(State != c("Total", "Unknown")) 

Analysis$Plots$DSS_Welfare_Payments_State_Summary_YouthAllowance <- Analysis$Tables$DSS_Welfare_Payments_State_Summary_YouthAllowance %>%
  ggplot(aes(x = State, y = Youth_Allowance_Payments)) +
  geom_bar(stat = 'identity') +
  labs(title = "Analysis of Total Student Youth Allowance Welfare Payments by State in 2014 (Electorate Data)", y = "Total Payments ($)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

Analysis$Tables$DSS_Welfare_Payments_State_Summary_AusStudy <- Data_4$DSS_Welfare_Payments_March2014 %>%
  group_by(State = `state_of_commonwealth_electoral_`) %>%
  summarise(Austudy_Payments = sum(austudy)) %>%
  filter(State != c("Total", "Unknown"))

Analysis$Plots$DSS_Welfare_Payments_State_Summary_AusStudy <- Analysis$Tables$DSS_Welfare_Payments_State_Summary_AusStudy %>%
  ggplot(aes(x = State, y = Austudy_Payments)) +
  geom_bar(stat = 'identity') +
  labs(title = "Analysis of Total Austudy Welfare Payments by State in 2014 (Electorate Data)", y = "Total Payments ($)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

