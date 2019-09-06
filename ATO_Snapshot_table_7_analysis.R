# 0. SETUP ENVIRONMENT ----------------------------------------------------

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")


# 1. GET DATA -------------------------------------------------------------

Data_1 <- list()

Data_1$ATO_Snapshot_table_7B <- read_csv("_data/ATO_Snapshot_table_7_data/ATO_Snapshot_table_7B.csv")
Data_1$ATO_Snapshot_table_7C <- read_csv("_data/ATO_Snapshot_table_7_data/ATO_Snapshot_table_7C.csv")

# 2. CLEAN DATA ------------------------------------------------------------

Data_2 <- Data_1

Data_2$ATO_Snapshot_table_7B_state_summary <- Data_2$ATO_Snapshot_table_7B %>%
  filter(`Top or bottom` == "Top") %>%
  group_by(State = `State/ Territory1`) %>%
  summarise(`Individuals no.` = sum(`Individuals\nno.`),
            `Average taxable income or loss` = sum(`Average taxable income or loss3\n$`),
            `Median taxable income or loss` = sum(`Median taxable income or loss3\n$`),
            `Australian rank descending` = sum(`Australian rank5 descending`))

# 3. TIDY DATA ------------------------------------------------------------

Data_3 <- Data_2

Data_3$ATO_Snapshot_table_7B_state_summary_gathered <- Data_3$ATO_Snapshot_table_7B_state_summary %>%
  gather(`Individuals no.`,
         `Average taxable income or loss`,
         `Median taxable income or loss`,
         `Australian rank descending`,
         key = "variable",
         value = "value")


# 4. VISUALISE DATA -------------------------------------------------------

Data_4 <- Data_3

Data_4$ATO_Snapshot_table_7B_state_avgTaxIncome_plot <- Data_4$ATO_Snapshot_table_7B_state_summary_gathered %>%
  filter(variable == "Average taxable income or loss") %>%
  ggplot(aes(x = State, y = value)) +
  geom_bar(stat = 'identity') +
  labs(title = "Analysis of top 10 post codes per state, based on average taxable income ", y = "Avg Taxable Income ($)") +
  theme(plot.title = element_text(hjust = 0.5))

Data_4$ATO_Snapshot_table_7B_state_medTaxIncome_plot <- Data_4$ATO_Snapshot_table_7B_state_summary_gathered %>%
  filter(variable == "Median taxable income or loss") %>%
  ggplot(aes(x = State, y = value)) +
  geom_bar(stat = 'identity') +
  labs(title = "Analysis of top 10 post codes per state, based on average taxable income ", y = "Median Taxable Income ($)") +
  theme(plot.title = element_text(hjust = 0.5))

Data_4$ATO_Snapshot_table_7B_state_AusRankDesc_plot <- Data_4$ATO_Snapshot_table_7B_state_summary_gathered %>%
  filter(variable == "Australian rank descending") %>%
  ggplot(aes(x = State, y = value)) +
  geom_bar(stat = 'identity') +
  labs(title = "Analysis of top 10 post codes per state, based on average taxable income ", y = "Aus Rank Desc") +
  theme(plot.title = element_text(hjust = 0.5))

Data_4$ATO_Snapshot_table_7B_state_NumberOfIndividuals_plot <- Data_4$ATO_Snapshot_table_7B_state_summary_gathered %>%
  filter(variable == "Individuals no.") %>%
  ggplot(aes(x = State, y = value)) +
  geom_bar(stat = 'identity') +
  labs(title = "Analysis of top 10 post codes per state, based on average taxable income ", y = "No. of Individuals") +
  theme(plot.title = element_text(hjust = 0.5))