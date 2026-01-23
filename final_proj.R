library(tidyverse)
library(skimr)
df <- read_csv("C:\\Users\\johnsedoriosa\\Downloads\\scfp2022excel\\SCFP2022.csv")

#reading dataset
df <- read_csv("\\Users\\johnsedoriosa\\Downloads\\scfp2022excel\\SCFP2022.csv")
df

#cleaning dataset
df_clean <- df |> filter(Y1 %% 10 ==1) #filters every 10 so same house isn't counted multiple times
df_clean

#EDA stuff
print(dim(df_clean))
print(names(df_clean))
print(str(df_clean))
print(head(df_clean))

##basic analysis of income and wealth
summary(df_clean$INCOME)
quantile(df_clean$NETWORTH, probs = c(0.25, 0.5, 0.75, 0.9))

colSums(is.na(df_clean)) #checks for missing values in each column

prop.table(table(df_clean$SAVED)) #shows percentage of households who saved

skim(df_clean) #summaries of every column and gives mini-histograms 

table(df_clean$EDUC, df_clean$SAVED)

df_clean <- df_clean |>
  mutate(EDUC_LABEL = case_when(
    EDUC < 8 ~ "No HS Diploma",
    EDUC == 8 ~ "HS Diploma or Equivalent", 
    EDUC == 9 ~ "Some College",
    EDUC <= 11  ~ "Associate Degree",
    EDUC == 12 ~ "Bachelor's Degree",
    EDUC == 13 ~ "Master's Degree",
    EDUC == 14 ~ "Doctorate or Professional Degree"
  ))


#How does education level relate to retirement saving behavior? 
edu_retire <- df_clean |>
  select(EDUC_LABEL, THRIFT, WGT) |>
  filter(!is.na(EDUC_LABEL), !is.na(THRIFT), !is.na(WGT)) |>
  group_by(EDUC_LABEL) |>
  summarize(
    MEDIAN_THRIFT = median(THRIFT, na.rm = TRUE),
    WEIGHTED_THRIFT = weighted.mean(THRIFT, WGT, na.rm = TRUE)
  )

edu_retire <- edu_retire %>%
  mutate(
    EDUC_LABEL = factor(
      EDUC_LABEL,
      levels = c(
        "No HS Diploma",
        "HS Diploma or Equivalent",
        "Some College",
        "Associate Degree",
        "Bachelor's Degree",
        "Master's Degree",
        "Doctorate or Professional Degree"
      )
    )
  )

q1 <- ggplot(edu_retire, aes(x = EDUC_LABEL, y = WEIGHTED_THRIFT)) +
  geom_col(fill = "blue", width = 0.6) +
  labs(
    title = "Education Level and Average Amount Saved for Retirement",
    x = "Highest Education Level Completed", 
    y = "Money Saved"
  ) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_dollar())
q1

#2 The Hidden Cost of Debt on the Future
debt_cost <- df_clean |> 
  select("DEBT","INCOME","THRIFT","DEBT2INC") |>
  filter(INCOME >= 0) |>
  mutate("DEBT_QUARTILE" = ntile(DEBT2INC, 4))|>
  mutate("DEBT_QUARTILE" = case_when(
    DEBT_QUARTILE == 1 ~ "Low Debt Burden",
    DEBT_QUARTILE == 2 ~ "Moderate-Low Debt Burden",
    DEBT_QUARTILE == 3 ~ "Moderate-High Debt Burden",
    DEBT_QUARTILE == 4 ~ "High Debt Burden"
  )
  ) |>
  mutate("INCOME75" = quantile(INCOME, 0.75),
         "INCOME25" = quantile(INCOME, 0.25))

table(debt_cost$DEBT_QUARTILE)
summary(debt_cost$DEBT_QUARTILE)

q2 <- ggplot(debt_cost, aes(THIRFT,INCOME)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(debt_cost, mapping=aes(x=THRIFT, ymin=INCOME25, ymax=INCOME75), colour = "wheat2", alpha=.1)
q2


