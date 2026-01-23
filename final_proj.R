library(tidyverse)
library(skimr)
library(scales)
library(plotly)
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
q1 <- edu_retire %>% 
  filter(!is.na(EDUC_LABEL)) %>% 
  ggplot(aes(x = EDUC_LABEL, y = WEIGHTED_THRIFT)) +
  geom_col(fill = "blue", width = 0.6) +
  labs(
    title = "Education Level and Average Amount Saved for Retirement",
    x = "Highest Education Level Completed",
    y = "Money Saved"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_dollar())
q1

#2
ques2 <- df_clean |>
  filter(DEBT < 500000, AGE < 67) |>
  group_by(AGE) |>
  filter(n() >= 20) |> #filters if there's at least 20 rows at the same age
  mutate(
    INCOME.25 = quantile(INCOME, 0.25, na.rm = TRUE),
    INCOME.75 = quantile(INCOME, 0.75, na.rm = TRUE),
    WEIGHTED_THRIFT = weighted.mean(THRIFT, WGT, na.rm = TRUE),
    WEIGHTED_DEBT = weighted.mean(DEBT, WGT, na.rm = TRUE)
  )

q2 <- ggplot(ques2, aes(x = AGE)) +
  #Background range (25th to 75th percentile)
  geom_linerange(aes(ymin = INCOME.25, ymax = INCOME.75), 
                 colour = "gray80", linewidth = 2, alpha = 0.5) +
  #Debt line
  geom_line(aes(y = WEIGHTED_DEBT), color = "firebrick", linewidth = 1) +
  
  #retirement Line
  geom_line(aes(y = WEIGHTED_THRIFT), color = "royalblue", linewidth = 1) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.y = element_line(color = "gray80"),
    axis.text = element_text(color = "gray40"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  labs(title = "Financial Outlook by Approaching Retirement Age",
       subtitle = "Income Range (25-75th percentile) with Debt and Savings",
       x = "Age", y = "Amount ($)") +
  scale_y_continuous(labels = label_dollar())
print(q2)

q2_plotly <- ggplotly(q2)
q2_plotly
