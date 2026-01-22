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
  ))
table(debt_cost$DEBT_QUARTILE)
summary(debt_cost$DEBT_QUARTILE)

num2 <- ggplot(debt_cost, mapping = aes())
