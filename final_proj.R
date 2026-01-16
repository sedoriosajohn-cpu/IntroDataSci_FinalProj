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

#keeping variables that we'll use for project
retire <- df_clean |> 
  select(YY1, Y1, OCCAT1, OCCAT2, RACE, LIFECL, 
         INCOME, SAVED, FINLIT, IRAKH, THRIFT, 
         CCBAL, EDUC, HOUSECL,ASSET, SAVING, CHECKING, 
         EQUITY, VEHIC, HBROK, NOWN, NLEASE, DEBT, RENT,
         BNPL, TPAY, REVPAY, MORTPAY, CONSPAY, KNOWL, 
         FOODHOME, FOODDELV, FOODAWAY, RETQLIQ, WGT, AGE)

#labeling the education level 
retire <- retire |>
  mutate(EDUC_LABEL = case_when(
    EDUC < 8 ~ "No HS Diploma",
    EDUC == 8 ~ "HS Diploma or Equivalent", 
    EDUC == 9 ~ "Some College",
    EDUC <= 11  ~ "Associate Degree",
    EDUC == 12 ~ "Bachelor's Degree",
    EDUC == 13 ~ "Master's Degree",
    EDUC == 14 ~ "Doctorate or Professional Degree"
  ))

#checking if it's correct 
#print(retire[, c("EDUC","EDUC_LABEL")], n = 40)

#summary of total retirement summary for each retirement 
savings_summary <- retire |>
  group_by(EDUC_LABEL) |>
  summarize(
    AVG_RETIRE_ASSETS = weighted.mean(RETQLIQ, WGT),
    MEDIAN_RETIRE_ASSETS = median(RETQLIQ),
    SAMPLE_SIZE = n()
  )

print(savings_summary)

#finding the amount of money they have in 
edu_saving <- retire |>
  filter(AGE <= 45) |>
  mutate(SAV_INC_RATIO = RETQLIQ / (INCOME + 1)) |>
  group_by(EDUC_LABEL) |>
  summarize(
    POP_RATIO = weighted.mean(SAV_INC_RATIO, WGT),
    TYP_RATIO = median(SAV_INC_RATIO),
    AVG_RATIO = weighted.mean(RETQLIQ, WGT)
  )

print(edu_saving)

edu_saving_graph <- edu_saving |>
  ggplot(mapping = aes(x = reorder(EDUC_LABEL, POP_RATIO), 
                       y = POP_RATIO, fill = EDUC_LABEL)) +
  geom_col() + 
  geom_text(aes(label = scales::percent(POP_RATIO, accuracy = 0.1)), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, .15))) +
  labs(
    title = "Weighted Savings-to-Income Ratio by Education",
    subtitle = "Households Headed by Adults Age 45 and Under",
    x = "Education Level",
    y = "Savings Ratio (Savings / Income)",
    caption = "Source: 2022 Survey of Consumer Finances (SCF)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(edu_saving_graph)
  
