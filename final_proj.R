library(tidyverse)
library(skimr)
library(ggridges) #from gemini 
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


#saving to income ratio (weighted to represent entire US population) by education level
edu_saving_graph <- edu_saving |>
  ggplot(mapping = aes(x = reorder(EDUC_LABEL, POP_RATIO), 
                       y = POP_RATIO, fill = EDUC_LABEL)) +
  geom_col() + 
  geom_text(aes(label = scales::percent(POP_RATIO, accuracy = 0.1)), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, .15))) +
  labs(
    title = "Savings-to-Income Ratio by Highest Education Completed",
    subtitle = "Households Headed by Adults Age 45 and Under",
    x = "Education Level",
    y = "Savings Ratio ",
    caption = "Source: 2022 Survey of Consumer Finances (SCF)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust =1))
print(edu_saving_graph)

#checking how much each age appears
AGE_COUNT <- retire |>
  count(AGE)
AGE_COUNT

#good graph
retire_sum <- retire |>
  filter(DEBT < 500000, AGE < 67) |>
  group_by(AGE) |>
  filter(n() >= 20) |> #filters if there's at least 20 rows at the same age
  summarize(
    INCOME.25 = quantile(INCOME, 0.25, na.rm = TRUE),
    INCOME.75 = quantile(INCOME, 0.75, na.rm = TRUE),
    AVG_DEBT = median(DEBT, na.rm=TRUE),
    AVG_SAVING = median(SAVING, na.rm = TRUE)
  )

p <- ggplot(retire_sum, aes(x = AGE)) +
  #Background range (25th to 75th percentile)
  geom_linerange(aes(ymin = INCOME.25, ymax = INCOME.75), 
                 colour = "gray80", linewidth = 2, alpha = 0.5) +
  #Debt line
  geom_line(aes(y = AVG_DEBT), color = "firebrick", linewidth = 1) +
  
  #Saving Line
  geom_line(aes(y = AVG_SAVING), color = "royalblue", linewidth = 1) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.y = element_line(color = "gray80"),
    axis.text = element_text(color = "gray40"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  labs(title = "Financial Outlook by Age",
       subtitle = "Income Range (25-75th percentile) with Debt and Savings",
       x = "Age", y = "Amount ($)")
print(p)

income <- df_clean |>
  select(INCOME, EDUC, EDUCL) |>
 mutate(EDUC_LABEL = case_when(
    EDUC < 8 ~ "No HS Diploma",
    EDUC == 8 ~ "HS Diploma or Equivalent", 
    EDUC == 9 ~ "Some College",
    EDUC <= 11  ~ "Associate Degree",
    EDUC == 12 ~ "Bachelor's Degree",
    EDUC == 13 ~ "Master's Degree",
    EDUC == 14 ~ "Doctorate or Professional Degree"
  ))

ggplot(income, aes(x = INCOME, y = EDUC_LABEL, fill = EDUC_LABEL)) +
  geom_density_ridges(alpha = 0.6, bandwidth = 10000) +
  coord_cartesian(xlim = c(0, 300000)) + 
  theme_ridges() + 
  labs(title = "Income Distribution by Education Level",
       x = "Annual Income ($)", y = "Education Category") +
  guides(fill = "none")
  
  