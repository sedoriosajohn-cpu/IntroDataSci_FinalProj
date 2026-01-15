library(tidyverse)

#reading dataset
df <- read_csv("\\Users\\johnsedoriosa\\Downloads\\scfp2022excel\\SCFP2022.csv")
df

#cleaning dataset
df_clean <- df |> filter(Y1 %% 10 ==1) #filters every 10 so same house isn't counted multiple times
df_clean

#keeping variables that we'll use for project
retire <- df_clean |> 
  select(YY1, Y1, OCCAT1, OCCAT2, RACE, LIFECL, 
         INCOME, SAVED, FINLIT, IRAKH, THRIFT, 
         CCBAL, EDUC, HOUSECL,ASSET, SAVING, CHECKING, 
         EQUITY, VEHIC, HBROK, NOWN, NLEASE, DEBT, RENT,
         BNPL, TPAY, REVPAY, MORTPAY, CONSPAY, KNOWL, 
         FOODHOME, FOODDELV, FOODAWAY)
#labeling the education level 
retire <- retire |>
  mutate(EDUC_LABEL = case_when(
    EDUC < 8 ~ "No HS Diploma",
    EDUC == 8 ~ "HS Diploma", 
    EDUC == 9 ~ "Some College",
    EDUC <= 11  ~ "Associate Degree",
    EDUC == 12 ~ "Bachelor's Degree",
    EDUC == 13 ~ "Master's Degree",
    EDUC == 14 ~ "Doctorate or Professional Degree"
  ))
#checking if it's correct 
print(retire[, c("EDUC","EDUC_LABEL")], n = 40)

