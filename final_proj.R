library(tidyverse)
df <- read_csv("\\Users\\michaelthomas\\Downloads\\scfp2022excel\\SCFP2022.csv")
df
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
retire

