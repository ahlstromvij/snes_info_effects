set.seed(100)

library(tidyverse)
library(visdat)
library(Hmisc)

df_all_years <- readRDS("data/df_all_years.rds")

vis_miss(df_all_years)

impute_arg <- aregImpute(~ d_gender +
                           d_age +
                           d_education +
                           d_income +
                           d_marital_status +
                           d_class +
                           d_partisanship +
                           k_m_rep +
                           k_s_rep +
                           k_nat_insurance +
                           k_num_mps +
                           k_spain_eu +
                           a_reduce_pub_spend +
                           a_sell_pub_comp +
                           a_priv_healthcare +
                           a_fewer_refugees +
                           a_law_order +
                           a_gender_equal +
                           a_no_nuclear +
                           a_leave_eu +
                           a_join_nato,
                         data = df_all_years, n.impute = 10, tlinear = FALSE)
impute_arg
imp_data <- as.data.frame(impute.transcan(impute_arg, imputation=1, data=df_all_years, list.out=TRUE, pr=FALSE, check=FALSE)) 
head(imp_data, 10)
df_all_years <- cbind(df_all_years$year, imp_data, df_all_years$weight) %>% 
  rename(year = `df_all_years$year`,
         weight = `df_all_years$weight`)

saveRDS(df_all_years, "data/df_all_years_imputed.rds")
