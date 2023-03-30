set.seed(100)

library(tidyverse)
library(visdat)
library(Hmisc)

df_all_years <- readRDS("data/df_all_years.rds")

df_all_years %>% 
  group_by(year) %>% 
  summarise(n = length(year))

sum(is.na(df_all_years[,1:22])) / (dim(df_all_years[,1:22])[1] * dim(df_all_years[,1:22])[2])
# 58% missing values

# how many rows have NA on all knowledge items?
sum(rowSums(is.na(df_all_years[,7:13]))==7) # 23058

# how many rows have NA on all attitude items + party choice?
sum(rowSums(is.na(df_all_years[,c(6,14:22)]))==10) # 17719

# remove rows with all NA on knowledge items
df_all_years <- df_all_years[rowSums(is.na(df_all_years[,7:13]))!=7,]

# remove remaining rows with all NA on attitude items + party choice
df_all_years <- df_all_years[rowSums(is.na(df_all_years[,c(6,14:22)]))!=10,]

sum(is.na(df_all_years[,1:22])) / (dim(df_all_years[,1:22])[1] * dim(df_all_years[,1:22])[2])
# 58% missing values

df_all_years %>% 
  vis_miss()

# look at missing by year
df_all_years %>% 
  filter(year == 1998) %>% 
  vis_miss()

df_all_years %>% 
  filter(year == 2002) %>% 
  vis_miss()

df_all_years %>% 
  filter(year == 2006) %>% 
  vis_miss()

df_all_years %>% 
  filter(year == 2010) %>% 
  vis_miss()

df_all_years %>% 
  filter(year == 2014) %>% 
  vis_miss()

df_all_years %>% 
  filter(year == 2018) %>% 
  vis_miss()

impute_arg <- aregImpute(~ d_gender +
                           d_age +
                           d_education +
                           d_income +
                           d_class +
                           d_partisanship +
                           k_m_rep +
                           k_s_rep +
                           k_c_rep +
                           k_v_rep +
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
