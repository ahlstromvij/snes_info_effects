set.seed(100)

library(tidyverse)
library(haven)

# 2014 data
df_snes_2014 <- read_sav("data/VU2014SND.sav")

df_2014_subset <- df_snes_2014 %>% 
  dplyr::select(r1, # gender
                v7031, # age
                v7033, # education
                v7051, # income
                f78, # marital status
                f75, # religion (yes/no)
                v7024, # partisanship,
                f47a, f47d, # k: party reps for s and m
                f48a, f48b, f48d, # remaining k variables
                f34a, # reduce public spending
                f34e, # sell off public companies
                f34g, # more private healthcare
                f39b, # accept fewer refugees
                f42d, # focus on law and order
                f42e, # more gender equality
                f34i, # no nuclear
                f39a, # leave EU
                f39d, # join NATO
                v7029) # party choice weight

df_2014_subset <- df_2014_subset %>% 
  rename(d_gender = r1,
         d_age = v7031,
         d_education = v7033,
         d_income = v7051,
         d_marital_status = f78,
         d_religion = f75,
         d_partisanship = v7024,
         k_m_rep = f47a,
         k_s_rep = f47d,
         k_nat_insurance = f48a,
         k_num_mps = f48b,
         k_spain_eu = f48d,
         a_reduce_pub_spend = f34a,
         a_sell_pub_comp = f34e,
         a_priv_healthcare = f34g,
         a_fewer_refugees = f39b,
         a_law_order = f42d,
         a_gender_equal = f42e,
         a_no_nuclear = f34i,
         a_leave_eu = f39a,
         a_join_nato = f39d,
         weight = v7029)

df_2014_subset <- df_2014_subset %>% 
  mutate(d_gender = case_when(d_gender == 1 ~ "male",
                              d_gender == 2 ~ "female"),
         d_age = case_when(d_age == 1 ~ "18_30",
                           d_age == 2 ~ "31_60",
                           d_age == 3 ~ "61plus"),
         d_age = factor(d_age, levels = c("18_30", "31_60", "61plus")),
         d_education = case_when(d_education == 1 ~ "low_edu",
                                 d_education == 2 ~ "middle_edu",
                                 d_education == 3 ~ "high_edu",
                                 TRUE ~ NA_character_),
         d_education = factor(d_education, levels = c("low_edu", "middle_edu", "high_edu")),
         d_income = as_factor(d_income),
         d_income = case_when(d_income == "Mycket låg inkomst" ~ "inc_very_low",
                              d_income == "Ganska låg inkomst" ~ "inc_fairly_low",
                              d_income == "Varken låg eller hög inkomst" ~ "inc_medium",
                              d_income == "Ganska hög inkomst" ~ "inc_fairly_high",
                              d_income == "Mycket hög inkomst" ~ "inc_very_high"),
         d_income = factor(d_income, levels = c("inc_very_low", "inc_fairly_low", "inc_medium", "inc_fairly_high", "inc_very_high")),
         d_marital_status = as_factor(d_marital_status),
         d_marital_status = case_when(d_marital_status == "Ensamstående" ~ "single",
                                      d_marital_status == "Sambo" ~ "married",
                                      d_marital_status == "Gift/partnerskap" ~ "married",
                                      d_marital_status == "Änka/änkling" ~ "widow"),
         d_religion = case_when(d_religion == 1 ~ "yes",
                                d_religion == 5 ~ "no"),
         d_partisanship = as_factor(d_partisanship),
         k_m_rep = case_when(k_m_rep == 5 ~ 1,
                             k_m_rep == 1 ~ 0,
                             k_m_rep == 2 ~ 0,
                             k_m_rep == 3 ~ 0,
                             k_m_rep == 4 ~ 0,
                             k_m_rep == 6 ~ 0,
                             k_m_rep == 12 ~ 0,
                             k_m_rep == 88 ~ 0,
                             TRUE ~ NA_real_),
         k_s_rep = case_when(k_s_rep == 2 ~ 1,
                             k_s_rep == 1 ~ 0,
                             k_s_rep == 3 ~ 0,
                             k_s_rep == 4 ~ 0,
                             k_s_rep == 5 ~ 0,
                             k_s_rep == 6 ~ 0,
                             k_s_rep == 7 ~ 0,
                             k_s_rep == 8 ~ 0,
                             k_s_rep == 88 ~ 0,
                             TRUE ~ NA_real_),
         k_nat_insurance = case_when(k_nat_insurance == 5 ~ 1,
                                     k_nat_insurance == 1 ~ 0,
                                     k_nat_insurance == 8 ~ 0,
                                     TRUE ~ NA_real_),
         k_num_mps = case_when(k_num_mps == 1 ~ 1,
                               k_num_mps == 5 ~ 0,
                               k_num_mps == 8 ~ 0,
                               TRUE ~ NA_real_),
         k_spain_eu = case_when(k_spain_eu == 1 ~ 1,
                                k_spain_eu == 5 ~ 0,
                                k_spain_eu == 8 ~ 0,
                                TRUE ~ NA_real_),
         a_reduce_pub_spend = case_when(a_reduce_pub_spend < 3 ~ 1, # good proposal
                                        a_reduce_pub_spend > 2 ~ 0,
                                        TRUE ~ NA_real_),
         a_sell_pub_comp = case_when(a_sell_pub_comp < 3 ~ 1, # good proposal
                                     a_sell_pub_comp > 2 ~ 0,
                                     TRUE ~ NA_real_),
         a_priv_healthcare = case_when(a_priv_healthcare < 3 ~ 1, # good proposal
                                       a_priv_healthcare > 2 ~ 0,
                                       TRUE ~ NA_real_),
         a_fewer_refugees = case_when(a_fewer_refugees < 3 ~ 1, # good proposal
                                      a_fewer_refugees > 2 ~ 0,
                                      TRUE ~ NA_real_),
         a_law_order = case_when(a_law_order > 5 ~ 1, # good proposal
                                 a_fewer_refugees < 6 ~ 0,
                                 TRUE ~ NA_real_),
         a_gender_equal = case_when(a_gender_equal > 5 ~ 1, # good proposal
                                    a_gender_equal < 6 ~ 0,
                                    TRUE ~ NA_real_),
         a_no_nuclear = case_when(a_no_nuclear < 3 ~ 1, # good proposal
                                  a_no_nuclear > 2 ~ 0,
                                  TRUE ~ NA_real_),
         a_leave_eu = case_when(a_leave_eu < 3 ~ 1, # good proposal
                                a_leave_eu > 2 ~ 0,
                                TRUE ~ NA_real_),
         a_join_nato = case_when(a_join_nato < 3 ~ 1, # good proposal
                                 a_join_nato > 2 ~ 0,
                                 TRUE ~ NA_real_))

# 2010 data
df_snes_2010 <- read_sav("data/SND0876_VU2010.sav")

df_2010_subset <- df_snes_2010 %>% 
  dplyr::select(VU10_S1, # gender
                VU10_V7031, # age
                VU10_V7033, # education
                VU10_V7045, # income
                VU10_S7, # marital status
                VU10_V1122, # religion (yes/no)
                VU10_V7022, # partisanship,
                VU10_RF5, VU10_RF1, # k: party reps for s and m
                VU10_RF10, VU10_RF11, VU10_RF14, # remaining k variables
                VU10_V571, # reduce public spending
                VU10_V577, # sell off public companies
                VU10_V579, # more private healthcare
                VU10_V840, # accept fewer refugees
                VU10_V856, # focus on law and order
                VU10_V857, # more gender equality
                VU10_V581, # no nuclear
                VU10_V835, # leave EU
                VU10_V837, # join NATO
                VU10_V7018) # electoral participation weight

df_2010_subset <- df_2010_subset %>% 
  rename(d_gender = VU10_S1,
         d_age = VU10_V7031,
         d_education = VU10_V7033,
         d_income = VU10_V7045,
         d_marital_status = VU10_S7,
         d_religion = VU10_V1122,
         d_partisanship = VU10_V7022,
         k_m_rep = VU10_RF1,
         k_s_rep = VU10_RF5,
         k_nat_insurance = VU10_RF10,
         k_num_mps = VU10_RF11,
         k_spain_eu = VU10_RF14,
         a_reduce_pub_spend = VU10_V571,
         a_sell_pub_comp = VU10_V577,
         a_priv_healthcare = VU10_V579,
         a_fewer_refugees = VU10_V840,
         a_law_order = VU10_V856,
         a_gender_equal = VU10_V857,
         a_no_nuclear = VU10_V581,
         a_leave_eu = VU10_V835,
         a_join_nato = VU10_V837,
         weight = VU10_V7018)

df_2010_subset <- df_2010_subset %>% 
  mutate(d_gender = case_when(d_gender == 1 ~ "male",
                              d_gender == 2 ~ "female"),
         d_age = case_when(d_age == 1 ~ "18_30",
                           d_age == 2 ~ "31_60",
                           d_age == 3 ~ "61plus"),
         d_age = factor(d_age, levels = c("18_30", "31_60", "61plus")),
         d_education = case_when(d_education == 1 ~ "low_edu",
                                 d_education == 2 ~ "middle_edu",
                                 d_education == 3 ~ "high_edu",
                                 TRUE ~ NA_character_),
         d_education = factor(d_education, levels = c("low_edu", "middle_edu", "high_edu")),
         d_income = as_factor(d_income),
         d_income = case_when(d_income == "Mycket låg" ~ "inc_very_low",
                              d_income == "Ganska låg" ~ "inc_fairly_low",
                              d_income == "Varken låg eller hög" ~ "inc_medium",
                              d_income == "Ganska hög" ~ "inc_fairly_high",
                              d_income == "Mycket hög" ~ "inc_very_high"),
         d_income = factor(d_income, levels = c("inc_very_low", "inc_fairly_low", "inc_medium", "inc_fairly_high", "inc_very_high")),
         d_marital_status = as_factor(d_marital_status),
         d_marital_status = case_when(d_marital_status == "Ogift" ~ "single",
                                      d_marital_status == "Skild" ~ "single",
                                      d_marital_status == "SP" ~ "single",
                                      d_marital_status == "Gift" ~ "married",
                                      d_marital_status == "Änka/änkeman" ~ "widow"),
         d_religion = case_when(d_religion == 1 ~ "no",
                                d_religion == 2 ~ "no",
                                d_religion == 3 ~ "yes",
                                d_religion == 4 ~ "yes",
                                TRUE ~ NA_character_),
         d_partisanship = as_factor(d_partisanship),
         a_reduce_pub_spend = case_when(a_reduce_pub_spend < 3 ~ 1, # good proposal
                                        a_reduce_pub_spend > 2 & a_reduce_pub_spend < 88 ~ 0,
                                        TRUE ~ NA_real_),
         a_sell_pub_comp = case_when(a_sell_pub_comp < 3 ~ 1, # good proposal
                                     a_sell_pub_comp > 2 & a_sell_pub_comp < 88 ~ 0,
                                     TRUE ~ NA_real_),
         a_priv_healthcare = case_when(a_priv_healthcare < 3 ~ 1, # good proposal
                                       a_priv_healthcare > 2 & a_priv_healthcare < 88 ~ 0,
                                       TRUE ~ NA_real_),
         a_fewer_refugees = case_when(a_priv_healthcare < 3 ~ 1, # good proposal
                                      a_priv_healthcare > 2 & a_priv_healthcare < 88 ~ 0,
                                      TRUE ~ NA_real_),
         a_law_order = case_when(a_law_order > 5 ~ 1, # good proposal
                                 a_fewer_refugees < 6 ~ 0,
                                 TRUE ~ NA_real_),
         a_gender_equal = case_when(a_gender_equal > 5 ~ 1, # good proposal
                                    a_gender_equal < 6 ~ 0,
                                    TRUE ~ NA_real_),
         a_no_nuclear = case_when(a_no_nuclear < 3 ~ 1, # good proposal
                                  a_no_nuclear > 2 & a_no_nuclear < 88 ~ 0,
                                  TRUE ~ NA_real_),
         a_leave_eu = case_when(a_leave_eu < 3 ~ 1, # good proposal
                                a_leave_eu > 2 & a_leave_eu < 88 ~ 0,
                                TRUE ~ NA_real_),
         a_join_nato = case_when(a_join_nato < 3 ~ 1, # good proposal
                                 a_join_nato > 2 & a_join_nato < 88 ~ 0,
                                 TRUE ~ NA_real_))

# 2006 data
df_snes_2006 <- read_sav("data/0861us.sav")

df_2006_subset <- df_snes_2006 %>% 
  dplyr::select(sex, # gender
                age3, # age
                utbny, # education
                ink5, # income
                c9, # marital status
                v779, # religion (yes/no)
                pipa, # partisanship,
                v603, v605, # k: party reps for s and m (2 and 5 correct, respectively)
                v611, v612, v614, # remaining k variables (5, 1, 1 correct)
                v400, # reduce public spending
                v405, # sell off public companies
                v407, # more private healthcare
                v547, # accept fewer refugees
                v562, # focus on law and order
                v563, # more gender equality
                v410, # no nuclear
                v542, # leave EU
                v544) # join NATO
                # electoral participation weight missing!

df_2006_subset <- df_2006_subset %>% 
  rename(d_gender = sex,
         d_age = age3,
         d_education = utbny,
         d_income = ink5,
         d_marital_status = c9,
         d_religion = v779,
         d_partisanship = pipa,
         k_m_rep = v605,
         k_s_rep = v603,
         k_nat_insurance = v611,
         k_num_mps = v612,
         k_spain_eu = v614,
         a_reduce_pub_spend = v400,
         a_sell_pub_comp = v405,
         a_priv_healthcare = v407,
         a_fewer_refugees = v547,
         a_law_order = v562,
         a_gender_equal = v563,
         a_no_nuclear = v410,
         a_leave_eu = v542,
         a_join_nato = v544) %>% 
  mutate(weight = NA_real_)

df_2006_subset <- df_2006_subset %>% 
  mutate(d_gender = case_when(d_gender == 1 ~ "male",
                              d_gender == 2 ~ "female"),
         d_age = case_when(d_age == 1 ~ "18_30",
                           d_age == 2 ~ "31_60",
                           d_age == 3 ~ "61plus"),
         d_age = factor(d_age, levels = c("18_30", "31_60", "61plus")),
         d_education = case_when(d_education == 1 ~ "low_edu",
                                 d_education == 2 ~ "middle_edu",
                                 d_education == 3 ~ "high_edu",
                                 TRUE ~ NA_character_),
         d_education = factor(d_education, levels = c("low_edu", "middle_edu", "high_edu")),
         d_income = as_factor(d_income),
         d_income = case_when(d_income == 1 ~ "inc_very_low",
                              d_income == 2 ~ "inc_fairly_low",
                              d_income == 3 ~ "inc_medium",
                              d_income == 4 ~ "inc_fairly_high",
                              d_income == 5 ~ "inc_very_high"),
         d_income = factor(d_income, levels = c("inc_very_low", "inc_fairly_low", "inc_medium", "inc_fairly_high", "inc_very_high")),
         d_marital_status = as_factor(d_marital_status),
         d_marital_status = case_when(d_marital_status == "OG" ~ "single",
                                      d_marital_status == "S" ~ "single",
                                      d_marital_status == "SP" ~ "single",
                                      d_marital_status == "G" ~ "married",
                                      d_marital_status == "Ä" ~ "widow"),
         d_religion = case_when(d_religion == 1 ~ "no",
                                d_religion == 2 ~ "no",
                                d_religion == 3 ~ "yes",
                                d_religion == 4 ~ "yes",
                                TRUE ~ NA_character_),
         d_partisanship = as_factor(d_partisanship),
         d_partisanship = case_when(d_partisanship == 1 ~ "Vänsterpartiet",
                                    d_partisanship == 2 ~ "Socialdemokraterna",
                                    d_partisanship == 3 ~ "Centerpartiet",
                                    d_partisanship == 4 ~ "Folkpartiet",
                                    d_partisanship == 5 ~ "Moderaterna",
                                    d_partisanship == 6 ~ "Kristdemokraterna",
                                    d_partisanship == 7 ~ "Miljöpartiet",
                                    d_partisanship == 9 ~ "Annat parti",
                                    d_partisanship == 20 ~ "Sverigedemokraterna",
                                    d_partisanship == 21 ~ "FI",
                                    d_partisanship == 22 ~ "Junilistan",
                                    TRUE ~ NA_character_),
         k_m_rep = case_when(k_m_rep == 5 ~ 1,
                             k_m_rep == 1 ~ 0,
                             k_m_rep == 2 ~ 0,
                             k_m_rep == 3 ~ 0,
                             k_m_rep == 4 ~ 0,
                             k_m_rep == 6 ~ 0,
                             k_m_rep == 7 ~ 0,
                             k_m_rep == 9 ~ 0,
                             k_m_rep == 8888 ~ 0,
                             TRUE ~ NA_real_),
         k_s_rep = case_when(k_s_rep == 2 ~ 1,
                             k_s_rep == 1 ~ 0,
                             k_s_rep == 3 ~ 0,
                             k_s_rep == 4 ~ 0,
                             k_s_rep == 5 ~ 0,
                             k_s_rep == 6 ~ 0,
                             k_s_rep == 7 ~ 0,
                             k_s_rep == 8888 ~ 0,
                             TRUE ~ NA_real_),
         k_nat_insurance = case_when(k_nat_insurance == 5 ~ 1,
                                     k_nat_insurance == 1 ~ 0,
                                     k_nat_insurance == 8888 ~ 0,
                                     TRUE ~ NA_real_),
         k_num_mps = case_when(k_num_mps == 1 ~ 1,
                               k_num_mps == 5 ~ 0,
                               k_num_mps == 8888 ~ 0,
                               TRUE ~ NA_real_),
         k_spain_eu = case_when(k_spain_eu == 1 ~ 1,
                                k_spain_eu == 5 ~ 0,
                                k_spain_eu == 8888 ~ 0,
                                TRUE ~ NA_real_),
         a_reduce_pub_spend = case_when(a_reduce_pub_spend < 3 ~ 1, # good proposal
                                        a_reduce_pub_spend > 2 & a_reduce_pub_spend < 88 ~ 0,
                                        TRUE ~ NA_real_),
         a_sell_pub_comp = case_when(a_sell_pub_comp < 3 ~ 1, # good proposal
                                     a_sell_pub_comp > 2 & a_sell_pub_comp < 88 ~ 0,
                                     TRUE ~ NA_real_),
         a_priv_healthcare = case_when(a_priv_healthcare < 3 ~ 1, # good proposal
                                       a_priv_healthcare > 2 & a_priv_healthcare < 88 ~ 0,
                                       TRUE ~ NA_real_),
         a_fewer_refugees = case_when(a_priv_healthcare < 3 ~ 1, # good proposal
                                      a_priv_healthcare > 2 & a_priv_healthcare < 88 ~ 0,
                                      TRUE ~ NA_real_),
         a_law_order = case_when(a_law_order > 5 & a_law_order < 88 ~ 1, # good proposal
                                 a_fewer_refugees < 6 ~ 0,
                                 TRUE ~ NA_real_),
         a_gender_equal = case_when(a_gender_equal > 5 & a_gender_equal < 88 ~ 1, # good proposal
                                    a_gender_equal < 6 ~ 0,
                                    TRUE ~ NA_real_),
         a_no_nuclear = case_when(a_no_nuclear < 3 ~ 1, # good proposal
                                  a_no_nuclear > 2 & a_no_nuclear < 88 ~ 0,
                                  TRUE ~ NA_real_),
         a_leave_eu = case_when(a_leave_eu < 3 ~ 1, # good proposal
                                a_leave_eu > 2 & a_leave_eu < 88 ~ 0,
                                TRUE ~ NA_real_),
         a_join_nato = case_when(a_join_nato < 3 ~ 1, # good proposal
                                 a_join_nato > 2 & a_join_nato < 88 ~ 0,
                                 TRUE ~ NA_real_))

# 2002 data
df_snes_2002 <- read_sav("data/0812EV.SAV")

df_2002_subset <- df_snes_2002 %>% 
  dplyr::select(V12, # gender
                V584, # age
                V592, # education
                V599, # income
                V587, # marital status
                V509, # religion (yes/no)
                V236, # partisanship,
                V290, V286, # k: party reps for s and m (2 and 5 correct, respectively)
                V291, V292, V295, # remaining k variables (5, 1, 1 correct)
                V147, # reduce public spending
                V152, # sell off public companies
                V154, # more private healthcare
                V206, # accept fewer refugees
                V228, # focus on law and order
                V229, # more gender equality
                V166, # no nuclear
                V209, # leave EU
                V211) # join NATO
# electoral participation weight missing!

df_2002_subset <- df_2002_subset %>% 
  rename(d_gender = V12,
         d_age = V584,
         d_education = V592,
         d_income = V599,
         d_marital_status = V587,
         d_religion = V509,
         d_partisanship = V236,
         k_m_rep = V286,
         k_s_rep = V290,
         k_nat_insurance = V291,
         k_num_mps = V292,
         k_spain_eu = V295,
         a_reduce_pub_spend = V147,
         a_sell_pub_comp = V152,
         a_priv_healthcare = V154,
         a_fewer_refugees = V206,
         a_law_order = V228,
         a_gender_equal = V229,
         a_no_nuclear = V166,
         a_leave_eu = V209,
         a_join_nato = V211) %>% 
  mutate(weight = NA_real_)

df_2002_subset <- df_2002_subset %>% 
  mutate(d_gender = case_when(d_gender == 1 ~ "male",
                              d_gender == 2 ~ "female"),
         d_age = case_when(d_age == 1 ~ "18_30",
                           d_age == 2 ~ "31_60",
                           d_age == 3 ~ "61plus"),
         d_age = factor(d_age, levels = c("18_30", "31_60", "61plus")),
         d_education = case_when(d_education == 1 ~ "low_edu",
                                 d_education == 2 ~ "middle_edu",
                                 d_education == 3 ~ "high_edu",
                                 TRUE ~ NA_character_),
         d_education = factor(d_education, levels = c("low_edu", "middle_edu", "high_edu")),
         d_income = as_factor(d_income),
         d_income = case_when(d_income == "Very low income (15% lowest incomes)" ~ "inc_very_low",
                              d_income == "Low income (16-35 percentile)" ~ "inc_fairly_low",
                              d_income == "Middle income (36-65 percentile)" ~ "inc_medium",
                              d_income == "High income (66-85 percentile)" ~ "inc_fairly_high",
                              d_income == "Very high income (15% highest incomes)" ~ "inc_very_high"),
         d_income = factor(d_income, levels = c("inc_very_low", "inc_fairly_low", "inc_medium", "inc_fairly_high", "inc_very_high")),
         d_marital_status = as_factor(d_marital_status),
         d_marital_status = case_when(d_marital_status == "Not married" ~ "single",
                                      d_marital_status == "Married" ~ "married",
                                      d_marital_status == "Divorced" ~ "single",
                                      d_marital_status == "Widow/widower" ~ "widow",
                                      d_marital_status == "Registred partner" ~ "married"),
         d_religion = case_when(d_religion == 1 ~ "yes",
                                d_religion == 2 ~ "yes",
                                d_religion == 3 ~ "no",
                                d_religion == 4 ~ "no",
                                TRUE ~ NA_character_),
         d_partisanship = as_factor(d_partisanship),
         d_partisanship = case_when(d_partisanship == "Left Party" ~ "Vänsterpartiet",
                                    d_partisanship == "Social Democrats" ~ "Socialdemokraterna",
                                    d_partisanship == "Centre Party" ~ "Centerpartiet",
                                    d_partisanship == "People's Party Liberals" ~ "Folkpartiet",
                                    d_partisanship == "Moderate Party" ~ "Moderaterna",
                                    d_partisanship == "Christian Democrats" ~ "Kristdemokraterna",
                                    d_partisanship == "Green Party" ~ "Miljöpartiet",
                                    d_partisanship == "Other party" ~ "Annat parti",
                                    TRUE ~ NA_character_),
         k_m_rep = case_when(k_m_rep == 5 ~ 1,
                             k_m_rep == 88 ~ 0,
                             TRUE ~ NA_real_),
         k_s_rep = case_when(k_s_rep == 2 ~ 1,
                             k_s_rep == 88 ~ 0,
                             TRUE ~ NA_real_),
         k_nat_insurance = case_when(k_nat_insurance == 5 ~ 1,
                                     k_nat_insurance == 1 ~ 0,
                                     k_nat_insurance == 8 ~ 0,
                                     TRUE ~ NA_real_),
         k_num_mps = case_when(k_num_mps == 1 ~ 1,
                               k_num_mps == 5 ~ 0,
                               k_num_mps == 8 ~ 0,
                               TRUE ~ NA_real_),
         k_spain_eu = case_when(k_spain_eu == 1 ~ 1,
                                k_spain_eu == 5 ~ 0,
                                k_spain_eu == 8 ~ 0,
                                TRUE ~ NA_real_),
         a_reduce_pub_spend = case_when(a_reduce_pub_spend < 3 ~ 1, # good proposal
                                        a_reduce_pub_spend > 2 & a_reduce_pub_spend < 8 ~ 0,
                                        TRUE ~ NA_real_),
         a_sell_pub_comp = case_when(a_sell_pub_comp < 3 ~ 1, # good proposal
                                     a_sell_pub_comp > 2 & a_sell_pub_comp < 8 ~ 0,
                                     TRUE ~ NA_real_),
         a_priv_healthcare = case_when(a_priv_healthcare < 3 ~ 1, # good proposal
                                       a_priv_healthcare > 2 & a_priv_healthcare < 8 ~ 0,
                                       TRUE ~ NA_real_),
         a_fewer_refugees = case_when(a_priv_healthcare < 3 ~ 1, # good proposal
                                      a_priv_healthcare > 2 & a_priv_healthcare < 8 ~ 0,
                                      TRUE ~ NA_real_),
         a_law_order = case_when(a_law_order > 5 & a_law_order < 88 ~ 1, # good proposal
                                 a_fewer_refugees < 6 ~ 0,
                                 TRUE ~ NA_real_),
         a_gender_equal = case_when(a_gender_equal > 5 & a_gender_equal < 88 ~ 1, # good proposal
                                    a_gender_equal < 6 ~ 0,
                                    TRUE ~ NA_real_),
         a_no_nuclear = case_when(a_no_nuclear < 3 ~ 1, # good proposal
                                  a_no_nuclear > 2 & a_no_nuclear < 8 ~ 0,
                                  TRUE ~ NA_real_),
         a_leave_eu = case_when(a_leave_eu < 3 ~ 1, # good proposal
                                a_leave_eu > 2 & a_leave_eu < 8 ~ 0,
                                TRUE ~ NA_real_),
         a_join_nato = case_when(a_join_nato < 3 ~ 1, # good proposal
                                 a_join_nato > 2 & a_join_nato < 8 ~ 0,
                                 TRUE ~ NA_real_))

# 1998 data
df_snes_1998 <- read_sav("data/0750EV.SAV")

df_1998_subset <- df_snes_1998 %>% 
  dplyr::select(v12, # gender
                v463, # age
                v475, # education
                v485, # income
                v486, # marital status
                v398, # religion (yes/no)
                v187, # partisanship,
                v213, v215, # k: party reps for s and m (22 and 55 correct, respectively)
                v219, v220, v224, # remaining k variables (5, 1, 1 correct)
                v137, # reduce public spending
                v141, # sell off public companies
                v145, # more private healthcare
                v151, # accept fewer refugees
                v178, # focus on law and order
                v179, # more gender equality
                v153, # no nuclear
                v156, # leave EU
                v157) # join NATO
# electoral participation weight missing!

df_1998_subset <- df_1998_subset %>% 
  rename(d_gender = v12,
         d_age = v463,
         d_education = v475,
         d_income = v485,
         d_marital_status = v486,
         d_religion = v398,
         d_partisanship = v187,
         k_m_rep = v215,
         k_s_rep = v213,
         k_nat_insurance = v219,
         k_num_mps = v220,
         k_spain_eu = v224,
         a_reduce_pub_spend = v137,
         a_sell_pub_comp = v141,
         a_priv_healthcare = v145,
         a_fewer_refugees = v151,
         a_law_order = v178,
         a_gender_equal = v179,
         a_no_nuclear = v153,
         a_leave_eu = v156,
         a_join_nato = v157) %>% 
  mutate(weight = NA_real_)

df_1998_subset <- df_1998_subset %>% 
  mutate(d_gender = case_when(d_gender == 1 ~ "male",
                              d_gender == 2 ~ "female"),
         d_age = case_when(d_age == 1 ~ "18_30",
                           d_age == 2 ~ "31_60",
                           d_age == 3 ~ "61plus"),
         d_age = factor(d_age, levels = c("18_30", "31_60", "61plus")),
         d_education = case_when(d_education == 1 ~ "low_edu",
                                 d_education == 2 ~ "middle_edu",
                                 d_education == 3 ~ "high_edu",
                                 TRUE ~ NA_character_),
         d_education = factor(d_education, levels = c("low_edu", "middle_edu", "high_edu")),
         d_income = as_factor(d_income),
         d_income = case_when(d_income == "0 - 49 999 SEK" ~ "inc_very_low",
                              d_income == "50 000 - 124 999 SEK" ~ "inc_fairly_low",
                              d_income == "125 000 - 189 999 SEK" ~ "inc_medium",
                              d_income == "190 000 - 249 999 SEK" ~ "inc_fairly_high",
                              d_income == "250 000 SEK -" ~ "inc_very_high"),
         d_income = factor(d_income, levels = c("inc_very_low", "inc_fairly_low", "inc_medium", "inc_fairly_high", "inc_very_high")),
         d_marital_status = as_factor(d_marital_status),
         d_marital_status = case_when(d_marital_status == "Not married" ~ "single",
                                      d_marital_status == "Married man" ~ "married",
                                      d_marital_status == "Married woman, not living together with her husband" ~ "married",
                                      d_marital_status == "Divorced" ~ "single",
                                      d_marital_status == "Widow/widower" ~ "widow",
                                      d_marital_status == "Married woman, living together with her husband" ~ "married",
                                      d_marital_status == "Child under the age of 18" ~ "single",
                                      d_marital_status == "Foster child" ~ "married"),
         d_religion = case_when(d_religion == 1 ~ "yes",
                                d_religion == 2 ~ "yes",
                                d_religion == 3 ~ "no",
                                d_religion == 4 ~ "no",
                                TRUE ~ NA_character_),
         d_partisanship = as_factor(d_partisanship),
         d_partisanship = case_when(d_partisanship == "Left Party" ~ "Vänsterpartiet",
                                    d_partisanship == "Social Democrats" ~ "Socialdemokraterna",
                                    d_partisanship == "Centre Party" ~ "Centerpartiet",
                                    d_partisanship == "People's Party Liberals" ~ "Folkpartiet",
                                    d_partisanship == "Moderate Party" ~ "Moderaterna",
                                    d_partisanship == "Christian Democrats" ~ "Kristdemokraterna",
                                    d_partisanship == "Green Party" ~ "Miljöpartiet",
                                    d_partisanship == "Other party" ~ "Annat parti",
                                    TRUE ~ NA_character_),
         k_m_rep = case_when(k_m_rep == 55 ~ 1,
                             k_m_rep == 88 ~ 0,
                             TRUE ~ NA_real_),
         k_s_rep = case_when(k_s_rep == 22 ~ 1,
                             k_s_rep == 88 ~ 0,
                             TRUE ~ NA_real_),
         k_nat_insurance = case_when(k_nat_insurance == 5 ~ 1,
                                     k_nat_insurance == 1 ~ 0,
                                     k_nat_insurance == 8 ~ 0,
                                     TRUE ~ NA_real_),
         k_num_mps = case_when(k_num_mps == 1 ~ 1,
                               k_num_mps == 5 ~ 0,
                               k_num_mps == 8 ~ 0,
                               TRUE ~ NA_real_),
         k_spain_eu = case_when(k_spain_eu == 1 ~ 1,
                                k_spain_eu == 5 ~ 0,
                                k_spain_eu == 8 ~ 0,
                                TRUE ~ NA_real_),
         a_reduce_pub_spend = case_when(a_reduce_pub_spend < 3 ~ 1, # good proposal
                                        a_reduce_pub_spend > 2 & a_reduce_pub_spend < 8 ~ 0,
                                        TRUE ~ NA_real_),
         a_sell_pub_comp = case_when(a_sell_pub_comp < 3 ~ 1, # good proposal
                                     a_sell_pub_comp > 2 & a_sell_pub_comp < 8 ~ 0,
                                     TRUE ~ NA_real_),
         a_priv_healthcare = case_when(a_priv_healthcare < 3 ~ 1, # good proposal
                                       a_priv_healthcare > 2 & a_priv_healthcare < 8 ~ 0,
                                       TRUE ~ NA_real_),
         a_fewer_refugees = case_when(a_priv_healthcare < 3 ~ 1, # good proposal
                                      a_priv_healthcare > 2 & a_priv_healthcare < 8 ~ 0,
                                      TRUE ~ NA_real_),
         a_law_order = case_when(a_law_order > 5 & a_law_order < 88 ~ 1, # good proposal
                                 a_fewer_refugees < 6 ~ 0,
                                 TRUE ~ NA_real_),
         a_gender_equal = case_when(a_gender_equal > 5 & a_gender_equal < 88 ~ 1, # good proposal
                                    a_gender_equal < 6 ~ 0,
                                    TRUE ~ NA_real_),
         a_no_nuclear = case_when(a_no_nuclear < 3 ~ 1, # good proposal
                                  a_no_nuclear > 2 & a_no_nuclear < 8 ~ 0,
                                  TRUE ~ NA_real_),
         a_leave_eu = case_when(a_leave_eu < 3 ~ 1, # good proposal
                                a_leave_eu > 2 & a_leave_eu < 8 ~ 0,
                                TRUE ~ NA_real_),
         a_join_nato = case_when(a_join_nato < 3 ~ 1, # good proposal
                                 a_join_nato > 2 & a_join_nato < 8 ~ 0,
                                 TRUE ~ NA_real_))

# combine all years
df_1998_subset$year <- "1998"
df_2002_subset$year <- "2002"
df_2006_subset$year <- "2006"
df_2010_subset$year <- "2010"
df_2014_subset$year <- "2014"

df_all_years <- rbind(df_1998_subset,
                      df_2002_subset,
                      df_2006_subset,
                      df_2010_subset,
                      df_2014_subset)

table(df_all_years$d_gender)
table(df_all_years$d_age)
table(df_all_years$d_education)
table(df_all_years$d_income)
table(df_all_years$d_marital_status)
table(df_all_years$d_religion)
table(df_all_years$d_partisanship)

df_all_years <- df_all_years %>% 
  mutate(d_partisanship = case_when(d_partisanship == "Annat parti" ~ "annat_parti",
                                    d_partisanship == "Annat parti (inkl PP)" ~ "annat_parti",
                                    d_partisanship == "Centerpartiet" ~ "centerpartiet",
                                    d_partisanship == "Feministiskt initiativ" ~ "annat_parti",
                                    d_partisanship == "Feministiskt intitiativ" ~ "annat_parti",
                                    d_partisanship == "FI" ~ "annat_parti",
                                    d_partisanship == "Folkpartiet" ~ "folkpartiet",
                                    d_partisanship == "Junilistan" ~ "annat_parti",
                                    d_partisanship == "Kristdemokraterna" ~ "kristdemokraterna",
                                    d_partisanship == "Miljöpartiet" ~ "miljöpartiet",
                                    d_partisanship == "Moderaterna" ~ "moderaterna",
                                    d_partisanship == "Övriga partier" ~ "annat_parti",
                                    d_partisanship == "Piratpartiet" ~ "annat_parti",
                                    d_partisanship == "Socialdemokraterna" ~ "socialdemokraterna",
                                    d_partisanship == "Sverigedemokraterna" ~ "sverigedemokraterna",
                                    d_partisanship == "Vänsterpartiet" ~ "vänsterpartiet"))

saveRDS(df_all_years, "data/df_all_years.rds")
