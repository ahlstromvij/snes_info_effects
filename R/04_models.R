set.seed(100)

library(tidyverse)

df_all_years <- readRDS("data/df_all_years_scales.rds")

# fit models
m_reduce_pub_spend <- glm(a_reduce_pub_spend ~ 
                            knowledge_binary +
                            year:knowledge_binary +
                            d_age +
                            d_gender +
                            d_religion +
                            d_education +
                            d_marital_status +
                            d_income,
                          data = df_all_years, 
                          family = "binomial",
                          weights = prop_score)

m_sell_pub_comp  <- glm(a_sell_pub_comp ~ 
                          knowledge_binary +
                          year:knowledge_binary +
                          d_age +
                          d_gender +
                          d_religion +
                          d_education +
                          d_marital_status +
                          d_income,
                        data = df_all_years, 
                        family = "binomial",
                        weights = prop_score)

m_priv_healthcare  <- glm(a_priv_healthcare ~ 
                            knowledge_binary +
                            year:knowledge_binary +
                            d_age +
                            d_gender +
                            d_religion +
                            d_education +
                            d_marital_status +
                            d_income,
                          data = df_all_years, 
                          family = "binomial",
                          weights = prop_score)

m_fewer_refugees  <- glm(a_fewer_refugees ~ 
                           knowledge_binary +
                           year:knowledge_binary +
                           d_age +
                           d_gender +
                           d_religion +
                           d_education +
                           d_marital_status +
                           d_income,
                         data = df_all_years, 
                         family = "binomial",
                         weights = prop_score)

m_law_order  <- glm(a_law_order ~ 
                      knowledge_binary +
                      year:knowledge_binary +
                      d_age +
                      d_gender +
                      d_religion +
                      d_education +
                      d_marital_status +
                      d_income,
                    data = df_all_years, 
                    family = "binomial",
                    weights = prop_score)

m_gender_equal  <- glm(a_gender_equal ~ 
                         knowledge_binary +
                         year:knowledge_binary +
                         d_age +
                         d_gender +
                         d_religion +
                         d_education +
                         d_marital_status +
                         d_income,
                       data = df_all_years, 
                       family = "binomial",
                       weights = prop_score)

m_no_nuclear  <- glm(a_no_nuclear ~ 
                       knowledge_binary +
                       year:knowledge_binary +
                       d_age +
                       d_gender +
                       d_religion +
                       d_education +
                       d_marital_status +
                       d_income,
                     data = df_all_years, 
                     family = "binomial",
                     weights = prop_score)

m_leave_eu  <- glm(a_leave_eu ~ 
                     knowledge_binary +
                     year:knowledge_binary +
                     d_age +
                     d_gender +
                     d_religion +
                     d_education +
                     d_marital_status +
                     d_income,
                   data = df_all_years, 
                   family = "binomial",
                   weights = prop_score)

m_join_nato  <- glm(a_join_nato ~ 
                      knowledge_binary +
                      year:knowledge_binary +
                      d_age +
                      d_gender +
                      d_religion +
                      d_education +
                      d_marital_status +
                      d_income,
                    data = df_all_years, 
                    family = "binomial",
                    weights = prop_score)

df_informed <- df_all_years %>% 
  mutate(knowledge_binary = 1)

df_informed$a_reduce_pub_spend_informed <- predict(m_reduce_pub_spend, newdata = df_informed, type = "response")
df_informed$a_sell_pub_comp_informed <- predict(m_sell_pub_comp, newdata = df_informed, type = "response")
df_informed$a_priv_healthcare_informed <- predict(m_priv_healthcare, newdata = df_informed, type = "response")
df_informed$a_fewer_refugees_informed <- predict(m_fewer_refugees, newdata = df_informed, type = "response")
df_informed$a_law_order_informed <- predict(m_law_order, newdata = df_informed, type = "response")
df_informed$a_gender_equal_informed <- predict(m_gender_equal, newdata = df_informed, type = "response")
df_informed$a_no_nuclear_informed <- predict(m_no_nuclear, newdata = df_informed, type = "response")
df_informed$a_leave_eu_informed <- predict(m_leave_eu, newdata = df_informed, type = "response")
df_informed$a_join_nato_informed <- predict(m_join_nato, newdata = df_informed, type = "response")

png(file="plots/info_effect_over_time.png", width = 10, height = 8, units = 'in', res = 300)
df_informed %>% 
  group_by(year) %>% 
  summarise("Reduce public sector" = mean(a_reduce_pub_spend_informed) - mean(a_reduce_pub_spend),
            "Privatise state-owned businesses" = mean(a_sell_pub_comp_informed) - mean(a_sell_pub_comp),
            "Increase private health care" = mean(a_priv_healthcare_informed) - mean(a_priv_healthcare),
            "Accept fewer refugees" = mean(a_fewer_refugees_informed) - mean(a_fewer_refugees),
            "More law and order" = mean(a_law_order_informed) - mean(a_law_order),
            "More gender equality" = mean(a_gender_equal_informed) - mean(a_gender_equal),
            "Abolish nuclear power" = mean(a_no_nuclear_informed) - mean(a_no_nuclear),
            "Leave the EU" = mean(a_leave_eu_informed) - mean(a_leave_eu),
            "Join NATO" = mean(a_join_nato_informed) - mean(a_join_nato))  %>% 
  pivot_longer(cols = -year,
               names_to = "variable",
               values_to = "difference")  %>% 
  mutate(difference = difference * 100) %>% 
  ggplot() +
  aes(x = year, y = difference, color = variable) +
  geom_point() +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Information effects in the Swedish electorate over time",
       subtitle = "Effects measure differences between actual and simulated fully informed levels of support",
       caption = "Data: SNES 1998, 2002, 2006, 2010 and 2014",
       x = "",
       y = "Difference (percentage points)") +
  theme(plot.title = element_text(face="bold")) +
  theme(legend.position="none")
dev.off()
