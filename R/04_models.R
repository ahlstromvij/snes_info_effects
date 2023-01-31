set.seed(100)

library(tidyverse)
library(ggeffects)
library(multcomp)

df_all_years <- readRDS("data/df_all_years_scales.rds")

# add party preference dummies
table(df_all_years$d_partisanship)
df_all_years <- df_all_years %>% 
  mutate(right_party = case_when(d_partisanship == "centerpartiet" |
                                     d_partisanship == "folkpartiet" |
                                     d_partisanship == "kristdemokraterna" |
                                     d_partisanship == "moderaterna" |
                                     d_partisanship == "sverigedemokraterna" ~ 1,
                                   TRUE ~ 0),
         left_party = case_when(d_partisanship == "miljöpartiet" |
                                    d_partisanship == "socialdemokraterna" |
                                    d_partisanship == "vänsterpartiet" ~ 1,
                                  TRUE ~ 0),
         no_party = case_when(d_partisanship == "inget_parti" ~ 1,
                              TRUE ~ 0))

df_all_years %>% 
  group_by(year) %>% 
  summarise("right parties" = mean(right_party),
            "left parties" = mean(left_party),
            "no party" = mean(no_party)) %>% 
  pivot_longer(cols = -year,
               names_to = "party",
               values_to = "proportion") %>% 
  ggplot() +
  aes(x = year, y = proportion, color = party, group = party) +
  geom_point() +
  geom_line()

# probability of affirmation as a function of knowledge (continuous) and covariates
m_reduce_pub_spend_cont <- glm(a_reduce_pub_spend ~ 
                                 knowledge +
                                 year:knowledge +
                                 d_age +
                                 d_gender +
                                 d_class +
                                 d_education +
                                 d_income,
                               data = df_all_years, 
                               family = "binomial")

m_sell_pub_comp_cont <- glm(a_sell_pub_comp ~ 
                                 knowledge +
                                 year:knowledge +
                                 d_age +
                                 d_gender +
                                 d_class +
                                 d_education +
                                 d_income,
                               data = df_all_years, 
                               family = "binomial")

m_priv_healthcare_cont <- glm(a_priv_healthcare ~ 
                              knowledge +
                              year:knowledge +
                              d_age +
                              d_gender +
                              d_class +
                              d_education +
                              d_income,
                            data = df_all_years, 
                            family = "binomial")

m_fewer_refugees_cont <- glm(a_fewer_refugees ~ 
                                knowledge +
                                year:knowledge +
                                d_age +
                                d_gender +
                                d_class +
                                d_education +
                                d_income,
                              data = df_all_years, 
                              family = "binomial")

m_law_order_cont <- glm(a_law_order ~ 
                               knowledge +
                               year:knowledge +
                               d_age +
                               d_gender +
                               d_class +
                               d_education +
                               d_income,
                             data = df_all_years, 
                             family = "binomial")

m_gender_equal_cont <- glm(a_gender_equal ~ 
                          knowledge +
                          year:knowledge +
                          d_age +
                          d_gender +
                          d_class +
                          d_education +
                          d_income,
                        data = df_all_years, 
                        family = "binomial")

m_no_nuclear_cont <- glm(a_no_nuclear ~ 
                           knowledge +
                           year:knowledge +
                           d_age +
                           d_gender +
                           d_class +
                           d_education +
                           d_income,
                         data = df_all_years, 
                         family = "binomial")

m_leave_eu_cont <- glm(a_leave_eu ~ 
                         knowledge +
                         year:knowledge +
                         d_age +
                         d_gender +
                         d_class +
                         d_education +
                         d_income,
                       data = df_all_years, 
                       family = "binomial")

m_join_nato_cont <- glm(a_join_nato ~ 
                          knowledge +
                          year:knowledge +
                          d_age +
                          d_gender +
                          d_class +
                          d_education +
                          d_income,
                        data = df_all_years, 
                        family = "binomial")


m_right_party_cont <- glm(right_party ~ 
                          knowledge +
                          year:knowledge +
                          d_age +
                          d_gender +
                          d_class +
                          d_education +
                          d_income,
                        data = df_all_years, 
                        family = "binomial")

m_left_party_cont <- glm(left_party ~ 
                            knowledge +
                            year:knowledge +
                            d_age +
                            d_gender +
                            d_class +
                            d_education +
                            d_income,
                          data = df_all_years, 
                          family = "binomial")

m_no_party_cont <- glm(no_party ~ 
                           knowledge +
                           year:knowledge +
                           d_age +
                           d_gender +
                           d_class +
                           d_education +
                           d_income,
                         data = df_all_years, 
                         family = "binomial")

pred_reduce_pub_spend <- ggpredict(m_reduce_pub_spend_cont, c("year","knowledge"))
pred_sell_pub_comp <- ggpredict(m_sell_pub_comp_cont, c("year","knowledge"))
pred_priv_healthcare <- ggpredict(m_priv_healthcare_cont, c("year","knowledge"))
pred_fewer_refugees <- ggpredict(m_fewer_refugees_cont, c("year","knowledge"))
pred_law_order <- ggpredict(m_law_order_cont, c("year","knowledge"))
pred_gender_equal <- ggpredict(m_gender_equal_cont, c("year","knowledge"))

pred_no_nuclear <- ggpredict(m_no_nuclear_cont, c("year","knowledge"))
pred_leave_eu <- ggpredict(m_leave_eu_cont, c("year","knowledge"))
pred_join_nato <- ggpredict(m_join_nato_cont, c("year","knowledge"))
pred_right_party <- ggpredict(m_right_party_cont, c("year","knowledge"))
pred_left_party <- ggpredict(m_left_party_cont, c("year","knowledge"))
pred_no_party <- ggpredict(m_no_party_cont, c("year","knowledge"))

pred_reduce_pub_spend$var <- "reduce_pub_spend"
pred_sell_pub_comp$var <- "sell_pub"
pred_priv_healthcare$var <- "priv_healthcare"
pred_fewer_refugees$var <- "fewer_refugees"
pred_law_order$var <- "law_order"
pred_gender_equal$var <- "gender_equal"

pred_no_nuclear$var <- "no_nuclear"
pred_leave_eu$var <- "leave_eu"
pred_join_nato$var <- "join_nato"
pred_right_party$var <- "right_party"
pred_left_party$var <- "left_party"
pred_no_party$var <- "no_party"

all_preds <- rbind(pred_reduce_pub_spend,
                   pred_sell_pub_comp,
                   pred_priv_healthcare,
                   pred_fewer_refugees,
                   pred_law_order,
                   pred_gender_equal,
                   pred_no_nuclear,
                   pred_leave_eu,
                   pred_join_nato,
                   pred_right_party,
                   pred_left_party,
                   pred_no_party)

# has the relationship between knowledge and the variable in question been
# constant over the years?
all_preds %>% 
  mutate(group = as.numeric(as.character(group))) %>% 
  ggplot() +
  aes(x = group, y = predicted, group = x, color = x) +
  geom_line() +
  # geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=.3, linetype=0) +
  facet_wrap(~var, scales = "free_y")

# look at whether the effect has decreased over time
# fit models
m_reduce_pub_spend <- glm(a_reduce_pub_spend ~ 
                            knowledge_binary +
                            year:knowledge_binary +
                            d_age +
                            d_gender +
                            d_class +
                            d_education +
                            d_income,
                          data = df_all_years, 
                          family = "binomial",
                          weights = prop_score)

m_sell_pub_comp  <- glm(a_sell_pub_comp ~ 
                          knowledge_binary +
                          year:knowledge_binary +
                          d_age +
                          d_gender +
                          d_class +
                          d_education +
                          d_income,
                        data = df_all_years, 
                        family = "binomial",
                        weights = prop_score)

m_priv_healthcare  <- glm(a_priv_healthcare ~ 
                            knowledge_binary +
                            year:knowledge_binary +
                            d_age +
                            d_gender +
                            d_class +
                            d_education +
                            d_income,
                          data = df_all_years, 
                          family = "binomial",
                          weights = prop_score)

m_fewer_refugees  <- glm(a_fewer_refugees ~ 
                           knowledge_binary +
                           year:knowledge_binary +
                           d_age +
                           d_gender +
                           d_class +
                           d_education +
                           d_income,
                         data = df_all_years, 
                         family = "binomial",
                         weights = prop_score)

m_law_order  <- glm(a_law_order ~ 
                      knowledge_binary +
                      year:knowledge_binary +
                      d_age +
                      d_gender +
                      d_class +
                      d_education +
                      d_income,
                    data = df_all_years, 
                    family = "binomial",
                    weights = prop_score)

m_gender_equal  <- glm(a_gender_equal ~ 
                         knowledge_binary +
                         year:knowledge_binary +
                         d_age +
                         d_gender +
                         d_class +
                         d_education +
                         d_income,
                       data = df_all_years, 
                       family = "binomial",
                       weights = prop_score)

m_no_nuclear  <- glm(a_no_nuclear ~ 
                       knowledge_binary +
                       year:knowledge_binary +
                       d_age +
                       d_gender +
                       d_class +
                       d_education +
                       d_income,
                     data = df_all_years, 
                     family = "binomial",
                     weights = prop_score)

m_leave_eu  <- glm(a_leave_eu ~ 
                     knowledge_binary +
                     year:knowledge_binary +
                     d_age +
                     d_gender +
                     d_class +
                     d_education +
                     d_income,
                   data = df_all_years, 
                   family = "binomial",
                   weights = prop_score)

m_join_nato  <- glm(a_join_nato ~ 
                      knowledge_binary +
                      year:knowledge_binary +
                      d_age +
                      d_gender +
                      d_class +
                      d_education +
                      d_income,
                    data = df_all_years, 
                    family = "binomial",
                    weights = prop_score)

m_right_party  <- glm(right_party ~ 
                      knowledge_binary +
                      year:knowledge_binary +
                      d_age +
                      d_gender +
                      d_class +
                      d_education +
                      d_income,
                    data = df_all_years, 
                    family = "binomial",
                    weights = prop_score)

m_left_party  <- glm(left_party ~ 
                        knowledge_binary +
                        year:knowledge_binary +
                        d_age +
                        d_gender +
                        d_class +
                        d_education +
                        d_income,
                      data = df_all_years, 
                      family = "binomial",
                      weights = prop_score)

m_no_party  <- glm(no_party ~ 
                        knowledge_binary +
                        year:knowledge_binary +
                        d_age +
                        d_gender +
                        d_class +
                        d_education +
                        d_income,
                      data = df_all_years, 
                      family = "binomial",
                      weights = prop_score)

# look at modelled effect over time
effect_over_time <- tibble("variable" = (c(rep("a_reduce_pub_spend",6),
                                          rep("a_sell_pub_comp",6),
                                          rep("a_priv_healthcare",6),
                                          rep("a_fewer_refugees",6),
                                          rep("a_law_order",6),
                                          rep("a_gender_equal",6),
                                          rep("a_no_nuclear",6),
                                          rep("a_leave_eu",6),
                                          rep("a_join_nato",6),
                                          rep("right_party",6),
                                          rep("left_party",6),
                                          rep("no_party",6))),
                           "year" = rep(c("1998","2002","2006","2010","2014","2018"),12),
                           "est" = NA,
                           "lwr" = NA,
                           "upr" = NA)

calc_time_effect <- function(models, df) {
  for(i in 1:length(models)){
    dep_var <- all.vars(models[[i]]$formula)[1]
    target_rows <- which(df$variable == dep_var, arr.ind=TRUE)
    df[target_rows[1], 3:5] <- confint(glht(models[[i]], linfct = c("knowledge_binary = 0")))$confint # 1998
    df[target_rows[2], 3:5] <- confint(glht(models[[i]], linfct = c("knowledge_binary + knowledge_binary:year2002 = 0")))$confint
    df[target_rows[3], 3:5] <- confint(glht(models[[i]], linfct = c("knowledge_binary + knowledge_binary:year2006 = 0")))$confint
    df[target_rows[4], 3:5] <- confint(glht(models[[i]], linfct = c("knowledge_binary + knowledge_binary:year2010 = 0")))$confint
    df[target_rows[5], 3:5] <- confint(glht(models[[i]], linfct = c("knowledge_binary + knowledge_binary:year2014 = 0")))$confint
    df[target_rows[6], 3:5] <- confint(glht(models[[i]], linfct = c("knowledge_binary + knowledge_binary:year2018 = 0")))$confint
  }
  return(df)
}

effect_over_time <- calc_time_effect(models = list(m_reduce_pub_spend,
                                                   m_sell_pub_comp,
                                                   m_priv_healthcare,
                                                   m_fewer_refugees,
                                                   m_law_order,
                                                   m_gender_equal,
                                                   m_no_nuclear,
                                                   m_leave_eu,
                                                   m_join_nato,
                                                   m_right_party,
                                                   m_left_party,
                                                   m_no_party), 
                                     df = effect_over_time)

effect_over_time <- effect_over_time %>% 
  mutate(variable = recode(variable,
                           "a_reduce_pub_spend" = "Reduce public sector",
                           "a_sell_pub_comp" = "Privatise state-owned businesses",
                           "a_priv_healthcare" = "Increase private health care",
                           "a_fewer_refugees" = "Accept fewer refugees",
                           "a_law_order" = "More law and order",
                           "a_gender_equal" = "More gender equality",
                           "a_no_nuclear" = "Abolish nuclear power",
                           "a_leave_eu" = "Leave the EU",
                           "a_join_nato" = "Join NATO",
                           "right_party" = "Supporting right parties",
                           "left_party" = "Supporting left parties",
                           "no_party" = "Supporting no party"))

png(file="plots/knowledge_effects.png", width = 12, height = 8, units = 'in', res = 300)
effect_over_time %>% 
  ggplot() +
  aes(x = year, y = est, color = variable) +
  geom_line(group = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  geom_hline(yintercept=0, color = "grey") +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Effects from knowledge in the Swedish electorate over time",
       subtitle = "Effects measure difference in logged odds of agreement of moving from uninformed to informed",
       caption = "Data: SNES 1998, 2002, 2006, 2010, 2014, and 2018",
       x = "",
       y = "Difference (logged odds)") +
  theme(plot.title = element_text(face="bold")) +
  theme(legend.position="none")
dev.off()

# look at percentage point information effect over time
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
df_informed$right_party_informed <- predict(m_right_party, newdata = df_informed, type = "response")
df_informed$left_party_informed <- predict(m_left_party, newdata = df_informed, type = "response")
df_informed$no_party_informed <- predict(m_no_party, newdata = df_informed, type = "response")

png(file="plots/info_effect_party.png", width = 10, height = 5, units = 'in', res = 300)
df_informed %>% 
  group_by(year) %>% 
  summarise("Supporting right parties (M, FP, KD, C, SD)" = mean(right_party_informed) - mean(right_party),
            "Supporting left parties (S, MP, V)" = mean(left_party_informed) - mean(left_party),
            "Supporting no party" = mean(no_party_informed) - mean(no_party))  %>% 
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
  labs(title = "Information effects on PARTY PREFERENCE in the Swedish electorate over time",
       subtitle = "Effects measure differences between actual and simulated fully informed levels of support",
       caption = "Data: SNES 1998, 2002, 2006, 2010, 2014, and 2018",
       x = "",
       y = "Difference (percentage points)") +
  theme(plot.title = element_text(face="bold")) +
  theme(legend.position="none")
dev.off()

png(file="plots/info_effect_spending.png", width = 10, height = 5, units = 'in', res = 300)
df_informed %>% 
  group_by(year) %>% 
  summarise("Reduce public sector" = mean(a_reduce_pub_spend_informed) - mean(a_reduce_pub_spend),
            "Privatise state-owned businesses" = mean(a_sell_pub_comp_informed) - mean(a_sell_pub_comp),
            "Increase private health care" = mean(a_priv_healthcare_informed) - mean(a_priv_healthcare))  %>% 
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
  labs(title = "Information effects on PUBLIC SPENDING in the Swedish electorate over time",
       subtitle = "Effects measure differences between actual and simulated fully informed levels of support",
       caption = "Data: SNES 1998, 2002, 2006, 2010 and 2014",
       x = "",
       y = "Difference (percentage points)") +
  theme(plot.title = element_text(face="bold")) +
  theme(legend.position="none")
dev.off()

png(file="plots/info_effect_values.png", width = 10, height = 5, units = 'in', res = 300)
df_informed %>% 
  group_by(year) %>% 
  summarise("Accept fewer refugees" = mean(a_fewer_refugees_informed) - mean(a_fewer_refugees),
            "More law and order" = mean(a_law_order_informed) - mean(a_law_order),
            "More gender equality" = mean(a_gender_equal_informed) - mean(a_gender_equal))  %>% 
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
  labs(title = "Information effects on POLITICAL VALUES in the Swedish electorate over time",
       subtitle = "Effects measure differences between actual and simulated fully informed levels of support",
       caption = "Data: SNES 1998, 2002, 2006, 2010 and 2014",
       x = "",
       y = "Difference (percentage points)") +
  theme(plot.title = element_text(face="bold")) +
  theme(legend.position="none")
dev.off()

png(file="plots/info_effect_policies.png", width = 10, height = 5, units = 'in', res = 300)
df_informed %>% 
  group_by(year) %>% 
  summarise("Abolish nuclear power" = mean(a_no_nuclear_informed) - mean(a_no_nuclear),
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
  labs(title = "Information effects on POLITICAL POLICIES in the Swedish electorate over time",
       subtitle = "Effects measure differences between actual and simulated fully informed levels of support",
       caption = "Data: SNES 1998, 2002, 2006, 2010 and 2014",
       x = "",
       y = "Difference (percentage points)") +
  theme(plot.title = element_text(face="bold")) +
  theme(legend.position="none")
dev.off()

