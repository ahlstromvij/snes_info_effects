set.seed(100)

library(tidyverse)
library(ggeffects)
library(multcomp)

df_all_years <- readRDS("data/df_all_years_scales.rds")

# add party preference dummies
table(df_all_years$d_partisanship)
df_all_years <- df_all_years %>% 
  mutate(centerpartiet = case_when(d_partisanship == "centerpartiet" ~ 1,
                                   TRUE ~ 0),
         folkpartiet = case_when(d_partisanship == "folkpartiet" ~ 1,
                                   TRUE ~ 0),
         kristdemokraterna = case_when(d_partisanship == "kristdemokraterna" ~ 1,
                                   TRUE ~ 0),
         moderaterna = case_when(d_partisanship == "moderaterna" ~ 1,
                                       TRUE ~ 0),
         miljopartiet = case_when(d_partisanship == "miljöpartiet" ~ 1,
                                       TRUE ~ 0),
         socialdemokraterna = case_when(d_partisanship == "socialdemokraterna" ~ 1,
                                         TRUE ~ 0),
         sverigedemokraterna = case_when(d_partisanship == "sverigedemokraterna" ~ 1,
                                        TRUE ~ 0),
         vansterpartiet = case_when(d_partisanship == "vänsterpartiet" ~ 1,
                                         TRUE ~ 0),
         rostade_inte = case_when(d_partisanship == "rostade_inte" ~ 1,
                              TRUE ~ 0))


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


m_centerpartiet_cont <- glm(centerpartiet ~ 
                          knowledge +
                          year:knowledge +
                          d_age +
                          d_gender +
                          d_class +
                          d_education +
                          d_income,
                        data = df_all_years, 
                        family = "binomial")

m_folkpartiet_cont <- glm(folkpartiet ~ 
                            knowledge +
                            year:knowledge +
                            d_age +
                            d_gender +
                            d_class +
                            d_education +
                            d_income,
                          data = df_all_years, 
                          family = "binomial")

m_rostade_inte_cont <- glm(rostade_inte ~ 
                           knowledge +
                           year:knowledge +
                           d_age +
                           d_gender +
                           d_class +
                           d_education +
                           d_income,
                         data = df_all_years, 
                         family = "binomial")

m_kristdemokraterna_cont <- glm(kristdemokraterna ~ 
                            knowledge +
                            year:knowledge +
                            d_age +
                            d_gender +
                            d_class +
                            d_education +
                            d_income,
                          data = df_all_years, 
                          family = "binomial")

m_miljopartiet_cont <- glm(miljopartiet ~ 
                             knowledge +
                             year:knowledge +
                             d_age +
                             d_gender +
                             d_class +
                             d_education +
                             d_income,
                           data = df_all_years, 
                           family = "binomial")

m_moderaterna_cont <- glm(moderaterna ~ 
                             knowledge +
                             year:knowledge +
                             d_age +
                             d_gender +
                             d_class +
                             d_education +
                             d_income,
                           data = df_all_years, 
                           family = "binomial")

m_socialdemokraterna_cont <- glm(socialdemokraterna ~ 
                                   knowledge +
                                   year:knowledge +
                                   d_age +
                                   d_gender +
                                   d_class +
                                   d_education +
                                   d_income,
                                 data = df_all_years, 
                                 family = "binomial")

m_sverigedemokraterna_cont <- glm(sverigedemokraterna ~ 
                                   knowledge +
                                   year:knowledge +
                                   d_age +
                                   d_gender +
                                   d_class +
                                   d_education +
                                   d_income,
                                 data = df_all_years, 
                                 family = "binomial")

m_vansterpartiet_cont <- glm(vansterpartiet ~ 
                                    knowledge +
                                    year:knowledge +
                                    d_age +
                                    d_gender +
                                    d_class +
                                    d_education +
                                    d_income,
                                  data = df_all_years, 
                                  family = "binomial")

# attitudes
pred_reduce_pub_spend <- ggpredict(m_reduce_pub_spend_cont, c("year","knowledge"))
pred_sell_pub_comp <- ggpredict(m_sell_pub_comp_cont, c("year","knowledge"))
pred_priv_healthcare <- ggpredict(m_priv_healthcare_cont, c("year","knowledge"))
pred_fewer_refugees <- ggpredict(m_fewer_refugees_cont, c("year","knowledge"))
pred_law_order <- ggpredict(m_law_order_cont, c("year","knowledge"))
pred_gender_equal <- ggpredict(m_gender_equal_cont, c("year","knowledge"))
pred_no_nuclear <- ggpredict(m_no_nuclear_cont, c("year","knowledge"))
pred_leave_eu <- ggpredict(m_leave_eu_cont, c("year","knowledge"))
pred_join_nato <- ggpredict(m_join_nato_cont, c("year","knowledge"))

# party preference
pred_centerpartiet <- ggpredict(m_centerpartiet_cont, c("year","knowledge"))
pred_folkpartiet <- ggpredict(m_folkpartiet_cont, c("year","knowledge"))
pred_rostade_inte <- ggpredict(m_rostade_inte_cont, c("year","knowledge"))
pred_kristdemokraterna <- ggpredict(m_kristdemokraterna_cont, c("year","knowledge"))
pred_moderaterna <- ggpredict(m_moderaterna_cont, c("year","knowledge"))
pred_miljopartiet <- ggpredict(m_miljopartiet_cont, c("year","knowledge"))
pred_socialdemokraterna <- ggpredict(m_socialdemokraterna_cont, c("year","knowledge"))
pred_sverigedemokraterna <- ggpredict(m_sverigedemokraterna_cont, c("year","knowledge"))
pred_vansterpartiet <- ggpredict(m_vansterpartiet_cont, c("year","knowledge"))

pred_reduce_pub_spend$var <- "reduce_pub_spend"
pred_sell_pub_comp$var <- "sell_pub"
pred_priv_healthcare$var <- "priv_healthcare"
pred_fewer_refugees$var <- "fewer_refugees"
pred_law_order$var <- "law_order"
pred_gender_equal$var <- "gender_equal"
pred_no_nuclear$var <- "no_nuclear"
pred_leave_eu$var <- "leave_eu"
pred_join_nato$var <- "join_nato"

pred_centerpartiet$var <- "centerpartiet"
pred_folkpartiet$var <- "folkpartiet"
pred_rostade_inte$var <- "rostade_inte"
pred_kristdemokraterna$var <- "kristdemokraterna"
pred_moderaterna$var <- "moderaterna"
pred_miljopartiet$var <- "miljopartiet"
pred_socialdemokraterna$var <- "socialdemokraterna"
pred_sverigedemokraterna$var <- "sverigedemokraterna"
pred_vansterpartiet$var <- "vansterpartiet"

all_preds <- rbind(pred_reduce_pub_spend,
                   pred_sell_pub_comp,
                   pred_priv_healthcare,
                   pred_fewer_refugees,
                   pred_law_order,
                   pred_gender_equal,
                   pred_no_nuclear,
                   pred_leave_eu,
                   pred_join_nato,
                   pred_centerpartiet,
                   pred_folkpartiet,
                   pred_rostade_inte,
                   pred_kristdemokraterna,
                   pred_moderaterna,
                   pred_miljopartiet,
                   pred_socialdemokraterna,
                   pred_sverigedemokraterna,
                   pred_vansterpartiet)

# has the relationship between knowledge and the variable in question been
# constant over the years?

all_parties <- c("folkpartiet","centerpartiet","rostade_inte","kristdemokraterna",
                 "moderaterna","miljopartiet","socialdemokraterna","sverigedemokraterna",
                 "vansterpartiet", "Folkpartiet","Centerpartiet","Röstade inte","Kristdemokraterna",
                 "Moderaterna","Miljöpartiet","Socialdemokraterna","Sverigedemokraterna",
                 "Vänsterpartiet")
# party preferences
all_preds %>% 
  filter(var %in% all_parties) %>% 
  mutate(group = as.numeric(as.character(group))) %>% 
  ggplot() +
  aes(x = group, y = predicted, group = x, color = x) +
  geom_line() +
  # geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=.3, linetype=0) +
  facet_wrap(~var, scales = "free_y")

all_preds %>% 
  filter(!var %in% all_parties) %>% 
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

m_centerpartiet  <- glm(centerpartiet ~ 
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

m_folkpartiet  <- glm(folkpartiet ~ 
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

m_rostade_inte  <- glm(rostade_inte ~ 
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

m_kristdemokraterna  <- glm(kristdemokraterna ~ 
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

m_miljopartiet  <- glm(miljopartiet ~ 
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

m_moderaterna  <- glm(moderaterna ~ 
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

m_socialdemokraterna  <- glm(socialdemokraterna ~ 
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

m_sverigedemokraterna  <- glm(sverigedemokraterna ~ 
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

m_vansterpartiet  <- glm(vansterpartiet ~ 
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
                                          rep("centerpartiet",6),
                                          rep("folkpartiet",6),
                                          rep("rostade_inte",6),
                                          rep("kristdemokraterna",6),
                                          rep("miljopartiet",6),
                                          rep("moderaterna",6),
                                          rep("socialdemokraterna",6),
                                          rep("sverigedemokraterna",6),
                                          rep("vansterpartiet",6))),
                           "year" = rep(c("1998","2002","2006","2010","2014","2018"),18),
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
                                                   m_centerpartiet,
                                                   m_folkpartiet,
                                                   m_rostade_inte,
                                                   m_kristdemokraterna,
                                                   m_miljopartiet,
                                                   m_moderaterna,
                                                   m_socialdemokraterna,
                                                   m_sverigedemokraterna,
                                                   m_vansterpartiet), 
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
                           "centerpartiet" = "Centerpartiet",
                           "folkpartiet" = "Folkpartiet",
                           "kristdemokraterna" = "Kristdemokraterna",
                           "miljopartiet" = "Miljöpartiet",
                           "moderaterna" = "Moderaterna",
                           "socialdemokraterna" = "Socialdemokraterna",
                           "sverigedemokraterna" = "Sverigedemokraterna",
                           "vansterpartiet" = "Vänsterpartiet",
                           "rostade_inte" = "Röstade inte"))

table(df_all_years$d_partisanship, df_all_years$year) # remove 1998 - 2006 for SD
effect_over_time <- effect_over_time %>% 
  filter(., !(variable == "Sverigedemokraterna" & (year == 1998 | year == 2002 | year == 2006)))

png(file="plots/knowledge_effects_party.png", width = 12, height = 8, units = 'in', res = 300)
effect_over_time %>% 
  filter(variable %in% all_parties) %>% 
  ggplot() +
  aes(x = year, y = est, color = variable) +
  geom_line(group = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  geom_hline(yintercept=0, color = "grey") +
  facet_wrap(~variable) +
  labs(title = "Effects from knowledge on PARTY PREFERENCE in the Swedish electorate over time",
       subtitle = "Effects measure difference in logged odds of agreement of moving from uninformed to informed",
       caption = "Data: SNES 1998, 2002, 2006, 2010, 2014, and 2018",
       x = "",
       y = "Difference (logged odds)") +
  theme(plot.title = element_text(face="bold")) +
  theme(legend.position="none")
dev.off()

png(file="plots/knowledge_effects_attitudes.png", width = 12, height = 8, units = 'in', res = 300)
effect_over_time %>% 
  filter(!variable %in% all_parties) %>% 
  ggplot() +
  aes(x = year, y = est, color = variable) +
  geom_line(group = 1) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  geom_hline(yintercept=0, color = "grey") +
  facet_wrap(~variable) +
  labs(title = "Effects from knowledge on POLITICAL ATTITUDES in the Swedish electorate over time",
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

df_informed$centerpartiet_informed <- predict(m_centerpartiet, newdata = df_informed, type = "response")
df_informed$folkpartiet_informed <- predict(m_folkpartiet, newdata = df_informed, type = "response")
df_informed$rostade_inte_informed <- predict(m_rostade_inte, newdata = df_informed, type = "response")
df_informed$kristdemokraterna_informed <- predict(m_kristdemokraterna, newdata = df_informed, type = "response")
df_informed$miljopartiet_informed <- predict(m_miljopartiet, newdata = df_informed, type = "response")
df_informed$moderaterna_informed <- predict(m_moderaterna, newdata = df_informed, type = "response")
df_informed$socialdemokraterna_informed <- predict(m_socialdemokraterna, newdata = df_informed, type = "response")
df_informed$sverigedemokraterna_informed <- predict(m_sverigedemokraterna, newdata = df_informed, type = "response")
df_informed$vansterpartiet_informed <- predict(m_vansterpartiet, newdata = df_informed, type = "response")

df_informed %>% 
  mutate(diff = centerpartiet_informed - centerpartiet) %>% 
  aov(diff ~ year, data = .) %>% 
  summary()

df_informed %>% 
  mutate(diff = folkpartiet_informed - folkpartiet) %>% 
  aov(diff ~ year, data = .) %>% 
  summary()

sig_diff <- function(inf_var, act_var, data) {
  diff = data[inf_var] - data[act_var]
  year = data$year
  df <- cbind(diff, year)
  m <- aov(df[[1]] ~ df[[2]])
  return(summary(m)[[1]]$`Pr(>F)`[1])
}

sig_diffs_party <- tibble(
  variable = c("Centerpartiet",
               "Folkpartiet",
               "Kristdemokraterna",
               "Miljöpartiet",
               "Moderaterna",
               "Socialdemokraterna",
               "Sverigedemokraterna",
               "Vänsterpartiet",
               "Röstade inte"),
  p_val = c(sig_diff("centerpartiet_informed", "centerpartiet", df_informed),
            sig_diff("folkpartiet_informed", "folkpartiet", df_informed),
            sig_diff("centerpartiet_informed", "centerpartiet", df_informed),
            sig_diff("miljopartiet_informed", "miljopartiet", df_informed),
            sig_diff("moderaterna_informed", "moderaterna", df_informed),
            sig_diff("socialdemokraterna_informed", "socialdemokraterna", df_informed),
            sig_diff("sverigedemokraterna_informed", "sverigedemokraterna", df_informed),
            sig_diff("vansterpartiet_informed", "vansterpartiet", df_informed),
            sig_diff("rostade_inte_informed", "rostade_inte", df_informed))
) %>% 
  mutate(p_val = round(p_val, 4))

png(file="plots/info_effect_party.png", width = 12, height =8, units = 'in', res = 300)
df_informed %>% 
  group_by(year) %>% 
  summarise("Centerpartiet" = mean(centerpartiet_informed) - mean(centerpartiet),
            "Folkpartiet" = mean(folkpartiet_informed) - mean(folkpartiet),
            "Kristdemokraterna" = mean(kristdemokraterna_informed) - mean(kristdemokraterna),
            "Miljöpartiet" = mean(miljopartiet_informed) - mean(miljopartiet),
            "Moderaterna" = mean(moderaterna_informed) - mean(moderaterna),
            "Socialdemokraterna" = mean(socialdemokraterna_informed) - mean(socialdemokraterna),
            "Sverigedemokraterna" = mean(sverigedemokraterna_informed) - mean(sverigedemokraterna),
            "Vänsterpartiet" = mean(vansterpartiet_informed) - mean(vansterpartiet),
            "Röstade inte" = mean(rostade_inte_informed) - mean(rostade_inte))  %>% 
  pivot_longer(cols = -year,
               names_to = "variable",
               values_to = "difference")  %>% 
  mutate(difference = difference * 100) %>% 
  ggplot() +
  aes(x = year, y = difference, color = variable) +
  geom_point() +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  geom_text(aes(x=1, y=5.5, label=paste("p = ", p_val), colour = NULL), data = sig_diffs_party, hjust = 0) +
  labs(title = "Information effects on PARTY CHOICE in the Swedish electorate over time",
       subtitle = "Effects measure differences between actual and simulated fully informed levels of support",
       caption = "Data: SNES 1998, 2002, 2006, 2010, 2014, and 2018",
       x = "",
       y = "Difference (percentage points)") +
  theme(plot.title = element_text(face="bold")) +
  theme(legend.position="none") +
  facet_wrap(~variable)
dev.off()

df_informed <- df_informed %>% 
  mutate(a_reduce_pub_spend = as.numeric(a_reduce_pub_spend),
         a_sell_pub_comp = as.numeric(a_sell_pub_comp),
         a_priv_healthcare = as.numeric(a_priv_healthcare),
         a_fewer_refugees = as.numeric(a_fewer_refugees),
         a_law_order = as.numeric(a_law_order),
         a_gender_equal = as.numeric(a_gender_equal),
         a_no_nuclear = as.numeric(a_no_nuclear),
         a_leave_eu = as.numeric(a_leave_eu),
         a_join_nato = as.numeric(a_join_nato))

sig_diffs_attitude <- tibble(
  variable = c("Reduce public sector",
               "Privatise state-owned businesses",
               "Increase private health care",
               "Accept fewer refugees",
               "More law and order",
               "More gender equality",
               "Abolish nuclear power",
               "Leave the EU",
               "Join NATO"),
  p_val = c(sig_diff("a_reduce_pub_spend_informed", "a_reduce_pub_spend", df_informed),
            sig_diff("a_sell_pub_comp_informed", "a_sell_pub_comp", df_informed),
            sig_diff("a_priv_healthcare_informed", "a_priv_healthcare", df_informed),
            sig_diff("a_fewer_refugees_informed", "a_fewer_refugees", df_informed),
            sig_diff("a_law_order_informed", "a_law_order", df_informed),
            sig_diff("a_gender_equal_informed", "a_gender_equal", df_informed),
            sig_diff("a_no_nuclear_informed", "a_no_nuclear", df_informed),
            sig_diff("a_leave_eu_informed", "a_leave_eu", df_informed),
            sig_diff("a_join_nato_informed", "a_join_nato", df_informed))
) %>% 
  mutate(p_val = round(p_val, 4))

png(file="plots/info_effect_attitudes.png", width = 12, height = 8, units = 'in', res = 300)
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
  geom_text(aes(x=1, y=3.5, label=paste("p = ", p_val), colour = NULL), data = sig_diffs_attitude, hjust = 0) +
  labs(title = "Information effects on POLITICAL ATTITUDES in the Swedish electorate over time",
       subtitle = "Effects measure differences between actual and simulated fully informed levels of support",
       caption = "Data: SNES 1998, 2002, 2006, 2010, 2014, and 2018",
       x = "",
       y = "Difference (percentage points)") +
  theme(plot.title = element_text(face="bold")) +
  theme(legend.position="none") +
  facet_wrap(~variable)
dev.off()

