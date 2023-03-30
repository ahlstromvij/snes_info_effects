set.seed(100)

library(tidyverse)
library(informationeffects)
library(polycor)
library(psych)
library(lavaan)

df_all_years <- readRDS("data/df_all_years_imputed.rds")

# convert from impute to numeric
df_all_years <- df_all_years %>% 
  mutate(k_m_rep = as.numeric(k_m_rep),
         k_s_rep = as.numeric(k_s_rep),
         k_c_rep = as.numeric(k_c_rep),
         k_v_rep = as.numeric(k_v_rep),
         k_nat_insurance = as.numeric(k_nat_insurance),
         k_num_mps = as.numeric(k_num_mps),
         k_spain_eu = as.numeric(k_spain_eu))

# does knowledge correlate with expected patterns in gender, education, age, and income
df_all_years <- df_all_years %>% 
  rowwise() %>%
  mutate(k_sum = sum(c_across(k_m_rep:k_spain_eu)))

df_all_years %>% 
  ggplot() +
  aes(x = k_sum) +
  geom_histogram()

df_all_years %>% 
  group_by(d_education) %>% 
  summarise(score = mean(k_sum)) %>% 
  ggplot() +
  aes(x = d_education, y = score, fill = d_education) +
  geom_bar(stat = "identity")

df_all_years %>% 
  group_by(d_gender) %>% 
  summarise(score = mean(k_sum)) %>% 
  ggplot() +
  aes(x = d_gender, y = score, fill = d_gender) +
  geom_bar(stat = "identity")

df_all_years %>% 
  group_by(d_income) %>% 
  summarise(score = mean(k_sum)) %>% 
  ggplot() +
  aes(x = d_income, y = score, fill = d_income) +
  geom_bar(stat = "identity")

df_all_years %>% 
  group_by(d_age) %>% 
  summarise(score = mean(k_sum)) %>% 
  ggplot() +
  aes(x = d_age, y = score, fill = d_age) +
  geom_bar(stat = "identity")

# knowledge by partisanship
df_all_years %>% 
  group_by(d_partisanship) %>% 
  summarise(score = mean(k_sum)) %>% 
  ggplot() +
  aes(x = reorder(d_partisanship, score), y = score, fill = d_partisanship) +
  geom_bar(stat = "identity") +
  theme(legend.position="none") +
  coord_flip()

# knowledge scale
know_scale <- info_scale(items = c("k_s_rep",
                                   "k_m_rep",
                                   "k_c_rep",
                                   "k_v_rep",
                                   "k_nat_insurance",
                                   "k_num_mps",
                                   "k_spain_eu"),
                         data = df_all_years,
                         binary_cutoff = 0.75)

know_scale$know_scores %>% 
  as.data.frame() %>% 
  ggplot() +
  aes(x = know_scale$know_scores) +
  geom_histogram(bins = 7, fill = 'salmon', color = 'black')

know_scale$know_scores_binary_tbl
know_scale$model_summary
know_scale$model_coef
know_scale$par_analysis
know_scale$trace_plot
know_scale$info_plot
know_scale$q3
know_scale$empirical_plots

# let's look at these with efa, using polychoric correlations
efa_poly <- fa.poly(df_all_years[,8:14], 2)
print(efa_poly$fa$loadings)

# let's compare fit of two models using CFA
# one factor
mod_1f <- 'knowledge =~ k_s_rep + k_m_rep + k_c_rep + k_v_rep + k_nat_insurance + k_num_mps + k_spain_eu'
mod_1f.fit <- cfa(mod_1f, data=df_all_years, ordered = TRUE)
summary(mod_1f.fit, standardized=TRUE)
# the Std.all column gives the type of loadings we would see in efa correlations between items and its latent. we want > 0.3.

# RMSEA values less than 0.05 or 0.01 correspond to good and very good fit respectively (Andrews 2021)
# want Comparative Fit Index (CFI) >0.95 (Dima 2018)
# want Tucker-Lewis index (TLI) >0.95 (Dima 2018)
# agfi: adjusted goodness of fit; 0-1, higher better

fitmeasures(mod_1f.fit)["rmsea"] # 0.08119324
fitmeasures(mod_1f.fit)["cfi"] # 0.9593425
fitmeasures(mod_1f.fit)["tli"] # 0.9390138
fitmeasures(mod_1f.fit)["agfi"] # 0.9546318

# two factors
mod_2f <- '
          person_knowledge =~ k_s_rep + k_m_rep + k_c_rep + k_v_rep
          issue_knowledge =~ k_nat_insurance + k_num_mps + k_spain_eu'
mod_2f.fit <- cfa(mod_2f, data=df_all_years, ordered = TRUE)
summary(mod_2f.fit, standardized=TRUE)

fitmeasures(mod_2f.fit)["rmsea"] # 0.06153559
fitmeasures(mod_2f.fit)["cfi"] # 0.9783145
fitmeasures(mod_2f.fit)["tli"] # 0.9649696
fitmeasures(mod_2f.fit)["agfi"] # 0.9737486

# construct validity
df_all_years$knowledge <- know_scale$know_scores
marginal_means <- info_emmeans(knowledge_var = "knowledge",
                               covariates = c("d_income", "d_education","d_gender","d_age","d_class"), 
                               data = df_all_years)
marginal_means

# show visually
png(file="plots/emmeans_income.png", width = 6, height = 6, units = 'in', res = 300)
data.frame(marginal_means[[1]]) %>% 
  ggplot() +
  aes(x = d_income, y = emmean, color = "salmon") +
  geom_pointrange(aes(ymax=upper.CL, ymin=lower.CL)) +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  theme(legend.position="none")
dev.off()

png(file="plots/emmeans_education.png", width = 6, height = 6, units = 'in', res = 300)
data.frame(marginal_means[[2]]) %>% 
  ggplot() +
  aes(x = d_education, y = emmean, color = "salmon") +
  geom_pointrange(aes(ymax=upper.CL, ymin=lower.CL)) +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  theme(legend.position="none")
dev.off()

png(file="plots/emmeans_gender.png", width = 6, height = 6, units = 'in', res = 300)
data.frame(marginal_means[[3]]) %>% 
  ggplot() +
  aes(x = d_gender, y = emmean, color = "salmon") +
  geom_pointrange(aes(ymax=upper.CL, ymin=lower.CL)) +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  theme(legend.position="none")
dev.off()

png(file="plots/emmeans_age.png", width = 6, height = 6, units = 'in', res = 300)
data.frame(marginal_means[[4]]) %>% 
  ggplot() +
  aes(x = d_age, y = emmean, color = "salmon") +
  geom_pointrange(aes(ymax=upper.CL, ymin=lower.CL)) +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  theme(legend.position="none")
dev.off()

png(file="plots/emmeans_class.png", width = 6, height = 6, units = 'in', res = 300)
data.frame(marginal_means[[5]]) %>% 
  ggplot() +
  aes(x = d_class, y = emmean, color = "salmon") +
  geom_pointrange(aes(ymax=upper.CL, ymin=lower.CL)) +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  theme(legend.position="none")
dev.off()

# calculate prop scores
df_all_years$knowledge_binary <- know_scale$know_scores_binary
df_all_years$prop_score <- info_prop_scores(knowledge_var = "knowledge_binary", 
                                            covariates = c("d_income","d_education","d_gender","d_age"), 
                                            data = df_all_years)

df_all_years %>% 
  ggplot() +
  aes(x = prop_score) +
  geom_histogram(binwidth=0.1, color = "black", fill = "salmon")

# evaluate prop scores
info_bal_plots(knowledge_var = "knowledge_binary", 
               covariates = c("d_income","d_education","d_gender","d_age"), 
               prop_score ="prop_score", 
               data = df_all_years)

saveRDS(df_all_years, "data/df_all_years_scales.rds")
