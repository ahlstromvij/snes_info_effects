set.seed(100)

library(tidyverse)
library(informationeffects)

df_all_years <- readRDS("data/df_all_years_imputed.rds")

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
know_scale <- info_scale(items = c("k_s_rep","k_num_mps", "k_spain_eu"),
                         data = df_all_years,
                         binary_cutoff = 1)

know_scale$know_scores_binary_tbl
know_scale$model_summary
know_scale$model_coef
know_scale$par_analysis
know_scale$trace_plot
know_scale$info_plot
know_scale$q3
know_scale$empirical_plots

# construct validity
df_all_years$knowledge <- know_scale$know_scores
marginal_means <- info_emmeans(knowledge_var = "knowledge",
                               covariates = c("d_income", "d_education","d_gender","d_age"), 
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
