library(tidyverse)
library(dplyr)
library(mice)
library(car)
library(nnet)

### Multivariate imputation by chained equations 
d1 <- read.csv('data1.csv')

# data type & standardization
scale_numeric <- function(column) as.numeric(scale(column))

numeric_vars <- c('income', 'education_mother', 'education_father', 'pubertal_stage', 'iq', 'aspiration', 
                  'self_control', 'prosocial_behavior', 'sibling', 'grandparent', 'life_satisfaction_of_caregiver', 
                  'talk_with_caregiver', 'cooperative_spouse_in_childcare', 
                  'satisfied_with_family', 'want_to_be_like_mother', 'want_to_be_like_father',  
                  'family_involvement_in_future_carrer_preferences', 'caregiver_life_plan_is_stability_oriented',
                  'like_school', 'social_relationships_outside_school',
                  'good_neighborhood_for_childcare', 'neighborhood_cohesion_and_trust')
d1[numeric_vars] <- lapply(d1[numeric_vars], scale_numeric)

categorical_vars <- c('sex', 'primary_caregiver', 'ethnic_minority', 'house_ownership', 
                      'employment_mother', 'employment_father', 'help_seeking_intention', 
                      'kindergarten_entrance_exam', 'elementary_school_entrance_exam', 'tutoring_school', 
                      'chronic_health_problems', 'special_needs', 'overweight', 
                      'separation_mother', 'separation_father', 'elder_sibling', 'smoker_in_household', 
                      'chronic_health_problems_mother', 'chronic_health_problems_father', 
                      'park', 'neighborhood_with_friends_or_relatives', 
                      'class_parallel_process_lcga')
d1[categorical_vars] <- lapply(d1[categorical_vars], factor)

selected_vars <- c('sex', 'primary_caregiver', 'ethnic_minority', 'income', 'education_mother', 'education_father', 
                   'house_ownership', 'employment_father', 'employment_mother', 
                   'pubertal_stage', 'iq', 'help_seeking_intention', 'aspiration', 'self_control', 'prosocial_behavior', 
                   'kindergarten_entrance_exam', 'elementary_school_entrance_exam', 'tutoring_school',
                   'chronic_health_problems', 'special_needs', 'overweight', 
                   'separation_mother', 'separation_father', 'sibling', 'elder_sibling', 'grandparent',  
                   'smoker_in_household', 'life_satisfaction_of_caregiver', 'chronic_health_problems_mother', 'chronic_health_problems_father', 
                   'cooperative_spouse_in_childcare', 'satisfied_with_family', 'talk_with_caregiver', 
                   'want_to_be_like_mother', 'want_to_be_like_father', 'family_involvement_in_future_carrer_preferences', 'caregiver_life_plan_is_stability_oriented', 
                   'like_school', 'social_relationships_outside_school', 'park', 'neighborhood_with_friends_or_relatives', 
                   'good_neighborhood_for_childcare', 'neighborhood_cohesion_and_trust', 
                   'class_parallel_process_lcga')
df <- d1[selected_vars]
imp_df <- mice(df, m=100, maxit=50, seed=1234)

# Preparation for pairwise comparison 1
df$class_parallel_process_lcga <- factor(df$class_parallel_process_lcga, levels=c('2', '1', '3', '4'))
imp_df_ref2 <- mice(df, m=100, maxit=50, seed=1234)

# Preparation for pairwise comparison 2
df$class_parallel_process_lcga <- factor(df$class_parallel_process_lcga, levels=c('3', '1', '2', '4'))
imp_df_ref3 <- mice(df, m=100, maxit=50, seed=1234)


### Multinomial logistic regression analysis
# Following variables were excluded: Primary caregiver, employment of mother/father, house ownership, help-seeking intention, park, and neighborhood with friends or relatives

equation <- 'class_parallel_process_lcga~sex+ethnic_minority+income+education_mother+education_father+pubertal_stage+iq+aspiration+self_control+prosocial_behavior+kindergarten_entrance_exam+elementary_school_entrance_exam+tutoring_school+chronic_health_problems+special_needs+overweight+separation_mother+separation_father+sibling+elder_sibling+grandparent+smoker_in_household+life_satisfaction_of_caregiver+chronic_health_problems_mother+chronic_health_problems_father+cooperative_spouse_in_childcare+satisfied_with_family+talk_with_caregiver+want_to_be_like_mother+want_to_be_like_father+family_involvement_in_future_carrer_preferences+caregiver_life_plan_is_stability_oriented+like_school+social_relationships_outside_school+good_neighborhood_for_childcare+neighborhood_cohesion_and_trust'

fit <-with(imp_df, nnet::multinom(as.formula(equation)))
summary <- summary(pool(fit))
OR <- exp(summary$estimate)
ORlow <- exp(summary$estimate-1.96*summary$std.error)
ORhigh <- exp(summary$estimate+1.96*summary$std.error)
result <- cbind(summary, OR, ORlow, ORhigh)
result <- result[, c('term', 'y.level', 'OR', 'ORlow', 'ORhigh', 'p.value')] %>%
  filter(term != "(Intercept)") %>%
  mutate(OR = round(OR, 2), ORlow = round(ORlow, 2), ORhigh = round(ORhigh, 2), p.value = round(p.value, 5), `95%CI` = paste(ORlow, "-", ORhigh))
result <- result[, c('term', 'y.level', 'OR', '95%CI', 'p.value')]
result <- result %>%
  pivot_wider(names_from = y.level, values_from = c(OR, `95%CI`, p.value), names_sep = "_")
result<- result[, c('term','OR_2', '95%CI_2', 'p.value_2', 'OR_3', '95%CI_3', 'p.value_3', 'OR_4', '95%CI_4', 'p.value_4')]

write.csv(result, 'multinominal_logistic_regression_result.csv')

# VIF
dataimp <- complete(imp_df, action='long')
dataimp <- dataimp[, -2]
res <- data.frame()
for (i in 1:100){
  d <- dataimp[dataimp$.imp==i, ]
  d <- d[, -1]
  model <- nnet::multinom(as.formula(equation), d)
  vif <- data.frame(car::vif(model))
  vifresult <- data.frame(t(as.vector(vif[,1]))) 
  colnames(vifresult) <- as.vector(rownames(vif))
  vifresult$m=i
  res <- rbind(res, vifresult)
}
write.csv(res, 'multinominal_logistic_regeression_vif.csv')


### Pairwise logistic regression: 
# Using class 2 (high SWB-mid PP) as a reference
fit <-with(imp_df_ref2, nnet::multinom(as.formula(equation)))
summary <- summary(pool(fit))
OR <- exp(summary$estimate)
ORlow <- exp(summary$estimate-1.96*summary$std.error)
ORhigh <- exp(summary$estimate+1.96*summary$std.error)
result <- cbind(summary, OR, ORlow, ORhigh)

result <- result[, c('term', 'y.level', 'estimate', 'std.error', 'OR', 'ORlow', 'ORhigh', 'p.value')] %>%
  filter(term != "(Intercept)"&y.level!='1'&y.level!='4') %>%
  mutate(OR = round(OR, 2), ORlow = round(ORlow, 2), ORhigh = round(ORhigh, 2), p.value = round(p.value, 5), `95%CI` = paste(ORlow, "-", ORhigh))
result <- result[, c('term', 'y.level', 'OR', '95%CI', 'p.value')]
result <- result %>%
  pivot_wider(names_from = y.level, values_from = c(OR, `95%CI`, p.value), names_sep = "_")
result<- result[, c('OR_3', '95%CI_3', 'p.value_3')]
write.csv(result, 'pairwise_logistic_regression_ref2_result.csv')

# Using class 3 (low SWB-mid PP) as a reference
fit <-with(imp_df_ref3, nnet::multinom(as.formula(equation)))
summary <- summary(pool(fit))
OR <- exp(summary$estimate)
ORlow <- exp(summary$estimate-1.96*summary$std.error)
ORhigh <- exp(summary$estimate+1.96*summary$std.error)
result <- cbind(summary, OR, ORlow, ORhigh)

result <- result[, c('term', 'y.level', 'estimate', 'std.error', 'OR', 'ORlow', 'ORhigh', 'p.value')] %>%
  filter(term != "(Intercept)"&y.level!='1'&y.level!='2') %>%
  mutate(OR = round(OR, 2), ORlow = round(ORlow, 2), ORhigh = round(ORhigh, 2), p.value = round(p.value, 5), `95%CI` = paste(ORlow, "-", ORhigh))

result <- result[, c('term', 'y.level', 'OR', '95%CI', 'p.value')]
result <- result %>%
  pivot_wider(names_from = y.level, values_from = c(OR, `95%CI`, p.value), names_sep = "_")

result<- result[, c('term', 'OR_4', '95%CI_4', 'p.value_4')]
write.csv(result, 'pairwise_logistic_regression_ref3_result.csv')
