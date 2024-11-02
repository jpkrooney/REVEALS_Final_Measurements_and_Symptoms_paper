library(tidyverse)
library(tableone)
library(patchwork)
library(brms)
library(tidybayes)
library(bayesplot)
library(future)
library(corrplot)
library(lme4)
library(openxlsx)

# Set brms options for multithreading
options(mc.cores = 8, brms.backend = "cmdstanr")

# Get support functions

##### Load data #####
dflong <- readRDS("../Final_dataset/REVEALS_finallongitudinaldata.rds")
dfbase <- readRDS("../Final_dataset/REVEALS_finalbaselinedata.rds")
dfeq5 <- read.xlsx("../Final_dataset/Final Reveals Jan 2022.xlsx", sheet = "EQ5D5L", detectDates = FALSE)
dfeq5 <- janitor::clean_names(dfeq5)

# join eq5 data to long data - drop drom dflong where same name
dflong <- dflong %>% 
    dplyr::select(-mobility, -self_care, -usual_act, -pain_dis_com, -anx_dep) %>% 
    left_join(dfeq5)
dflong <- dflong %>% 
    mutate(mobility = factor(case_when(mobility == 1 ~ "No problems",
                                mobility == 2 ~ "Slight problems",
                                mobility == 3 ~ "Moderate problems",
                                mobility == 4 ~ "Severe problems",
                                mobility == 5 ~ "Unable to walk"),
                             ordered = TRUE, levels = c("No problems", "Slight problems",
                                        "Moderate problems", "Severe problems", "Unable to walk")),
           self_care = factor(case_when(self_care == 1 ~ "No problems",
                                       self_care == 2 ~ "Slight problems",
                                       self_care == 3 ~ "Moderate problems",
                                       self_care == 4 ~ "Severe problems",
                                       self_care == 5 ~ "Unable to wash/dress"),
                             ordered = TRUE, levels = c("No problems", "Slight problems",
                                                        "Moderate problems", "Severe problems",
                                                        "Unable to wash/dress")),
           usual_act = factor(case_when(usual_act == 1 ~ "No problems",
                                        usual_act == 2 ~ "Slight problems",
                                        usual_act == 3 ~ "Moderate problems",
                                        usual_act == 4 ~ "Severe problems",
                                        usual_act == 5 ~ "Unable to do activities"),
                              ordered = TRUE, levels = c("No problems", "Slight problems",
                                                         "Moderate problems", "Severe problems",
                                                         "Unable to do activities")),
           pain_dis_com = factor(case_when(pain_dis_com == 1 ~ "No pain or discomfort",
                                        pain_dis_com == 2 ~ "Slight pain or discomfort",
                                        pain_dis_com == 3 ~ "Moderate pain or discomfort ",
                                        pain_dis_com == 4 ~ "Severe pain or discomfort ",
                                        pain_dis_com == 5 ~ "Extreme pain or discomfort "),
                              ordered = TRUE, levels = c("No pain or discomfort", "Slight pain or discomfort",
                                                         "Moderate pain or discomfort ",
                                                         "Severe pain or discomfort ",
                                                         "Extreme pain or discomfort ")))


##### Data preparation #####
dflong$uin <- as.factor(dflong$uin)
# extract unique IDS
id_list <- unique(dflong$uin)


# Determine number of longitudinal measurements per person
outcome_counts <- dflong %>% count(uin)
dfbase$count_long <- outcome_counts[ match(dfbase$uin, outcome_counts$uin), ]$n

# Exclude any individuals not at stage 2 or 3 at presentation
dfbase <- dfbase[ dfbase$staging %in% c("Kings 2", "Kings 3") |
                      is.na(dfbase$staging), ]
dflong <- dflong[dflong$uin %in% dfbase$uin, ]


# Rename outcome variables with shorter names to make model specification more concise
dflong <- dflong %>% rename(outfvc = fvc_max_l,
                            outsvc = svc_l,
                            predfvc = fvc_max_percent_pred,
                            predsvc = svc_percent_pred,
                            outsnip = snip_max_score_cm_h2o,
                            outpeak = peak_cough_flow_max_score)

# Regroup factor variables for model building
dflong <- dflong %>% 
    mutate(mcgillsimp = case_when(mc_gill_qol_scale %in% 0:3 ~ "0 to 3",
                                  mc_gill_qol_scale %in% 4:6 ~ "4 to 6",
                                  mc_gill_qol_scale %in% 7:10 ~ "7 to 10"),
           mcgillsimp = factor(mcgillsimp, ordered = TRUE, levels = c("0 to 3",
                                                                      "4 to 6",
                                                                      "7 to 10")))
dflong$fatigue_vas <- factor(dflong$fatigue_vas, ordered = TRUE)

dflong <- dflong %>% 
    mutate(fatigvas = case_when(fatigue_vas %in% 0:3 ~ "0 to 3",
                                fatigue_vas %in% 4:6 ~ "4 to 6",
                                fatigue_vas %in% 7:10 ~ "7 to 10"),
           fatigvas = factor(fatigvas, ordered = TRUE, levels = c("0 to 3",
                                                                  "4 to 6",
                                                                  "7 to 10")))
dflong$pittsburgh_sleep_quality <- factor(dflong$pittsburgh_sleep_quality, ordered = TRUE)

dflong$difficulty_clearing_chest_in_past_week <- factor(dflong$difficulty_clearing_chest_in_past_week, ordered = TRUE)
dflong$difficulty_clearing_nose_in_past_week <- factor(dflong$difficulty_clearing_nose_in_past_week, ordered = TRUE)
dflong$difficulty_clearing_saliva_in_past_week <- factor(dflong$difficulty_clearing_saliva_in_past_week, ordered = TRUE)

dflong <- dflong %>% 
    mutate(fatigclear = case_when(fatigue_with_clearing_sec %in% 0:2 ~ "0 to 2",
                                  fatigue_with_clearing_sec %in% 3:6 ~ "3 to 6",
                                  fatigue_with_clearing_sec %in% 7:10 ~ "7 to 10"),
           fatigclear = factor(fatigclear, ordered = TRUE, levels = c("0 to 2",
                                                                      "3 to 6",
                                                                      "7 to 10")))



######## Comparisons of respiratory outcomes using REVEALS dataset #######

#### Make Table 1 of descriptive statistics of key variables ####
vars <- c("gender", "site_onset", "staging","height_cm", 
          "weight_kg", "age_dx", "onset2censor_months", "count_long")
non_par <- c("onset2censor_months", "count_long")

tab1 <- CreateTableOne(vars, strata = "cohort", data = dfbase )
tab1b <- print(tab1, nonnormal = non_par,
               exact = c("gender", "site_onset", "staging"),
               quote = FALSE, noSpaces = TRUE,
               printToggle = FALSE, showAllLevels = TRUE)
tab1_total <- CreateTableOne(vars, data = dfbase)
tab1_total <- print(tab1_total, nonnormal = non_par,
                    exact = c("gender", "site_onset", "staging"),
                    quote = FALSE, noSpaces = TRUE,
                    printToggle = FALSE, showAllLevels = TRUE)
table1 <- cbind(tab1b, tab1_total)
write.csv(table1, "Results/NewRevealsAnalysistable1.csv")

# How many in each cohort have only one measurement ?
table(dfbase$count_long, dfbase$cohort)



# Summarise longitudinal variables
longvars <- c("outfvc", "outsvc", "predfvc", "predsvc", "outsnip", "outpeak",
              "alsfrs_resp", "total_eq5d5", "breathless_in_lying",
              "sob_rest", "sob_when_active",
              "difficulty_clearing_chest_in_past_week",
              "difficulty_clearing_nose_in_past_week",
              "difficulty_clearing_saliva_in_past_week",
              "regular_productive_cough", 
              "pittsburgh_sleep_quality",
              "zarit_score",
              "mcgillsimp", "fatigvas", 
              "fatigclear")

tablongbase <- CreateTableOne(longvars, strata = "cohort",
                              data = dflong %>% 
                                  filter(ax_number ==1))
tablongbaseB <- print(tablongbase, #nonnormal = longvars,
                  quote = FALSE, noSpaces = TRUE,
                  printToggle = FALSE, showAllLevels = TRUE)
tablongbase_total <- CreateTableOne(longvars, data = dflong %>% 
                                        filter(ax_number ==1))
tablongbase_total <- print(tablongbase_total, nonnormal = longvars,
                       quote = FALSE, noSpaces = TRUE,
                       printToggle = FALSE, showAllLevels = TRUE)
tablelongbase <- cbind(tablongbaseB, tablongbase_total)
write.csv(tablelongbase , "Results/Longitudinal_baseline_summary_table.csv")


# Missing values are < 5% in outcome variables. Therefore we can remove these rows without significant bias (only 2 rows are missing the time variable).
df2 <- dflong[ rowSums( !is.na(dflong[, c("predfvc", "predsvc", "outsnip",
                                          "outpeak")]) ) == 4 ,  ]




#### Prior Predictive checks Bernoulli models ####
bf_breathlesslying_fvc <- bf(breathless_in_lying ~ days_from_baseline*sexsite + cohort + predfvc + age +
                                 (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()

priorbern1 <- c(set_prior("normal(0, 100)", class = "b" ),
          set_prior("student_t(5, 0, 5)", class = "sd" ))
priorbern2 <- c(set_prior("normal(0, 10)", class = "b" ))

fit_priorberntest1 <- brm(bf_breathlesslying_fvc,
                          data = df2, chains=4, threads = threading(2),
                          prior = priorbern1,
                          seed = 3453, sample_prior = "only")
fit_priorberntest2 <- brm(bf_breathlesslying_fvc,
                          data = df2, chains=4, threads = threading(2),
                          prior = priorbern2,
                          seed = 3453, sample_prior = "only")
pp_check(fit_priorberntest1, ndraws = 200) / pp_check(fit_priorberntest2, ndraws = 200)

# fit with data and check posterior predictive checks
fit_databerntest1 <- brm(bf_breathlesslying_fvc,
                          data = df2, chains=4, threads = threading(2),
                          prior = priorbern1,
                          seed = 3453)
fit_databerntest2 <- brm(bf_breathlesslying_fvc,
                          data = df2, chains=4, threads = threading(2),
                          prior = priorbern2,
                          seed = 3453)
pp_check(fit_databerntest1, ndraws = 200) / pp_check(fit_databerntest2, ndraws = 200)

# Bernoulli models appear relatively insensitive to priors
# Therefore will use the weakly informative prior priorbern1


#### Prior Predictive checks ordinal models ####
# use mcgill sleep score after grouping categories
table(df2$mc_gill_qol_scale, useNA = "ifany")
table(df2$mc_gill_qol_scale)
table(df2$mcgillsimp)

bf_mcgill_fvc <- bf(mcgillsimp ~ days_from_baseline*sexsite + cohort + predfvc + age +
                        (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()

priorord1 <- c(set_prior("normal(0, 100)", class = "b" ),
                set_prior("student_t(5, 0, 5)", class = "sd" ))
priorord2 <- c(set_prior("normal(0, 10)", class = "b" ))


fit_priorordtest1 <- brm(bf_mcgill_fvc,
                      data = df2, chains=4, threads = threading(2),
                      prior = priorord1,
                      seed = 5736434, sample_prior = "only")
fit_priorordtest2 <- brm(bf_mcgill_fvc,
                         data = df2, chains=4, threads = threading(2),
                         prior = priorord2,
                         seed = 5736434, sample_prior = "only")

pp_check(fit_priorordtest1, ndraws = 200) / pp_check(fit_priorordtest2, ndraws = 200)

# fit with data and check posterior predictive checks
fit_dataordtest1 <- brm(bf_mcgill_fvc,
                         data = df2, chains=4, threads = threading(2),
                         prior = priorord1,
                         seed = 5736434)
fit_dataordtest2 <- brm(bf_mcgill_fvc,
                         data = df2, chains=4, threads = threading(2),
                         prior = priorord2,
                         seed = 5736434)

pp_check(fit_dataordtest1, ndraws = 200) / pp_check(fit_dataordtest2, ndraws = 200)

# Ordinal models also appear relatively insensitive to priors
# Therefore will use the weakly informative prior priorord1



#### Prior Predictive checks continuous models ####
bf_eq5d5lind_svc <- bf(eq_5d_5l_index_value ~ days_from_baseline*sexsite + cohort + predsvc + age +
                           (1 | p | uin) + (0 + days_from_baseline | uin))

priorcont1 <- c(set_prior("normal(0, 10)", class = "b" ))
priorcont2 <- c(set_prior("normal(0, 1)", class = "b" ))


fit_datacontinuous_test1 <- brm(bf_eq5d5lind_svc,
                               prior = priorcont1,
                         data = df2, chains=4, seed = 5736434, threads = threading(2),
                         sample_prior = "only")
fit_datacontinuous_test2 <- brm(bf_eq5d5lind_svc,
                                prior = priorcont2,
                                data = df2, chains=4, seed = 5736434, threads = threading(2),
                                sample_prior = "only")

pp_check(fit_datacontinuous_test1, ndraws = 200) / pp_check(fit_datacontinuous_test2, ndraws = 200)
# model relatively insensitive to prior but priorcont2 selected as gives slightly better containment of parameter space




#### Main Analysis ####
# Model each of the exploratory hypotheses variables in turn for the relevant outcomes variable

#### FVC/SVC models ####

## "breathless_in_lying" ##
table(df2$breathless_in_lying, useNA = "ifany")
# Variable is dichotomous will use binomial family

bf_breathlesslying_fvc <- bf(breathless_in_lying ~ days_from_baseline*sexsite + cohort + predfvc + age +
                                 (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()
bf_breathlesslying_svc <- bf(breathless_in_lying ~ days_from_baseline*sexsite + cohort + predsvc + age +
                                 (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()


fit_breathless_fvc <- brm(bf_breathlesslying_fvc, prior = priorbern1,
                          data = df2, chains=4, seed = 3453, threads = threading(2))
fit_breathless_svc <- brm(bf_breathlesslying_svc, prior = priorbern1,
                          data = df2, chains=4, seed = 5675, threads = threading(2))


# "sob_rest"
bf_sobrest_fvc <- bf(sob_rest ~ days_from_baseline*sexsite + cohort + predfvc + age +
                         (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()
bf_sobrest_svc <- bf(sob_rest ~ days_from_baseline*sexsite + cohort + predsvc + age +
                         (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()

fit_sobrest_fvc <- brm(bf_sobrest_fvc, prior = priorbern1,
                       data = df2, chains=4, seed = 29384, threads = threading(2))
fit_sobrest_svc <- brm(bf_sobrest_svc, prior = priorbern1,
                       data = df2, chains=4, seed = 3453456, threads = threading(2))


# "sob_when_active"
bf_sobwhenactive_fvc <- bf(sob_when_active ~ days_from_baseline*sexsite + cohort + predfvc + age + 
                               (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()
bf_sobwhenactive_svc <- bf(sob_when_active ~ days_from_baseline*sexsite + cohort + predsvc + age +
                               (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()

fit_sobwhenactive_fvc <- brm(bf_sobwhenactive_fvc, prior = priorbern1,
                             data = df2, chains=4, seed = 4364, threads = threading(2))
fit_sobwhenactive_svc <- brm(bf_sobwhenactive_svc, prior = priorbern1,
                             data = df2, chains=4, seed = 474575, threads = threading(2))



# save FVC/SVC SOB models
saveRDS(fit_breathless_fvc, "FittedModels/brms_breathless_fvc.RDS")
saveRDS(fit_breathless_svc, "FittedModels/brms_breathless_svc.RDS")
saveRDS(fit_sobrest_fvc, "FittedModels/brms_sobrest_fvc.RDS")
saveRDS(fit_sobrest_svc, "FittedModels/brms_sobrest_svc.RDS")
saveRDS(fit_sobwhenactive_fvc, "FittedModels/brms_sobwhenactive_fvc.RDS")
saveRDS(fit_sobwhenactive_svc, "FittedModels/brms_sobwhenactive_svc.RDS")



# "fatigue_vas"
table(df2$fatigue_vas, useNA = "ifany")
table(df2$fatigue_vas)
table(df2$fatigvas)


bf_fatigue_vas_fvc <- bf(fatigvas ~ days_from_baseline*sexsite + cohort + predfvc + age +
                             (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()
bf_fatigue_vas_svc <- bf(fatigvas ~ days_from_baseline*sexsite + cohort + predsvc + age +
                             (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()

fit_fatigue_vas_fvc <- brm(bf_fatigue_vas_fvc, prior = priorord1,
                           data = df2, chains=4, seed = 5736434, threads = threading(2))
fit_fatigue_vas_svc <- brm(bf_fatigue_vas_svc, prior = priorord1,
                           data = df2, chains=4, seed = 69078, threads = threading(2))

# save fatigue models
saveRDS(fit_fatigue_vas_fvc, "FittedModels/brms_fatigue_vas_fvc.RDS")
saveRDS(fit_fatigue_vas_svc, "FittedModels/brms_fatigue_vas_svc.RDS")


# "mc_gill_qol_scale"

bf_mcgill_fvc <- bf(mcgillsimp ~ days_from_baseline*sexsite + cohort + predfvc + age +
                        (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()
bf_mcgill_svc <- bf(mcgillsimp ~ days_from_baseline*sexsite + cohort + predsvc + age +
                        (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()

fit_mcgill_fvc <- brm(bf_mcgill_fvc, prior = priorord1,
                      data = df2, chains=4, seed = 5736434, threads = threading(2))
fit_mcgill_svc <- brm(bf_mcgill_svc, prior = priorord1,
                      data = df2, chains=4, seed = 69078, threads = threading(2))

# save mcgill models
saveRDS(fit_mcgill_fvc, "FittedModels/brms_mcgill_fvc.RDS")
saveRDS(fit_mcgill_svc, "FittedModels/brms_mcgill_svc.RDS")



# "pittsburgh_sleep_quality"
table(df2$pittsburgh_sleep_quality, useNA = "ifany")
ggplot(df2, aes(x = pittsburgh_sleep_quality)) + geom_bar()

bf_pitts_fvc <- bf(pittsburgh_sleep_quality ~ days_from_baseline*sexsite + cohort + predfvc + age +
                       (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()
bf_pitts_svc <- bf(pittsburgh_sleep_quality ~ days_from_baseline*sexsite + cohort + predsvc + age +
                       (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()

fit_pitts_fvc <- brm(bf_pitts_fvc, prior = priorord1,
                     data = df2, chains=4, seed = 5736434, threads = threading(2))
fit_pitts_svc <- brm(bf_pitts_svc, prior = priorord1,
                     data = df2, chains=4, seed = 69078, threads = threading(2))


# save pitts models
saveRDS(fit_pitts_fvc, "FittedModels/brms_pitts_fvc.RDS")
saveRDS(fit_pitts_svc, "FittedModels/brms_pitts_svc.RDS")


# eq5d5l
bf_eq5d5lind_svc <- bf(eq_5d_5l_index_value ~ days_from_baseline*sexsite + cohort + predsvc + age +
                       (1 | p | uin) + (0 + days_from_baseline | uin))
bf_eq5d5lind_fvc <- bf(eq_5d_5l_index_value ~ days_from_baseline*sexsite + cohort + predfvc + age +
                           (1 | p | uin) + (0 + days_from_baseline | uin))

fit_eq5d5lind_svc <- brm(bf_eq5d5lind_svc,
                         prior = priorcont2,
                     data = df2, chains=4, seed = 5736434, threads = threading(2))
fit_eq5d5lind_fvc <- brm(bf_eq5d5lind_fvc,
                         prior = priorcont2,
                         data = df2, chains=4, seed = 5736434, threads = threading(2))

# save eq5 model
saveRDS(fit_eq5d5lind_svc, "FittedModels/brms_eq5d5l_svc.RDS")
saveRDS(fit_eq5d5lind_fvc, "FittedModels/brms_eq5d5l_fvc.RDS")



#### SNIP models ####
# 
bf_breathlesslying_snip <- bf(breathless_in_lying ~ days_from_baseline*sexsite + cohort + outsnip + age +
                                  (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()

fit_breathless_snip <- brm(bf_breathlesslying_snip, prior = priorbern1,
                           data = df2, chains=4, seed = 3453, threads = threading(2))

# "sob_rest"
bf_sobrest_snip <- bf(sob_rest ~ days_from_baseline*sexsite + cohort + outsnip + age +
                          (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()

fit_sobrest_snip <- brm(bf_sobrest_snip, prior = priorbern1,
                        data = df2, chains=4, seed = 29384, threads = threading(2))
# "sob_when_active"
bf_sobwhenactive_snip <- bf(sob_when_active ~ days_from_baseline*sexsite + cohort + outsnip + age + 
                                (1 | p | uin) + (0 + days_from_baseline | uin)) + bernoulli()
fit_sobwhenactive_snip <- brm(bf_sobwhenactive_snip, prior = priorbern1,
                              data = df2, chains=4, seed = 4364, threads = threading(2))
# save snip SOB models
saveRDS(fit_breathless_snip, "FittedModels/brms_breathless_snip.RDS")
saveRDS(fit_sobrest_snip, "FittedModels/brms_sobrest_snip.RDS")
saveRDS(fit_sobwhenactive_snip, "FittedModels/brms_sobwhenactive_snip.RDS")



# "mc_gill_qol_scale"

bf_mcgill_snip <- bf(mcgillsimp ~ days_from_baseline*sexsite + cohort + outsnip + age +
                        (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()

fit_mcgill_snip <- brm(bf_mcgill_snip, prior = priorord1,
                      data = df2, chains=4, seed = 5736434, threads = threading(2))


# "pittsburgh_sleep_quality"
bf_pitts_snip <- bf(pittsburgh_sleep_quality ~ days_from_baseline*sexsite + cohort + outsnip + age +
                       (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()

fit_pitts_snip <- brm(bf_pitts_snip, prior = priorord1,
                     data = df2, chains=4, seed = 5736434, threads = threading(2))


# "fatigue_vas"
bf_fatigue_vas_snip <- bf(fatigvas ~ days_from_baseline*sexsite + cohort + outsnip + age +
                             (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()

fit_fatigue_vas_snip <- brm(bf_fatigue_vas_snip, prior = priorord1,
                           data = df2, chains=4, seed = 5736434, threads = threading(2))


# save models
saveRDS(fit_mcgill_snip, "FittedModels/brms_mcgill_snip.RDS")
saveRDS(fit_pitts_snip, "FittedModels/brms_pitts_snip.RDS")
saveRDS(fit_fatigue_vas_snip, "FittedModels/brms_fatigue_vas_snip.RDS")



#### PCF models ####
table(df2$difficulty_clearing_chest_in_past_week, useNA = "ifany")
table(df2$difficulty_clearing_nose_in_past_week, useNA = "ifany")
table(df2$difficulty_clearing_saliva_in_past_week, useNA = "ifany")

gchest <- ggplot(df2, aes(x= difficulty_clearing_chest_in_past_week)) + geom_bar()
gnose <- ggplot(df2, aes(x= difficulty_clearing_nose_in_past_week)) + geom_bar()
gsaliva <- ggplot(df2, aes(x= difficulty_clearing_saliva_in_past_week)) + geom_bar()
gchest + gnose + gsaliva


bf_diff_chest_peak <- bf(difficulty_clearing_chest_in_past_week ~ days_from_baseline*sexsite + 
                             cohort + outpeak + age + (1 | p | uin) +
                             (0 + days_from_baseline | uin)) + cumulative()

bf_diff_nose_peak <- bf(difficulty_clearing_nose_in_past_week ~ days_from_baseline*sexsite + 
                             cohort + outpeak + age + (1 | p | uin) +
                             (0 + days_from_baseline | uin)) + cumulative()

bf_diff_saliva_peak <- bf(difficulty_clearing_saliva_in_past_week ~ days_from_baseline*sexsite + 
                            cohort + outpeak + age + (1 | p | uin) +
                            (0 + days_from_baseline | uin)) + cumulative()

bf_repro_cough_peak <- bf(regular_productive_cough ~ days_from_baseline*sexsite + 
                              cohort + outpeak + age + (1 | p | uin) +
                              (0 + days_from_baseline | uin)) + bernoulli()


fit_diff_chest_peak <- brm(bf_diff_chest_peak, prior = priorord1,
                           data = df2, chains=4, seed = 686797,threads = threading(2))

fit_diff_nose_peak <- brm(bf_diff_nose_peak, prior = priorord1,
                           data = df2, chains=4, seed = 686797,threads = threading(2))

fit_diff_saliva_peak <- brm(bf_diff_saliva_peak, prior = priorord1,
                           data = df2, chains=4, seed = 686797,threads = threading(2))

fit_repro_cough_peak <- brm(bf_repro_cough_peak, prior = priorbern1,
                            data = df2, chains=4, seed = 686797,threads = threading(2))

# save diff_peak models
saveRDS(fit_diff_chest_peak, "FittedModels/brms_diffchest_peak.RDS")
saveRDS(fit_diff_nose_peak, "FittedModels/brms_diffnose_peak.RDS")
saveRDS(fit_diff_saliva_peak, "FittedModels/brms_diffsaliva_peak.RDS")
saveRDS(fit_repro_cough_peak, "FittedModels/brms_repro_cough_peak.RDS")



# fatigue with secretions
table(df2$fatigue_with_clearing_sec, useNA = "ifany")
table(df2$fatigue_with_clearing_sec)
table(df2$fatigclear)


bf_fatigclear_peak <- bf(fatigclear ~ days_from_baseline*sexsite + 
                             cohort + outpeak + age + (1 | p | uin) +
                             (0 + days_from_baseline | uin)) + cumulative()

fit_fatigclear_peak <- brm(bf_fatigclear_peak, prior = priorord1,
                           data = df2, chains=4, seed = 686797,threads = threading(2))

saveRDS(fit_fatigclear_peak, "FittedModels/brms_fatigclear_peak.RDS")



# "mc_gill_qol_scale"
bf_mcgill_peak <- bf(mcgillsimp ~ days_from_baseline*sexsite + cohort + outpeak + age +
                        (1 | p | uin) + (0 + days_from_baseline | uin)) + cumulative()
fit_mcgill_peak <- brm(bf_mcgill_peak, prior = priorord1,
                      data = df2, chains=4, seed = 69078, threads = threading(2))

# save mcgill models
saveRDS(fit_mcgill_peak, "FittedModels/brms_mcgill_peak.RDS")



