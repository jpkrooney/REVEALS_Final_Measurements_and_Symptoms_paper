library(tidyverse)
library(patchwork)
library(brms)
library(future)
library(openxlsx)

# Set brms options for multithreading
options(mc.cores = 8, brms.backend = "cmdstanr")

# Get support functions

##### Load data #####
# load FVC/SVC models
fit_breathless_fvc <- readRDS("FittedModels/brms_breathless_fvc.RDS")
fit_breathless_svc <- readRDS("FittedModels/brms_breathless_svc.RDS")
fit_sobrest_fvc <- readRDS("FittedModels/brms_sobrest_fvc.RDS")
fit_sobrest_svc <- readRDS("FittedModels/brms_sobrest_svc.RDS")
fit_sobwhenactive_fvc <- readRDS("FittedModels/brms_sobwhenactive_fvc.RDS")
fit_sobwhenactive_svc <- readRDS("FittedModels/brms_sobwhenactive_svc.RDS")

fit_fatigue_vas_fvc <- readRDS("FittedModels/brms_fatigue_vas_fvc.RDS")
fit_fatigue_vas_svc <- readRDS( "FittedModels/brms_fatigue_vas_svc.RDS")
fit_mcgill_fvc <- readRDS("FittedModels/brms_mcgill_fvc.RDS")
fit_mcgill_svc <- readRDS("FittedModels/brms_mcgill_svc.RDS")
fit_pitts_fvc <- readRDS("FittedModels/brms_pitts_fvc.RDS")
fit_pitts_svc <- readRDS("FittedModels/brms_pitts_svc.RDS")

fit_eq5d5lind_svc <- readRDS("FittedModels/brms_eq5d5l_svc.RDS")
fit_eq5d5lind_fvc <- readRDS("FittedModels/brms_eq5d5l_fvc.RDS")

# SNIP models
fit_breathless_snip <- readRDS("FittedModels/brms_breathless_snip.RDS")
fit_sobrest_snip <- readRDS("FittedModels/brms_sobrest_snip.RDS")
fit_sobwhenactive_snip <- readRDS("FittedModels/brms_sobwhenactive_snip.RDS")

fit_mcgill_snip <- readRDS("FittedModels/brms_mcgill_snip.RDS")
fit_pitts_snip <- readRDS("FittedModels/brms_pitts_snip.RDS")
fit_fatigue_vas_snip <- readRDS("FittedModels/brms_fatigue_vas_snip.RDS")

# PCF models
fit_diff_chest_peak <- readRDS("FittedModels/brms_diffchest_peak.RDS")
fit_diff_nose_peak <- readRDS("FittedModels/brms_diffnose_peak.RDS")
fit_diff_saliva_peak <- readRDS("FittedModels/brms_diffsaliva_peak.RDS")
fit_fatigclear_peak <- readRDS("FittedModels/brms_fatigclear_peak.RDS")
fit_mcgill_peak <- readRDS("FittedModels/brms_mcgill_peak.RDS")
fit_prod_cough_peak <- readRDS("FittedModels/brms_repro_cough_peak.RDS")



#### First summarise continuous models ####
summary(fit_eq5d5lind_svc)
summary(fit_eq5d5lind_fvc)




#### generate conditional effects ####
pldat_breathless_fvc <- conditional_effects(fit_breathless_fvc, "predfvc")
pldat_breathless_svc <- conditional_effects(fit_breathless_svc, "predsvc")
pldat_sobrest_fvc <- conditional_effects(fit_sobrest_fvc, "predfvc")
pldat_sobrest_svc <- conditional_effects(fit_sobrest_svc, "predsvc")
pldat_sobwhenactive_fvc <- conditional_effects(fit_sobwhenactive_fvc, "predfvc")
pldat_sobwhenactive_svc <- conditional_effects(fit_sobwhenactive_svc, "predsvc")
pldat_fatigue_vas_fvc <- conditional_effects(fit_fatigue_vas_fvc, effects = "predfvc", categorical = TRUE)
pldat_fatigue_vas_svc <- conditional_effects(fit_fatigue_vas_svc, effects = "predsvc", categorical = TRUE)
pldat_mcgill_fvc <- conditional_effects(fit_mcgill_fvc, effects = "predfvc", categorical = TRUE)
pldat_mcgill_svc <- conditional_effects(fit_mcgill_svc, effects = "predsvc", categorical = TRUE)
pldat_pitts_fvc <- conditional_effects(fit_pitts_fvc, effects = "predfvc", categorical = TRUE)
pldat_pitts_svc <- conditional_effects(fit_pitts_svc, effects = "predsvc", categorical = TRUE)


pldat_breathless_fvcsexsite <- conditional_effects(fit_breathless_fvc, "predfvc:sexsite")
pldat_sobrest_fvcsexsite <- conditional_effects(fit_sobrest_fvc, "predfvc:sexsite")
pldat_sobwhenactive_fvcsexsite <- conditional_effects(fit_sobwhenactive_fvc, "predfvc:sexsite")


pldat_breathless_fvcsexsite$`predfvc:sexsite`$sex <- factor(
    str_split(pldat_breathless_fvcsexsite$`predfvc:sexsite`$sexsite,
              " ", simplify = TRUE)[,1], levels = c("Male", "Female"))
pldat_sobrest_fvcsexsite$`predfvc:sexsite`$sex <- factor(
    str_split(pldat_sobrest_fvcsexsite$`predfvc:sexsite`$sexsite,
              " ", simplify = TRUE)[,1], levels = c("Male", "Female"))
pldat_sobwhenactive_fvcsexsite$`predfvc:sexsite`$sex <- factor(
    str_split(pldat_sobwhenactive_fvcsexsite$`predfvc:sexsite`$sexsite,
              " ", simplify = TRUE)[,1], levels = c("Male", "Female"))





# SNIP models
pldat_breathless_snip <- conditional_effects(fit_breathless_snip, effects = "outsnip")
pldat_sobrest_snip <- conditional_effects(fit_sobrest_snip, effects = "outsnip")
pldat_sobwhenactive_snip <- conditional_effects(fit_sobwhenactive_snip, effects = "outsnip")
pldat_mcgill_snip <- conditional_effects(fit_mcgill_snip, effects = "outsnip", categorical = TRUE)
pldat_pitts_snip <- conditional_effects(fit_pitts_snip, effects = "outsnip", categorical = TRUE)
pldat_fatigue_vas_snip <- conditional_effects(fit_fatigue_vas_snip, effects = "outsnip", categorical = TRUE)


# PCF models
pldat_diff_chest_peak <- conditional_effects(fit_diff_chest_peak, effects = "outpeak", categorical = TRUE)
pldat_diff_nose_peak <- conditional_effects(fit_diff_nose_peak, effects = "outpeak", categorical = TRUE)
pldat_diff_saliva_peak <- conditional_effects(fit_diff_saliva_peak, effects = "outpeak", categorical = TRUE)
pldat_fatigclear_peak <- conditional_effects(fit_fatigclear_peak, effects = "outpeak", categorical = TRUE)
pldat_mcgill_peak <- conditional_effects(fit_mcgill_peak, effects = "outpeak", categorical = TRUE)
pldat_prod_cough_peak <- conditional_effects(fit_prod_cough_peak, effects = "outpeak")


#### Make plots ####
g_breathless_fvc <- ggplot(pldat_breathless_fvc$predfvc,
                           aes(x =predfvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    ggtitle("Orthopnea") + 
    xlab("FVC (%)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_breathless_svc <- ggplot(pldat_breathless_svc$predsvc,
       aes(x =predsvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    #ggtitle("Orthopnea") + 
    xlab("SVC (%)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_breathless_snip <- ggplot(pldat_breathless_snip$outsnip,
                           aes(x =outsnip, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    #ggtitle("Orthopnea") + 
    xlab("SNIP (cmH2O)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal()
    #geom_vline(xintercept = 50, col = "maroon") +
    #annotate(geom="text", x=40, y=0.85, col = "maroon",
    #         label="50%", fontface="bold") +
    #geom_vline(xintercept = 80) +
    #annotate(geom="text", x=90, y=0.85,
    #         label="80%", fontface="bold")

g_sobrest_fvc <- ggplot(pldat_sobrest_fvc$predfvc,
                        aes(x =predfvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    ggtitle("Dyspnea at rest") + 
    xlab("FVC (%)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_sobrest_svc <- ggplot(pldat_sobrest_svc$predsvc,
                       aes(x =predsvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    #ggtitle("Dyspnea at rest") + 
    xlab("SVC (%)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_sobrest_snip <- ggplot(pldat_sobrest_snip$outsnip,
                        aes(x =outsnip, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    #ggtitle("Dyspnea at rest") + 
    xlab("SNIP (cmH2O)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() #+
    #geom_vline(xintercept = 50, col = "maroon") +
    #annotate(geom="text", x=40, y=0.85, col = "maroon",
    #         label="50%", fontface="bold") +
    #geom_vline(xintercept = 80) +
    #annotate(geom="text", x=90, y=0.85,
    #         label="80%", fontface="bold")

g_sobwhenactive_fvc <- ggplot(pldat_sobwhenactive_fvc$predfvc,
                    aes(x =predfvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    ggtitle("Dyspnea when active") + 
    xlab("FVC (%)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_sobwhenactive_svc <- ggplot(pldat_sobwhenactive_svc$predsvc,
                              aes(x =predsvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    #ggtitle("Dyspnea when active") + 
    xlab("SVC (%)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_sobwhenactive_snip <- ggplot(pldat_sobwhenactive_snip$outsnip,
                              aes(x =outsnip, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    #ggtitle("Dyspnea when active") + 
    xlab("SNIP (cmH2O)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() #+
    #geom_vline(xintercept = 50, col = "maroon") +
    #annotate(geom="text", x=40, y=0.85, col = "maroon",
    #         label="50%", fontface="bold") +
    #geom_vline(xintercept = 80) +
    #annotate(geom="text", x=90, y=0.85,
    #         label="80%", fontface="bold")


g_sobwhenactive_fvc + g_sobrest_fvc + g_breathless_fvc +
    g_sobwhenactive_svc + g_sobrest_svc + g_breathless_svc +
    g_sobwhenactive_snip + g_sobrest_snip + g_breathless_snip

tiff("Plots/Fig_SOBs.tiff", width=1000, height=700, res=108)
    print(g_sobwhenactive_fvc + g_sobrest_fvc + g_breathless_fvc +
              g_sobwhenactive_svc + g_sobrest_svc + g_breathless_svc +
              g_sobwhenactive_snip + g_sobrest_snip + g_breathless_snip)
dev.off()


# Dyspnea plots by sex and site onset
g_ss1 <- ggplot(pldat_breathless_fvcsexsite$`predfvc:sexsite`,
       aes(x =predfvc, y = estimate__, col = sexsite)) + geom_line() + facet_wrap(~sexsite) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = sexsite, col = NULL),
                alpha = 0.25, show.legend = FALSE) +
    ggtitle("Orthopnea") + 
    xlab("FVC (%)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_ss2 <- ggplot(pldat_sobrest_fvcsexsite$`predfvc:sexsite`,
       aes(x =predfvc, y = estimate__, col = sexsite)) + geom_line() + facet_wrap(~sexsite) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = sexsite, col = NULL),
                alpha = 0.25, show.legend = FALSE) +
    ggtitle("Dyspnea at rest") + 
    xlab("FVC (%)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_ss3 <- ggplot(pldat_sobwhenactive_fvcsexsite$`predfvc:sexsite`,
       aes(x =predfvc, y = estimate__, col = sexsite)) + geom_line() + facet_wrap(~sexsite) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = sexsite, col = NULL),
                alpha = 0.25, show.legend = FALSE) +
    ggtitle("Dyspnea when active") + 
    xlab("FVC (%)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_ss1 + g_ss2 + g_ss3 +plot_layout(nrow=3)


g_genfat_fvc <- ggplot(pldat_fatigue_vas_fvc$`predfvc:cats__`,
                   aes(x =predfvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    ggtitle("General fatigue") + scale_colour_discrete(name = "Fatigue score") + 
    xlab("FVC (%)") + ylab("Probability") +
    expand_limits(x=c(0, 155), y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_genfat_svc <- ggplot(pldat_fatigue_vas_svc$`predsvc:cats__`,
                       aes(x =predsvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    #ggtitle("General fatigue") +
    scale_colour_discrete(name = "Fatigue score") +
    xlab("SVC (%)") + ylab("Probability") +
    expand_limits(x=c(0, 155), y=1) +
    theme_minimal() +
    geom_vline(xintercept = 50, col = "maroon") +
    annotate(geom="text", x=40, y=0.85, col = "maroon",
             label="50%", fontface="bold") +
    geom_vline(xintercept = 80) +
    annotate(geom="text", x=90, y=0.85,
             label="80%", fontface="bold")

g_genfat_snip <- ggplot(pldat_fatigue_vas_snip$`outsnip:cats__`,
                       aes(x =outsnip, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    #ggtitle("General fatigue") +
    scale_colour_discrete(name = "Fatigue score") +
    xlab("Snip (cmH2O)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()

tiff("Plots/Fig_genfatigue.tiff", width=700, height=1000, res=108)
    print(g_genfat_fvc / g_genfat_svc / g_genfat_snip + plot_layout(guides = "collect"))
dev.off()



g_mcgill_fvc <- ggplot(pldat_mcgill_fvc$`predfvc:cats__`,
                   aes(x =predfvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "McGill score") +
    #ggtitle("Probability of McGill QoL") + 
    xlab("FVC (%)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()
g_mcgill_svc <- ggplot(pldat_mcgill_svc$`predsvc:cats__`,
                       aes(x =predsvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "McGill score") +
    #ggtitle("Probability of McGill QoL") + 
    xlab("SVC (%)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()
g_mcgill_snip <- ggplot(pldat_mcgill_snip$`outsnip:cats__`,
                       aes(x =outsnip, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "McGill score") +
    #ggtitle("Probability of McGill QoL") + 
    xlab("Snip (cmH2O)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()
g_mcgill_peak <- ggplot(pldat_mcgill_peak$`outpeak:cats__`,
                        aes(x =outpeak, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "McGill score") +
    #ggtitle("Probability of McGill QoL") + 
    xlab("PCF (L/min)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()

tiff("Plots/Fig_mcgill.tiff", width=1000, height=700, res=108)
    print(g_mcgill_fvc + g_mcgill_svc +
              g_mcgill_snip + g_mcgill_peak + plot_layout(guides = "collect"))
dev.off()



g_pitts_fvc <- ggplot(pldat_pitts_fvc$`predfvc:cats__`,
                  aes(x =predfvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "Pittsburgh sleep\nquality score") +
    #ggtitle("Pittsburgh sleep quality score") + 
    xlab("FVC (%)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()
g_pitts_svc <- ggplot(pldat_pitts_svc$`predsvc:cats__`,
                      aes(x =predsvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "Pittsburgh sleep\nquality score") +
    #ggtitle("Probability of Pittsburgh sleep quality score") + 
    xlab("SVC (%)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()
g_pitts_snip <- ggplot(pldat_pitts_snip$`outsnip:cats__`,
                      aes(x =outsnip, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "Pittsburgh sleep\nquality score") +
    #ggtitle("Probability of Pittsburgh sleep quality score") + 
    xlab("Snip (cmH2O)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()

tiff("Plots/Fig_Pittsburgh.tiff", width=700, height=1000, res=108)
    print(g_pitts_fvc / g_pitts_svc / g_pitts_snip +plot_layout(guides = "collect"))
dev.off()



#### PCF plots ####
g_diff_chest_peak <- ggplot(pldat_diff_chest_peak$`outpeak:cats__`,
                        aes(x =outpeak, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "Difficulty clearing") +
    ggtitle("Chest") + 
    xlab("PCF (L/min)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal() +
    geom_vline(xintercept = 160, col = "maroon") +
    annotate(geom="text", x=115, y=0.95, col = "maroon",
             label="160\nL/min", fontface="bold") +
    geom_vline(xintercept = 270) +
    annotate(geom="text", x=315, y=0.95,
             label="270\nL/min", fontface="bold")
g_diff_nose_peak <- ggplot(pldat_diff_nose_peak$`outpeak:cats__`,
                            aes(x =outpeak, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "Difficulty clearing") +
    ggtitle("Nose") + 
    xlab("PCF (L/min)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal() +
    geom_vline(xintercept = 160, col = "maroon") +
    annotate(geom="text", x=115, y=0.95, col = "maroon",
             label="160\nL/min", fontface="bold") +
    geom_vline(xintercept = 270) +
    annotate(geom="text", x=315, y=0.95,
             label="270\nL/min", fontface="bold")
g_diff_saliva_peak <- ggplot(pldat_diff_saliva_peak$`outpeak:cats__`,
                           aes(x =outpeak, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "Difficulty clearing") +
    ggtitle("Saliva") + 
    xlab("PCF (L/min)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal() +
    geom_vline(xintercept = 160, col = "maroon") +
    annotate(geom="text", x=115, y=0.95, col = "maroon",
             label="160\nL/min", fontface="bold") +
    geom_vline(xintercept = 270) +
    annotate(geom="text", x=315, y=0.95,
             label="270\nL/min", fontface="bold")

tiff("Plots/Fig_difficulty_clearing.tiff", width=700, height=1000, res=108)
    print(g_diff_nose_peak / g_diff_saliva_peak / g_diff_chest_peak + plot_layout(guides = "collect"))
dev.off()



g_fatigclear_peak <- ggplot(pldat_fatigclear_peak$`outpeak:cats__`,
                            aes(x =outpeak, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    ggtitle("Fatigue clearing chest") + 
    scale_colour_discrete(name = "Fatigue score") +
    xlab("PCF (L/min)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal() +
    geom_vline(xintercept = 160, col = "maroon") +
    annotate(geom="text", x=115, y=0.95, col = "maroon",
             label="160\nL/min", fontface="bold") +
    geom_vline(xintercept = 270) +
    annotate(geom="text", x=315, y=0.95,
             label="270\nL/min", fontface="bold")
tiff("Plots/Fig_fatigue_clearing chest.tiff", width=800, height=600, res=108)
    print(g_fatigclear_peak)
dev.off()



g_prod_cough_peak <- ggplot(pldat_prod_cough_peak$outpeak,
       aes(x = outpeak, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    ggtitle("Regular productive cough") + 
    xlab("PCF (L/min)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal() +
    geom_vline(xintercept = 160, col = "maroon") +
    annotate(geom="text", x=115, y=0.95, col = "maroon",
             label="160\nL/min", fontface="bold") +
    geom_vline(xintercept = 270) +
    annotate(geom="text", x=315, y=0.95,
             label="270\nL/min", fontface="bold")
tiff("Plots/Fig_prodd_cough_PCF.tiff", width=800, height=600, res=108)
    print(g_prod_cough_peak)
dev.off()


