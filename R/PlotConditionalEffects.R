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
fit_mcgill_snip <- readRDS("FittedModels/brms_mcgill_snip.RDS")
fit_pitts_snip <- readRDS("FittedModels/brms_pitts_snip.RDS")
fit_fatigue_vas_snip <- readRDS("FittedModels/brms_fatigue_vas_snip.RDS")

# PCF models
fit_diff_chest_peak <- readRDS("FittedModels/brms_diffchest_peak.RDS")
fit_diff_nose_peak <- readRDS("FittedModels/brms_diffnose_peak.RDS")
fit_diff_saliva_peak <- readRDS("FittedModels/brms_diffsaliva_peak.RDS")
fit_fatigclear_peak <- readRDS("FittedModels/brms_fatigclear_peak.RDS")
fit_mcgill_peak <- readRDS("FittedModels/brms_mcgill_peak.RDS")



#### First summarise continuous models ####
summary(fit_eq5d5lind_svc)
summary(fit_eq5d5lind_fvc)




#### generate conditional effects ####
pldat_breathless_fvc <- conditional_effects(fit_breathless_fvc, "outfvc")
pldat_breathless_svc <- conditional_effects(fit_breathless_svc, "outsvc")
pldat_sobrest_fvc <- conditional_effects(fit_sobrest_fvc, "outfvc")
pldat_sobrest_svc <- conditional_effects(fit_sobrest_svc, "outsvc")
pldat_sobwhenactive_fvc <- conditional_effects(fit_sobwhenactive_fvc, "outfvc")
pldat_sobwhenactive_svc <- conditional_effects(fit_sobwhenactive_svc, "outsvc")
pldat_fatigue_vas_fvc <- conditional_effects(fit_fatigue_vas_fvc, effects = "outfvc", categorical = TRUE)
pldat_fatigue_vas_svc <- conditional_effects(fit_fatigue_vas_svc, effects = "outsvc", categorical = TRUE)
pldat_mcgill_fvc <- conditional_effects(fit_mcgill_fvc, effects = "outfvc", categorical = TRUE)
pldat_mcgill_svc <- conditional_effects(fit_mcgill_svc, effects = "outsvc", categorical = TRUE)
pldat_pitts_fvc <- conditional_effects(fit_pitts_fvc, effects = "outfvc", categorical = TRUE)
pldat_pitts_svc <- conditional_effects(fit_pitts_svc, effects = "outsvc", categorical = TRUE)

# SNIP models
pldat_mcgill_snip <- conditional_effects(fit_mcgill_snip, effects = "outsnip", categorical = TRUE)
pldat_pitts_snip <- conditional_effects(fit_pitts_snip, effects = "outsnip", categorical = TRUE)
pldat_fatigue_vas_snip <- conditional_effects(fit_fatigue_vas_snip, effects = "outsnip", categorical = TRUE)


# PCF models
pldat_diff_chest_peak <- conditional_effects(fit_diff_chest_peak, effects = "outpeak", categorical = TRUE)
pldat_diff_nose_peak <- conditional_effects(fit_diff_nose_peak, effects = "outpeak", categorical = TRUE)
pldat_diff_saliva_peak <- conditional_effects(fit_diff_saliva_peak, effects = "outpeak", categorical = TRUE)
pldat_fatigclear_peak <- conditional_effects(fit_fatigclear_peak, effects = "outpeak", categorical = TRUE)
pldat_mcgill_peak <- conditional_effects(fit_mcgill_peak, effects = "outpeak", categorical = TRUE)



#### Make plots ####
g_breathless_fvc <- ggplot(pldat_breathless_fvc$outfvc,
                           aes(x =outfvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    ggtitle("Dyspnea when lying") + 
    xlab("FVC (L)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal()
g_breathless_svc <- ggplot(pldat_breathless_svc$outsvc,
       aes(x =outsvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    #ggtitle("Dyspnea when lying") + 
    xlab("SVC (L)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal()

g_sobrest_fvc <- ggplot(pldat_sobrest_fvc$outfvc,
                        aes(x =outfvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    ggtitle("Dyspnea at rest") + 
    xlab("FVC (L)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal()
g_sobrest_svc <- ggplot(pldat_sobrest_svc$outsvc,
                       aes(x =outsvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    #ggtitle("Dyspnea at rest") + 
    xlab("SVC (L)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal()

g_sobwhenactive_fvc <- ggplot(pldat_sobwhenactive_fvc$outfvc,
                    aes(x =outfvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    ggtitle("Dyspnea when active") + 
    xlab("FVC (L)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal()
g_sobwhenactive_svc <- ggplot(pldat_sobwhenactive_svc$outsvc,
                              aes(x =outsvc, y = estimate__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1), show.legend = FALSE) +
    #ggtitle("Dyspnea when active") + 
    xlab("SVC (L)") + ylab("Probability") +
    expand_limits(x=0, y=1) +
    theme_minimal()

g_sobwhenactive_fvc + g_sobrest_fvc + g_breathless_fvc +
    g_sobwhenactive_svc + g_sobrest_svc + g_breathless_svc

tiff("Plots/Fig_SOBs.tiff", width=1000, height=700, res=108)
    print(g_sobwhenactive_fvc + g_sobrest_fvc + g_breathless_fvc +
          g_sobwhenactive_svc + g_sobrest_svc + g_breathless_svc)
dev.off()



g_genfat_fvc <- ggplot(pldat_fatigue_vas_fvc$`outfvc:cats__`,
                   aes(x =outfvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    ggtitle("General fatigue") + scale_colour_discrete(name = "Fatigue score") + 
    xlab("FVC (L)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()
g_genfat_svc <- ggplot(pldat_fatigue_vas_svc$`outsvc:cats__`,
                       aes(x =outsvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    #ggtitle("General fatigue") +
    scale_colour_discrete(name = "Fatigue score") +
    xlab("SVC (L)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()
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



g_mcgill_fvc <- ggplot(pldat_mcgill_fvc$`outfvc:cats__`,
                   aes(x =outfvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "McGill score") +
    #ggtitle("Probability of McGill QoL") + 
    xlab("FVC (L)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()
g_mcgill_svc <- ggplot(pldat_mcgill_svc$`outsvc:cats__`,
                       aes(x =outsvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "McGill score") +
    #ggtitle("Probability of McGill QoL") + 
    xlab("SVC (L)") + ylab("Probability") +
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



g_pitts_fvc <- ggplot(pldat_pitts_fvc$`outfvc:cats__`,
                  aes(x =outfvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "Pittsburgh sleep\nquality score") +
    #ggtitle("Pittsburgh sleep quality score") + 
    xlab("FVC (L)") + ylab("Probability") +
    expand_limits(x=0,y=1) +
    theme_minimal()
g_pitts_svc <- ggplot(pldat_pitts_svc$`outsvc:cats__`,
                      aes(x =outsvc, y = estimate__, col = cats__)) + geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, alpha = 0.1, fill = cats__), show.legend = FALSE) +
    scale_colour_discrete(name = "Pittsburgh sleep\nquality score") +
    #ggtitle("Probability of Pittsburgh sleep quality score") + 
    xlab("SVC (L)") + ylab("Probability") +
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
    annotate(geom="text", x=200, y=0.95, col = "maroon",
             label="160 L/min", fontface="bold") +
    geom_vline(xintercept = 270) +
    annotate(geom="text", x=310, y=0.95,
             label="270 L/min", fontface="bold")
tiff("Plots/Fig_fatigue_clearing chest.tiff", width=800, height=600, res=108)
    print(g_fatigclear_peak)
dev.off()

