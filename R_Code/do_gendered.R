
# We always discuss things this order: surveillance-general, static-general, surveillance-policy, static-policy.

png("Manuscript/Hypothesis_Tests/Figures/gender_gap.png", height = 600, width = 600)
par(mfrow = c(4,2), mar = rep(0.75, 4), oma = c(3,12,1,1), family = "serif")
source("R_Code/plot_fem_surv_gen.R")
source("R_Code/plot_fem_stat_gen.R")
source("R_Code/plot_fem_surv_ps.R")
source("R_Code/plot_fem_stat_ps.R")
dev.off()