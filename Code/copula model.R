
# Install required packages if needed
#install.packages(c("survey", "copula", "MASS", "fitdistrplus", "readxl", "weights", "wCorr"))
install.packages("patchwork")
library(patchwork)

library(survey)
library(copula)
library(MASS)
library(fitdistrplus)
library(readxl)
library(Hmisc)
library(weights)
library(wCorr)
library(tidyr)
library(dplyr)

set.seed(123)

# 1. Load data
df <- read_xlsx("C:/Users/andrew.carr-harris/Desktop/flukeRDM_iterative_data/test_copula2") %>% 
      mutate(sf_cat=sf_keep+sf_rel)


# 2. Define survey design
svy_design <- svydesign(ids=~psu_id,strata=~strat_id,
                        weights=~wp_int,nest=TRUE,data=df)
options(survey.lonely.psu = "certainty")

# 3. Estimate means, variances using survey design
mean_keep <- svymean(~sf_keep, svy_design)
mean_rel  <- svymean(~sf_rel,  svy_design)
mean_cat  <- svymean(~sf_cat,  svy_design)

var_keep <- attr(mean_keep, "var")
var_rel  <- attr(mean_rel, "var")
var_cat  <- attr(mean_cat, "var")

mu_keep <- coef(mean_keep)
mu_rel  <- coef(mean_rel)
mu_cat  <- coef(mean_cat)

mean_keep
sqrt(var_keep)

mean_rel
sqrt(var_rel)

mean_cat
sqrt(var_cat)

corr <- as.data.frame(wtd.cor(df$sf_keep, df$sf_rel, weight = df$wp_int))
corr  <- corr$correlation
corr


max_keep <- max(df$sf_keep)
max_rel <- max(df$sf_rel)
max(df$sf_cat)

# Bootstrap replicate design (R = number of replicates)
rep_design <- as.svrepdesign(svy_design, type = "bootstrap", replicates = 200)

# Replicate means
rep_means_keep <- coef(svymean(~sf_keep, rep_design, return.replicates = TRUE, na.rm = TRUE))  # scalar mean
rep_means_rel  <- coef(svymean(~sf_rel,  rep_design, return.replicates = TRUE, na.rm = TRUE))


# Replicate variances
# Extract the full replicate weights matrix
rep_wgts <- weights(rep_design, type = "analysis")  # matrix: rows = obs, cols = replicates

# Now loop over columns (replicates)
rep_vars_keep <- sapply(1:ncol(rep_wgts), function(i) {
  rep_data <- rep_wgts[, i]
  svy_var  <- svydesign(ids=~psu_id,strata=~strat_id,
                        nest=TRUE, weights = ~rep_data, data = df)
  coef(svyvar(~sf_keep, svy_var, na.rm = TRUE))
})


rep_vars_rel <- sapply(1:ncol(rep_wgts), function(i) {
  rep_data <- rep_wgts[, i]
  svy_var  <- svydesign(ids=~psu_id,strata=~strat_id,
                       nest=TRUE, weights = ~rep_data, data = df)
  coef(svyvar(~sf_rel, svy_var, na.rm = TRUE))
})


# 4. Estimate dispersion for negative binomial
# Protect against negative denominators
rep_theta_keep <- rep_means_keep^2 / pmax(rep_vars_keep - rep_means_keep, 1e-6)
rep_theta_rel  <- rep_means_rel^2  / pmax(rep_vars_rel  - rep_means_rel,  1e-6)

# 5. Create pseudo-observations (rank-based empirical CDFs)
df$w_int_rounded <- round(df$wp_int)
df_expanded <- uncount(df, weights = w_int_rounded) #expand the data so that each row represents a single trip outcome

nsims<-nrow(df_expanded)

df_expanded <- df_expanded %>%
  mutate(
    rank_keep = rank(sf_keep, ties.method = "average"),
    rank_rel  = rank(sf_rel,  ties.method = "average"),
    u_keep = rank_keep / (n() + 1),
    u_rel  = rank_rel / (n() + 1)
  )

sd(df_expanded$sf_cat)
sd(df_expanded$sf_keep)
sd(df_expanded$sf_rel)

# 6. Fit Gumbel copula using pseudo-observations
#fit the copula to a sample (max 30,000 obs.) of the original data b/c it can take a while
df_expanded <- df_expanded %>% sample_n(min(nrow(df_expanded), 30000)) 
sample_corr  <- cor(df_expanded$sf_keep, df_expanded$sf_rel, method = "pearson")
sample_corr

#u_mat <- cbind(df_expanded$u_keep, df_expanded$u_rel)
u_mat <- cbind(df_expanded$u_keep, df_expanded$u_rel)

gumbel_fit <- fitCopula(gumbelCopula(dim = 2), u_mat, method = "mpl")
gumbel_fit

# 7. Simulate from the fitted copula
n_sim <- 10000   # number of samples per draw
n_draws <- 150  # number of simulated datasets

sim_datasets <- list()
i <- 1

while (i <= n_draws) {
  sim_u <- rCopula(n_sim, gumbel_fit@copula)
  
  # Sample mu_keep and mu_rel with uncertainty
  sampled_mu_keep <-  rnorm(1, mu_keep, sqrt(var_keep))
  sampled_mu_rel  <- rnorm(1, mu_rel,  sqrt(var_rel))
  
  sampled_theta_keep <- sample(rep_theta_keep, 1, replace = TRUE)
  sampled_theta_rel  <- sample(rep_theta_rel,  1, replace = TRUE)
  
  
  # Convert uniform to NB using quantiles
  sim_keep <- qnbinom(sim_u[,1], size = sampled_theta_keep, mu = sampled_mu_keep)
  sim_rel  <- qnbinom(sim_u[,2], size = sampled_theta_rel, mu = sampled_mu_rel)
  sim_keep <- pmin(sim_keep, max_keep+1) 
  sim_rel <- pmin(sim_rel, round(max_rel*2)) 
  sim_catch <- sim_keep+sim_rel

    if (!any(is.nan(sim_keep)) && !any(is.nan(sim_rel))) {
    sim_datasets[[i]] <- data.frame(sim_id = i, sf_keep_sim = sim_keep, sf_rel_sim = sim_rel, sf_catch_sim=sim_catch) #%>% 
      #mutate(across(everything(), ~ replace(., is.nan(.) | is.na(.), 0)))
    i <- i + 1  # Only increment if no NaNs
  }

}

# 8. Combine all simulated datasets, tagging each with its simulation ID
combined_sim <- bind_rows(
  lapply(seq_along(sim_datasets), function(i) {
    sim_datasets[[i]] %>%
      mutate(sim_id = i)
  })
)
max(combined_sim$sf_rel_sim)
max(combined_sim$sf_keep_sim)

# Now summarize the means by simulation ID
sim_means <- combined_sim %>%
  dplyr::group_by(sim_id) %>%
  dplyr::summarize(
    mean_sf_keep = mean(sf_keep_sim),
    mean_sf_rel  = mean(sf_rel_sim),
    mean_sf_catch  = mean(sf_catch_sim),
    sd_sf_keep = sd(sf_keep_sim),
    sd_sf_rel  = sd(sf_rel_sim),
    sd_sf_catch  = sd(sf_catch_sim),
    corr= cor(sf_keep_sim, sf_rel_sim, method = "pearson"),
    .groups = "drop"
  )
sim_means$corr[is.na(sim_means$corr)] <- 0

# Summarize simulated means
sim_summary <- sim_means %>%
  dplyr::summarize(
    mean_keep = mean(mean_sf_keep),
    mean_rel  = mean(mean_sf_rel),
    mean_cat  = mean(mean_sf_catch),
    sdm_keep   = sd(mean_sf_keep),
    sdm_rel    = sd(mean_sf_rel),
    sdm_cat    = sd(mean_sf_catch),
    sd_keep   = mean(sd_sf_keep),
    sd_rel    = mean(sd_sf_rel),
    sd_cat    = mean(sd_sf_catch), 
    sd_sd_keep   = sd(sd_sf_keep),
    sd_sd_rel    = sd(sd_sf_rel),
    sd_sd_cat    = sd(sd_sf_catch),
    mean_corr=mean(corr), 
    sd_corr=sd(corr)

  )

# Observed SDs from expanded dataset
obs_sd_keep <- sd(df_expanded$sf_keep)
obs_sd_rel  <- sd(df_expanded$sf_rel)
obs_sd_cat  <- sd(df_expanded$sf_cat)

# Build tidy comparison table
comparison_plot_df <- tibble::tibble(
  metric = rep(c("sf_keep", "sf_rel", "sf_catch"), times = 2),
  value = c(
    # Means
    coef(mean_keep), coef(mean_rel), coef(mean_cat),
    sim_summary$mean_keep, sim_summary$mean_rel, sim_summary$mean_cat
  ),
  se = c(
    sqrt(var_keep), sqrt(var_rel), sqrt(var_cat),
    sim_summary$sdm_keep, sim_summary$sdm_rel, sim_summary$sdm_cat
  ),
  source = rep(c("Survey", "Simulated"), each = 3),
  statistic = "Mean"
)

# Add SD comparison
sd_plot_df <- tibble::tibble(
  metric = rep(c("sf_keep", "sf_rel", "sf_catch"), times = 2),
  value = c(
    obs_sd_keep, obs_sd_rel, obs_sd_cat,
    sim_summary$sd_keep, sim_summary$sd_rel, sim_summary$sd_cat
  ),
  se = c(
    NA, NA, NA,
    sim_summary$sd_sd_keep, sim_summary$sd_sd_rel, sim_summary$sd_sd_cat
  ),
  source = rep(c("Survey", "Simulated"), each = 3),
  statistic = "SD"
)

# Create correlation plot data
corr_data <- tibble(
  variable = "Correlation",
  value = c(corr, sim_summary$mean_corr),  # your observed correlation
  se = c(0, sim_summary$sd_corr),
  source = c("Survey", "Simulated")
)

# Replace NA standard errors with 0
corr_data$se[is.na(corr_data$se)] <- 0

# Combine for plotting
plot_data <- bind_rows(comparison_plot_df, sd_plot_df)
plot_data$se[is.na(plot_data$se)] <- 0


pA<- ggplot(plot_data, aes(x = metric, y = value, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
  aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se),
  width = 0.2,
  position = position_dodge(width = 0.8)
) +
  facet_wrap(~statistic, scales = "free_y") +
  labs(
    title = "Survey vs Simulated: Means and Standard Deviations",
    x = "Metric",
    y = "Value (Fish per Trip)",
    fill = "Source"
  ) +
  scale_fill_manual(values = c("Survey" = "red", "Simulated" = "steelblue")) +
  theme_minimal()

p3 <- ggplot(corr_data, aes(x = variable, y = value, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5) +
  geom_errorbar(aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se),
                width = 0.2, position = position_dodge(width = 0.8)) +
  labs(title = "Pearson Correlation ± 95% CI", y = "Correlation", x = NULL) +
  theme_minimal() +
  scale_fill_manual(values = c("Survey" = "steelblue", "Simulated" = "orange"))

# Combine with patchwork
(pA | p3) + plot_layout(guides = "collect") & theme(legend.position = "bottom")



### When only mean keep or mean release in non-zero, and the other is zero
library(survey)
library(dplyr)
library(tidyr)

# 1. Load your dataset 
df <- read_xlsx("C:/Users/andrew.carr-harris/Desktop/flukeRDM_iterative_data/test_copula2.xlsx")%>% 
  mutate(sf_cat=sf_keep+sf_rel)
# Ensure variables: sf_keep (positive integers), sf_rel (all zero), wp_int (survey weights)

# 2. Define survey design
svy_design <- svydesign(ids = ~psu_id, strata = ~strat_id, weights = ~wp_int, data = df, nest = TRUE)
options(survey.lonely.psu = "certainty")

max_rel <- max(df$sf_rel)

# 3. Estimate mean and variance for sf_keep
mean_keep <- svymean(~sf_keep, svy_design)
mu_keep <- coef(mean_keep)
var_keep <- attr(mean_keep, "var")

mean_rel <- svymean(~sf_rel, svy_design)
mu_rel <- coef(mean_rel)
var_rel <- attr(mean_rel, "var")

mean_cat <- svymean(~sf_cat, svy_design)
mu_cat <- coef(mean_cat)
var_cat <- attr(mean_cat, "var")

# 4. Estimate Negative Binomial dispersion parameter
theta_rel <- mu_rel^2 / max(var_rel - mu_rel, 1e-6)  # avoid division by zero

# 5. Create pseudo-observations (rank-based empirical CDFs)
df$w_int_rounded <- round(df$wp_int)
df_expanded <- uncount(df, weights = w_int_rounded) #expand the data so that each row represents a single trip outcome

df_expanded <- df_expanded %>%
  mutate(
    rank_keep = rank(sf_keep, ties.method = "average"),
    rank_rel  = rank(sf_rel,  ties.method = "average"),
    u_keep = rank_keep / (n() + 1),
    u_rel  = rank_rel / (n() + 1)
  )

# Observed SDs from expanded dataset
obs_sd_rel <- sd(df_expanded$sf_rel)
obs_sd_cat  <- sd(df_expanded$sf_cat)

# 5. Simulate 100 datasets incorporating sampling uncertainty
set.seed(123)
n_sim <- 10000   # number of samples per draw
n_draws <- 100

sim_datasets <- list()
i <- 1

while (i <= n_draws) {
  # Sample new mean from normal distribution with survey SE
  sampled_mu <- rnorm(1, mean = mu_rel, sd = sqrt(var_rel))
  
  # Draw from NB using qnbinom to preserve count structure
  sim_rel <- rnbinom(n_sim, size = theta_rel, mu = sampled_mu)
  sim_rel <- pmin(sim_rel, round(max_rel*2)) 
  sim_catch <- sim_rel
  
  if (!any(is.nan(sim_rel))) {
    sim_datasets[[i]] <- data.frame(sim_id = i, sf_rel_sim = sim_rel, sf_catch_sim=sim_catch) #%>% 
    #mutate(across(everything(), ~ replace(., is.nan(.) | is.na(.), 0)))
    i <- i + 1  # Only increment if no NaNs
  }
}

combined_sim <- bind_rows(sim_datasets)

# Now summarize the means by simulation ID
sim_means <- combined_sim %>%
  dplyr::group_by(sim_id) %>%
  dplyr::summarize(
    mean_sf_rel  = mean(sf_rel_sim),
    mean_sf_catch  = mean(sf_catch_sim),
    sd_sf_rel  = sd(sf_rel_sim),
    sd_sf_catch  = sd(sf_catch_sim),
    .groups = "drop"
  )

# Summary
# Summarize simulated means
sim_summary <- sim_means %>%
  dplyr::summarize(
    mean_rel  = mean(mean_sf_rel),
    mean_cat  = mean(mean_sf_catch),
    sdm_rel    = sd(mean_sf_rel),
    sdm_cat    = sd(mean_sf_catch),
    sd_rel    = mean(sd_sf_rel),
    sd_cat    = mean(sd_sf_catch), 
    sd_sd_rel    = sd(sd_sf_rel),
    sd_sd_cat    = sd(sd_sf_catch),

  )

# Build tidy comparison table
comparison_plot_df <- tibble::tibble(
  metric = rep(c( "sf_rel", "sf_catch"), times = 2),
  value = c(
    # Means
     coef(mean_rel), coef(mean_cat),
     sim_summary$mean_rel, sim_summary$mean_cat
  ),
  se = c(
    sqrt(var_rel), sqrt(var_cat),
     sim_summary$sdm_rel, sim_summary$sdm_cat
  ),
  source = rep(c("Survey", "Simulated"), each = 2),
  statistic = "Mean"
)

# Add SD comparison
sd_plot_df <- tibble::tibble(
  metric = rep(c( "sf_rel", "sf_catch"), times = 2),
  value = c(
     obs_sd_rel, obs_sd_cat,
     sim_summary$sd_rel, sim_summary$sd_cat
  ),
  se = c(
    NA, NA, 
    sim_summary$sd_sd_rel, sim_summary$sd_sd_cat
  ),
  source = rep(c("Survey", "Simulated"), each = 2),
  statistic = "SD"
)


# Combine for plotting
plot_data <- bind_rows(comparison_plot_df, sd_plot_df)


pA<- ggplot(plot_data, aes(x = metric, y = value, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  facet_wrap(~statistic, scales = "free_y") +
  labs(
    title = "Survey vs Simulated: Means and Standard Deviations",
    x = "Metric",
    y = "Value (Fish per Trip)",
    fill = "Source"
  ) +
  scale_fill_manual(values = c("Survey" = "red", "Simulated" = "steelblue")) +
  theme_minimal()
pA
