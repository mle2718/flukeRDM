
# Install required packages if needed
install.packages(c("survey", "copula", "MASS", "fitdistrplus", "readxl", "weights", "wCorr"))
install.packages("patchwork")
#install.packages("ggplot2")

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
library(ggplot2)


# 1. Load data
df <- read_xlsx("C:/Users/andrew.carr-harris/Desktop/flukeRDM_iterative_data/baseline_mrip_catch_processed.xlsx") %>% 
  filter(state=="MA")

# Strata can fall into one of four categories:

  # 1) both mean harvest- and discards-per-trip>0 
  # 2) mean discards-per-trip>0, mean harvest-per-trip==0
  # 3) mean harvest-per-trip>0, mean discards-per-trip==0
  # 4) mean discards-per-trip==0, mean discards-per-trip==0

# I used copula model to simulate 1), whereas 2) and 3) are distributed NB

n_sim <- 5000   # number of samples per draw
n_draws <- 50  # number of simulated datasets


### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0
df_full1 <- df %>% filter(sf_keep_and_rel==1 )

### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)==0
df_full2 <- df %>% filter(sf_only_rel==1 )

### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)>0
df_full3 <- df %>% filter(sf_only_keep==1)

### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)==0
df_full4 <- df %>% filter(sf_no_catch==1)


### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)>0
if (nrow(df_full1) > 0){
  
all_results1 <- list()

for (dom in unique(df_full1$my_dom_id_string)) {

df <- df_full1 %>% filter(my_dom_id_string == dom)

# 2. Define survey design
svy_design <- svydesign(ids=~psu_id,strata=~strat_id,
                        weights=~wp_int,nest=TRUE,data=df)
options(survey.lonely.psu = "certainty")

# 3. Estimate means, variances using survey design
mean_keep <- svymean(~sf_keep, svy_design)
mean_rel  <- svymean(~sf_rel,  svy_design)
mean_cat  <- svymean(~sf_catch,  svy_design)

var_keep <- attr(mean_keep, "var")
var_rel  <- attr(mean_rel, "var")
var_cat  <- attr(mean_cat, "var")

mu_keep <- coef(mean_keep)
mu_rel  <- coef(mean_rel)
mu_cat  <- coef(mean_cat)

# Handle zero or missing variance (certainty units)
# Use imputed linearized standard error 
if (is.na(var_keep) || var_keep == 0) {
  imputed_se <- mean(df$sesf_keep)  
  var_rel <- imputed_se^2
}

if (is.na(var_rel) || var_rel == 0) {
  imputed_se <- mean(df$sesf_rel)  
  var_rel <- imputed_se^2
}


corr <- as.data.frame(wtd.cor(df$sf_keep, df$sf_rel, weight = df$wp_int))
corr  <- corr$correlation

max_keep <- round(max(df$sf_keep))
max_rel <- round(max(df$sf_rel))

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


# 6. Fit Gumbel copula using pseudo-observations
#fit the copula to a sample (max 30,000 obs.) of the original data b/c it can take a while
df_expanded <- df_expanded %>% sample_n(min(nrow(df_expanded), 10000)) 

u_mat <- cbind(df_expanded$u_keep, df_expanded$u_rel)
gumbel_fit <- fitCopula(gumbelCopula(dim = 2), u_mat, method = "mpl")


# 7. Simulate from the fitted copula
sim_datasets <- list()
i <- 1

while (i <= n_draws) {
  set.seed(i)
  
  sim_u <- rCopula(n_sim, gumbel_fit@copula)

  # Sample mu_keep and mu_rel with uncertainty
  sampled_mu_keep <-  rnorm(1, mu_keep, sqrt(var_keep))
  sampled_mu_rel  <- rnorm(1, mu_rel,  sqrt(var_rel))
  
  sampled_theta_keep <- sample(rep_theta_keep, 1, replace = TRUE)
  sampled_theta_rel  <- sample(rep_theta_rel,  1, replace = TRUE)
  
  # Convert uniform to NB using quantiles
  sim_keep <- qnbinom(sim_u[,1], size = sampled_theta_keep, mu = sampled_mu_keep)
  sim_rel  <- qnbinom(sim_u[,2], size = sampled_theta_rel, mu = sampled_mu_rel)

  sim_keep[is.na(sim_keep)] <- 0
  sim_rel[is.na(sim_rel)]   <- 0

  sim_keep <- pmin(sim_keep, max_keep+2)
  sim_rel <- pmin(sim_rel, max_rel*2)
  
  sim_catch <- sim_keep+sim_rel
  my_dom_id_string<-dom

    sim_datasets[[i]] <- data.frame(sim_id = i, sf_keep_sim = sim_keep, sf_rel_sim = sim_rel, sf_catch_sim = sim_catch, my_dom_id_string = my_dom_id_string) 
    i <- i + 1

}

# 8. Combine all simulated datasets, tagging each with its simulation ID
combined_sim <- bind_rows(
  lapply(seq_along(sim_datasets), function(i) {
    sim_datasets[[i]] %>%
      mutate(sim_id = i)
  })
)

all_results1[[dom]] <- combined_sim
}

final_result1 <- bind_rows(all_results1)
final_result1 <- final_result1 %>%
  group_by(my_dom_id_string, sim_id) %>%
  mutate(id = row_number()) %>%
  ungroup()
}

# List the objects you want to keep
keep <- c("final_results1", "df_full1", "df_full2", "df_full3", "df_full4", "n_sim", "n_draws")

# Remove everything else
rm(list = setdiff(ls(), keep))



### MEAN(DISCARDS-PER-TRIP)>0, MEAN(HARVEST-PER-TRIP)==0
####Summer flounder####
if (nrow(df_full2) > 0){
  
all_results2 <- list()

for (dom in unique(df_full2$my_dom_id_string)) {
  
df <- df_full2 %>% filter(my_dom_id_string == dom)

# Define survey design
svy_design <- svydesign(ids = ~psu_id, strata = ~strat_id, weights = ~wp_int, data = df, nest = TRUE)
options(survey.lonely.psu = "certainty")

max_rel <- round(max(df$sf_rel))

df$w_int_rounded <- round(df$wp_int)
df_expanded <- uncount(df, weights = w_int_rounded) #expand the data so that each row represents a single trip outcome
df_expanded <- df_expanded %>% sample_n(min(nrow(df_expanded), 10000)) 
obs_sd_rel <- sd(df_expanded$sf_rel)

# 3. Estimate mean and variance for sf_keep
mean_rel <- svymean(~sf_rel, svy_design)
mu_rel <- coef(mean_rel)
var_rel <- attr(mean_rel, "var")

mean_cat <- svymean(~sf_catch, svy_design)
mu_cat <- coef(mean_cat)
var_cat <- attr(mean_cat, "var")


# Handle zero or missing variance (certainty units)
# Use imputed linearized standard error 
if (is.na(var_rel) || var_rel == 0) {
  imputed_se <- mean(df$sesf_rel)  
  var_rel <- imputed_se^2
}

# Bootstrap replicate design
rep_design <- as.svrepdesign(svy_design, type = "bootstrap", replicates = 200)

# Extract the full replicate weights matrix
rep_wgts <- weights(rep_design, type = "analysis")  # matrix: rows = obs, cols = replicates

# Replicate mean and variance
rep_means_rel <- coef(svymean(~sf_rel, rep_design, return.replicates = TRUE, na.rm = TRUE))

rep_vars_rel <- sapply(1:ncol(rep_wgts), function(i) {
  rep_data <- rep_wgts[, i]
  svy_var  <- svydesign(ids=~psu_id,strata=~strat_id,
                        nest=TRUE, weights = ~rep_data, data = df)
  coef(svyvar(~sf_rel, svy_var, na.rm = TRUE))
})

# Estimate NB dispersion (theta)
rep_theta_rel <- rep_means_rel^2 / pmax(rep_vars_rel - rep_means_rel, 1e-6)

sim_datasets <- vector("list", n_draws)

for (i in 1:n_draws) {
  set.seed(i)
  sampled_mu <- rnorm(1, mu_rel, sqrt(var_rel))                      # Sample mean with uncertainty
  sampled_theta <- sample(rep_theta_rel, 1, replace = TRUE)         # Sample theta
  
  sim_rel <- qnbinom(runif(n_sim), size = sampled_theta, mu = sampled_mu)
  sim_rel[is.na(sim_rel)] <- 0
  
  sim_rel <- pmin(sim_rel, max_rel*2)
  
  my_dom_id_string<-dom
  sim_datasets[[i]] <- data.frame(sim_id = i, sf_rel_sim = sim_rel, my_dom_id_string = my_dom_id_string)
}

# Combine all simulations
combined_sim <- bind_rows(
  lapply(seq_along(sim_datasets), function(i) {
    sim_datasets[[i]] %>%
      mutate(sim_id = i)
  })
)

all_results2[[dom]] <- combined_sim
}

final_result2 <- bind_rows(all_results2)
final_result2 <- final_result2 %>%
  group_by(my_dom_id_string, sim_id) %>%
  mutate(id = row_number(), sf_keep_sim=0) %>%
  ungroup()
}

# List the objects you want to keep
keep <- c("final_results1", "final_results2", "df_full1", "df_full2", "df_full3", "df_full4", "n_sim", "n_draws")

# Remove everything else
rm(list = setdiff(ls(), keep))


### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)>0
if (nrow(df_full3) > 0){
  
all_results3 <- list()

for (dom in unique(df_full3$my_dom_id_string)) {
  
df <- df_full3 %>% filter(my_dom_id_string == dom)

# Define survey design
svy_design <- svydesign(ids = ~psu_id, strata = ~strat_id, weights = ~wp_int, data = df, nest = TRUE)
options(survey.lonely.psu = "certainty")

max_keep <- round(max(df$sf_keep))

df$w_int_rounded <- round(df$wp_int)
df_expanded <- uncount(df, weights = w_int_rounded) #expand the data so that each row represents a single trip outcome
df_expanded <- df_expanded %>% sample_n(min(nrow(df_expanded), 10000)) 
obs_sd_keep <- sd(df_expanded$sf_keep)

# 3. Estimate mean and variance for sf_keep
mean_keep <- svymean(~sf_keep, svy_design)
mu_keep <- coef(mean_keep)
var_keep <- attr(mean_keep, "var")

mean_cat <- svymean(~sf_catch, svy_design)
mu_cat <- coef(mean_cat)
var_cat <- attr(mean_cat, "var")


# Handle zero or missing variance (certainty units)
# Use imputed linearized standard error 
if (is.na(var_keep) || var_keep == 0) {
  imputed_se <- mean(df$sesf_keep)  
  var_rel <- imputed_se^2
}

# Bootstrap replicate design
rep_design <- as.svrepdesign(svy_design, type = "bootstrap", replicates = 200)

# Extract the full replicate weights matrix
rep_wgts <- weights(rep_design, type = "analysis")  # matrix: rows = obs, cols = replicates

# Replicate mean and variance
rep_means_keep <- coef(svymean(~sf_keep, rep_design, return.replicates = TRUE, na.rm = TRUE))

rep_vars_keep <- sapply(1:ncol(rep_wgts), function(i) {
  rep_data <- rep_wgts[, i]
  svy_var  <- svydesign(ids=~psu_id,strata=~strat_id,
                        nest=TRUE, weights = ~rep_data, data = df)
  coef(svyvar(~sf_keep, svy_var, na.rm = TRUE))
})

# Estimate NB dispersion (theta)
rep_theta_keep <- rep_means_keep^2 / pmax(rep_vars_keep - rep_means_keep, 1e-6)

sim_datasets <- vector("list", n_draws)

i <- 1
while (i <= n_draws) {
  set.seed(i)
  
  sampled_mu <- rnorm(1, mu_keep, sqrt(var_keep))                    # Sample mean with uncertainty
  sampled_theta <- sample(rep_theta_keep, 1, replace = TRUE)         # Sample theta

  sim_keep <- qnbinom(runif(n_sim), size = sampled_theta, mu = sampled_mu)

  sim_keep[is.na(sim_keep)] <- 0
  sim_keep <- pmin(sim_keep, max_keep+2)
  
  sim_datasets[[i]] <- data.frame(sim_id = i, sf_keep_sim = sim_keep)
  i <- i + 1  # Only increment if no NaNs
  

}

# Combine all simulations
combined_sim <- bind_rows(
  lapply(seq_along(sim_datasets), function(i) {
    sim_datasets[[i]] %>%
      mutate(sim_id = i)
  })
)

all_results3[[dom]] <- combined_sim
}

final_result3 <- bind_rows(all_results3)
final_result3 <- final_result3 %>%
  group_by(my_dom_id_string, sim_id) %>%
  mutate(id = row_number(), sf_rel_sim=0) %>%
  ungroup()
}

# List the objects you want to keep
keep <- c("final_results1", "final_results2","final_results3", "df_full1", "df_full2", "df_full3", "df_full4", "n_sim", "n_draws")

# Remove everything else
rm(list = setdiff(ls(), keep))


### MEAN(DISCARDS-PER-TRIP)==0, MEAN(HARVEST-PER-TRIP)>0

####Summer flounder####
if (nrow(df_full4) > 0){
  
all_results4 <- list()
  
for (dom in unique(df_full4$my_dom_id_string)) {

sim_datasets <- list()
i <- 1
while (i <= n_draws) {
  sim_keep <- 0
  sim_rel <- 0
  
  sim_datasets[[i]] <- data.frame(sim_id = i, sf_keep_sim = sim_keep, sf_rel_sim = sim_rel)
  i <- i + 1 
}

# Combine all simulations
combined_sim <- bind_rows(
  lapply(seq_along(sim_datasets), function(i) {
    sim_datasets[[i]] %>%
      mutate(sim_id = i)
  })
)

all_results4[[dom]] <- combined_sim
}

final_result4 <- bind_rows(all_results4)
final_result4 <- final_result4 %>%
  group_by(my_dom_id_string, sim_id) %>%
  mutate(id = row_number()) %>%
  ungroup()
}

# List the objects you want to keep
keep <- c("final_results1", "final_results2","final_results3", "final_results4","df_full1", "df_full2", "df_full3", "df_full4", "n_sim", "n_draws")

# Remove everything else
rm(list = setdiff(ls(), keep))

#########COMBINE DRAWS ACROSS DOMAINS AND SIMULATIONS
# Initialize an empty list
results_list <- list()

# Check for each final_results object and add to the list if it exists
if (exists("final_results1")) results_list <- append(results_list, list(final_results1))
if (exists("final_results2")) results_list <- append(results_list, list(final_results2))
if (exists("final_results3")) results_list <- append(results_list, list(final_results3))
if (exists("final_results4")) results_list <- append(results_list, list(final_results4))

# Combine all existing results into one data frame
combined_results <- do.call(rbind, results_list)