pacman::p_load(brms, tidyverse, performance)

df_mlr <- read.csv('df_mlr.csv')

# Verifying the df
head(df_mlr)
str(df_mlr)

# Only keeping legislative votes 
## copying the df and filtering
legislative_mlr <- df_mlr %>%
  filter(leg.non.leg.bud == 'legislative' & parliamentary_term == 'EP7')
# IF YOU EXCLUDE ONLY THE PARLIAMENTARY TERM YOU DONT GET A MULTICOLINEARITY PROBLEM
#legislative_mlr <- df_mlr

## removing old df
rm(df_mlr)

sampled_df <- legislative_mlr
# mlr

# Converting necessary columns to appropriate types with lables and levels
sampled_df$procedure <- factor(sampled_df$procedure, levels = c('first_reading', 'late_reading'), labels = c('first_reading', 'late_reading'))

sampled_df$final_vote <- factor(sampled_df$final_vote, levels = c('No', 'Yes'), labels = c('No', 'Yes'))

sampled_df$environment_only <- factor(sampled_df$environment_only, levels = c('Non-environment', 'Environment'), labels = c('Non-environment', 'Environment'))

sampled_df$vote_id_unique <- factor(sampled_df$vote_id_unique)

sampled_df$mep_unique <- factor(sampled_df$mep_unique)

#sampled_df$leg.non.leg.bud <- factor(sampled_df$leg.non.leg.bud, levels = c('budgetary', 'legislative', 'non_legislative'), labels = c('budgetary', 'legislative', 'non_legislative'))

# Centering the predictors
## Copying the df
legislative_mlr_centered <- sampled_df

## removing old df
rm(legislative_mlr)

# eu_position

## Calculating the cluster-specific mean of eu_position
cluster_mean_eu_position <- data.frame(tapply(legislative_mlr_centered$eu_position, legislative_mlr_centered$vote_id_unique, mean))

## Adding cluster IDs as a new column
cluster_mean_eu_position$id_cluster <- rownames(cluster_mean_eu_position)

## Renaming the columns
names(cluster_mean_eu_position) <- c('cluster_mean_eu_position', 'vote_id_unique')

## Merging the cluster means with the original data
legislative_mlr_centered <- merge(legislative_mlr_centered, cluster_mean_eu_position, by = 'vote_id_unique')

## Creating the cluster-mean centered variable
legislative_mlr_centered$eu_position_cmc <- legislative_mlr_centered$eu_position - legislative_mlr_centered$cluster_mean_eu_position

# lrgen

## Calculating the cluster-specific mean of lrgen
cluster_mean_lrgen <- data.frame(tapply(legislative_mlr_centered$lrgen, legislative_mlr_centered$vote_id_unique, mean))

## Adding cluster IDs as a new column
cluster_mean_lrgen$id_cluster <- rownames(cluster_mean_lrgen)

## Renaming the columns
names(cluster_mean_lrgen) <- c('cluster_mean_lrgen', 'vote_id_unique')

## Merging the cluster means with the original data
legislative_mlr_centered <- merge(legislative_mlr_centered, cluster_mean_lrgen, by = 'vote_id_unique')

## Creating the cluster-mean centered variable
legislative_mlr_centered$lrgen_cmc <- legislative_mlr_centered$lrgen - legislative_mlr_centered$cluster_mean_lrgen

# galtan

## Calculating the cluster-specific mean of galtan
cluster_mean_galtan <- data.frame(tapply(legislative_mlr_centered$galtan, legislative_mlr_centered$vote_id_unique, mean))

## Adding cluster IDs as a new column
cluster_mean_galtan$id_cluster <- rownames(cluster_mean_galtan)

## Renaming the columns
names(cluster_mean_galtan) <- c('cluster_mean_galtan', 'vote_id_unique')

## Merging the cluster means with the original data
legislative_mlr_centered <- merge(legislative_mlr_centered, cluster_mean_galtan, by = 'vote_id_unique')

## Creating the cluster-mean centered variable
legislative_mlr_centered$galtan_cmc <- legislative_mlr_centered$galtan - legislative_mlr_centered$cluster_mean_galtan

# glmmTMB for galtan

library(glmmTMB)
galtan_glmmtmb <- glmmTMB(true_vote_galtan ~ 
                            lrgen_cmc * (environment_only + procedure + final_vote) +
                            eu_position_cmc * (environment_only + procedure + final_vote) +
                            galtan_cmc * (environment_only + procedure + final_vote) +
                            lrgen_cmc * environment_only +
                            eu_position_cmc * environment_only +
                            galtan_cmc * environment_only +
                            (1 | mep_unique) +
                            (1 | vote_id_unique),
                          data = legislative_mlr_centered,
                          family = binomial())
summary(galtan_glmmtmb)

check_collinearity(galtan_glmmtmb)
kappa(model.matrix(galtan_glmmtmb))
cor(legislative_mlr_centered[, c("lrgen_cmc", "eu_position_cmc", "galtan_cmc")])

#NO MEAN YES FINAL VOTE----
# Define the extreme values for galtan_cmc (galtan_ep7_nonenvironment)
galtan_ep7_nonenvironment_extreme_values <- data.frame(
  galtan_cmc = c(min(legislative_mlr_centered$galtan_cmc), max(legislative_mlr_centered$galtan_cmc)),
  environment_only = "Non-environment", # or other relevant value
  procedure = 'first_reading',
  final_vote = 'Yes',
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  eu_position_cmc = c(mean(legislative_mlr_centered$eu_position_cmc))
)

# Generate predicted probabilities
predictions <- predict(galtan_glmmtmb, newdata = galtan_ep7_nonenvironment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[2] - predictions[1]

# Generate the summary
summary_glmmtmb <- summary(galtan_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_galtan_cmc <- fixed_effects["galtan_cmc", "Std. Error"]

# Calculate the standard error of the difference
se_diff <- sqrt(se_galtan_cmc^2 + se_galtan_cmc^2)

# Calculate the z-score and p-value
z_score <- diff_predicted_probs / se_diff
p_value <- 2 * pnorm(-abs(z_score))

# Calculate the 95% confidence interval
z_critical <- 1.96  # 95% confidence level
ci_lower <- diff_predicted_probs - z_critical * se_diff
ci_upper <- diff_predicted_probs + z_critical * se_diff

# Print the results
cat("Difference in predicted probabilities:", diff_predicted_probs, "\n")
cat("Standard error of the difference:", se_diff, "\n")
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")
cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")


# Define the extreme values for galtan_cmc (galtan_ep7_environment)
galtan_ep7_environment_extreme_values <- data.frame(
  galtan_cmc = c(min(legislative_mlr_centered$galtan_cmc), max(legislative_mlr_centered$galtan_cmc)),
  environment_only = "Environment", # or other relevant value
  procedure = 'first_reading',
  final_vote = 'Yes',
  mep_unique = NA, 
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  eu_position_cmc = c(mean(legislative_mlr_centered$eu_position_cmc))
)

# Generate predicted probabilities
predictions <- predict(galtan_glmmtmb, newdata = galtan_ep7_environment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[2] - predictions[1]

# Generate the summary
summary_glmmtmb <- summary(galtan_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_galtan_cmc <- fixed_effects["galtan_cmc", "Std. Error"]

# Extract the variance-covariance matrix
vcov_matrix <- vcov(galtan_glmmtmb)$cond

# Extract relevant standard errors and covariance for galtan_cmc and interaction term
se_galtan_cmc <- sqrt(vcov_matrix["galtan_cmc", "galtan_cmc"])
se_interaction <- sqrt(vcov_matrix["environment_onlyEnvironment:galtan_cmc", "environment_onlyEnvironment:galtan_cmc"])
cov_galtan_interaction <- vcov_matrix["galtan_cmc", "environment_onlyEnvironment:galtan_cmc"]

# Standard error of the combined effect
se_diff <- sqrt(se_galtan_cmc^2 + se_interaction^2 + 2 * cov_galtan_interaction)

# Calculate the z-score and p-value
z_score <- diff_predicted_probs / se_diff
p_value <- 2 * pnorm(-abs(z_score))

# Calculate the 95% confidence interval
z_critical <- 1.96  # 95% confidence level
ci_lower <- diff_predicted_probs - z_critical * se_diff
ci_upper <- diff_predicted_probs + z_critical * se_diff

# Print the results
cat("Difference in predicted probabilities:", diff_predicted_probs, "\n")
cat("Standard error of the difference:", se_diff, "\n")
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")
cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")

#NO MEAN NO FINAL VOTE----
# Define the extreme values for galtan_cmc (galtan_ep7_nonenvironment)
galtan_ep7_nonenvironment_extreme_values <- data.frame(
  galtan_cmc = c(min(legislative_mlr_centered$galtan_cmc), max(legislative_mlr_centered$galtan_cmc)),
  environment_only = "Non-environment", # or other relevant value
  procedure = 'first_reading',
  final_vote = 'No',
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  eu_position_cmc = c(mean(legislative_mlr_centered$eu_position_cmc))
)

# Generate predicted probabilities
predictions <- predict(galtan_glmmtmb, newdata = galtan_ep7_nonenvironment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[2] - predictions[1]

# Generate the summary
summary_glmmtmb <- summary(galtan_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_galtan_cmc <- fixed_effects["galtan_cmc", "Std. Error"]

# Calculate the standard error of the difference
se_diff <- sqrt(se_galtan_cmc^2 + se_galtan_cmc^2)

# Calculate the z-score and p-value
z_score <- diff_predicted_probs / se_diff
p_value <- 2 * pnorm(-abs(z_score))

# Calculate the 95% confidence interval
z_critical <- 1.96  # 95% confidence level
ci_lower <- diff_predicted_probs - z_critical * se_diff
ci_upper <- diff_predicted_probs + z_critical * se_diff

# Print the results
cat("Difference in predicted probabilities:", diff_predicted_probs, "\n")
cat("Standard error of the difference:", se_diff, "\n")
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")
cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")


# Define the extreme values for galtan_cmc (galtan_ep7_environment)
galtan_ep7_environment_extreme_values <- data.frame(
  galtan_cmc = c(min(legislative_mlr_centered$galtan_cmc), max(legislative_mlr_centered$galtan_cmc)),
  environment_only = "Environment", # or other relevant value
  procedure = 'first_reading',
  final_vote = 'No',
  mep_unique = NA, 
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  eu_position_cmc = c(mean(legislative_mlr_centered$eu_position_cmc))
)

# Generate predicted probabilities
predictions <- predict(galtan_glmmtmb, newdata = galtan_ep7_environment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[2] - predictions[1]

# Generate the summary
summary_glmmtmb <- summary(galtan_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_galtan_cmc <- fixed_effects["galtan_cmc", "Std. Error"]

# Extract the variance-covariance matrix
vcov_matrix <- vcov(galtan_glmmtmb)$cond

# Extract relevant standard errors and covariance for galtan_cmc and interaction term
se_galtan_cmc <- sqrt(vcov_matrix["galtan_cmc", "galtan_cmc"])
se_interaction <- sqrt(vcov_matrix["environment_onlyEnvironment:galtan_cmc", "environment_onlyEnvironment:galtan_cmc"])
cov_galtan_interaction <- vcov_matrix["galtan_cmc", "environment_onlyEnvironment:galtan_cmc"]

# Standard error of the combined effect
se_diff <- sqrt(se_galtan_cmc^2 + se_interaction^2 + 2 * cov_galtan_interaction)

# Calculate the z-score and p-value
z_score <- diff_predicted_probs / se_diff
p_value <- 2 * pnorm(-abs(z_score))

# Calculate the 95% confidence interval
z_critical <- 1.96  # 95% confidence level
ci_lower <- diff_predicted_probs - z_critical * se_diff
ci_upper <- diff_predicted_probs + z_critical * se_diff

# Print the results
cat("Difference in predicted probabilities:", diff_predicted_probs, "\n")
cat("Standard error of the difference:", se_diff, "\n")
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")
cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")


#YES MEAN YES FINAL VOTE----
# Define a function to calculate the mean of the lowest 10% and the highest 10%
mean_quartiles <- function(x) {
  sorted_x <- sort(x)
  n <- length(sorted_x)
  lower_25_mean <- mean(sorted_x[1:floor(n * 0.10)])
  upper_25_mean <- mean(sorted_x[ceiling(n * 0.90):n])
  return(c(lower_25_mean, upper_25_mean))
}

# Apply the function to galtan_cmc
galtan_cmc_quartiles <- mean_quartiles(legislative_mlr_centered$galtan_cmc)

# Define the extreme values for galtan_cmc (galtan_ep7_environment)
galtan_ep7_environment_extreme_values <- data.frame(
  galtan_cmc = galtan_cmc_quartiles,
  environment_only = "Environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "Yes", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  eu_position_cmc = c(mean(legislative_mlr_centered$eu_position_cmc))
)

# Generate predicted probabilities
predictions <- predict(galtan_glmmtmb, newdata = galtan_ep7_environment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[2] - predictions[1]

# Generate the summary
summary_glmmtmb <- summary(galtan_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_galtan_cmc <- fixed_effects["galtan_cmc", "Std. Error"]

# Extract the variance-covariance matrix
vcov_matrix <- vcov(galtan_glmmtmb)$cond

# Extract relevant standard errors and covariance for galtan_cmc and interaction term
se_galtan_cmc <- sqrt(vcov_matrix["galtan_cmc", "galtan_cmc"])
se_interaction <- sqrt(vcov_matrix["environment_onlyEnvironment:galtan_cmc", "environment_onlyEnvironment:galtan_cmc"])
cov_galtan_interaction <- vcov_matrix["galtan_cmc", "environment_onlyEnvironment:galtan_cmc"]

# Standard error of the combined effect
se_diff <- sqrt(se_galtan_cmc^2 + se_interaction^2 + 2 * cov_galtan_interaction)

# Calculate the z-score and p-value
z_score <- diff_predicted_probs / se_diff
p_value <- 2 * pnorm(-abs(z_score))

# Calculate the 95% confidence interval
z_critical <- 1.96  # 95% confidence level
ci_lower <- diff_predicted_probs - z_critical * se_diff
ci_upper <- diff_predicted_probs + z_critical * se_diff

# Print the results
cat("Difference in predicted probabilities:", diff_predicted_probs, "\n")
cat("Standard error of the difference:", se_diff, "\n")
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")
cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")

# Define the extreme values for galtan_cmc (galtan_ep7_nonenvironment)
galtan_ep7_nonenvironment_extreme_values <- data.frame(
  galtan_cmc = galtan_cmc_quartiles,
  environment_only = "Non-environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "Yes", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  eu_position_cmc = c(mean(legislative_mlr_centered$eu_position_cmc))
)

# Generate predicted probabilities
predictions <- predict(galtan_glmmtmb, newdata = galtan_ep7_nonenvironment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[2] - predictions[1]

# Generate the summary
summary_glmmtmb <- summary(galtan_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_galtan_cmc <- fixed_effects["galtan_cmc", "Std. Error"]

# Calculate the standard error of the difference
se_diff <- sqrt(se_galtan_cmc^2 + se_galtan_cmc^2)

# Calculate the z-score and p-value
z_score <- diff_predicted_probs / se_diff
p_value <- 2 * pnorm(-abs(z_score))

# Calculate the 95% confidence interval
z_critical <- 1.96  # 95% confidence level
ci_lower <- diff_predicted_probs - z_critical * se_diff
ci_upper <- diff_predicted_probs + z_critical * se_diff

# Print the results
cat("Difference in predicted probabilities:", diff_predicted_probs, "\n")
cat("Standard error of the difference:", se_diff, "\n")
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")
cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")

#YES MEAN NO FINAL VOTE----
mean_quartiles <- function(x) {
  sorted_x <- sort(x)
  n <- length(sorted_x)
  lower_25_mean <- mean(sorted_x[1:floor(n * 0.10)])
  upper_25_mean <- mean(sorted_x[ceiling(n * 0.90):n])
  return(c(lower_25_mean, upper_25_mean))
}

# Apply the function to galtan_cmc
galtan_cmc_quartiles <- mean_quartiles(legislative_mlr_centered$galtan_cmc)

# Define the extreme values for galtan_cmc (galtan_ep7_environment)
galtan_ep7_environment_extreme_values <- data.frame(
  galtan_cmc = galtan_cmc_quartiles,
  environment_only = "Environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "No", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  eu_position_cmc = c(mean(legislative_mlr_centered$eu_position_cmc))
)

# Generate predicted probabilities
predictions <- predict(galtan_glmmtmb, newdata = galtan_ep7_environment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[2] - predictions[1]

# Generate the summary
summary_glmmtmb <- summary(galtan_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_galtan_cmc <- fixed_effects["galtan_cmc", "Std. Error"]

# Extract the variance-covariance matrix
vcov_matrix <- vcov(galtan_glmmtmb)$cond

# Extract relevant standard errors and covariance for galtan_cmc and interaction term
se_galtan_cmc <- sqrt(vcov_matrix["galtan_cmc", "galtan_cmc"])
se_interaction <- sqrt(vcov_matrix["environment_onlyEnvironment:galtan_cmc", "environment_onlyEnvironment:galtan_cmc"])
cov_galtan_interaction <- vcov_matrix["galtan_cmc", "environment_onlyEnvironment:galtan_cmc"]

# Standard error of the combined effect
se_diff <- sqrt(se_galtan_cmc^2 + se_interaction^2 + 2 * cov_galtan_interaction)

# Calculate the z-score and p-value
z_score <- diff_predicted_probs / se_diff
p_value <- 2 * pnorm(-abs(z_score))

# Calculate the 95% confidence interval
z_critical <- 1.96  # 95% confidence level
ci_lower <- diff_predicted_probs - z_critical * se_diff
ci_upper <- diff_predicted_probs + z_critical * se_diff

# Print the results
cat("Difference in predicted probabilities:", diff_predicted_probs, "\n")
cat("Standard error of the difference:", se_diff, "\n")
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")
cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")

# Define the extreme values for galtan_cmc (galtan_ep7_nonenvironment)
galtan_ep7_nonenvironment_extreme_values <- data.frame(
  galtan_cmc = galtan_cmc_quartiles,
  environment_only = "Non-environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "No", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  eu_position_cmc = c(mean(legislative_mlr_centered$eu_position_cmc))
)

# Generate predicted probabilities
predictions <- predict(galtan_glmmtmb, newdata = galtan_ep7_nonenvironment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[2] - predictions[1]

# Generate the summary
summary_glmmtmb <- summary(galtan_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_galtan_cmc <- fixed_effects["galtan_cmc", "Std. Error"]

# Calculate the standard error of the difference
se_diff <- sqrt(se_galtan_cmc^2 + se_galtan_cmc^2)

# Calculate the z-score and p-value
z_score <- diff_predicted_probs / se_diff
p_value <- 2 * pnorm(-abs(z_score))

# Calculate the 95% confidence interval
z_critical <- 1.96  # 95% confidence level
ci_lower <- diff_predicted_probs - z_critical * se_diff
ci_upper <- diff_predicted_probs + z_critical * se_diff

# Print the results
cat("Difference in predicted probabilities:", diff_predicted_probs, "\n")
cat("Standard error of the difference:", se_diff, "\n")
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")
cat("95% Confidence Interval: [", ci_lower, ",", ci_upper, "]\n")
