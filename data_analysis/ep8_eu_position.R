# Loading the packages
pacman::p_load(brms, tidyverse, performance, glmmTMB)

# Loading the df
df_mlr <- read.csv('df_mlr.csv')

# Verifying the df
head(df_mlr)
str(df_mlr)

# Only keeping legislative and EP8 votes 
## copying the df and filtering
legislative_mlr <- df_mlr %>%
  filter(leg.non.leg.bud == 'legislative' & parliamentary_term == 'EP8')

## removing old df
rm(df_mlr)

# mlr
# Converting necessary columns to appropriate types with lables and levels
legislative_mlr$procedure <- factor(legislative_mlr$procedure, levels = c('first_reading', 'late_reading'), labels = c('first_reading', 'late_reading'))

legislative_mlr$final_vote <- factor(legislative_mlr$final_vote, levels = c('No', 'Yes'), labels = c('No', 'Yes'))

legislative_mlr$environment_only <- factor(legislative_mlr$environment_only, levels = c('Non-environment', 'Environment'), labels = c('Non-environment', 'Environment'))

legislative_mlr$vote_id_unique <- factor(legislative_mlr$vote_id_unique)

legislative_mlr$mep_unique <- factor(legislative_mlr$mep_unique)

# Centering the predictors
## Copying the df
legislative_mlr_centered <- legislative_mlr

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

# glmmTMB for eu_position
eu_position_glmmtmb <- glmmTMB(true_vote_eu_position ~ 
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

check_collinearity(eu_position_glmmtmb)
kappa(model.matrix(eu_position_glmmtmb))

#NO MEAN YES FINAL VOTE----
# Define the extreme values for eu_position_cmc (eu_position_ep8_environment)
eu_position_ep8_environment_extreme_values <- data.frame(
  eu_position_cmc = c(min(legislative_mlr_centered$eu_position_cmc), max(legislative_mlr_centered$eu_position_cmc)),
  environment_only = "Environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "Yes", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  galtan_cmc = c(mean(legislative_mlr_centered$galtan_cmc))
)

# Generate predicted probabilities
predictions <- predict(eu_position_glmmtmb, newdata = eu_position_ep8_environment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[1] - predictions[2]

# Generate the summary
summary_glmmtmb <- summary(eu_position_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
eu_position_cmc <- fixed_effects["eu_position_cmc", "Std. Error"]

# Extract the variance-covariance matrix
vcov_matrix <- vcov(eu_position_glmmtmb)$cond

# Extract relevant standard errors and covariance for eu_position_cmc and interaction term
se_eu_position_cmc <- sqrt(vcov_matrix["eu_position_cmc", "eu_position_cmc"])
se_interaction <- sqrt(vcov_matrix["environment_onlyEnvironment:eu_position_cmc", "environment_onlyEnvironment:eu_position_cmc"])
cov_eu_position_interaction <- vcov_matrix["eu_position_cmc", "environment_onlyEnvironment:eu_position_cmc"]

# Standard error of the combined effect
se_diff <- sqrt(se_eu_position_cmc^2 + se_interaction^2 + 2 * cov_eu_position_interaction)

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

# Define the extreme values for eu_position_cmc (eu_position_ep8_nonenvironment)
eu_position_ep8_nonenvironment_extreme_values <- data.frame(
  eu_position_cmc = c(min(legislative_mlr_centered$eu_position_cmc), max(legislative_mlr_centered$eu_position_cmc)),
  environment_only = "Non-environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "Yes", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  galtan_cmc = c(mean(legislative_mlr_centered$galtan_cmc))
)

# Generate predicted probabilities
predictions <- predict(eu_position_glmmtmb, newdata = eu_position_ep8_nonenvironment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[1] - predictions[2]

# Generate the summary
summary_glmmtmb <- summary(eu_position_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
eu_position_cmc <- fixed_effects["eu_position_cmc", "Std. Error"]

# Calculate the standard error of the difference
se_diff <- sqrt(eu_position_cmc^2 + eu_position_cmc^2)

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
# Define the extreme values for eu_position_cmc (eu_position_ep8_environment)
eu_position_ep8_environment_extreme_values <- data.frame(
  eu_position_cmc = c(min(legislative_mlr_centered$eu_position_cmc), max(legislative_mlr_centered$eu_position_cmc)),
  environment_only = "Environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "No", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  galtan_cmc = c(mean(legislative_mlr_centered$galtan_cmc))
)

# Generate predicted probabilities
predictions <- predict(eu_position_glmmtmb, newdata = eu_position_ep8_environment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[1] - predictions[2]

# Generate the summary
summary_glmmtmb <- summary(eu_position_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
eu_position_cmc <- fixed_effects["eu_position_cmc", "Std. Error"]

# Extract the variance-covariance matrix
vcov_matrix <- vcov(eu_position_glmmtmb)$cond

# Extract relevant standard errors and covariance for eu_position_cmc and interaction term
se_eu_position_cmc <- sqrt(vcov_matrix["eu_position_cmc", "eu_position_cmc"])
se_interaction <- sqrt(vcov_matrix["environment_onlyEnvironment:eu_position_cmc", "environment_onlyEnvironment:eu_position_cmc"])
cov_eu_position_interaction <- vcov_matrix["eu_position_cmc", "environment_onlyEnvironment:eu_position_cmc"]

# Standard error of the combined effect
se_diff <- sqrt(se_eu_position_cmc^2 + se_interaction^2 + 2 * cov_eu_position_interaction)

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

# Define the extreme values for eu_position_cmc (eu_position_ep8_nonenvironment)
eu_position_ep8_nonenvironment_extreme_values <- data.frame(
  eu_position_cmc = c(min(legislative_mlr_centered$eu_position_cmc), max(legislative_mlr_centered$eu_position_cmc)),
  environment_only = "Non-environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "No", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = c(mean(legislative_mlr_centered$lrgen_cmc)),
  galtan_cmc = c(mean(legislative_mlr_centered$galtan_cmc))
)

# Generate predicted probabilities
predictions <- predict(eu_position_glmmtmb, newdata = eu_position_ep8_nonenvironment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[1] - predictions[2]

# Generate the summary
summary_glmmtmb <- summary(eu_position_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
eu_position_cmc <- fixed_effects["eu_position_cmc", "Std. Error"]

# Calculate the standard error of the difference
se_diff <- sqrt(eu_position_cmc^2 + eu_position_cmc^2)

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
mean_quartiles <- function(x) {
  sorted_x <- sort(x)
  n <- length(sorted_x)
  lower_25_mean <- mean(sorted_x[1:floor(n * 0.10)])
  upper_25_mean <- mean(sorted_x[ceiling(n * 0.90):n])
  return(c(lower_25_mean, upper_25_mean))
}

# Define the extreme values for eu_position_cmc (eu_position_ep8_environment)
eu_position_cmc_quartiles <- mean_quartiles(legislative_mlr_centered$eu_position_cmc)
eu_position_ep8_environment_extreme_values <- data.frame(
  eu_position_cmc = eu_position_cmc_quartiles,
  environment_only = "Environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "Yes", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = mean(legislative_mlr_centered$lrgen_cmc),
  galtan_cmc = mean(legislative_mlr_centered$galtan_cmc)
)

# Generate predicted probabilities
predictions <- predict(eu_position_glmmtmb, newdata = eu_position_ep8_environment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[1] - predictions[2]

# Generate the summary
summary_glmmtmb <- summary(eu_position_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_eu_position_cmc <- fixed_effects["eu_position_cmc", "Std. Error"]

# Extract the variance-covariance matrix
vcov_matrix <- vcov(eu_position_glmmtmb)$cond

# Extract relevant standard errors and covariance for eu_position_cmc and interaction term
se_eu_position_cmc <- sqrt(vcov_matrix["eu_position_cmc", "eu_position_cmc"])
se_interaction <- sqrt(vcov_matrix["environment_onlyEnvironment:eu_position_cmc", "environment_onlyEnvironment:eu_position_cmc"])
cov_eu_position_interaction <- vcov_matrix["eu_position_cmc", "environment_onlyEnvironment:eu_position_cmc"]

# Standard error of the combined effect
se_diff <- sqrt(se_eu_position_cmc^2 + se_interaction^2 + 2 * cov_eu_position_interaction)

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

# Define the extreme values for eu_position_cmc (eu_position_ep8_nonenvironment)
eu_position_ep8_nonenvironment_extreme_values <- data.frame(
  eu_position_cmc = eu_position_cmc_quartiles,
  environment_only = "Non-environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "Yes", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = mean(legislative_mlr_centered$lrgen_cmc),
  galtan_cmc = mean(legislative_mlr_centered$galtan_cmc)
)

# Generate predicted probabilities
predictions <- predict(eu_position_glmmtmb, newdata = eu_position_ep8_nonenvironment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[1] - predictions[2]

# Generate the summary
summary_glmmtmb <- summary(eu_position_glmmtmb)

# Extract the fixed effects and their standard errors
se_eu_position_cmc <- fixed_effects["eu_position_cmc", "Std. Error"]

# Calculate the standard error of the difference
se_diff <- sqrt(se_eu_position_cmc^2 + se_eu_position_cmc^2)

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

# Define the extreme values for eu_position_cmc (eu_position_ep8_environment)
eu_position_cmc_quartiles <- mean_quartiles(legislative_mlr_centered$eu_position_cmc)
eu_position_ep8_environment_extreme_values <- data.frame(
  eu_position_cmc = eu_position_cmc_quartiles,
  environment_only = "Environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "No", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = mean(legislative_mlr_centered$lrgen_cmc),
  galtan_cmc = mean(legislative_mlr_centered$galtan_cmc)
)

# Generate predicted probabilities
predictions <- predict(eu_position_glmmtmb, newdata = eu_position_ep8_environment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[1] - predictions[2]

# Generate the summary
summary_glmmtmb <- summary(eu_position_glmmtmb)

# Extract the fixed effects and their standard errors
fixed_effects <- summary_glmmtmb$coefficients$cond
se_eu_position_cmc <- fixed_effects["eu_position_cmc", "Std. Error"]

# Extract the variance-covariance matrix
vcov_matrix <- vcov(eu_position_glmmtmb)$cond

# Extract relevant standard errors and covariance for eu_position_cmc and interaction term
se_eu_position_cmc <- sqrt(vcov_matrix["eu_position_cmc", "eu_position_cmc"])
se_interaction <- sqrt(vcov_matrix["environment_onlyEnvironment:eu_position_cmc", "environment_onlyEnvironment:eu_position_cmc"])
cov_eu_position_interaction <- vcov_matrix["eu_position_cmc", "environment_onlyEnvironment:eu_position_cmc"]

# Standard error of the combined effect
se_diff <- sqrt(se_eu_position_cmc^2 + se_interaction^2 + 2 * cov_eu_position_interaction)

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

# Define the extreme values for eu_position_cmc (eu_position_ep8_nonenvironment)
eu_position_ep8_nonenvironment_extreme_values <- data.frame(
  eu_position_cmc = eu_position_cmc_quartiles,
  environment_only = "Non-environment", # or other relevant value
  procedure = "first_reading", # or other relevant value
  final_vote = "No", # or other relevant value
  mep_unique = NA, # setting random effects to zero
  vote_id_unique = NA,
  lrgen_cmc = mean(legislative_mlr_centered$lrgen_cmc),
  galtan_cmc = mean(legislative_mlr_centered$galtan_cmc)
)

# Generate predicted probabilities
predictions <- predict(eu_position_glmmtmb, newdata = eu_position_ep8_nonenvironment_extreme_values, type = "response")

# Calculate the difference in predicted probabilities
diff_predicted_probs <- predictions[1] - predictions[2]

# Generate the summary
summary_glmmtmb <- summary(eu_position_glmmtmb)

# Extract the fixed effects and their standard errors
se_eu_position_cmc <- fixed_effects["eu_position_cmc", "Std. Error"]

# Calculate the standard error of the difference
se_diff <- sqrt(se_eu_position_cmc^2 + se_eu_position_cmc^2)

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
