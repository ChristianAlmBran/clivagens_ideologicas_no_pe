# Loading the packages
pacman::p_load(brms, tidyverse, performance)

# Loading the df
df_mlr <- read.csv('df_mlr.csv')

# Verifying the df
head(df_mlr)
str(df_mlr)

# Only keeping legislative and EP7 votes 
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

# Step 1 - Building the empty model
## eu_position
model_empty_eu <- glmer(true_vote_eu_position ~ (1 | mep_unique) + (1 | vote_id_unique), data = legislative_mlr_centered, family = 'binomial')
summary(model_empty_eu)
var_eu_mep <- model_empty_eu@theta[1]^2
var_eu_vote <- model_empty_eu@theta[2]^2
icc_eu_vote_ep8 <- var_eu_vote / (var_eu_mep + var_eu_vote + (3.14159^2/3))
icc_eu_mep_ep8 <- var_eu_mep / (var_eu_mep + var_eu_vote + (3.14159^2/3))

## lrgen
model_empty_lrgen <- glmer(true_vote_lrgen ~ (1 | mep_unique) + (1 | vote_id_unique), data = legislative_mlr_centered, family = 'binomial')
summary(model_empty_lrgen)
var_lrgen_mep <- model_empty_lrgen@theta[1]^2
var_lrgen_vote <- model_empty_lrgen@theta[2]^2
icc_lrgen_vote_ep8 <- var_lrgen_vote / (var_lrgen_mep + var_lrgen_vote + (3.14159^2/3))
icc_lrgen_mep_ep8 <- var_lrgen_mep / (var_lrgen_mep + var_lrgen_vote + (3.14159^2/3))

## galtan
model_empty_galtan <- glmer(true_vote_galtan ~ (1 | mep_unique) + (1 | vote_id_unique), data = legislative_mlr_centered, family = 'binomial')
summary(model_empty_galtan)
var_galtan_mep <- model_empty_galtan@theta[1]^2
var_galtan_vote <- model_empty_galtan@theta[2]^2
icc_galtan_vote_ep8 <- var_galtan_vote / (var_galtan_mep + var_galtan_vote + (3.14159^2/3))
icc_galtan_mep_ep8 <- var_galtan_mep / (var_galtan_mep + var_galtan_vote + (3.14159^2/3))

# Step 2 - Building the intermediate models
## Creating the formulas - CIM
### eu_position
formula_cim_eu <- true_vote_eu_position ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 | mep_unique) +
  (1 | vote_id_unique)

### lrgen
formula_cim_lrgen <- true_vote_lrgen ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 | mep_unique) +
  (1 | vote_id_unique)

### galtan
formula_cim_galtan <- true_vote_galtan ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 | mep_unique) +
  (1 | vote_id_unique)

## Running the constrained intermediate models (CIM)
### eu_position
cim_eu <- glmer(formula_cim_eu, data = legislative_mlr_centered, family = 'binomial')

## lrgen
cim_lrgen <- glmer(formula_cim_lrgen, data = legislative_mlr_centered, family = 'binomial')

## galtan
cim_galtan <- glmer(formula_cim_galtan, data = legislative_mlr_centered, family = 'binomial')

## Creating the formulas - AIM
### eu_position
formula_aim_eu_random_eu <- true_vote_eu_position ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 + eu_position_cmc || mep_unique) +
  (1 | vote_id_unique)

formula_aim_eu_random_lrgen <- true_vote_eu_position ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 + lrgen_cmc || mep_unique) +
  (1 | vote_id_unique)

formula_aim_eu_random_galtan <- true_vote_eu_position ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 + galtan_cmc || mep_unique) +
  (1 | vote_id_unique)

### lrgen
formula_aim_lrgen_random_eu <- true_vote_lrgen ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 + eu_position_cmc || mep_unique) +
  (1 | vote_id_unique)

formula_aim_lrgen_random_lrgen <- true_vote_lrgen ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 + lrgen_cmc || mep_unique) +
  (1 | vote_id_unique)

formula_aim_lrgen_random_galtan <- true_vote_lrgen ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 + galtan_cmc || mep_unique) +
  (1 | vote_id_unique)

### galtan
formula_aim_galtan_random_eu <- true_vote_galtan ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 + eu_position_cmc || mep_unique) +
  (1 | vote_id_unique)

formula_aim_galtan_random_lrgen <- true_vote_galtan ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 + lrgen_cmc || mep_unique) +
  (1 | vote_id_unique)

formula_aim_galtan_random_galtan <- true_vote_galtan ~ lrgen_cmc +
  eu_position_cmc +
  galtan_cmc +
  environment_only +
  procedure +
  final_vote +
  (1 + galtan_cmc || mep_unique) +
  (1 | vote_id_unique)

## Running the augmented intermediate model (AIM)
### eu_position
aim_eu_random_eu <- glmer(formula_aim_eu_random_eu, data = legislative_mlr_centered, family = "binomial")
aim_eu_random_lrgen <- glmer(formula_aim_eu_random_lrgen, data = legislative_mlr_centered, family = "binomial")
aim_eu_random_galtan <- glmer(formula_aim_eu_random_galtan, data = legislative_mlr_centered, family = "binomial")

### lrgen
aim_lrgen_random_eu <- glmer(formula_aim_lrgen_random_eu, data = legislative_mlr_centered, family = "binomial")
aim_lrgen_random_lrgen <- glmer(formula_aim_lrgen_random_lrgen, data = legislative_mlr_centered, family = "binomial")
aim_lrgen_random_galtan <- glmer(formula_aim_lrgen_random_galtan, data = legislative_mlr_centered, family = "binomial")

### galtan
aim_galtan_random_eu <- glmer(formula_aim_galtan_random_eu, data = legislative_mlr_centered, family = "binomial")
aim_galtan_random_lrgen <- glmer(formula_aim_galtan_random_lrgen, data = legislative_mlr_centered, family = "binomial")
aim_galtan_random_galtan <- glmer(formula_aim_galtan_random_galtan, data = legislative_mlr_centered, family = "binomial")

## Executing the likelihood ratio test
### eu_position
anova(cim_eu, aim_eu_random_eu)
anova(cim_eu, aim_eu_random_lrgen)
anova(cim_eu, aim_eu_random_galtan)

### lrgen
anova(cim_lrgen, aim_lrgen_random)
anova(cim_lrgen, aim_lrgen_random)
anova(cim_lrgen, aim_lrgen_random)

### galtan
anova(cim_galtan, aim_galtan_random_eu)
anova(cim_galtan, aim_galtan_random_lrgen)
anova(cim_galtan, aim_galtan_random_galtan)
