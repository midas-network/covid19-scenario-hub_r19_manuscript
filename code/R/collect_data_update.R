##################################################################
## Gather all information needed for the report
## 1. ground truth data
## 2. projections
## 3. projection intervals
## 4. ensemble distribution (Vincent-median)
## 5. aggregate distribution (LOP)
##################################################################

######################
# Preamble
# --------------------
require(tidyverse)
require(reshape)
require(ggplot2)
require(dplyr)
require(readr)


## choose directory
#dir_path <- "C:/Users/clair/Documents/COVID/covid19-scenario-hub_visualization/"
#dir_path <- "/Users/rebeccaborchering/Documents/GitHub/covid19-scenario-hub_visualization/"



##################################################################
## load  ground truth data ('truth') and quantiles of model projections ('quant_data')
################################################################
# if(!round %in% c(17, 18)) {source(file.path(dir_path, "code/report/data_processing/load_data_update.R"))}
# if(round == 17) {source(file.path(dir_path, "code/report/data_processing/load_megaround_data.R"))}
# if(round == 18) {source(file.path(dir_path, "code/report/data_processing/load_R18_data.R"))}
source(file.path(dir_path, "code/R/load_processed_data.R"))


##################################################################
## calculate ensemble (Vincent-median) and aggregate (LOP)
##################################################################
## load aggregation functions
source(file.path(dir_path, "code/R/calculate_aggregate_distributions.R"))
## use "produce_ensemble.R" to add Vincent-median ensemble and LOP aggregate/ensemble


##### and format model projections, ensemble, and aggregate distributions
###### Get 'quant_data' and 'agg_results' ################
## use calculate_aggregate_distributions to obtain 'agg_results'##



##setwd("~/Documents/GitHub/covid19-scenario-hub_visualization")
#agg_results <- read.csv("code/report generation/output/agg_data/aggregate_distributions_test_r1.csv",stringsAsFactors = T)


# 
# 
# ## format 'agg_results' data to be consistent with 'quant_data'
# target_map <- quant_data[, c("target","target_type","type")] %>% distinct()
# agg_results <- left_join(agg_results,target_map)
# agg_results <- left_join(agg_results, loc)
# 
# ## NOTE: scenario_id is something we should keep when calculating the aggregate
# agg_results <- agg_results %>% mutate(model_projection_date = quant_data[1,"model_projection_date"],
#                                       target_end_date = quant_data[1,"target_end_date"],
#                                       scenario_id = "to_be_fixed",
#                                       teams = "aggregate"
# )
# 
# ## check all columns are included
# colnames(quant_data)[which(!colnames(quant_data) %in% colnames(agg_results))]
# 
# ## combine 'quant_data' and 'agg_results'
# quant_data_full <- full_join(quant_data,agg_results)
# quant_data_full <- quant_data_full %>% mutate(teams=NULL)
# 
# 
# ## add ensemble values generated in ensemble generation file
# ## maybe want to incorporate ensemble value calc in this script?
# 

