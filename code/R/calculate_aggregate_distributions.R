#!/usr/bin/env Rscript
# Collection of  functions to generate aggregate distribution across individual team submissions
# implementes "linear opinion pool" method (simple averaging of distributions)



######################
# Preamble
# --------------------
require(tidyverse)
require(reshape)
require(ggplot2)
require(dplyr)
require(readr)



#### aggregate functions ####

# create_interp_fns
# function: interpolate cdfs based on empirical distributions provided by each team (use linear interpolation)
#           any team that submits point estimates instead of distributions are omitted from analyses
# input:    df    data.frame formated following scenario hub and filtered to specific location-target-scenario, must contain 
#                 columns ---
#                   model - character with model name
#                   quantile - double containing quantiles (represented as values between 0 and 1)
#                   value - integer? containing team cdf for each quantile
# output:   a list of interpolated functions (named by model)
create_interp_fns <- function(dat){
  # create list to store interpolations
  interp_functions <- list()
  # for each model, subset df and create interpolation
  for(i in unique(dat$model)){
    df_sub <- subset(dat, model == i)
    # check to handle point estimates
    #   (1) if all quantiles equal to single value, do not call approxfun() 
    #   (2) submit single value in quantile 0.5, and 0s for all others
    #   (3) any quantile contains NA
    if(length(unique(df_sub$value))==1 | 
       (all(df_sub[df_sub$quantile != 0.50,"value"]==0) & df_sub[df_sub$quantile == 0.50,"value"]!=0) | 
       any(is.na(df_sub$value))){  #interp_functions[[i]] <- NA
      next 
    }
    interp_functions[[i]] <- approxfun(x = df_sub$value, 
                                       y = df_sub$quantile, 
                                       method = "linear", 
                                       yleft = 0, yright = 1, rule = 2, ties = mean)
  }
  return(interp_functions)
}

# calc_aggregate_cdf
# function: average over each team cdf to create an aggregate cumulative distribution function (linear opinion pool method)
#           for a single location-target-scenario (l-t-s) combination
# input:    limits           vector of length 2 that contains the minimum and maximum values across teams for that l-t-s combo
#           interp_functions list of interpolted cdfs (as functions) named by model name (output from create_interp_fns())
#           model_weights    data.frame to define weights for each model when averaging
#                            columns ---
#                                model - character, one row for each unique model
#                                weight - double, weight given to this model in average
# output:   data.frame containing aggregate cdf
#           columns ---
#              quantile - double, value between 0 and 1 representing quantile (in 0.05 increments)
#              value - double, aggregate cdf values
calc_aggregate_cdf <- function(limits, interp_functions, model_weights){ #dat, 
  # Create a list to store output
  out_agg <-cdf_out <- list()
  # Create a vector of values of the target (of length 1000) over which to interpolate
  vals <- seq(limits[1], limits[2], length.out = 1000)
  # for each model, estimate quantile for each value in vals 
  cdf_out <- lapply(interp_functions, function(fn){if(!is(fn,"function")){return(NA)};fn(vals)})
  # Find long dataset (easier for plotting in ggplot)
  df_long <- melt(cdf_out)
  names(df_long) <- c("quantile", "model")
  df_long <- df_long %>% filter(!is.na(quantile))
  df_long$vals <- rep(vals, length(which(!is.na(cdf_out))))
  # Average across team ID, using weights defined in model_weights
  df_long_weight <- merge(df_long, model_weights)
  gdf_cdf <- df_long_weight %>% 
    group_by(vals) %>% 
    summarise(value = weighted.mean(quantile, weight), .groups = 'drop')
  # Convert back to a CDF function and interpolate for the quantiles we've asked for
  if(nrow(gdf_cdf) == 1){ # check for point estimates - can remove?
    df_agg <- data.frame("quantile" = 0:100, "value" = gdf_cdf$value)
  }
  else{
    cdf_agg <- approxfun(x = gdf_cdf$value, y = gdf_cdf$vals, rule = 2, ties = mean)
    x <- seq(0,1,0.05)
    df_agg <- data.frame("quantile" = x, "value" = cdf_agg(x))
  }
  return(df_agg)
}


# calculate_agg
# function: implement full aggregate calculation method for a single location-target-scenario (l-t-s) combination
#           including (1) interpolating team cdfs and (2) averaging
# input:    dat            data.frame formatted following scenario hub and filtered to specific l-t-s, must contain 
#                             columns ---
#                               model - character with model name
#                               quantile - double containing quantiles (represented as values between 0 and 1)
#                               value - integer? containing team cdf for each quantile
#           model_weights  data.frame to define weights for each model when averaging
#                             columns ---
#                                model - character, one row for each unique model
#                                weight - double, weight given to this model in average
# output:   data.frame containing aggregate cdf
#           columns ---
#              quantile - double, value between 0 and 1 representing quantile (in 0.05 increments)
#              value - double, aggregate cdf values
calculate_agg <- function(dat, model_weights){
  # interpolate team cdfs
  interp_functions <- create_interp_fns(dat)
  if(length(interp_functions) == 0){return(NA)}
  # find min/max values across all teams (used as min/max values for agg cdf)
  limits <- c(min(dat$value), max(dat$value))
  if(any(is.infinite(limits))){return(NA)}
  if(anyNA(limits)){return(NA)}
  # average across teams to calculate aggregate
  agg <- calc_aggregate_cdf(limits, interp_functions, model_weights)
  return(agg)
}

# apply_agg_calculation
# function: implement aggregation across multiple l-t-s combinations
#           intended to be passed into apply() function for speed
# input:    dat            data.frame formated following scenario hub with data for all l-t-s scenarios to aggregate
#                             columns ---
#                               scenario_name - character, scenario name
#                               location - character, location code
#                               target - character, target name
#                               model - character with model name
#                               quantile - double containing quantiles (represented as values between 0 and 1)
#                               value - integer? containing team cdf for each quantile
#           model_weights  data.frame to define weights for each model when averaging
#                             columns ---
#                                model - character, one row for each unique model
#                                weight - double, weight given to this model in average
#           agg_combo      vector containing specific l-t-s combination to aggregate 
#                          elements ---
#                                loc - one specific location
#                                targ - one specific target
#                                scen - one specific scenario 
# output:   data.frame containing l-t-s combination and aggregate cdf
#           if no teams or only one team has submitted for this l-t-s combination, return NA quantile and value
#           columns ---
#              scenario_name - character, name of scenario
#              location - character?, location code
#              target - character, target
#              quantile - double, value between 0 and 1 representing quantile (in 0.05 increments)
#              value - double, aggregate cdf values
apply_agg_calculation <- function(dat, model_weights, agg_combo){
  with(as.list(agg_combo),{ # in order to call scen, loc, targ directly
    # filter data to specific l-t-s
    dat_sub <- dat %>% filter(scenario_name == scen, location == loc, target == targ)
    # calculate aggregate
    ret <- calculate_agg(dat_sub, model_weights)
    # check for NAs, and return l-t-s with aggregate
    ifelse(any(is.na(ret)), ret<-data.frame(quantile = NA, value = NA), ret<-ret)
    return(list(data.frame(scenario_name = scen, location =  loc, target = targ, model = "aggregate", ret)))
  })
}



