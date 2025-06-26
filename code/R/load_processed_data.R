##################################################################
## load  ground truth data and model projections 
##################################################################

## NOTE: we want to be careful about including specific file names directly in the
## code here. If the filenames will ever change we should define them in the 
## "collect_data.R" file OR use paste with a representative modifier as done with 
## 'projection_filename' below (modified for round choice)


if(!exists("loc_select")) {
  loc_select <- "US"
}


###############################################
# Read in submitted projection data ('quant_data') ---------------
###############################################

# Folder paths (repo data path to edit)
repo_data <- data_path
folder_path <- file.path("model-processed", round_id)

dc <- arrow::open_dataset(file.path(repo_data, folder_path),
                          partitioning = c("model_name", "target", "location"))

if (loc_select == "all") {
  quant_data <- dc %>% 
    dplyr::filter(output_type == "quantile",
                  model_name %in% model_names,
                  output_type_id %in% c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9, 0.25, 0.75, 0.5)) %>% 
    dplyr::collect()
  
} else {
  quant_data <- dc %>% 
    dplyr::filter(output_type == "quantile",
                  location == loc_select,
                  model_name %in% model_names,
                  output_type_id %in% c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9, 0.25, 0.75, 0.5)) %>% 
    dplyr::collect()
}


quant_data <- quant_data %>% dplyr::mutate(origin_date = as.Date(origin_date), 
                                           output_type_id = as.numeric(output_type_id)) %>%
  dplyr::rename("model" = "model_name",
                "quantile" = "output_type_id")



# quant_data %>% filter(model=="MOBS_NEU-GLEAM_COVID") %>% dplyr::filter(target == "inc hosp") -> temp; unique(temp$quantile)

# dplyr::collect() %>% 
#   dplyr::mutate(origin_date = as.Date(origin_date)) %>%
#   dplyr::rename("model" = "model_name",
#                 "quantile" = "output_type_id") %>%
#   filter(model %in% model_names)
# 
# quant_data <- dplyr::filter(dc, output_type == "quantile",
#                    output_type_id %in% c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9, 0.25, 0.75, 0.5)) %>% 
#   dplyr::collect() %>% 
#   dplyr::mutate(origin_date = as.Date(origin_date)) %>%
#   dplyr::rename("model" = "model_name",
#                 "quantile" = "output_type_id") %>%
#   filter(model %in% model_names)

# quant_data %>% filter(model=="MOBS_NEU-GLEAM_COVID") %>% dplyr::filter(target == "inc hosp") -> temp; unique(temp$quantile)
# quant_data %>% filter(model=="MOBS_NEU-GLEAM_COVID") %>% dplyr::filter(target == "inc death") -> temp; unique(temp$quantile)
# quant_data %>% filter(model=="MOBS_NEU-GLEAM_COVID") %>% dplyr::filter(target == "cum hosp") -> temp; unique(temp$quantile)
# quant_data %>% filter(model=="MOBS_NEU-GLEAM_COVID") %>% dplyr::filter(target == "cum death") -> temp; unique(temp$quantile)
# 
# quant_data %>% filter(model=="JHU_UNC-flepiMoP") %>% dplyr::filter(target == "inc hosp") -> temp; unique(temp$quantile)
# quant_data %>% filter(model=="JHU_UNC-flepiMoP") %>% dplyr::filter(target == "inc death") -> temp; unique(temp$quantile)
# quant_data %>% filter(model=="JHU_UNC-flepiMoP") %>% dplyr::filter(target == "cum hosp") -> temp; unique(temp$quantile)
# quant_data %>% filter(model=="JHU_UNC-flepiMoP") %>% dplyr::filter(target == "cum death") -> temp; unique(temp$quantile)

quant_data["target_end_date"] <- quant_data$origin_date - 1 + (quant_data$horizon * 7)

# denote median as point estimate
quant_data_point <- quant_data %>%
  filter(quantile == 0.5) %>%
  mutate(type = "point")
quant_data <- quant_data %>% bind_rows(quant_data_point)

loc <- read.csv(file.path(dir_path, "data-locations/locations_2022.csv"), stringsAsFactors = T)
# quant_data <- left_join(quant_data, loc)
quant_data <- merge(quant_data, loc %>% dplyr::select(abbreviation, location, population, age_group), 
                    by = c("location", "age_group"), all.quant_data = TRUE)
quant_data <- quant_data %>% filter(str_detect(scenario_id, scenario.id))

# indices for each target type  
hosp_index <- grep("hosp", quant_data$target)

# create target_type column for faceting of plots
quant_data$target_type <- "Deaths"
quant_data$target_type[hosp_index] <- "Hospitalizations"

# Select desired ensemble and rename
ens <- filter(quant_data, str_detect(model,"Ensemble")) %>%
  filter(model != ens_choice)

bad <- unique(ens$model)

quant_data <- quant_data %>% filter(!(model %in% bad)) %>%
  mutate(model = ifelse(str_detect(model, "Ensemble"), "Ensemble", model))



# sort 
quant_data <- quant_data %>%
  arrange(target, scenario_id, location, age_group, model, target_end_date, horizon, quantile)




## READ IN GROUND TRUTH ########################################################################
## combined together as 'truth'

raw_truth <- read_data_files(data_repo_path = data_path,
                             API = FALSE, truth_data = TRUE)

# inc and cum ground truth
full_truth <- rbindlist(raw_truth, use.names = TRUE, fill = TRUE, idcol = "target")

if (loc_select != "all") {
  full_truth <- full_truth %>% dplyr::filter(fips == loc_select)
} 

# inc ground truth only
# inc ground truth only
truth <- full_truth %>%
  filter(target %in% c("fv_death_incidence_num", "covid_nhsn_hosp_inc")) %>%
  mutate(target_type = ifelse(target == "fv_death_incidence_num", "Deaths", "Hospitalizations")) %>%
  dplyr::rename("location_name" = "geo_value_fullname", "location" = "fips", "date" = "time_value")

# remove hospitalization data before October
truth <- as.data.frame(truth) %>%
  mutate(value = ifelse(date < as.Date("2020-10-01") & target_type == "Hospitalizations", NA, value))

# remove death and case data before march
truth <- truth %>%
  mutate(value = ifelse(date < as.Date("2020-03-01") & target_type == "Deaths", NA, value)) %>%
  mutate(value = ifelse(value < 0, NA, value))




## CREATE PROJECTION INTERVALS #################################################################

# indicator for whether to include CIs for ensemble
CI_ensemble <- TRUE

# create new matrix with column for point estimate and upper and lower CI bounds
quant_data_low <- quant_data %>% 
  dplyr::filter(quantile == pi_low) %>%
  dplyr::rename(lower = value) %>%
  dplyr::select(-type, -quantile)
quant_data_high <- quant_data %>% 
  dplyr::filter(quantile == pi_high) %>%
  dplyr::rename(upper = value) %>%
  dplyr::select(-type, -quantile)

quant_data_ci <- quant_data %>%
  dplyr::filter(type == "point") %>%
  dplyr::select(-type, -quantile) %>%
  dplyr::left_join(quant_data_low) %>%
  dplyr::left_join(quant_data_high)


if(!CI_ensemble) {
  quant_data_ci$upper[which(quant_data_ci$model == "Ensemble")] <- NA
  quant_data_ci$lower[which(quant_data_ci$model == "Ensemble")] <- NA
}

