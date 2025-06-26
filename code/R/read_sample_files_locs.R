library(arrow)
library(tidyverse)


collect_in_parallel <- FALSE


##load the data
# data_path <- "../covid-19-smh_data/"
folder_path <- "model-processed/"

schema <- arrow::schema(
  arrow::field("origin_date", arrow::string()),
  arrow::field("scenario_id", arrow::string()),
  arrow::field("location", arrow::string()),
  arrow::field("target", arrow::string()),
  arrow::field("horizon", double()),
  arrow::field("output_type", arrow::string()),
  arrow::field("output_type_id", double()),
  arrow::field("age_group", arrow::string()),
  arrow::field("model_name", arrow::string()),
  arrow::field("value", double()),
)

dc <- arrow::open_dataset(file.path(data_path, folder_path), schema = schema, 
                          partitioning = c("origin_date", "model_name", "target", "location"))

# if (!collect_in_parallel){
  
  df_sample <- dc %>% 
    dplyr::filter(output_type == "sample",
                  target %in% c("inc death", "inc hosp"),
                  horizon <= 52,
                  location == loc_select) %>% 
    dplyr::collect()
  gc()
  
# } else {
#   
#   
#   library(parallel)
#   library(foreach)
#   library(doParallel)
#   library(dplyr)
#   
#   
#   # Get unique locations for parallelization
#   locations <- dc %>% 
#     dplyr::select(location) %>% 
#     dplyr::distinct() %>% 
#     dplyr::collect() %>% 
#     dplyr::pull(location) %>% 
#     sort()
#   
#     # Set up parallel backend
#   n_cores <- detectCores() - 1  # Leave one core free
#   cl <- makeCluster(n_cores)
#   clusterExport(cl, c("dc", "schema", "locations"))
#   registerDoParallel(cl)
# 
#   # Parallel processing by location
#   df_sample <- foreach(loc = locations, .combine = rbind, .packages = c("dplyr", "arrow")) %dopar% {
#     dc %>%
#       dplyr::filter(location == loc,
#                     output_type == "sample",
#                     target %in% c("inc death", "inc hosp"),
#                     horizon <= 52) %>%
#       dplyr::collect()
#   }
#   
#   df_sample <- foreach(loc = locations, .combine = rbind, 
#                        .packages = c("dplyr", "arrow")) %dopar% {
#                          # Create new connection in each worker (adjust based on your connection type)
#                          local_dc <- arrow::open_dataset(file.path(data_path, folder_path), schema = schema, 
#                                                          partitioning = c("origin_date", "model_name", "target", "location"))
#                          
#                          res <- local_dc %>% 
#                            dplyr::filter(location == loc,
#                                          output_type == "sample",
#                                          target %in% c("inc death", "inc hosp"),
#                                          horizon <= 52) %>% 
#                            dplyr::collect()
#                          
#                          # Clean up connection
#                          close_dataset(local_dc)  # or DBI::dbDisconnect() etc.
#                          res
#                          
#                        }
#   
#   # Clean up
#   stopCluster(cl)
#   gc()
# }



##recreate projection date
df_sample <- df_sample %>% mutate(origin_date = lubridate::as_date(origin_date))

gc()

library(data.table)

# Convert df_sample to data.table if it isn't already
setDT(df_sample)

gc()
