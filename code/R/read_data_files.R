#' Read different file format
#' 
#' Read files in csv, zip or gz (use vroom if package installed)
#' 
#' @param path string path to the file to read
#' 
#' Function copied from the covid-scenario-hub_data repository (code/utils.R)
#' 
read_files <- function(path) {
  if (grepl(".pqt$", basename(path))) {
    require(arrow)
    df <- arrow::read_parquet(path, as_data_frame = TRUE)
  } else if (grepl(".rda$", basename(path))) {
    load(path, envir = globalenv())
    df <- NULL
  } else if (grepl(".rds$", basename(path))) {
    df <- readRDS(path)
  } else {
    if (any(rownames(installed.packages()) == "data.table")) {
      if (grepl(".csv$", basename(path))) {
        df <- data.table::fread(path, sep = ",", na.strings = c("", "NA", 
                                                                "NaN"))
      }
      if (grepl(".zip$", basename(path))) {
        file_name <- unzip(path,list = TRUE)[,"Name", TRUE]
        unzip(path)
        df <- data.table::fread(file_name[1], sep = ",", 
                                na.strings = c("", "NA", "NaN"))
        file.remove(file_name)
      }
      if (grepl(".gz$", basename(path))) {
        file_name <- gzfile(path)
        df <- data.table::fread(file_name, sep = ",", 
                                na.strings = c("", "NA", "NaN"))
      }
    } else {
      if (grepl(".csv$", basename(path))) {
        df <- read.csv(path, sep = ",", na.strings = c("", "NA", "NaN"))
      }
      if (grepl(".zip$", basename(path))) {
        file_name <- unzip(path,list = TRUE)[,"Name", TRUE]
        unzip(path)
        df <- read.csv(file_name[1], sep = ",", na.strings = c("", "NA", "NaN"))
        file.remove(file_name)
      }
      if (grepl(".gz$", basename(path))) {
        file_name <- gzfile(path)
        df <- read.csv(file_name, sep = ",", na.strings = c("", "NA", "NaN"))
      }
    }
  }
  return(df)
}


#' Extract files information (files, sha and link) via GITHUB API
#' 
#' @param data_repo_path string, name of the repo (i.e. 
#'   `"midas-network/covid19-scenario-hub_data/")
#' @param .token additional parameter used in the function `gh::gh()`: 
#'  Authentication token. Defaults to GITHUB_PAT or GITHUB_TOKEN environment 
#'  variables, in this order if any is set. See gh_token() if you need more 
#'  flexibility, e.g. different tokens for different GitHub Enterprise 
#'  deployments. 
#'  
#'  @importFrom gh gh
#'  @importFrom dplyr tibble
#'  @importFrom purrr map
extract_tree_api <- function(data_repo_path, .token) {
  tree <- gh::gh(paste0("GET /repos/", data_repo_path, 
                        "git/trees/master?recursive=1"), .token = .token)
  files_api <- dplyr::tibble(files =  unlist(purrr::map(tree$tree, "path")), 
                             sha =  unlist(purrr::map(tree$tree, "sha")),
                             url = unlist(purrr::map(tree$tree, "url")))
  return(files_api)
}

#' Download file from GITHUB repository using GITHUB API
#' 
#' Function use to donwload CSV file from GITHUB repository unsing GITHUB API.
#' 
#' @param file_url string, url of the file containing the file_sha
#'  (for more information, https://docs.github.com/en/rest/reference/git#get-a-blob)
#' @param .token additional parameter used in the function `gh::gh()`: 
#'  Authentication token. Defaults to GITHUB_PAT or GITHUB_TOKEN environment 
#'  variables, in this order if any is set. See gh_token() if you need more 
#'  flexibility, e.g. different tokens for different GitHub Enterprise 
#'  deployments. 
#'  
#'  @importFrom gh gh
#'  @importFom base64enc base64decode
#'  @importFrom tidyr separate
#'  @importFrom dplyr mutate_all
extract_file_api <- function(file_url, .token) {
  file <- gh::gh(paste0("GET ", file_url), .token = .token)
  df <- base64enc::base64decode(file[["content"]])
  df <- readBin(df, character())
  df <- data.frame(strsplit(df, "\\\r\\\n|\\n"))
  df <- tidyr::separate(df[-1, , FALSE], col = 1, 
                        into = gsub('\\"', "", unlist(strsplit(df[1, ], ","))),
                        sep = ",")
  df <- dplyr::mutate_all(df, function(x) gsub('\\"', "", x))
  return(df)
}



#' Read files from the Github repository
#' 
#' Function to load in R environment as a list of one or multiple data frame,
#' one or multiple files from a Github repository (private repository); 2 
#' strategies are available to download one or multiple files.
#' 
#' @param data_repo_path string, either name of the repo (i.e. 
#'   `"midas-network/covid19-scenario-hub_data/"`, API strategy) or the complete
#'   path to your local clone of the data repository
#' @param API boolean, is set to `TRUE` will use the API strategy, if `FALSE` 
#'  will use the clone strategy. Default, `FALSE`
#' @param unique_file string, name or relative path of the file 
#' @param truth_data boolean, if `TRUE` donwload the 5 truth data 
#' timeseries available: (incident and cumulative cases and deaths, and 
#' hospitalization)
#' @param age_data boolean, if `TRUE`download the data from the standardized 
#' zip file containing all the submission containing age group information
#' @param model_metadata boolean, , if `TRUE`download the data from the standardized 
#' zip file containing all the submission containing age group information
#' @param round_number numeric,one specific round number to download specific 
#' round zip files and zeroed files. If `NULL` select all rounds.  
#' @param raw_file boolean, if TRUE, download the raw_model ZIP/PQT files for 
#'  either all round or one specific round (cf. `round_number` parameter)
#' @param zeroed_file boolean, if TRUE, download the zeroed files for each target
#'  for either all round or one specific round (cf. `round_number` parameter)
#' @param dictionary boolean, if TRUE, downalod the sysdata.rda file containing
#'  multiple dictionary that can be use to translate some columns (directly 
#'  load into the environment)
#' @param .token additional parameter used in the function `gh::gh()`: 
#'  Authentication token. Defaults to GITHUB_PAT or GITHUB_TOKEN environment 
#'  variables, in this order if any is set. See gh_token() if you need more 
#'  flexibility, e.g. different tokens for different GitHub Enterprise 
#'  deployments. 
#' 
#' @details As the data repository is private, 2 strategies are available to 
#' read one or multiple files from a repository into your R environment:
#' (1) By using the GitHub API. To download the files you will need to create
#'  a Personal Access Token (please find more information here: 
#'  https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
#'  To call the API, the function uses the package `gh` 
#'  (https://gh.r-lib.org/index.html). 
#'  You can find more information on PAT usage there. 
#' (2) By using a local clone of the DATA repository. To be sure to download 
#'  the last version of the file, don't forget to pull/fetch the last version
#'  of the repo.  (`git pull`)
#' 
#' The downloaded files will not be saved 
#' 
#' 
#' @return list of named data frame. The name is the name of the file without
#' extension
#' 
#'@examples
#'
#'\dontrun{
#'### For the first strategy, the package can either have access to your PAT 
#'### (see gh package help) or the PAT can be imputed with the `.token` 
#'### parameter
#'
#' # Download truth data with PAT as additional parameter
#' read_data_files("<ORGNAME>/<REPONAME>", .token = "<PAT>")
#' 
#' # Download additional file (truth data + age data)
#' read_data_files("<ORGNAME>/<REPONAME>", age_data = TRUE, .token = "<PAT>")
#'  
#' ### Download truth data from local clone
#' read_data_files("<LOCAL>/<PATH>/<TO>/<CLONE>", API = FALSE)
#' 
#' # Download only a specific file
#' read_data_files("<LOCAL>/<PATH>/<TO>/<CLONE>", API = FALSE, 
#' TRUTH_DATA = FALSE, unique_file = "<NAMEOFTHEFILE>)  
#' 
#' }
#' 
#' @importFrom gh gh
read_data_files <- function(data_repo_path, API = TRUE, unique_file = NULL,
                            truth_data = TRUE, age_data = FALSE, 
                            model_metadata = FALSE, round_number = NULL,
                            raw_file = FALSE, zeroed_file = FALSE, 
                            dictionary = FALSE, .token = NULL) {
  # Extract tree
  if (isTRUE(API)) {
    files_api <- extract_tree_api(data_repo_path, .token = .token)
    files <- files_api$files
  } else {
    files <- dir(data_repo_path, recursive = TRUE, full.names = TRUE)
  }
  
  # Selection of files
  if (!is.null(round_number) & !is.numeric(round_number))
    stop("round_number should be numeric or NULL")
  
  if (!is.null(unique_file)) {
    unique_file <- grep(unique_file, files) 
  } else {
    unique_file <- NULL
  }
  
  if (isTRUE(truth_data)) {
    truth <- grep("data-goldstandard/.{,25}\\.csv", files) 
  } else {
    truth <- NULL  
  }
  
  if (isTRUE(age_data)) {
    age_file <- grep("data-processed_output/.*age-data.zip", files)
  } else {
    age_file <- NULL  
  }
  
  if (isTRUE(model_metadata)) {
    model_file <- grep("model_description.csv", files)
  } else {
    model_file <- NULL  
  }
  
  if (is.null(round_number)){ 
    search <- "round.{1,2}/" 
  } else {
    search <- paste0("round", round_number, "/")
  }
  
  if (isTRUE(raw_file)) {
    raw_file <- grep(paste0(search,"(raw_model.zip|raw_model.pqt|raw_model.rds)"), files)
  } else {
    raw_file <- NULL  
  }
  
  if (isTRUE(zeroed_file)) {
    zero_file <- grep(paste0(search,"zeroed"), files)
  } else {
    zero_file <- NULL  
  }
  
  if (isTRUE(dictionary)) {
    dict <- grep("sysdata", files)
  } else {
    dict <- NULL
  }
  
  files_num <- c(unique_file, truth, age_file, model_file, raw_file, zero_file, 
                 dict)
  
  # read files
  if (isTRUE(API)) {
    files <- files_api[files_num, ]
    lst_df <- lapply(seq_len(dim(files)[1]), function(x) {
      if (grepl(".csv$", files$files[x])) {
        df <- extract_file_api(file_url = files$url[x], .token = .token)
      } else if(grepl(".zip$", files$files[x])) {
        file <- gh::gh(paste0("GET ", files$url[x]), .token = .token, 
                       .accept = "application/vnd.github.v3.raw", 
                       .destfile = tempfile("output", fileext = ".zip"))
        df <- data.frame(suppressMessages(read_files(file[1])))
      } else if(grepl(".pqt$", files$files[x])) {
        file <- gh::gh(paste0("GET ", files$url[x]), .token = .token, 
                       .accept = "application/vnd.github.v3.raw", 
                       .destfile = tempfile("output", fileext = ".pqt"))
        df <- data.frame(suppressMessages(read_files(file[1])))
        } else if(grepl(".rda$", files$files[x])) {
          file <- gh::gh(paste0("GET ", files$url[x]), .token = .token, 
                         .accept = "application/vnd.github.v3.raw", 
                         .destfile = tempfile("output", fileext = ".rda"))
          load(file[1], envir = globalenv())
          df <- NULL
        } else if(grepl(".rds$", files$files[x])) {
          file <- gh::gh(paste0("GET ", files$url[x]), .token = .token, 
                         .accept = "application/vnd.github.v3.raw", 
                         .destfile = tempfile("output", fileext = ".rds"))
          df <- readRDS(file)
        } else {
        warning(
          "File extention not recognized, should be `csv`, `pqt`, `rda` or ",
          "`zip`, returns `NULL` for file: ", files$files[x])
        df <- NULL
      }
    })
    lst_df <- setNames(lst_df, gsub("\\..*$", "", basename(files$files)))
  } else {
    files <- files[files_num]
    lst_df <- lapply(files, function(x) suppressMessages(read_files(x))) 
    lst_df <- setNames(lst_df, gsub("\\..*$", "", basename(files)))
  }
  # return named list of data frame
  return(lst_df)
}


# read_data_files <- function(data_repo_path, 
#                             smh_path, 
#                             API = TRUE, unique_file = NULL,
#                             truth_data = TRUE, age_data = FALSE, 
#                             model_metadata = FALSE, round_number = NULL,
#                             raw_file = FALSE, zeroed_file = FALSE, 
#                             dictionary = FALSE, .token = NULL) {
#   # Extract tree
#   if (isTRUE(API)) {
#     files_api <- extract_tree_api(data_repo_path, .token = .token)
#     files <- files_api$files
#   } else {
#     files <- dir(data_repo_path, recursive = TRUE, full.names = TRUE)
#   }
#   
#   # Selection of files
#   if (!is.null(round_number) & !is.numeric(round_number))
#     stop("round_number should be numeric or NULL")
#   
#   if (!is.null(unique_file)) {
#     unique_file <- grep(unique_file, files) 
#   } else {
#     unique_file <- NULL
#   }
#   
#   if (isTRUE(truth_data)) {
#     truth <- file.path(smh_path, "target-data/time-series.csv")
#   } else {
#     truth <- NULL  
#   }
#   
#   if (isTRUE(age_data)) {
#     age_file <- grep("data-processed_output/.*age-data.zip", files)
#   } else {
#     age_file <- NULL  
#   }
#   
#   if (isTRUE(model_metadata)) {
#     model_file <- grep("model_description.csv", files)
#   } else {
#     model_file <- NULL  
#   }
#   
#   if (is.null(round_number)){ 
#     search <- "round.{1,2}/" 
#   } else {
#     search <- paste0("round", round_number, "/")
#   }
#   
#   if (isTRUE(raw_file)) {
#     raw_file <- grep(paste0(search,"(raw_model.zip|raw_model.pqt|raw_model.rds)"), files)
#   } else {
#     raw_file <- NULL  
#   }
#   
#   if (isTRUE(zeroed_file)) {
#     zero_file <- grep(paste0(search,"zeroed"), files)
#   } else {
#     zero_file <- NULL  
#   }
#   
#   if (isTRUE(dictionary)) {
#     dict <- grep("sysdata", files)
#   } else {
#     dict <- NULL
#   }
#   
#   files_num <- c(unique_file, age_file, model_file, raw_file, zero_file, 
#                  dict)
#   
#   # read files
#   if (isTRUE(API)) {
#     files <- files_api[files_num, ]
#     lst_df <- lapply(seq_len(dim(files)[1]), function(x) {
#       if (grepl(".csv$", files$files[x])) {
#         df <- extract_file_api(file_url = files$url[x], .token = .token)
#       } else if(grepl(".zip$", files$files[x])) {
#         file <- gh::gh(paste0("GET ", files$url[x]), .token = .token, 
#                        .accept = "application/vnd.github.v3.raw", 
#                        .destfile = tempfile("output", fileext = ".zip"))
#         df <- data.frame(suppressMessages(read_files(file[1])))
#       } else if(grepl(".pqt$", files$files[x])) {
#         file <- gh::gh(paste0("GET ", files$url[x]), .token = .token, 
#                        .accept = "application/vnd.github.v3.raw", 
#                        .destfile = tempfile("output", fileext = ".pqt"))
#         df <- data.frame(suppressMessages(read_files(file[1])))
#         } else if(grepl(".rda$", files$files[x])) {
#           file <- gh::gh(paste0("GET ", files$url[x]), .token = .token, 
#                          .accept = "application/vnd.github.v3.raw", 
#                          .destfile = tempfile("output", fileext = ".rda"))
#           load(file[1], envir = globalenv())
#           df <- NULL
#         } else if(grepl(".rds$", files$files[x])) {
#           file <- gh::gh(paste0("GET ", files$url[x]), .token = .token, 
#                          .accept = "application/vnd.github.v3.raw", 
#                          .destfile = tempfile("output", fileext = ".rds"))
#           df <- readRDS(file)
#         } else {
#         warning(
#           "File extention not recognized, should be `csv`, `pqt`, `rda` or ",
#           "`zip`, returns `NULL` for file: ", files$files[x])
#         df <- NULL
#       }
#     })
#     lst_df <- setNames(lst_df, gsub("\\..*$", "", basename(files$files)))
#   } else {
#     files <- files[files_num]
#     if (isTRUE(truth_data)){
#       files <- c("truth_data" = truth, files)
#     }
#     lst_df <- lapply(files, function(x) suppressMessages(read_files(x))) 
#     lst_df <- setNames(lst_df, gsub("\\..*$", "", basename(files)))
#   }
#   # return named list of data frame
#   return(lst_df)
# }




#' Transform files format
#' 
#' To reduce size of the files, we adopted a format with a reduce number of 
#' column and with value as numeric instead of redundant long character string. 
#' This function transform the data frame to the standard format uses in the 
#' Scenario Modeling Hub
#' 
#' @param df data frame you wanted to transform.
#' @param target_name string, the name of the target (will be use to create the
#'  "target" column )
#' @param zeroed boolean, will add "zeroed" in the target column. Default, TRUE
#' 
#' @details 
#' WARNING: the function has to have multiple "dictionaries" load in the 
#' environment to work:
#' \itemize{
#'  \item{ number2scenario: named vector containing the scenario_id and the 
#'  number associated with each id as a name}
#'  \item{ number2model: named vector containing the models names and the number
#'   associated with each model as a name}
#'  \item{scenariosmname: named vector containing the short name of the scenario
#'   (for example: "pessWan_lowBoo") and the scenario id associated with each 
#'   short name as a name.}
#' }
#' 
#''@examples
#'
#'\dontrun{
#' # Read zeroed file + dictionary.
#' df0 <- read_data_files("PATH/TO/REPOSITORY/", API = F, truth_data = F,
#'                         zeroed_file = T, dictionary = T, round_number = 10)
#' # Transform the "cum case" zeroed file:
#' df_case <- transform_df(df0[[1]], 
#'                         gsub("_", " ", gsub("zeroed_", "", names(df0[1]))))
#' } 
transform_df <- function(df, target_name, zeroed = T) {
  df[, ':=' (scenario_id = number2scenario[scenario_id], 
             target = paste0(target_wk, " wk ahead ", target_name),
             model_name = number2model[model_name],
             type = ifelse(is.na(quantile), "point", "quantile"))]
  df[, scenario_name := scenario_smname[scenario_id]]
  if (isTRUE(zeroed)) {
    df[, target := paste0("zeroed ", target)]
  }
  df <- df[, !c("target_wk", "target_end_date")]
  return(df)
}


