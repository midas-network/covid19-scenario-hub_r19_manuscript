library(arrow)
library(tidyverse)
library(gt)
library(gridExtra)
library(grid)

##load the data
repo_data <- "../covid19-megaround_data/"
folder_path <- "output-processed/"
schema <- arrow::schema(
  arrow::field("origin_date", arrow::string()),
  arrow::field("scenario_id", arrow::string()),
  arrow::field("location", arrow::string()),
  arrow::field("target", arrow::string()),
  arrow::field("horizon", double()),
  arrow::field("type", arrow::string()),
  arrow::field("type_id", double()),
  arrow::field("model_name", arrow::string()),
  arrow::field("value", double()),
)

dc <- arrow::open_dataset(paste0(repo_data, folder_path),
                          partitioning = c("model_name", "origin_date", "target", "location"))

df_sample <- dplyr::filter(dc, output_type == "sample" & (target == "inc death" |  target == "inc hosp")) %>% 
  dplyr::collect()

##filter out longer time horizons
df_sample <- df_sample %>% filter(horizon<=52)

##regreate projection date
df_sample <- df_sample %>% mutate(origin_date = lubridate::as_date(origin_date))

##Create a season indicator
##First let's recreate the projection date
df_sample <- df_sample %>%
  mutate(proj_date = origin_date + horizon*7 )

##Let's chop up on date. First let's make it based on 4 equa
##week periods
#wk_cuts <- c(0,26,52,78,104)
date_cuts <- c("2024-04-15","2024-09-01","2025-05-01")
date_cuts<-lubridate::as_date(date_cuts)
df_sample <- df_sample %>% 
  mutate(period=cut(proj_date, date_cuts, 
                    labels = c("Warm season 2024", "Cold season 2024-25")),
         in_season = !stringr::str_starts(period,"Warm"))


##Now get a version of this with the season totals
season_res <- df_sample %>% group_by(scenario_id, target, model_name, period, location, output_type_id, age_group) %>%
  summarize(total=sum(value), mx=max(value)) %>%
  ungroup()

##now we get the stats to plot
season_res_sum <- season_res %>%
  group_by(scenario_id, target, model_name, period, location, age_group) %>%
  summarize(tot_median = median(total),
            tot_q975=quantile(total,0.975),
            tot_q75=quantile(total,0.75),
            tot_q25=quantile(total,0.25),
            tot_q025=quantile(total,0.025),
            mx_median = median(mx),
            mx_q975=quantile(mx,0.975),
            mx_q75=quantile(mx,0.75),
            mx_q25=quantile(mx,0.25),
            mx_q025=quantile(mx,0.025))%>%
  ungroup()

##add in overall
seas_res_overall <- season_res %>%
  group_by(scenario_id, target, period, location, age_group) %>%
  summarize(tot_median = median(total),
            tot_q975=quantile(total,0.975),
            tot_q75=quantile(total,0.75),
            tot_q25=quantile(total,0.25),
            tot_q025=quantile(total,0.025),
            mx_median = median(mx),
            mx_q975=quantile(mx,0.975),
            mx_q75=quantile(mx,0.75),
            mx_q25=quantile(mx,0.25),
            mx_q025=quantile(mx,0.025))%>%
  ungroup() %>%
  mutate(model_name="Ensemble")

season_res_sum <- bind_rows(season_res_sum, seas_res_overall)

unique(season_res_sum$model_name)

#### Summarize in a tabular format
agegroups <- c("0-130", "0-64", "65-130")
targets <- c("inc death", "inc hosp")
imm_esc <- c("low", "high")

table_max <- list()
for(k in imm_esc){
  for(g in targets){
    for(i in agegroups){
  age_group_label <- ifelse(i == "0-130", "total populatuon", ifelse(i == "0-64", "those aged 0-64", "those aged 65+"))
  target_label <- ifelse(g == "inc death", "deaths", "hospitalizations")
  sc_group <- ifelse(g == "low", list(c("A-2024-03-01", "C-2024-03-01", "E-2024-03-01")), list(c("B-2024-03-01", "D-2024-03-01", "F-2024-03-01")))
  
  season_res_sum %>% filter(location=="US", scenario_id %in% unlist(sc_group), age_group == i, target == g) %>%
    mutate(scenario_id=recode(scenario_id, 
                              "A-2024-03-01" = "No booster \n - Low immune escape",
                              "C-2024-03-01"="Booster for high-risk \n - Low immune escape", 
                              "E-2024-03-01"="Booster for all \n - Low immune escape",
                              "B-2024-03-01" = "No booster \n - High immune escape",
                              "D-2024-03-01"="Booster for high-risk \n - High immune escape",
                              "F-2024-03-01"="Booster for all \n - High immune escape")) %>%
    mutate(model_name=relevel(as.factor(model_name), ref="Ensemble")) %>% 
    mutate(IQR = paste0(format(round(mx_median,0), nsmall=0, big.mark=","),
                        " (",format(round(mx_q025,0), nsmall=0, big.mark=","),"-",
                        format(round(mx_q975,0), nsmall=0, big.mark=","),")")) %>%
    dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
    arrange(scenario_id, factor(model_name, levels = c("Ensemble", "CFA-Scenarios", "JHU_UNC-flepiMoP", "MOBS_NEU-GLEAM_COVID", "NotreDame-FRED",
                                                       "PSI-PROF", "UNCC-hierbin", "USC-SIkJalpha", "UTA-ImmunoSEIRS", "UVA-adaptive"))) %>%
    gt(groupname_col = "scenario_id") %>% 
    tab_header(title = md(paste0("**Max ", target_label, " (", age_group_label, ") - ", k, " immune escape**"))) %>%
    cols_label(model_name = md("**Model**"), 
               'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
    cols_align(columns = c('Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
    tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
    cols_width(model_name ~ px(180), contains("season")~px(180)) %>%
    tab_options(heading.title.font.size = 17, table.font.size = 14) -> table_max[[k]][[g]][[i]]
    }
  }
}

gtsave(table_max[[1]][[1]][[1]], "code/report/analysis/round18_period_analysis/dths_mx_tbl_lowI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_max[[1]][[1]][[2]], "code/report/analysis/round18_period_analysis/dths_mx_tbl_lowI_0_64.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_max[[1]][[1]][[3]], "code/report/analysis/round18_period_analysis/dths_mx_tbl_lowI_65+.png", vwidth = 1500, vheight = 1000, expand=10) 

gtsave(table_max[[1]][[2]][[1]], "code/report/analysis/round18_period_analysis/hosp_mx_tbl_lowI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_max[[1]][[2]][[2]], "code/report/analysis/round18_period_analysis/hosp_mx_tbl_lowI_0_64.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_max[[1]][[2]][[3]], "code/report/analysis/round18_period_analysis/hosp_mx_tbl_lowI_65+.png", vwidth = 1500, vheight = 1000, expand=10) 

gtsave(table_max[[2]][[1]][[1]], "code/report/analysis/round18_period_analysis/dths_mx_tbl_highI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_max[[2]][[1]][[2]], "code/report/analysis/round18_period_analysis/dths_mx_tbl_highI_0_64.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_max[[2]][[1]][[3]], "code/report/analysis/round18_period_analysis/dths_mx_tbl_highI_65+.png", vwidth = 1500, vheight = 1000, expand=10) 

gtsave(table_max[[2]][[2]][[1]], "code/report/analysis/round18_period_analysis/hosp_mx_tbl_highI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_max[[2]][[2]][[2]], "code/report/analysis/round18_period_analysis/hosp_mx_tbl_highI_0_64.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_max[[2]][[2]][[3]], "code/report/analysis/round18_period_analysis/hosp_mx_tbl_highI_65+.png", vwidth = 1500, vheight = 1000, expand=10) 


table_tot <- list()
for(k in imm_esc){
  for(g in targets){
    for(i in agegroups){
      age_group_label <- ifelse(i == "0-130", "total populatuon", ifelse(i == "0-64", "those aged 0-64", "those aged 65+"))
      target_label <- ifelse(g == "inc death", "deaths", "hospitalizations")
      sc_group <- ifelse(g == "low", list(c("A-2024-03-01", "C-2024-03-01", "E-2024-03-01")), list(c("B-2024-03-01", "D-2024-03-01", "F-2024-03-01")))
      
      season_res_sum %>% filter(location=="US", scenario_id %in% unlist(sc_group), age_group == i, target == g) %>%
        mutate(scenario_id=recode(scenario_id, 
                                  "A-2024-03-01" = "No booster \n - Low immune escape",
                                  "C-2024-03-01"="Booster for high-risk \n - Low immune escape", 
                                  "E-2024-03-01"="Booster for all \n - Low immune escape",
                                  "B-2024-03-01" = "No booster \n - High immune escape",
                                  "D-2024-03-01"="Booster for high-risk \n - High immune escape",
                                  "F-2024-03-01"="Booster for all \n - High immune escape")) %>%
        mutate(model_name=relevel(as.factor(model_name), ref="Ensemble")) %>% 
        mutate(IQR = paste0(format(round(tot_median,0), nsmall=0, big.mark=","),
                            " (",format(round(tot_q025,0), nsmall=0, big.mark=","),"-",
                            format(round(tot_q975,0), nsmall=0, big.mark=","),")")) %>%
        dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
        arrange(scenario_id, factor(model_name, levels = c("Ensemble", "CFA-Scenarios", "JHU_UNC-flepiMoP", "MOBS_NEU-GLEAM_COVID", "NotreDame-FRED",
                                                           "PSI-PROF", "UNCC-hierbin", "USC-SIkJalpha", "UTA-ImmunoSEIRS", "UVA-adaptive"))) %>%
        gt(groupname_col = "scenario_id") %>% 
        tab_header(title = md(paste0("**Total ", target_label, " (", age_group_label, ") - ", k, " immune escape**"))) %>%
        cols_label(model_name = md("**Model**"), 
                   'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
        cols_align(columns = c('Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
        tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
        cols_width(model_name ~ px(180), contains("season")~px(180)) %>%
        tab_options(heading.title.font.size = 17, table.font.size = 12) -> table_tot[[k]][[g]][[i]]
    }
  }
}

gtsave(table_tot[[1]][[1]][[1]], "code/report/analysis/round18_period_analysis/dths_tot_tbl_lowI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_tot[[1]][[1]][[2]], "code/report/analysis/round18_period_analysis/dths_tot_tbl_lowI_0_64.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_tot[[1]][[1]][[3]], "code/report/analysis/round18_period_analysis/dths_tot_tbl_lowI_65+.png", vwidth = 1500, vheight = 1000, expand=10) 

gtsave(table_tot[[1]][[2]][[1]], "code/report/analysis/round18_period_analysis/hosp_tot_tbl_lowI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_tot[[1]][[2]][[2]], "code/report/analysis/round18_period_analysis/hosp_tot_tbl_lowI_0_64.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_tot[[1]][[2]][[3]], "code/report/analysis/round18_period_analysis/hosp_tot_tbl_lowI_65+.png", vwidth = 1500, vheight = 1000, expand=10) 

gtsave(table_tot[[2]][[1]][[1]], "code/report/analysis/round18_period_analysis/dths_tot_tbl_highI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_tot[[2]][[1]][[2]], "code/report/analysis/round18_period_analysis/dths_tot_tbl_highI_0_64.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_tot[[2]][[1]][[3]], "code/report/analysis/round18_period_analysis/dths_tot_tbl_highI_65+.png", vwidth = 1500, vheight = 1000, expand=10) 

gtsave(table_tot[[2]][[2]][[1]], "code/report/analysis/round18_period_analysis/hosp_tot_tbl_highI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_tot[[2]][[2]][[2]], "code/report/analysis/round18_period_analysis/hosp_tot_tbl_highI_0_64.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(table_tot[[2]][[2]][[3]], "code/report/analysis/round18_period_analysis/hosp_tot_tbl_highI_65+.png", vwidth = 1500, vheight = 1000, expand=10) 


#### Figures by state (using only ensemble result)
read.csv("data-locations/locations_2022.csv") %>% dplyr::select(abbreviation, location, population, age_group) %>%
  filter(location!="US") %>% mutate(location=as.numeric(location)) -> df_loc
merge(season_res_sum %>% filter(location!="US") %>% mutate(location=as.numeric(location)), 
      df_loc, by=c("location", "age_group"), all.x=TRUE) %>% 
  filter(!(location %in% c(60, 66, 69, 72, 74, 78))) -> season_res_sum_loc

#write csv for easy incorporation into report
write.csv(season_res_sum_loc, file = paste0(getwd(),"/code/report/analysis/round18_period_analysis/season_res_sum_loc.csv"))
season_res_sum_loc %>% filter(is.na(abbreviation))
season_res_sum %>% filter(location == 72)
df_loc %>% filter(location == 72)
