library(arrow)
library(tidyverse)
library(gt)

theme_set(theme_bw())

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

##load the data
repo_data <- "../covid19-megaround_data/"
folder_path <- "megaround-processed/"

dc <- arrow::open_dataset(paste0(repo_data, folder_path), partitioning = "model_name", 
                          format = "parquet", schema = schema,
                          factory_options = list(
                            exclude_invalid_files = TRUE))

df_sample <- dplyr::filter(dc, type == "sample" & (target == "inc death" |  target == "inc hosp")) %>% 
  dplyr::collect()


##filter out longer time horizons
df_sample <- df_sample %>% filter(horizon<=104)

##regreate projection date
df_sample <- df_sample %>% mutate(origin_date = lubridate::as_date(origin_date))

##Create a season indicator
##First let's recreate the projection date
df_sample <- df_sample %>%
  mutate(proj_date = origin_date + horizon*7 )

##Let's chop up on date. First let's make it based on 4 equa
##week periods
#wk_cuts <- c(0,26,52,78,104)
date_cuts <- c("2023-04-15","2023-09-01","2024-04-15","2024-09-01","2025-05-01")
date_cuts<-lubridate::as_date(date_cuts)
df_sample <- df_sample %>% 
  mutate(period=cut(proj_date, date_cuts, 
                    labels = c("Warm season 2023",
                               "Cold season 2023-24",
                               "Warm season 2024",
                               "Cold season 2024-25")),
         in_season = !stringr::str_starts(period,"Warm"))


##Now get a version of this with the season totals
season_res <- df_sample %>% group_by(scenario_id, target,
                                     model_name,
                                     period, location,
                                     type_id) %>%
  summarize(total=sum(value), mx=max(value)) %>%
  ungroup()


##now we get the stats to plot
season_res_sum <- season_res %>%
  group_by(scenario_id, target, model_name, period, location) %>%
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
  group_by(scenario_id, target, period, location) %>%
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
season_res_sum$model_name <- factor(season_res_sum$model_name,
                                    levels=c("UVA-adaptive", "UVA-EpiHiper", "UTA-ImmunoSEIRS", "USC-SIkJalpha", 
                                             "UNCC-hierbin", "NotreDame-FRED", "MOBS_NEU-GLEAM_COVID", "JHU_IDD-CovidSP", "Ensemble"))

## Summary table for max deaths for the US
season_dths_mx_tbl_lowI <- season_res_sum %>%
  filter(target=="inc death", location=="US", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "No booster \n Low immune escape","C-2023-04-16"="Booster for 65+ \n Low immune escape",  "E-2023-04-16"="Booster for all \n Low immune escape"))%>%
  mutate(model_name=relevel(as.factor(model_name), ref="Ensemble")) %>% 
  
  mutate(IQR = paste0(format(round(mx_median,0), nsmall=0, big.mark=","),
                      " (",format(round(mx_q025,0), nsmall=0, big.mark=","),"-",
                      format(round(mx_q975,0), nsmall=0, big.mark=","),")")) %>%
  dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
  arrange(scenario_id, factor(model_name, levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                                   "NotreDame-FRED", "MOBS_NEU-GLEAM_COVID", "JHU_IDD-CovidSP", "Ensemble"))) %>%
  gt(groupname_col = "scenario_id") %>% 
  tab_header(title = md("**Max deaths - low immune escape**")) %>%
  cols_label(model_name = md("**Model**"), 
             'Warm season 2023' = md("**Warm season 2023**"), 'Cold season 2023-24' = md("**Cold season 2023-24**"), 
             'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
  cols_align(columns = c('Warm season 2023', 'Cold season 2023-24', 'Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
  tab_footnote(footnote = md("Each value represnts the median with the 95% projection interval")) %>%
  cols_width(model_name ~ px(180), contains("season")~px(180))


season_dths_mx_tbl_highI <- season_res_sum%>%
  filter(target=="inc death", location=="US", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "No booster \n High immune escape","D-2023-04-16"="Booster for 65+ \n High immune escape",  "F-2023-04-16"="Booster for all \n High immune escape"))%>%
  mutate(model_name=relevel(as.factor(model_name), ref="Ensemble"))%>%
  
  mutate(IQR = paste0(format(round(mx_median,0), nsmall=0, big.mark=","),
                      " (",format(round(mx_q025,0), nsmall=0, big.mark=","),"-",
                      format(round(mx_q975,0), nsmall=0, big.mark=","),")")) %>%
  dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
  arrange(scenario_id, factor(model_name, levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                                   "NotreDame-FRED", "MOBS_NEU-GLEAM_COVID", "JHU_IDD-CovidSP", "Ensemble"))) %>%
  gt(groupname_col = "scenario_id") %>% 
  tab_header(title = md("**Max deaths - high immune escape**")) %>%
  cols_label(model_name = md("**Model**"), 
             'Warm season 2023' = md("**Warm season 2023**"), 'Cold season 2023-24' = md("**Cold season 2023-24**"), 
             'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
  cols_align(columns = c('Warm season 2023', 'Cold season 2023-24', 'Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
  tab_footnote(footnote = md("Each value represnts the median with the 95% projection interval")) %>%
  cols_width(model_name ~ px(180), contains("season")~px(180))

gtsave(season_dths_mx_tbl_lowI, "code/report/analysis/round17_period_analysis/dths_mx_tbl_lowI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(season_dths_mx_tbl_highI, "code/report/analysis/round17_period_analysis/dths_mx_tbl_highI.png", vwidth = 1500, vheight = 1000, expand=10) 


## Summary table for total deahts for the US
season_dths_tot_tbl_lowI <- season_res_sum%>%
  filter(target=="inc death", location=="US", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "No booster \n Low immune escape","C-2023-04-16"="Booster for 65+ \n Low immune escape",  "E-2023-04-16"="Booster for all \n Low immune escape"))%>%
  mutate(model_name=relevel(as.factor(model_name), ref="Ensemble"))%>%
  
  mutate(IQR = paste0(format(round(tot_median,0), nsmall=0, big.mark=","),
                      " (",format(round(tot_q025,0), nsmall=0, big.mark=","),"-",
                      format(round(tot_q975,0), nsmall=0, big.mark=","),")")) %>%
  dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
  arrange(scenario_id, factor(model_name, levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                                   "NotreDame-FRED", "MOBS_NEU-GLEAM_COVID", "JHU_IDD-CovidSP", "Ensemble"))) %>%
  gt(groupname_col = "scenario_id") %>% 
  tab_header(title = md("**Total deaths - low immune escape**")) %>%
  cols_label(model_name = md("**Model**"), 
             'Warm season 2023' = md("**Warm season 2023**"), 'Cold season 2023-24' = md("**Cold season 2023-24**"), 
             'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
  cols_align(columns = c('Warm season 2023', 'Cold season 2023-24', 'Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
  tab_footnote(footnote = md("Each value represnts the median with the 95% projection interval")) %>%
  cols_width(model_name ~ px(180), contains("season")~px(230))


season_dths_tot_tbl_highI <- season_res_sum%>%
  filter(target=="inc death", location=="US", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "No booster \n High immune escape","D-2023-04-16"="Booster for 65+ \n High immune escape",  "F-2023-04-16"="Booster for all \n High immune escape"))%>%
  mutate(model_name=relevel(as.factor(model_name), ref="Ensemble"))%>%
  
  mutate(IQR = paste0(format(round(tot_median,0), nsmall=0, big.mark=","),
                      " (",format(round(tot_q025,0), nsmall=0, big.mark=","),"-",
                      format(round(tot_q975,0), nsmall=0, big.mark=","),")")) %>%
  dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
  arrange(scenario_id, factor(model_name, levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                                   "NotreDame-FRED", "MOBS_NEU-GLEAM_COVID", "JHU_IDD-CovidSP", "Ensemble"))) %>%
  gt(groupname_col = "scenario_id") %>% 
  tab_header(title = md("**Total deaths - high immune escape**")) %>%
  cols_label(model_name = md("**Model**"), 
             'Warm season 2023' = md("**Warm season 2023**"), 'Cold season 2023-24' = md("**Cold season 2023-24**"), 
             'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
  cols_align(columns = c('Warm season 2023', 'Cold season 2023-24', 'Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
  tab_footnote(footnote = md("Each value represnts the median with the 95% projection interval")) %>%
  cols_width(model_name ~ px(180), contains("season")~px(230))

gtsave(season_dths_tot_tbl_lowI, "code/report/analysis/round17_period_analysis/dths_tot_tbl_lowI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(season_dths_tot_tbl_highI, "code/report/analysis/round17_period_analysis/dths_tot_tbl_highI.png", vwidth = 1500, vheight = 1000, expand=10) 


## Summary table for max hospitalizations for the US
season_hosp_mx_tbl_lowI <- season_res_sum%>%
  filter(target=="inc hosp", location=="US", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "No booster \n Low immune escape","C-2023-04-16"="Booster for 65+ \n Low immune escape",  "E-2023-04-16"="Booster for all \n Low immune escape"))%>%
  mutate(model_name=relevel(as.factor(model_name), ref="Ensemble"))%>%
  
  mutate(IQR = paste0(format(round(mx_median,0), nsmall=0, big.mark=","),
                      " (",format(round(mx_q025,0), nsmall=0, big.mark=","),"-",
                      format(round(mx_q975,0), nsmall=0, big.mark=","),")")) %>%
  dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
  arrange(scenario_id, factor(model_name, levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                                   "NotreDame-FRED", "MOBS_NEU-GLEAM_COVID", "JHU_IDD-CovidSP", "Ensemble"))) %>%
  gt(groupname_col = "scenario_id") %>% 
  tab_header(title = md("**Max hospitalizations - low immune escape**")) %>%
  cols_label(model_name = md("**Model**"), 
             'Warm season 2023' = md("**Warm season 2023**"), 'Cold season 2023-24' = md("**Cold season 2023-24**"), 
             'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
  cols_align(columns = c('Warm season 2023', 'Cold season 2023-24', 'Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
  tab_footnote(footnote = md("Each value represnts the median with the 95% projection interval")) %>%
  cols_width(model_name ~ px(180), contains("season")~px(220))

season_hosp_mx_tbl_highI <- season_res_sum%>%
  filter(target=="inc hosp", location=="US", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "No booster \n High immune escape","D-2023-04-16"="Booster for 65+ \n High immune escape",  "F-2023-04-16"="Booster for all \n High immune escape"))%>%
  mutate(model_name=relevel(as.factor(model_name), ref="Ensemble"))%>%
  
  mutate(IQR = paste0(format(round(mx_median,0), nsmall=0, big.mark=","),
                      " (",format(round(mx_q025,0), nsmall=0, big.mark=","),"-",
                      format(round(mx_q975,0), nsmall=0, big.mark=","),")")) %>%
  dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
  arrange(scenario_id, factor(model_name, levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                                   "NotreDame-FRED", "MOBS_NEU-GLEAM_COVID", "JHU_IDD-CovidSP", "Ensemble"))) %>%
  gt(groupname_col = "scenario_id") %>% 
  tab_header(title = md("**Max hospitalizations - high immune escape**")) %>%
  cols_label(model_name = md("**Model**"), 
             'Warm season 2023' = md("**Warm season 2023**"), 'Cold season 2023-24' = md("**Cold season 2023-24**"), 
             'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
  cols_align(columns = c('Warm season 2023', 'Cold season 2023-24', 'Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
  tab_footnote(footnote = md("Each value represnts the median with the 95% projection interval")) %>%
  cols_width(model_name ~ px(180), contains("season")~px(220))

gtsave(season_hosp_mx_tbl_lowI, "code/report/analysis/round17_period_analysis/hosp_mx_tbl_lowI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(season_hosp_mx_tbl_highI, "code/report/analysis/round17_period_analysis/hosp_mx_tbl_highI.png", vwidth = 1500, vheight = 1000, expand=10) 


## Summary table for total hospitalizations for the US
season_hosp_tot_tbl_lowI <- season_res_sum%>%
  filter(target=="inc hosp", location=="US", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "No booster \n Low immune escape","C-2023-04-16"="Booster for 65+ \n Low immune escape",  "E-2023-04-16"="Booster for all \n Low immune escape"))%>%
  mutate(model_name=relevel(as.factor(model_name), ref="Ensemble"))%>%
  
  mutate(IQR = paste0(format(round(tot_median,0), nsmall=0, big.mark=","),
                      " (",format(round(tot_q025,0), nsmall=0, big.mark=","),"-",
                      format(round(tot_q975,0), nsmall=0, big.mark=","),")")) %>%
  dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
  arrange(scenario_id, factor(model_name, levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                                   "NotreDame-FRED", "MOBS_NEU-GLEAM_COVID", "JHU_IDD-CovidSP", "Ensemble"))) %>%
  gt(groupname_col = "scenario_id") %>% 
  tab_header(title = md("**Total hospitalizations - low immune escape**")) %>%
  cols_label(model_name = md("**Model**"), 
             'Warm season 2023' = md("**Warm season 2023**"), 'Cold season 2023-24' = md("**Cold season 2023-24**"), 
             'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
  cols_align(columns = c('Warm season 2023', 'Cold season 2023-24', 'Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
  tab_footnote(footnote = md("Each value represnts the median with the 95% projection interval")) %>%
  cols_width(model_name ~ px(180), contains("season")~px(300))


season_hosp_tot_tbl_highI <- season_res_sum%>%
  filter(target=="inc hosp", location=="US", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "No booster \n High immune escape","D-2023-04-16"="Booster for 65+ \n High immune escape",  "F-2023-04-16"="Booster for all \n High immune escape"))%>%
  mutate(model_name=relevel(as.factor(model_name), ref="Ensemble"))%>%
  
  mutate(IQR = paste0(format(round(tot_median,0), nsmall=0, big.mark=","),
                      " (",format(round(tot_q025,0), nsmall=0, big.mark=","),"-",
                      format(round(tot_q975,0), nsmall=0, big.mark=","),")")) %>%
  dplyr::select(scenario_id, model_name, period, IQR) %>% spread(period, IQR) %>% 
  arrange(scenario_id, factor(model_name, levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                                   "NotreDame-FRED", "MOBS_NEU-GLEAM_COVID", "JHU_IDD-CovidSP", "Ensemble"))) %>%
  gt(groupname_col = "scenario_id") %>% 
  tab_header(title = md("**Total hospitalizations - high immune escape**")) %>%
  cols_label(model_name = md("**Model**"), 
             'Warm season 2023' = md("**Warm season 2023**"), 'Cold season 2023-24' = md("**Cold season 2023-24**"), 
             'Warm season 2024' = md("**Warm season 2024**"), 'Cold season 2024-25' = md("**Cold season 2024-25**")) %>%
  cols_align(columns = c('Warm season 2023', 'Cold season 2023-24', 'Warm season 2024', 'Cold season 2024-25'), align = "center") %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_row_groups()) %>%
  tab_footnote(footnote = md("Each value represnts the median with the 95% projection interval")) %>%
  cols_width(model_name ~ px(180), contains("season")~px(300))

gtsave(season_hosp_tot_tbl_lowI, "code/report/analysis/round17_period_analysis/hosp_tot_tbl_lowI.png", vwidth = 1500, vheight = 1000, expand=10) 
gtsave(season_hosp_tot_tbl_highI, "code/report/analysis/round17_period_analysis/hosp_tot_tbl_highI.png", vwidth = 1500, vheight = 1000, expand=10) 





#### Figures by state (using only ensemble result)
read.csv("data-locations/locations.csv") %>% dplyr::select(abbreviation, location, population) %>%
  filter(location!="US") %>% mutate(location=as.numeric(location)) -> df_loc
merge(season_res_sum %>% filter(location!="US") %>% mutate(location=as.numeric(location)), 
      df_loc, by=c("location"), all.x=TRUE) %>% 
  filter(!(abbreviation%in%c("AS", "GU", "MP", "PR", "UM", "VI"))) -> season_res_sum_loc

#write csv for easy incorporation into report
write.csv(season_res_sum_loc, file = paste0(getwd(),"/code/report/analysis/round17_period_analysis/season_res_sum_loc.csv"))


options(repr.plot.width=10,repr.plot.height=20)

## Plot total seasonal deaths by state
season_dths_tot_plt_lowI_state <- season_res_sum_loc%>%
  filter(target=="inc death", model_name=="Ensemble", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "No booster \n Low immune escape","C-2023-04-16"="Booster for 65+ \n Low immune escape",  "E-2023-04-16"="Booster for all \n Low immune escape"))%>%
  ggplot(aes(x=abbreviation, fill=tot_median/population*100000))+
  scale_fill_viridis_c(option = "turbo") +
  facet_grid(rows=vars(scenario_id), cols=vars(period))+
  geom_boxplot(aes(ymin=tot_q025/population*100000, lower=tot_q25/population*100000, 
                   middle=tot_median/population*100000, upper=tot_q75/population*100000, 
                   ymax=tot_q975/population*100000), stat="identity") + 
  scale_y_sqrt()+
  coord_flip()+
  theme(legend.position="none",
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="State \n", title="Total deaths per 100,000 - low immune escape (Ensemble)")



season_dths_tot_plt_highI_state <- season_res_sum_loc%>%
  filter(target=="inc death", model_name=="Ensemble", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "No booster \n High immune escape","D-2023-04-16"="Booster for 65+ \n High immune escape",  "F-2023-04-16"="Booster for all \n High immune escape"))%>%
  ggplot(aes(x=abbreviation, fill=tot_median/population*100000))+
  scale_fill_viridis_c(option = "turbo") +
  facet_grid(rows=vars(scenario_id), cols=vars(period))+
  geom_boxplot(aes(ymin=tot_q025/population*100000, lower=tot_q25/population*100000, 
                   middle=tot_median/population*100000, upper=tot_q75/population*100000, 
                   ymax=tot_q975/population*100000), stat="identity") + 
  scale_y_sqrt()+
  coord_flip()+
  theme(legend.position="none",
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="State \n", title="Total deaths per 100,000 - high immune escape (Ensemble)")


ggsave("code/report/analysis/round17_period_analysis/season_dths_tot_plt_lowI_state.png", season_dths_tot_plt_lowI_state, width=10, height=20)
ggsave("code/report/analysis/round17_period_analysis/season_dths_tot_plt_highI_state.png", season_dths_tot_plt_highI_state, width=10, height=20)


## Now do the same thing for maximum deaths 
season_dths_mx_plt_lowI_state <- season_res_sum_loc%>%
  filter(target=="inc death", model_name=="Ensemble", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "No booster \n Low immune escape","C-2023-04-16"="Booster for 65+ \n Low immune escape",  "E-2023-04-16"="Booster for all \n Low immune escape"))%>%
  ggplot(aes(x=abbreviation, fill=mx_median/population*100000))+
  scale_fill_viridis_c(option = "turbo") +
  facet_grid(rows=vars(scenario_id), cols=vars(period))+
  geom_boxplot(aes(ymin=mx_q025/population*100000, lower=mx_q25/population*100000, 
                   middle=mx_median/population*100000, upper=mx_q75/population*100000, 
                   ymax=mx_q975/population*100000), stat="identity") + 
  scale_y_sqrt()+
  coord_flip()+
  theme(legend.position="none",
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="State \n", title="max deaths per 100,000 - low immune escape (Ensemble)")


season_dths_mx_plt_highI_state <- season_res_sum_loc%>%
  filter(target=="inc death", model_name=="Ensemble", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "No booster \n High immune escape","D-2023-04-16"="Booster for 65+ \n High immune escape",  "F-2023-04-16"="Booster for all \n High immune escape"))%>%
  ggplot(aes(x=abbreviation, fill=mx_median/population*100000))+
  scale_fill_viridis_c(option = "turbo") +
  facet_grid(rows=vars(scenario_id), cols=vars(period))+
  geom_boxplot(aes(ymin=mx_q025/population*100000, lower=mx_q25/population*100000, 
                   middle=mx_median/population*100000, upper=mx_q75/population*100000, 
                   ymax=mx_q975/population*100000), stat="identity") + 
  scale_y_sqrt()+
  coord_flip()+
  theme(legend.position="none",
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="State \n", title="max deaths per 100,000 - high immune escape (Ensemble)")


ggsave("code/report/analysis/round17_period_analysis/season_dths_mx_plt_lowI_state.png", season_dths_mx_plt_lowI_state, width=10, height=20)
ggsave("code/report/analysis/round17_period_analysis/season_dths_mx_plt_highI_state.png", season_dths_mx_plt_highI_state, width=10, height=20)


## Plot total seasonal hosps by state
season_hosp_tot_plt_lowI_state <- season_res_sum_loc%>%
  filter(target=="inc hosp", model_name=="Ensemble", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "No booster \n Low immune escape","C-2023-04-16"="Booster for 65+ \n Low immune escape",  "E-2023-04-16"="Booster for all \n Low immune escape"))%>%
  ggplot(aes(x=abbreviation, fill=tot_median/population*100000))+
  scale_fill_viridis_c(option = "turbo") +
  facet_grid(rows=vars(scenario_id), cols=vars(period))+
  geom_boxplot(aes(ymin=tot_q025/population*100000, lower=tot_q25/population*100000, 
                   middle=tot_median/population*100000, upper=tot_q75/population*100000, 
                   ymax=tot_q975/population*100000), stat="identity") + 
  scale_y_sqrt()+
  coord_flip()+
  theme(legend.position="none",
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="State \n", title="Total Hospitalizations per 100,000 - low immune escape (Ensemble)")

season_hosp_tot_plt_highI_state  <- season_res_sum_loc%>%
  filter(target=="inc hosp", model_name=="Ensemble", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "No booster \n High immune escape","D-2023-04-16"="Booster for 65+ \n High immune escape",  "F-2023-04-16"="Booster for all \n High immune escape"))%>%
  ggplot(aes(x=abbreviation, fill=tot_median/population*100000))+
  scale_fill_viridis_c(option = "turbo") +
  facet_grid(rows=vars(scenario_id), cols=vars(period))+
  geom_boxplot(aes(ymin=tot_q025/population*100000, lower=tot_q25/population*100000, 
                   middle=tot_median/population*100000, upper=tot_q75/population*100000, 
                   ymax=tot_q975/population*100000), stat="identity") + 
  scale_y_sqrt()+
  coord_flip()+
  theme(legend.position="none",
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="State \n", title="Total Hospitalizations per 100,000 - high immune escape (Ensemble)")


ggsave("code/report/analysis/round17_period_analysis/season_hosp_tot_plt_lowI_state .png", season_hosp_tot_plt_lowI_state , width=10, height=20)
ggsave("code/report/analysis/round17_period_analysis/season_hosp_tot_plt_highI_state .png", season_hosp_tot_plt_highI_state , width=10, height=20)


## Now do the same thing for maximum hosps 
season_hosp_mx_plt_lowI_state  <- season_res_sum_loc%>%
  filter(target=="inc hosp", model_name=="Ensemble", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "No booster \n Low immune escape","C-2023-04-16"="Booster for 65+ \n Low immune escape",  "E-2023-04-16"="Booster for all \n Low immune escape"))%>%
  ggplot(aes(x=abbreviation, fill=mx_median/population*100000))+
  scale_fill_viridis_c(option = "turbo") +
  facet_grid(rows=vars(scenario_id), cols=vars(period))+
  geom_boxplot(aes(ymin=mx_q025/population*100000, lower=mx_q25/population*100000, 
                   middle=mx_median/population*100000, upper=mx_q75/population*100000, 
                   ymax=mx_q975/population*100000), stat="identity") + 
  scale_y_sqrt()+
  coord_flip()+
  theme(legend.position="none",
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="State \n", title="max Hospitalizations per 100,000 - low immune escape (Ensemble)")


season_hosp_mx_plt_highI_state  <- season_res_sum_loc%>%
  filter(target=="inc hosp", model_name=="Ensemble", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
  mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "No booster \n High immune escape","D-2023-04-16"="Booster for 65+ \n High immune escape",  "F-2023-04-16"="Booster for all \n High immune escape"))%>%
  ggplot(aes(x=abbreviation, fill=mx_median/population*100000))+
  scale_fill_viridis_c(option = "turbo") +
  facet_grid(rows=vars(scenario_id), cols=vars(period))+
  geom_boxplot(aes(ymin=mx_q025/population*100000, lower=mx_q25/population*100000,
                   middle=mx_median/population*100000, upper=mx_q75/population*100000, 
                   ymax=mx_q975/population*100000), stat="identity") + 
  scale_y_sqrt()+
  coord_flip()+
  theme(legend.position="none",
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="State \n", title="max Hospitalizations per 100,000 - high immune escape (Ensemble)")


ggsave("code/report/analysis/round17_period_analysis/season_hosp_mx_plt_lowI_state .png", season_hosp_mx_plt_lowI_state , width=10, height=20)
ggsave("code/report/analysis/round17_period_analysis/season_hosp_mx_plt_highI_state .png", season_hosp_mx_plt_highI_state , width=10, height=20)



##### Probability of peak

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

##load the data
repo_data <- "../covid19-megaround_data/"
folder_path <- "megaround-processed/"

dc <- arrow::open_dataset(paste0(repo_data, folder_path), partitioning = "model_name", 
                          format = "parquet", schema = schema,
                          factory_options = list(
                            exclude_invalid_files = TRUE))


df_sample <- dplyr::filter(dc, type == "sample" & (target == "inc death" |  target == "inc hosp")) %>% 
  dplyr::collect()


##filter out longer time horizons
df_sample <- df_sample %>% filter(horizon<=104)

##regreate projection date
df_sample <- df_sample %>% mutate(origin_date = lubridate::as_date(origin_date))

##Create a season indicator
##First let's recreate the projection date
df_sample <- df_sample %>%
  mutate(proj_date = origin_date + horizon*7 )

##Let's chop up on date. First let's make it based on 4 equa
##week periods
date_cuts <- c("2023-04-15","2024-04-15","2025-05-01")
date_cuts<-lubridate::as_date(date_cuts)
df_sample <- df_sample %>% 
  mutate(period=cut(proj_date, date_cuts, 
                    labels = c("season 2023-24",
                               "season 2024-25")))


df_sample_date <- df_sample %>% filter(location=="US") %>% 
  group_by(scenario_id, target, model_name, period, type_id) %>%
  slice(which.max(value)) %>% ungroup()


df_sample_date %>% filter(target=="inc death") %>%
  mutate(scenario=recode(scenario_id, 
                         "A-2023-04-16" = "No booster \n Low immune escape", 
                         "B-2023-04-16" = "No booster \n High immune escape", 
                         "C-2023-04-16"="Booster for 65+ \n Low immune escape", 
                         "D-2023-04-16"="Booster for 65+ \n High immune escape", 
                         "E-2023-04-16"="Booster for all \n Low immune escape",
                         "F-2023-04-16"="Booster for all \n High immune escape")) %>%
  filter(period=="season 2023-24") -> prob_death_2023_df

df_sample_date %>% filter(target=="inc death") %>%
  mutate(scenario=recode(scenario_id, 
                         "A-2023-04-16" = "No booster \n Low immune escape", 
                         "B-2023-04-16" = "No booster \n High immune escape", 
                         "C-2023-04-16"="Booster for 65+ \n Low immune escape", 
                         "D-2023-04-16"="Booster for 65+ \n High immune escape", 
                         "E-2023-04-16"="Booster for all \n Low immune escape",
                         "F-2023-04-16"="Booster for all \n High immune escape")) %>%
  filter(period=="season 2024-25"& proj_date <= as.Date("2025-04-19")) -> prob_death_2024_df

prob_death_2023_df %>%
  group_by(scenario, model_name) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() -> prob_death_2023_boxdf

prob_death_2024_df %>%
  group_by(scenario, model_name) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() -> prob_death_2024_boxdf

prob_death_2023_df %>%
  group_by(scenario) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() %>% mutate(model_name=c("Ensemble")) -> prob_death_2023_boxdf_ensemble

prob_death_2024_df %>%
  group_by(scenario) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() %>% mutate(model_name=c("Ensemble")) -> prob_death_2024_boxdf_ensemble


rbind(prob_death_2023_boxdf, prob_death_2023_boxdf_ensemble) %>%
  arrange(scenario, factor(model_name, 
                           levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                    "MOBS_NEU-GLEAM_COVID", "NotreDame-FRED", "JHU_IDD-CovidSP", "Ensemble"))) -> prob_death_2023_boxdf

rbind(prob_death_2024_boxdf, prob_death_2024_boxdf_ensemble) %>%
  arrange(scenario, factor(model_name, 
                           levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                    "MOBS_NEU-GLEAM_COVID", "NotreDame-FRED", "JHU_IDD-CovidSP", "Ensemble"))) -> prob_death_2024_boxdf


prob_death_2023_boxdf$scenario <- factor(prob_death_2023_boxdf$scenario, 
                                         levels=c("No booster \n Low immune escape", "No booster \n High immune escape",
                                                  "Booster for 65+ \n Low immune escape", "Booster for 65+ \n High immune escape",
                                                  "Booster for all \n Low immune escape", "Booster for all \n High immune escape"))

prob_death_2024_boxdf$scenario <- factor(prob_death_2024_boxdf$scenario, 
                                         levels=c("No booster \n Low immune escape", "No booster \n High immune escape",
                                                  "Booster for 65+ \n Low immune escape", "Booster for 65+ \n High immune escape",
                                                  "Booster for all \n Low immune escape", "Booster for all \n High immune escape"))


theme_set(theme_bw())
options(repr.plot.width=13,repr.plot.height=10)

prob_death_2023_boxdf %>%
  ggplot(aes(x=model_name))+
  facet_wrap(~scenario, nrow = 3)+
  geom_boxplot(aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975),
               stat="identity", color="#990000", width=0.7, alpha=0.5) + 
  geom_boxplot(data=prob_death_2024_boxdf, 
               aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975),
               stat="identity", color="#1380A1", width=0.7, alpha=0.5) + 
  coord_flip()+
  scale_y_date(date_labels = "%b-%Y", breaks = "4 months", date_minor_breaks = "1 month",
               limits=c(as.Date("2023-04-01"), as.Date("2025-04-19")), expand = c(0, 0)) +
  theme(legend.position="none", 
        text = element_text(size=15, family="sans",color="black"),
        axis.text = element_text(size=13, family="sans",color="black"),
        plot.title = element_text(size=15, family="sans",color="black"),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.spacing = unit(2, "lines")) +
  labs(x="", title="Peak timing of deaths in the US") -> peak_prob_death

peak_prob_death

ggsave("code/report/analysis/round17_period_analysis/peak_prob_death.png", peak_prob_death , 
       width=13, height=10)


## hospitalizations
df_sample_date %>% filter(target=="inc hosp") %>%
  mutate(scenario=recode(scenario_id, 
                         "A-2023-04-16" = "No booster \n Low immune escape", 
                         "B-2023-04-16" = "No booster \n High immune escape", 
                         "C-2023-04-16"="Booster for 65+ \n Low immune escape", 
                         "D-2023-04-16"="Booster for 65+ \n High immune escape", 
                         "E-2023-04-16"="Booster for all \n Low immune escape",
                         "F-2023-04-16"="Booster for all \n High immune escape")) %>%
  filter(period=="season 2023-24") -> prob_hosp_2023_df

df_sample_date %>% filter(target=="inc hosp") %>%
  mutate(scenario=recode(scenario_id, 
                         "A-2023-04-16" = "No booster \n Low immune escape", 
                         "B-2023-04-16" = "No booster \n High immune escape", 
                         "C-2023-04-16"="Booster for 65+ \n Low immune escape", 
                         "D-2023-04-16"="Booster for 65+ \n High immune escape", 
                         "E-2023-04-16"="Booster for all \n Low immune escape",
                         "F-2023-04-16"="Booster for all \n High immune escape")) %>%
  filter(period=="season 2024-25"& proj_date <= as.Date("2025-04-19")) -> prob_hosp_2024_df


prob_hosp_2023_df %>%
  group_by(scenario, model_name) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() -> prob_hosp_2023_boxdf

prob_hosp_2024_df %>%
  group_by(scenario, model_name) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() -> prob_hosp_2024_boxdf

prob_hosp_2023_df %>%
  group_by(scenario) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() %>% mutate(model_name=c("Ensemble")) -> prob_hosp_2023_boxdf_ensemble

prob_hosp_2024_df %>%
  group_by(scenario) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() %>% mutate(model_name=c("Ensemble")) -> prob_hosp_2024_boxdf_ensemble


rbind(prob_hosp_2023_boxdf, prob_hosp_2023_boxdf_ensemble) %>%
  arrange(scenario, factor(model_name, 
                           levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                    "MOBS_NEU-GLEAM_COVID", "NotreDame-FRED", "JHU_IDD-CovidSP", "Ensemble"))) -> prob_hosp_2023_boxdf

rbind(prob_hosp_2024_boxdf, prob_hosp_2024_boxdf_ensemble) %>%
  arrange(scenario, factor(model_name, 
                           levels=c("UVA-adaptive", "UVA-EpiHiper","UTA-ImmunoSEIRS", "USC-SIkJalpha", "UNCC-hierbin", 
                                    "MOBS_NEU-GLEAM_COVID", "NotreDame-FRED", "JHU_IDD-CovidSP", "Ensemble"))) -> prob_hosp_2024_boxdf


prob_hosp_2023_boxdf$scenario <- factor(prob_hosp_2023_boxdf$scenario, 
                                        levels=c("No booster \n Low immune escape", "No booster \n High immune escape",
                                                 "Booster for 65+ \n Low immune escape", "Booster for 65+ \n High immune escape",
                                                 "Booster for all \n Low immune escape", "Booster for all \n High immune escape"))

prob_hosp_2024_boxdf$scenario <- factor(prob_hosp_2024_boxdf$scenario, 
                                        levels=c("No booster \n Low immune escape", "No booster \n High immune escape",
                                                 "Booster for 65+ \n Low immune escape", "Booster for 65+ \n High immune escape",
                                                 "Booster for all \n Low immune escape", "Booster for all \n High immune escape"))


theme_set(theme_bw())
options(repr.plot.width=13,repr.plot.height=10)

prob_hosp_2023_boxdf %>%
  ggplot(aes(x=model_name))+
  facet_wrap(~scenario, nrow = 3)+
  geom_boxplot(aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975),
               stat="identity", color="#990000", width=0.7, alpha=0.5) + 
  geom_boxplot(data=prob_hosp_2024_boxdf, 
               aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975),
               stat="identity", color="#1380A1", width=0.7, alpha=0.5) + 
  coord_flip()+
  scale_y_date(date_labels = "%b-%Y", breaks = "4 months", date_minor_breaks = "1 month",
               limits=c(as.Date("2023-04-01"), as.Date("2025-04-19")), expand = c(0, 0)) +
  theme(legend.position="none", 
        text = element_text(size=15, family="sans",color="black"),
        axis.text = element_text(size=13, family="sans",color="black"),
        plot.title = element_text(size=15, family="sans",color="black"),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.spacing = unit(2, "lines")) +
  labs(x="", title="Peak timing of hospitalizations in the US") -> peak_prob_hosp

peak_prob_hosp

ggsave("code/report/analysis/round17_period_analysis/peak_prob_hosp.png", peak_prob_hosp , 
       width=13, height=10)

