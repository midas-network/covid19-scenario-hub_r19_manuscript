library(arrow)
library(tidyverse)
library(grid)
library(gridExtra)
library(scales)


# SETUP -------------------------------------------------------------------

####FIRST GET ALL THE SAMPLE BASED DATA
# run read_sample_files.R first to get the data


# Plot directory
plot_dir <- file.path(dir_path, "reports", paste0("round", round), "figures")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

age_text_snip <- ifelse(age_group_tmp != "0-130", paste0(", aged ", age_group_label), "")



# ANALYSIS ----------------------------------------------------------------

# GET THE MEAN AND STANDARD ERROR OF TOTAL FOR EACH TEAM AND SCENARIO
res_summary <- df_sample[age_group == age_group_tmp, 
                         .(tot = sum(value)), 
                         by = .(scenario_id, model_name, target, location, output_type_id)][
                           , .(scn_mn = mean(tot), 
                               scn_se = sd(tot)/sqrt(.N)), 
                           by = .(scenario_id, model_name, target, location)]

gc()



##Create a season indicator
##First let's recreate the projection date
df_sample <- df_sample %>%
  mutate(proj_date = origin_date + horizon*7 )

##Let's chop up on date. First let's make it based on 4 equa
##week periods
#wk_cuts <- c(0,26,52,78,104)
date_cuts <- c("2023-04-15","2023-09-01","2024-04-15","2024-09-01","2025-05-01")
date_cuts<-lubridate::as_date(date_cuts)

df_sample[, `:=`(
  period = cut(proj_date, date_cuts, 
               labels = c("off-season 2023",
                         "season 2023-24", 
                         "off-season 2024",
                         "season 2024-25")),
  in_season = !stringr::str_starts(period, "off")
)]

# Second part: Group by and summarize equivalent
season_res <- df_sample[, .(
  total = sum(value),
  mx = max(value)
), by = .(scenario_id, target, model_name, period, location, type_id)]


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
    mutate(model_name="ensembled")

season_res_sum <- bind_rows(season_res_sum, seas_res_overall)

##plot total seasonal deaths for the US
season_dths_tot_plt_lowI <- season_res_sum%>%
    filter(target=="inc death", location=="US", scenario_id %in% c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
    mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "A - no booster","C-2023-04-16"="C - 65+ boosters",  "E-2023-04-16"="E - all boosters"))%>%
    mutate(model_name=relevel(as.factor(model_name), ref="ensembled"))%>%
    ggplot(aes(x=model_name, fill=model_name))+
            facet_grid(rows=vars(scenario_id), cols=vars(period))+
            geom_boxplot(aes(ymin=tot_q025, lower=tot_q25, middle=tot_median, upper=tot_q75, ymax=tot_q975),
              stat="identity") + 
            scale_y_sqrt()+
            coord_flip()+
            theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1))+
            labs(x="team", title="Total Deaths - low immune escape")


season_dths_tot_plt_highI <- season_res_sum%>%
    filter(target=="inc death", location=="US", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
    mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "B - no booster","D-2023-04-16"="D - 65+ boosters",  "F-2023-04-16"="F - all boosters"))%>%
    mutate(model_name=relevel(as.factor(model_name), ref="ensembled"))%>%
    ggplot(aes(x=model_name, fill=model_name))+
            facet_grid(rows=vars(scenario_id), cols=vars(period))+
            geom_boxplot(aes(ymin=tot_q025, lower=tot_q25, middle=tot_median, upper=tot_q75, ymax=tot_q975),
              stat="identity") + 
            scale_y_sqrt()+
            coord_flip()+
            theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1))+
            labs(x="team", title="Total Deaths - high immune escape")


ggsave("season_dths_tot_plt_lowI.png", season_dths_tot_plt_lowI, width=6, height=4)
ggsave("season_dths_tot_plt_highI.png", season_dths_tot_plt_highI, width=6, height=4)


##Now do the same thing for maximum deaths 
season_dths_mx_plt_lowI <- season_res_sum%>%
    filter(target=="inc death", location=="US", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
    mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "A - no booster","C-2023-04-16"="C - 65+ boosters",  "E-2023-04-16"="E - all boosters"))%>%
    mutate(model_name=relevel(as.factor(model_name), ref="ensembled"))%>%
    ggplot(aes(x=model_name, fill=model_name))+
            facet_grid(rows=vars(scenario_id), cols=vars(period))+
            geom_boxplot(aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975),
              stat="identity") + 
            scale_y_sqrt()+
            coord_flip()+
            theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1))+
            labs(x="team", title="max Deaths - low immune escape")


season_dths_mx_plt_highI <- season_res_sum%>%
    filter(target=="inc death", location=="US", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
    mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "B - no booster","D-2023-04-16"="D - 65+ boosters",  "F-2023-04-16"="F - all boosters"))%>%
    mutate(model_name=relevel(as.factor(model_name), ref="ensembled"))%>%
    ggplot(aes(x=model_name, fill=model_name))+
            facet_grid(rows=vars(scenario_id), cols=vars(period))+
            geom_boxplot(aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975),
              stat="identity") + 
            scale_y_sqrt()+
            coord_flip()+
            theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1))+
            labs(x="team", title="max Deaths - high immune escape")


ggsave("season_dths_mx_plt_lowI.png", season_dths_mx_plt_lowI, width=6, height=4)
ggsave("season_dths_mx_plt_highI.png", season_dths_mx_plt_highI, width=6, height=4)



############Hospitalizatoins

##plot total seasonal deaths for the US
season_hosp_tot_plt_lowI <- season_res_sum%>%
    filter(target=="inc hosp", location=="US", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
    mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "A - no booster","C-2023-04-16"="C - 65+ boosters",  "E-2023-04-16"="E - all boosters"))%>%
    mutate(model_name=relevel(as.factor(model_name), ref="ensembled"))%>%
    ggplot(aes(x=model_name, fill=model_name))+
            facet_grid(rows=vars(scenario_id), cols=vars(period))+
            geom_boxplot(aes(ymin=tot_q025, lower=tot_q25, middle=tot_median, upper=tot_q75, ymax=tot_q975),
              stat="identity") + 
            scale_y_sqrt()+
            coord_flip()+
            theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1))+
            labs(x="team", title="Total Hospitalizations - low immune escape")


season_hosp_tot_plt_highI <- season_res_sum%>%
    filter(target=="inc hosp", location=="US", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
    mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "B - no booster","D-2023-04-16"="D - 65+ boosters",  "F-2023-04-16"="F - all boosters"))%>%
    mutate(model_name=relevel(as.factor(model_name), ref="ensembled"))%>%
    ggplot(aes(x=model_name, fill=model_name))+
            facet_grid(rows=vars(scenario_id), cols=vars(period))+
            geom_boxplot(aes(ymin=tot_q025, lower=tot_q25, middle=tot_median, upper=tot_q75, ymax=tot_q975),
              stat="identity") + 
            scale_y_sqrt()+
            coord_flip()+
            theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1))+
            labs(x="team", title="Total Hospitalizations - high immune escape")


ggsave("season_hosp_tot_plt_lowI.png", season_hosp_tot_plt_lowI, width=6, height=4)
ggsave("season_hosp_tot_plt_highI.png", season_hosp_tot_plt_highI, width=6, height=4)



##Now do the same thing for maximum deaths 
season_hosp_mx_plt_lowI <- season_res_sum%>%
    filter(target=="inc hosp", location=="US", scenario_id%in%c("A-2023-04-16","C-2023-04-16", "E-2023-04-16"))%>%
    mutate(scenario_id=recode(scenario_id, "A-2023-04-16" = "A - no booster","C-2023-04-16"="C - 65+ boosters",  "E-2023-04-16"="E - all boosters"))%>%
    mutate(model_name=relevel(as.factor(model_name), ref="ensembled"))%>%
    ggplot(aes(x=model_name, fill=model_name))+
            facet_grid(rows=vars(scenario_id), cols=vars(period))+
            geom_boxplot(aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975),
              stat="identity") + 
            scale_y_sqrt()+
            coord_flip()+
            theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1))+
            labs(x="team", title="max hosp - low immune escape")


season_hosp_mx_plt_highI <- season_res_sum%>%
    filter(target=="inc hosp", location=="US", scenario_id%in%c("B-2023-04-16","D-2023-04-16", "F-2023-04-16"))%>%
    mutate(scenario_id=recode(scenario_id, "B-2023-04-16" = "B - no booster","D-2023-04-16"="D - 65+ boosters",  "F-2023-04-16"="F - all boosters"))%>%
    mutate(model_name=relevel(as.factor(model_name), ref="ensembled"))%>%
    ggplot(aes(x=model_name, fill=model_name))+
            facet_grid(rows=vars(scenario_id), cols=vars(period))+
            geom_boxplot(aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975),
              stat="identity") + 
            scale_y_sqrt()+
            coord_flip()+
            theme(legend.position="none",
            axis.text.x = element_text(angle=45, hjust=1))+
            labs(x="team", title="max hosp - high immune escape")


ggsave("season_hosp_mx_plt_lowI.png", season_hosp_mx_plt_lowI, width=6, height=4)
ggsave("season_hosp_mx_plt_highI.png", season_hosp_mx_plt_highI, width=6, height=4)

##TODO - Summarize in a tabular format

##TODO - Make a figure where I drill down by state
