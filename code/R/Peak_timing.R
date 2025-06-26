library(arrow)
library(tidyverse)



##regreate projection date

df_sample <- df_sample %>%
  mutate(proj_date = origin_date + horizon*7)

##Now get a version of this with the season totals
df_sample_date <- df_sample %>% filter(location=="US") %>% 
  group_by(scenario_id, target, model_name, output_type_id) %>%
  slice(which.max(value)) %>% ungroup()


df_sample_date %>% filter(target=="inc death") %>%
  mutate(scenario=recode(scenario_id, 
                         "A-2024-03-01" = "No booster \n Low immune escape", 
                         "B-2024-03-01" = "No booster \n High immune escape", 
                         "C-2024-03-01"="Booster for high-risk \n Low immune escape", 
                         "D-2024-03-01"="Booster for high-risk \n High immune escape", 
                         "E-2024-03-01"="Booster for all \n Low immune escape",
                         "F-2024-03-01"="Booster for all \n High immune escape")) -> prob_death_df


prob_death_df %>% filter(model_name == "MOBS_NEU-GLEAM_COVID" & scenario_id == "A-2024-03-01") %>%
  group_by(horizon) %>%
  summarise(count = n())

prob_death_df %>%
  group_by(scenario, model_name) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() -> prob_death_boxdf

prob_death_df %>%
  group_by(scenario) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() %>% mutate(model_name=c("Ensemble")) -> prob_death_boxdf_ensemble

rbind(prob_death_boxdf, prob_death_boxdf_ensemble) %>% arrange(scenario) -> prob_death_boxdf

prob_death_boxdf$model_name <- factor(prob_death_boxdf$model_name, 
                                     levels = c("Ensemble", "CFA-Scenarios", "JHU_UNC-flepiMoP", "MOBS_NEU-GLEAM_COVID", "NotreDame-FRED",
                                                "PSI-PROF", "UNCC-hierbin", "USC-SIkJalpha", "UTA-ImmunoSEIRS", "UVA-adaptive"))
  
prob_death_boxdf$scenario <- factor(prob_death_boxdf$scenario, 
                                    levels=c("No booster \n Low immune escape", "No booster \n High immune escape",
                                             "Booster for high-risk \n Low immune escape", "Booster for high-risk \n High immune escape",
                                             "Booster for all \n Low immune escape", "Booster for all \n High immune escape"))

theme_set(theme_bw())
options(repr.plot.width=13,repr.plot.height=10)

prob_death_boxdf %>%
  ggplot(aes(x=model_name))+
  facet_wrap(~scenario, nrow = 3)+
  geom_boxplot(aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975,
                   color = ifelse(model_name == "Ensemble", "Ensemble", "other")),
               stat="identity", width=0.7, alpha=0.5) + 
  scale_color_manual(values = c("Ensemble" = "#990000", "other" = "#1380A1"), guide = "none") +
  coord_flip()+
  scale_y_date(date_labels = "%b-%Y", breaks = "2 months", date_minor_breaks = "1 month",
               limits=c(as.Date("2024-04-28"), as.Date("2025-04-30")), expand = c(0, 0)) +
  theme(legend.position="none", 
        text = element_text(size=15, family="sans",color="black"),
        axis.text = element_text(size=13, family="sans",color="black"),
        plot.title = element_text(size=15, family="sans",color="black"),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.spacing = unit(2, "lines")) +
  labs(x="", title="Peak timing of deaths in the US") -> peak_prob_death

peak_prob_death

ggsave("code/report/analysis/round18_period_analysis/peak_prob_death.png", peak_prob_death , 
       width=13, height=10)



## hospitalizations
df_sample_date %>% filter(target=="inc hosp") %>%
  mutate(scenario=recode(scenario_id, 
                         "A-2024-03-01" = "No booster \n Low immune escape", 
                         "B-2024-03-01" = "No booster \n High immune escape", 
                         "C-2024-03-01"="Booster for high-risk \n Low immune escape", 
                         "D-2024-03-01"="Booster for high-risk \n High immune escape", 
                         "E-2024-03-01"="Booster for all \n Low immune escape",
                         "F-2024-03-01"="Booster for all \n High immune escape")) -> prob_hosp_df

prob_hosp_df %>%
  group_by(scenario, model_name) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() -> prob_hosp_boxdf

prob_hosp_df %>%
  group_by(scenario) %>%
  summarize(mx_median = median(proj_date),
            mx_q975=as.Date(quantile(unclass(proj_date),0.90), origin=("1970-01-01")),
            mx_q75=as.Date(quantile(unclass(proj_date),0.75), origin=("1970-01-01")),
            mx_q25=as.Date(quantile(unclass(proj_date),0.25), origin=("1970-01-01")),
            mx_q025=as.Date(quantile(unclass(proj_date),0.10), origin=("1970-01-01")))%>%
  ungroup() %>% mutate(model_name=c("Ensemble")) -> prob_hosp_boxdf_ensemble

rbind(prob_hosp_boxdf, prob_hosp_boxdf_ensemble) %>% arrange(scenario) -> prob_hosp_boxdf

prob_hosp_boxdf$model_name <- factor(prob_hosp_boxdf$model_name, 
                                     levels = c("Ensemble", "CFA-Scenarios", "JHU_UNC-flepiMoP", "MOBS_NEU-GLEAM_COVID", "NotreDame-FRED",
                                                "PSI-PROF", "UNCC-hierbin", "USC-SIkJalpha", "UTA-ImmunoSEIRS", "UVA-adaptive"))

prob_hosp_boxdf$scenario <- factor(prob_hosp_boxdf$scenario, 
                                   levels=c("No booster \n Low immune escape", "No booster \n High immune escape",
                                            "Booster for high-risk \n Low immune escape", "Booster for high-risk \n High immune escape",
                                            "Booster for all \n Low immune escape", "Booster for all \n High immune escape"))

theme_set(theme_bw())
options(repr.plot.width=13,repr.plot.height=10)

prob_hosp_boxdf %>%
  ggplot(aes(x=model_name))+
  facet_wrap(~scenario, nrow = 3)+
  geom_boxplot(aes(ymin=mx_q025, lower=mx_q25, middle=mx_median, upper=mx_q75, ymax=mx_q975,
                   color = ifelse(model_name == "Ensemble", "Ensemble", "other")),
               stat="identity", width=0.7, alpha=0.5) + 
  scale_color_manual(values = c("Ensemble" = "#990000", "other" = "#1380A1"), guide = "none") +
  coord_flip()+
  scale_y_date(date_labels = "%b-%Y", breaks = "2 months", date_minor_breaks = "1 month",
               limits=c(as.Date("2024-04-28"), as.Date("2025-04-30")), expand = c(0, 0)) +
  theme(legend.position="none", 
        text = element_text(size=15, family="sans",color="black"),
        axis.text = element_text(size=13, family="sans",color="black"),
        plot.title = element_text(size=15, family="sans",color="black"),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.spacing = unit(2, "lines")) +
  labs(x="", title="Peak timing of hospitalizations in the US") -> peak_prob_hosp

peak_prob_hosp

ggsave("code/report/analysis/round18_period_analysis/peak_prob_hosp.png", peak_prob_hosp , 
       width=13, height=10)

