library(arrow)
library(tidyverse)
library(grid)
library(gridExtra)
library(scales)


# SETUP -------------------------------------------------------------------

if(!exists("panel_height")){
  panel_height <- 7
}

# age_group_tmp <- "0-64"
# age_group_label <- "0-64"
# data_path <- "../covid-19-smh_data/"
# folder_path <- "model-processed/"
if (!exists("loc_select")){
  loc_select <- "US"
}


meta_name <- ifelse(loc_select == "US", "meta_national", "meta_state")
loc_subname <- ifelse(loc_select == "US", "", paste0("_", loc_abbr))

# Schema
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


## within each level of vaccination 

res_summary %>% filter(scenario_id==c("A-2025-04-01")) %>% 
  mutate(vax_tmg=c("classic"), vax_lvl=c("Counterfactual")) -> temp1

res_summary %>% filter(scenario_id==c("A-2025-04-01")) %>% 
  mutate(vax_tmg=c("early"), vax_lvl=c("Counterfactual")) -> temp2

res_summary %>% filter(scenario_id==c("A-2025-04-01")) %>% 
  mutate(vax_tmg=c("Counterfactual"), vax_lvl=c("High")) -> temp3

res_summary %>% filter(scenario_id==c("A-2025-04-01")) %>% 
  mutate(vax_tmg=c("Counterfactual"), vax_lvl=c("All")) -> temp4

res_summary <- res_summary %>%
  filter(scenario_id != "A-2025-04-01") %>%
  mutate(vax_tmg = case_when(scenario_id%in%c("B-2025-04-01", "D-2025-04-01") ~ "classic",
                             scenario_id%in%c("C-2025-04-01", "E-2025-04-01") ~ "early"),
         vax_lvl = case_when(scenario_id%in%c("B-2025-04-01", "C-2025-04-01") ~ "High",
                             scenario_id%in%c("D-2025-04-01", "E-2025-04-01") ~ "All"))

res_summary <- rbind(res_summary, temp1, temp2, temp3, temp4)

##Pivot wider
res_summary_wide <- res_summary %>% 
  select(-scenario_id) %>%
  pivot_wider(names_from=vax_tmg, values_from = c(scn_mn, scn_se))

se_ratio <- function (mn1, mn2, se1, se2) {
  mn1/mn2 * sqrt((se1/mn1)^2 + (se2/mn2)^2)
}

res_compares <- res_summary_wide %>%
  mutate(counter_classic_mn_diff = scn_mn_Counterfactual-scn_mn_classic,
         counter_classic_se_diff = sqrt(scn_se_Counterfactual^2+scn_se_classic^2),
         counter_early_mn_diff = scn_mn_Counterfactual-scn_mn_early,
         counter_early_se_diff = sqrt(scn_se_Counterfactual^2+scn_se_early^2),
         
         classic_early_mn_diff = scn_mn_classic-scn_mn_early,
         classic_early_se_diff = sqrt(scn_se_classic^2+scn_se_early^2),
         
         counter_classic_mn_rel = scn_mn_classic/scn_mn_Counterfactual,
         counter_classic_se_rel = se_ratio(scn_mn_classic, scn_mn_Counterfactual,scn_se_classic, scn_se_Counterfactual),
         counter_early_mn_rel = scn_mn_early/scn_mn_Counterfactual,
         counter_early_se_rel = se_ratio(scn_mn_early, scn_mn_Counterfactual,scn_se_early, scn_se_Counterfactual),
         
         classic_early_mn_rel = scn_mn_early/scn_mn_classic,
         classic_early_se_rel = se_ratio(scn_mn_early, scn_mn_classic, scn_se_early, scn_se_classic)) %>%
  
  select(model_name, target, location, vax_lvl,
         counter_classic_mn_diff, counter_early_mn_diff, counter_classic_se_diff, counter_early_se_diff, classic_early_mn_diff, classic_early_se_diff,
         counter_classic_mn_rel, counter_early_mn_rel, counter_classic_se_rel, counter_early_se_rel, classic_early_mn_rel, classic_early_se_rel)

res_compares_lng <- res_compares %>%
  pivot_longer(cols=c(counter_classic_mn_diff, counter_classic_se_diff,
                      counter_early_mn_diff, counter_early_se_diff,
                      classic_early_mn_diff, classic_early_se_diff,
                      counter_classic_mn_rel, counter_classic_se_rel,
                      counter_early_mn_rel, counter_early_se_rel,
                      classic_early_mn_rel, classic_early_se_rel)) %>%
  mutate(measure=ifelse(stringr::str_detect(name,"mn"),"mn","se"),
         diff_rel = ifelse(stringr::str_detect(name,"diff"),"diff","rel"),
         compare=ifelse(stringr::str_detect(name,"counter_early"), "Early immunization vs Counterfactual", 
                        ifelse(stringr::str_detect(name,"counter_classic"), "Classic immunization vs Counterfactual", 
                               "Early vs Classic immunization")),
         target_labs=ifelse(stringr::str_detect(target, "inc death"), "Deaths", "Hospitalizations")) %>%
  select(-name)%>%
  pivot_wider(values_from = value, names_from = measure)


## REDOING THE SAME WITH PAIRING

res_summary_m <- df_sample[age_group == age_group_tmp & 
                             !(model_name %in% unmatched_models),
                           .(tot = sum(value)),
                           by = .(scenario_id, model_name, target, location, output_type_id)][
                             order(model_name, target, location, output_type_id, scenario_id)]

gc()

res_summary_wide_m <- res_summary_m %>% 
  mutate(scenario_id2=case_when(scenario_id=="A-2025-04-01" ~ "scA",
                                scenario_id=="B-2025-04-01" ~ "scB",
                                scenario_id=="C-2025-04-01" ~ "scC",
                                scenario_id=="D-2025-04-01" ~ "scD",
                                scenario_id=="E-2025-04-01" ~ "scE")) %>%
  select(-scenario_id) %>%
  pivot_wider(names_from=c(scenario_id2), values_from = tot) %>%
  arrange(model_name, target, location, output_type_id)


res_compares_m <- res_summary_wide_m %>%
  mutate(counter_B_abs_diff = scA-scB,
         counter_C_abs_diff = scA-scC,
         counter_D_abs_diff = scA-scD,
         counter_E_abs_diff = scA-scE,
         
         counter_B_rel_diff = scB/scA,
         counter_C_rel_diff = scC/scA,
         counter_D_rel_diff = scD/scA,
         counter_E_rel_diff = scE/scA,
         
         BC_abs_diff = scB-scC,
         BD_abs_diff = scB-scD,
         CE_abs_diff = scC-scE,
         DE_abs_diff = scD-scE,
         
         BC_rel_diff = scC/scB,
         BD_rel_diff = scD/scB,
         CE_rel_diff = scE/scC,
         DE_rel_diff = scE/scD) %>%
  group_by(model_name, location,target) %>%
  dplyr::summarize(check=n(),
                   
                   counter_B_abs_diff_se = sd(counter_B_abs_diff, na.rm = TRUE)/sqrt(sum(!is.na(counter_B_abs_diff))),
                   counter_C_abs_diff_se = sd(counter_C_abs_diff, na.rm = TRUE)/sqrt(sum(!is.na(counter_C_abs_diff))),
                   counter_D_abs_diff_se = sd(counter_D_abs_diff, na.rm = TRUE)/sqrt(sum(!is.na(counter_D_abs_diff))),
                   counter_E_abs_diff_se = sd(counter_E_abs_diff, na.rm = TRUE)/sqrt(sum(!is.na(counter_E_abs_diff))),
                   
                   counter_B_rel_diff_se = sd(counter_B_rel_diff, na.rm = TRUE)/sqrt(sum(!is.na(counter_B_rel_diff))),
                   counter_C_rel_diff_se = sd(counter_C_rel_diff, na.rm = TRUE)/sqrt(sum(!is.na(counter_C_rel_diff))),
                   counter_D_rel_diff_se = sd(counter_D_rel_diff, na.rm = TRUE)/sqrt(sum(!is.na(counter_D_rel_diff))),
                   counter_E_rel_diff_se = sd(counter_E_rel_diff, na.rm = TRUE)/sqrt(sum(!is.na(counter_E_rel_diff))),
                   
                   
                   BC_abs_diff_se = sd(BC_abs_diff, na.rm = TRUE)/sqrt(sum(!is.na(BC_abs_diff))),
                   DE_abs_diff_se = sd(DE_abs_diff, na.rm = TRUE)/sqrt(sum(!is.na(DE_abs_diff))),
                   
                   BC_rel_diff_se = sd(BC_rel_diff, na.rm = TRUE)/sqrt(sum(!is.na(BC_rel_diff))),
                   DE_rel_diff_se = sd(DE_rel_diff, na.rm = TRUE)/sqrt(sum(!is.na(DE_rel_diff))),
                   
                   counter_B_abs_diff_mn = mean(counter_B_abs_diff, na.rm = TRUE),
                   counter_C_abs_diff_mn = mean(counter_C_abs_diff, na.rm = TRUE),
                   counter_D_abs_diff_mn = mean(counter_D_abs_diff, na.rm = TRUE),
                   counter_E_abs_diff_mn = mean(counter_E_abs_diff, na.rm = TRUE),
                   
                   counter_B_rel_diff_mn = mean(counter_B_rel_diff, na.rm = TRUE),
                   counter_C_rel_diff_mn = mean(counter_C_rel_diff, na.rm = TRUE),
                   counter_D_rel_diff_mn = mean(counter_D_rel_diff, na.rm = TRUE),
                   counter_E_rel_diff_mn = mean(counter_E_rel_diff, na.rm = TRUE),
                   
                   BC_abs_diff_mn = mean(BC_abs_diff, na.rm = TRUE),
                   DE_abs_diff_mn = mean(DE_abs_diff, na.rm = TRUE),
                   
                   BC_rel_diff_mn = mean(BC_rel_diff, na.rm = TRUE),
                   DE_rel_diff_mn = mean(DE_rel_diff, na.rm = TRUE))




res_compares_m_lng <- res_compares_m %>%
  pivot_longer(cols=c(counter_B_abs_diff_se:DE_rel_diff_mn)) %>%
  mutate(measure=ifelse(stringr::str_detect(name,"se"),"se","mn"),
         diff_rel = ifelse(stringr::str_detect(name,"abs"),"diff","rel"),
         vax_lvl = ifelse(stringr::str_detect(name,"B|C"), "High-risk booster", "All booster"),
         compare=ifelse(stringr::str_detect(name,"counter_C|counter_E"), 
                        "Early immunization vs Counterfactual", 
                        ifelse(stringr::str_detect(name,"counter_B|counter_D"), 
                               "Classic immunization vs Counterfactual", 
                               "Early vs Classic immunization")),
         target_labs = ifelse(stringr::str_detect(target, "inc death"), "Deaths", "Hospitalizations")) %>%
  select(-name,-check) %>%
  pivot_wider(values_from = value, names_from = measure) %>% mutate(matched = "YES")

# merge(res_compares_lng, 
#       res_compares_m_lng %>% dplyr::rename(se_match = se, mn_match = mn) %>% dplyr::select(-matched),
#       by = c("model_name", "location", "target", "vax_lvl", "diff_rel", "compare", "target_labs"), all=TRUE)

res_compares_m_lng -> res_compares_lng

# res_compares_lng =  res_compares_lng %>% filter(model_name %in% unmatched_models) %>%
#   mutate(matched="NO") %>%
#   bind_rows(res_compares_m_lng) %>%
#   arrange(model_name)





# PLOT --------------------------------------------------------------------

## Gdt the random effect models for each level.
cex_val = 1
refline = 0
outcome_string  = "(Number Averted)"
outcome_colors = c("red","black")
tgt_vec <- c("Hospitalizations", "Deaths")



# ~ Absolute Differences --------------------------------------------------

## switch to 3x2 layout, absolute
# plot into pdf

mdls <- list()
figs <- list()
meta_res_abs <- tibble()

for (vax_level in c("All booster", "High-risk booster")) {
  
  vax_level_lab <- tolower(gsub(" |-", "", vax_level))
  
  png(filename = file.path(plot_dir,
                           paste0(meta_name, '_abs_', vax_level_lab, '_', gsub("-","_", age_group_tmp), loc_subname, '.png')),
      width = 1300, height = 1000, units = 'px', pointsize = 14.5)
  
  options(repr.plot.width=120, repr.plot.height=125)
  par(mfrow=c(3,2))
  
  for (comparison in c("Classic immunization vs Counterfactual", "Early immunization vs Counterfactual", "Early vs Classic immunization")) {
    mdls[[comparison]] <- list()
    figs[[comparison]] <- list()
    
    mdls[[comparison]][[vax_level]] <- list()
    figs[[comparison]][[vax_level]] <- list()
    for (tgt in tgt_vec) {
      outcome_col_i <- which(tgt_vec == tgt)
      meta <- metafor::rma.uni(yi=mn, sei=se, slab=model_name,
                               data=filter(res_compares_lng , str_detect(target, "inc") &
                                             location==loc_select & vax_lvl==vax_level &
                                             target_labs==tgt & compare==comparison &
                                             diff_rel=="diff"),
                               control=list(stepadj=0.5, maxiter=10000))
      
      mdls[[comparison]][[vax_level]][[tgt]] <- meta
      figs[[comparison]][[vax_level]][[tgt]] <- invisible(metafor::forest(meta,
                                                                          digits=0,
                                                                          col=outcome_colors[outcome_col_i],
                                                                          cex=cex_val,
                                                                          header=T,
                                                                          refline=refline,
                                                                          xlab=sprintf("%s (Averted) - %s, %s%s",
                                                                                       tgt, gsub("immunization", "vax", comparison), vax_level, age_text_snip)))
      meta_res_abs <- meta_res_abs %>% bind_rows( 
        tibble(comp = comparison, vax_lvl = vax_level, target = tgt, 
               mean = as.numeric(meta[["b"]]),
               lb = meta[["ci.lb"]],
               ub = meta[["ci.ub"]],
               se = meta[["se"]],
               n_ests = length(meta[["yi"]]))
      )
      
    }
  }
  dev.off()
}
gc()


# ~~ Plot Pooled Abs Diffs ------------------------------------------------


mdls <- list()
figs <- list()
meta_res_abs <- tibble()
for (vax_level in c("All booster", "High-risk booster")) {
  mdls[[vax_timing]] <- list()
  figs[[vax_timing]] <- list()
  for (comparison in c("Classic immunization vs Counterfactual", "Early immunization vs Counterfactual", "Early vs Classic immunization")) {
    mdls[[vax_level]][[comparison]] <- list()
    figs[[vax_level]][[comparison]] <- list()
    for (tgt in tgt_vec) {
      outcome_col_i <- which(tgt_vec == tgt)
      meta <- metafor::rma.uni(yi=mn, sei=se, slab=model_name,
                               data=filter(res_compares_lng , str_detect(target, "inc") &
                                             location==loc_select & vax_lvl==vax_level &
                                             target_labs==tgt & compare==comparison &
                                             diff_rel=="diff"),
                               control=list(stepadj=0.5, maxiter=10000))
      pdf(file = NULL)  # opens a null graphics device
      
      mdls[[vax_level]][[comparison]][[tgt]] <- meta
      figs[[vax_level]][[comparison]][[tgt]] <- invisible(metafor::forest(meta,
                                                                          digits=0,
                                                                          col=outcome_colors[outcome_col_i],
                                                                          cex=cex_val,
                                                                          header=T,
                                                                          refline=refline,
                                                                          xlab=sprintf("%s (Averted) - %s, %s%s",
                                                                                       tgt, gsub("immunization", "vax", comparison), vax_level, age_text_snip)))
      dev.off()
      meta_res_abs <- meta_res_abs %>% bind_rows(
        tibble(comp = comparison, vax_lvl = vax_level, target = tgt,
               mean = as.numeric(meta[["b"]]),
               lb = meta[["ci.lb"]],
               ub = meta[["ci.ub"]],
               se = meta[["se"]],
               n_ests = length(meta[["yi"]]))
      )
      
    }
  }
}
# dev.off()



meta_res_abs %>% 
  mutate(vax_lvl=case_when(vax_lvl==c("All booster")~c("All individuals"), 
                           vax_lvl==c("High-risk booster")~c("65+ and high-risk individuals"))) -> meta_res_abs_plot

# plot just the pooled

# deaths_meta_fig <- meta_res_abs %>% 
#   mutate(vax_lvl = paste0(vax_lvl, " Immunization")) %>%
#   as_tibble() %>%
#   filter(target == "Deaths") %>%
#   ggplot(aes(y = comp)) +
#   geom_errorbarh(aes(xmin = lb, xmax = ub), height = .25) + 
#   geom_point(aes(x = mean), size = 3, pch = 15) +
#   facet_grid(vax_lvl ~ target) +
#   scale_x_continuous(label = comma) +
#   # xlab("Cumulative Difference") + 
#   xlab(NULL) + 
#   ylab(NULL) +
#   theme_bw()  + 
#   theme(text = element_text(size = 20)) 
# 
# hosp_meta_fig <- meta_res_abs %>% 
#   mutate(vax_lvl = paste0(vax_lvl, " Immunization")) %>%
#   as_tibble() %>%
#   filter(target == "Hospitalizations") %>%
#   ggplot(aes(y = comp)) +
#   geom_errorbarh(aes(xmin = lb, xmax = ub), height = .25) + 
#   geom_point(aes(x = mean), size = 3, pch = 15) +
#   facet_grid(vax_lvl ~ target) +
#   scale_x_continuous(label = comma) +
#   # xlab("Cumulative Difference") + 
#   xlab(NULL) + 
#   ylab(NULL) +
#   theme_bw()  + 
#   theme(text = element_text(size = 20)) 
# 
# plotcomb <- cowplot::plot_grid(
#   hosp_meta_fig +
#     theme(
#       strip.background.y = element_blank(),
#       strip.text.y = element_blank()
#     ),
#   deaths_meta_fig + ylab(NULL) + theme(axis.text.y = element_blank()),
#   nrow = 1,
#   rel_widths = c(2, 1)
# )
# x.grob <- textGrob("              Cumulative difference between vaccination scenarios,\n              April 27, 2025 to April 25, 2026",
#                    gp=gpar(fontsize=18))
# 
# y.grob <- textGrob("Vaccination timing comparison", 
#                    gp=gpar(fontsize=18), rot=90)
# 
# title <- textGrob(paste0("Individuals aged ", age_group_label), gp=gpar(fontsize=18), hjust = 1.62)
# 
# png(filename = file.path(plot_dir, 
#                          paste0(meta_name, '_abs_timing_pooled_', gsub("-","_", age_group_tmp), loc_subname, '.png')),
#     width = 12,
#     height = 6,
#     units = 'in',
#     res = 600)
# 
# grid.arrange(arrangeGrob(plotcomb, 
#                          left = y.grob, 
#                          bottom = x.grob))
# 
# dev.off()





# ~ Relative Differences --------------------------------------------------

###3x2 layout, relative
refline = 1

mdls <- list()
figs <- list()
meta_res_rel <- tibble()

for (vax_level in c("All booster", "High-risk booster")) {
  
  vax_level_lab <- tolower(gsub(" |-", "", vax_level))
  
  png(filename = file.path(plot_dir,
                           paste0(meta_name, '_rel_', vax_level_lab, '_', gsub("-","_", age_group_tmp), loc_subname, '.png')),
      width = 1300, height = 1000, units = 'px', pointsize = 14.5)
  
  options(repr.plot.width=120, repr.plot.height=125)
  par(mfrow=c(3,2))
  
  for (comparison in c("Classic immunization vs Counterfactual", "Early immunization vs Counterfactual", "Early vs Classic immunization")) {
    mdls[[comparison]] <- list()
    figs[[comparison]] <- list()
    
    mdls[[comparison]][[vax_level]] <- list()
    figs[[comparison]][[vax_level]] <- list()
    
    for (tgt in tgt_vec) {
      outcome_col_i <- which(tgt_vec == tgt)
      meta <- metafor::rma.uni(yi=mn, sei=se, slab=model_name,
                               data=filter(res_compares_lng , str_detect(target, "inc") &
                                             location==loc_select & vax_lvl==vax_level &
                                             target_labs==tgt & compare==comparison &
                                             diff_rel=="rel"),
                               control=list(stepadj=0.5, maxiter=10000))
      
      mdls[[vax_level]][[tgt]][[comparison]] <- meta
      figs[[vax_level]][[tgt]][[comparison]] <- invisible(metafor::forest(meta,
                                                                          digits=3,
                                                                          col=outcome_colors[outcome_col_i],
                                                                          cex=cex_val,
                                                                          header=T,
                                                                          refline=refline,
                                                                          xlab=sprintf("%s (Relative) - %s, %s%s",
                                                                                       tgt, gsub("immunization", "vax", comparison), vax_level, age_text_snip)))
      
      meta_res_rel <- meta_res_rel %>% bind_rows( 
        tibble(comp = comparison, vax_lvl = vax_level, target = tgt, 
               mean = as.numeric(meta[["b"]]),
               lb = meta[["ci.lb"]],
               ub = meta[["ci.ub"]],
               se = meta[["se"]],
               n_ests = length(meta[["yi"]]))
      )
    }
  }
  dev.off()
}


# ~~ Plot Pooled Relative Diffs ------------------------------------------------


mdls <- list()
figs <- list()
meta_res_rel <- tibble()
for (vax_level in c("All booster", "High-risk booster")) {
  mdls[[vax_level]] <- list()
  figs[[vax_level]] <- list()
  for (comparison in c("Classic immunization vs Counterfactual", "Early immunization vs Counterfactual", "Early vs Classic immunization")) {
    mdls[[vax_level]][[comparison]] <- list()
    figs[[vax_level]][[comparison]] <- list()
    outcome_col_i <- which(tgt_vec == tgt)
    for (tgt in tgt_vec) {
      outcome_col_i <- which(tgt_vec == tgt)
      meta <- metafor::rma.uni(yi=mn, sei=se, slab=model_name,
                               data=filter(res_compares_lng , str_detect(target, "inc") &
                                             location==loc_select  & vax_lvl==vax_level &
                                             target_labs==tgt & compare==comparison &
                                             diff_rel=="rel"),
                               control=list(stepadj=0.5, maxiter=10000))
      
      pdf(file = NULL)  # opens a null graphics device
      
      mdls[[vax_level]][[tgt]][[comparison]] <- meta
      # if (imm_esc == "Low"){
      figs[[vax_level]][[tgt]][[comparison]] <- invisible(metafor::forest(meta,
                                                                          digits=3,
                                                                          col=outcome_colors[outcome_col_i],
                                                                          cex=cex_val,
                                                                          header=T,
                                                                          refline=refline,
                                                                          xlab=sprintf("%s (Relative) - %s, %s",
                                                                                       tgt, comparison, vax_level)))
      # }
      dev.off()
      
      meta_res_rel <- meta_res_rel %>% bind_rows(
        tibble(comp = comparison, vax_lvl = vax_level, target = tgt,
               mean = as.numeric(meta[["b"]]),
               lb = meta[["ci.lb"]],
               ub = meta[["ci.ub"]],
               se = meta[["se"]],
               n_ests = length(meta[["yi"]]))
      )
    }
  }
}
# dev.off()





meta_res_rel %>% 
  mutate(vax_lvl=case_when(vax_lvl==c("All booster")~c("All individuals"), 
                           vax_lvl==c("High-risk booster")~c("65+ and high-risk individuals"))) -> meta_res_rel_plot

# plot just the pooled

library(scales)
library(grid)
library(gridExtra)

# deaths_meta_fig <- meta_res_rel %>% 
#   mutate(mean = 1 - mean, lb = 1-lb, ub = 1- ub) %>%
#   # mutate(vax_lvl = paste0(vax_lvl, " Immunization")) %>%
#   mutate(vax_lvl = paste0(vax_lvl)) %>%
#   as_tibble() %>%
#   filter(target == "Deaths") %>%
#   ggplot(aes(y = comp)) +
#   geom_errorbarh(aes(xmin = lb, xmax = ub), height = .25) + 
#   geom_point(aes(x = mean), size = 3, pch = 15) +
#   facet_grid(vax_lvl ~ target) +
#   scale_x_continuous(label = percent) +
#   # xlab("Cumulative Difference") + 
#   xlab(NULL) + 
#   ylab(NULL) +
#   theme_bw()  + 
#   theme(text = element_text(size = 20)) 
# 
# hosp_meta_fig <- meta_res_rel %>% 
#   mutate(mean = 1 - mean, lb = 1-lb, ub = 1- ub) %>%
#   # mutate(vax_lvl = paste0(vax_lvl, " Immunization")) %>% 
#   mutate(vax_lvl = paste0(vax_lvl)) %>%
#   as_tibble() %>%
#   filter(target == "Hospitalizations") %>%
#   ggplot(aes(y = comp)) +
#   geom_errorbarh(aes(xmin = lb, xmax = ub), height = .25) + 
#   geom_point(aes(x = mean), size = 3, pch = 15) +
#   facet_grid(vax_lvl ~ target) +
#   scale_x_continuous(label = percent) +
#   # xlab("Cumulative Difference") + 
#   xlab(NULL) + 
#   ylab(NULL) +
#   theme_bw()  + 
#   theme(text = element_text(size = 20)) 
# 
# plotcomb <- cowplot::plot_grid(
#   hosp_meta_fig +
#     theme(
#       strip.background.y = element_blank(),
#       strip.text.y = element_blank()
#     ),
#   deaths_meta_fig + ylab(NULL) + theme(axis.text.y = element_blank()),
#   nrow = 1,
#   rel_widths = c(2, 1)
# )
# 
# # x.grob <- textGrob("              Cumulative percent prevented at 2 years", 
# x.grob <- textGrob("              Cumulative difference between vaccination scenarios,\n              April 27, 2025 to April 25, 2026",
#                    gp=gpar(fontsize=18))
# 
# y.grob <- textGrob("Vaccination timing comparison", 
#                    gp=gpar(fontsize=18), rot=90)
# 
# png(filename = file.path(plot_dir, 
#                          paste0(meta_name, '_rel_pooled_timing_', gsub("-","_", age_group_tmp), loc_subname, '.png')),
#     width = 12,
#     height = 6,
#     units = 'in',
#     res = 600)
# 
# grid.arrange(arrangeGrob(plotcomb, 
#                          left = y.grob, 
#                          bottom = x.grob))
# 
# dev.off()


# ~ Plot by Model ---------------------------------------------------------

theme_set(theme_bw())
options(repr.plot.width=8,repr.plot.height=10)

meta_res_rel_plot$target <- factor(meta_res_rel_plot$target, levels=c("Hospitalizations", "Deaths"))
meta_res_rel_plot$comp <- factor(meta_res_rel_plot$comp, levels=c("Early vs Classic immunization", "Classic immunization vs Counterfactual", "Early immunization vs Counterfactual"))
meta_res_rel_plot$vax_lvl <- factor(meta_res_rel_plot$vax_lvl, levels=c("65+ and high-risk individuals", "All individuals"))

meta_res_rel_plot %>% mutate(table=paste0(format(round((1-mean)*100,0), nsmall=0),"%"," (",
                                          format(round((1-ub)*100,0), nsmall=0),"-",
                                          format(round((1-lb)*100,0), nsmall=0),"%",")")) %>%
  ggplot(aes(y=comp))+
  facet_wrap(~target, nrow = 2)+
  geom_errorbarh(aes(xmin = 1-lb, xmax = 1-ub, group=vax_lvl, color=vax_lvl), 
                 stat="identity", position = position_dodge(width = 0.9), height = .2) +
  geom_point(aes(x = 1-mean, group=vax_lvl, color=vax_lvl), 
             stat="identity", position = position_dodge(width = 0.9), size = 3, pch = 16) +
  geom_text(aes(x = 1-mean, label = table, group = vax_lvl),
            stat="identity", position = position_dodge(width = 1), vjust = -0.9, size = 4.5) +
  scale_color_manual("Immunization target", values=c("#1380A1", "#990000")) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(text = element_text(size=15, family="sans",color="black"),
        axis.text = element_text(size=15, family="sans",color="black"),
        plot.title = element_text(size=18, family="sans",color="black", hjust=0.5),
        strip.text = element_text(size = 15, family="sans",color="black"),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size=15), legend.text = element_text(size=14),
        panel.spacing = unit(2, "lines"),
        legend.position = c(0.67, 0.61),
        legend.background = element_rect(fill = "white", colour = NA)) +
  scale_x_continuous(breaks=c(0, .1, .2, .3, .4), labels=c("0%", "10%", "20%", "30%", "40%")) +
  # labs(y="Vaccination scenario comparison \n", x="", title="Percent prevented (95% CI) \n")
  labs(y="Vaccination scenario comparison \n", 
       x="\n Cumulative percent prevented by vaccination, \n April 27, 2025 to April 25, 2026", 
       title="Percent prevented (95% CI)", ifelse(age_group_tmp=="0-130", "", paste0("\n  among those aged ", age_group_label))) -> rel_vaccine_impact

if (loc_abbr == "US") {
  convert_to_K <- function(number) {
    format(ifelse(abs(number) < 1000, round(number,0), round(number/1000, 0)*1000), nsmall=0, big.mark=",", justify = "left", trim = TRUE)
  }
} else {
  convert_to_K <- function(number) {
    format(ifelse(abs(number) >= 5000, round(number/1000, 0)*1000, 
                  ifelse(abs(number) >= 1000, round(number/100, 0)*100, 
                         ifelse(abs(number) >= 100, round(number/10, 0)*10, 
                                round(number,0)))), 
           nsmall=0, big.mark=",", justify = "left", trim = TRUE)
  }
}

meta_res_abs_plot %>% 
  mutate(table=case_when(vax_lvl==c("All individuals") ~ 
                           paste0("All: ", convert_to_K(mean)," (",convert_to_K(lb),"-",convert_to_K(ub),")"),
                         vax_lvl==c("65+ and high-risk individuals") ~ 
                           paste0("High-risk: ", convert_to_K(mean)," (",convert_to_K(lb),"-",convert_to_K(ub),")"))) -> table_total

table_total$target <- factor(table_total$target, levels=c("Hospitalizations", "Deaths"))
table_total$comp <- factor(table_total$comp, levels=c("Early vs Classic immunization", "Classic immunization vs Counterfactual", "Early immunization vs Counterfactual"))
table_total$vax_lvl <- factor(table_total$vax_lvl, levels=c("65+ and high-risk individuals", "All individuals"))

theme_set(theme_bw())
options(repr.plot.width=5,repr.plot.height=10)

table_total %>% mutate(mean=0.1) %>%
  ggplot(aes(y=comp))+
  facet_wrap(~target, nrow = 2)+
  geom_point(aes(x = 1-mean, group=vax_lvl, color=vax_lvl), 
             stat="identity", position = position_dodge(width = 1), size = 3, pch = 16) +
  geom_text(aes(x = 1-mean, label = table, group = vax_lvl),
            stat="identity", position = position_dodge(width = 0.75), size = 5) +
  scale_color_manual("Immunization target", values=c("#FFFFFF", "#FFFFFF")) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(text = element_text(size=15, family="sans",color="black"),
        plot.title = element_text(size=18, family="sans",color="black", hjust=0.5),
        strip.text = element_text(size = 15, family="sans",color="black"),
        legend.title = element_text(size=15), legend.text = element_text(size=14),
        panel.spacing = unit(2, "lines"),
        legend.position = 'none', 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major=element_blank()) +
  scale_x_continuous(breaks=c(0, .1, .2, .3, .4), labels=c("0%", "10%", "20%", "30%", "40%"))+
  labs(y="", x="\n Cumulative difference between scenarios, \n April 27, 2025 to April 25, 2026", 
       title="Total prevented (95% CI)", ifelse(age_group_tmp=="0-130", "", paste0("\n  among those aged ", age_group_label))) -> rel_vaccine_impact_table


options(repr.plot.width=14,repr.plot.height=10.5)
cowplot::plot_grid(rel_vaccine_impact, rel_vaccine_impact_table, align = "h", nrow=1,
                   rel_widths = c(2, 1)) -> Figure

numdiff_for_text <- numdiff_for_text %>% bind_rows(table_total %>% mutate(location = loc_abbr, age = age_group_label, axis_comp = "timing"))
reldiff_for_text <- reldiff_for_text %>% bind_rows(meta_res_rel_plot %>% mutate(location = loc_abbr, age = age_group_label, axis_comp = "timing"))


ggsave(file.path(plot_dir, 
                 paste0("combined_timing_", gsub("-","_", age_group_tmp), loc_subname, ".png")), Figure, width=14, height=panel_height, bg='white', dpi=300)



##### 
# meta_res_abs %>% head()


# remove all objects that start with "res_" and "meta_" and "temp"
rm(list=ls(pattern = "^res_|^meta_|^temp|^table_"))
gc()

