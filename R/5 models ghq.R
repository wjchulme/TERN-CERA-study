

# prelims -----------------------------------------------------------------


library('tidyverse')
library('lme4')
#library('nlme')
#library('mice')
library('viridis')
library('patchwork')
library('ggbeeswarm')
library('ggeffects')



summary_table <- function(data, catvar, contvar){
  
  dat = data %>% 
    mutate(
      catvar = data[[catvar]],
      catvar_explicit_na=fct_explicit_na(catvar, na_level="(Missing)"),
      contvar = data[[contvar]],
      contvar_explicit_na = if_else(is.na(contvar), -2, contvar)
      
    )
  
  Qdat <- dat %>%
    group_by(survey_chr, catvar_explicit_na) %>%
    summarise(
      n=n(),
      mean=mean(contvar, na.rm=TRUE),
      min=min(contvar, na.rm=TRUE),
      Q10=quantile(contvar, 0.1, na.rm=TRUE),
      Q25=quantile(contvar, 0.25, na.rm=TRUE),
      Q50=quantile(contvar, 0.5, na.rm=TRUE),
      Q75=quantile(contvar, 0.75, na.rm=TRUE),
      Q90=quantile(contvar, 0.9, na.rm=TRUE),
      max=max(contvar, na.rm=TRUE),
    )
  
  Qdat
}

#MIobj <- read_rds("syntheticdata.rds")

factor_dct <- read_rds(file=here::here("processed-data",  "factor_dictionary.rds"))

#ll rows in dataset consented or not
data_all <- read_rds(file=here::here("processed-data","data_all.rds"))

# removing duplicates
#data_distinctemails <- read_rds(file=here::here("processed-data", "data_distinctemails.rds"))

# all survey 1 consents
#data_s1 <- read_rds(file=here::here("processed-data", "data_s1.rds"))

# at least survey 1 consents
data_sany <- read_rds(file=here::here("processed-data", "data_sany.rds"))

# consented to all 3 surveys
#data_s123 <- read_rds(file=here::here("processed-data", "data_s123.rds")) 

# baseline info
# data_baseline <- data_s123 %>%
#   select(-ends_with("_s1"), -ends_with("_s2"), -ends_with("_s3"))

data_longany <- read_rds( file=here::here("processed-data", "data_longany.rds"))
data_long123 <- read_rds(file=here::here("processed-data", "data_long123.rds")) %>% 
  mutate(
    ghqsum0123_jit = ghqsum0123 + runif(n(), -0.2,0.2),
  ) %>% droplevels()


# 
# data_mod_MI <- data_sany_MI %>%
#   filter(
#     gender %in% c("Male", "Female"),
#     !is.na(ghqsum0123),
#     #!is.na(timestamp),
#     !is.na(age2),
#     !is.na(seniority),
#     !is.na(comb_dept2),
#   ) %>%
#   mutate(
#     ptid = paste0("p", ptid),
#   ) %>% droplevels()
# 


## create model dataset for ghq


data_descr <- data_long123 

data_mod <- data_long123 %>%
  filter(
    !is.na(ghqsum0123)
  ) %>% 
left_join(
  (data_all %>% select(ptid, comb_ppe.dondof_any_s1, comb_ppe.fit_any_s1, comb_ppe.exp_any_s1, comb_practicaled_any_s1)), by = "ptid"
) %>% droplevels()




# create models ------------------------------------------------------

# choose variables
# can only use those that were available across all three surveys, or those that are time-invariant

univariable_ord <- c(
  "age2", 
  "guid_govt", "guid_college", "guid_trust", "guid_dept", "guid_sm", "guid_blogs", "guid_lit",
  "exposure", "exposure_confirmed2", 
  NULL
)


univariable_div <- c(
  "confident_infectiontraining", "prepared_suspected", "prepared_normal", 
  "perceived_risk", 
  "worry_personal", "worry_family",
  #"supportcol_s2", "supportfam_s2", "supportcol_s3", "supportfam_s3"
  NULL
)

univariable_cat <- c(
  "gender", "ethnicity", "ethnicity3", "seniority","comb_dept2", "region", #"redeployed_s1", 
  "comb_outbreak_any",
  "comb_ppe.dondof_any_s1", "comb_ppe.fit_any_s1", "comb_ppe.exp_any_s1", "comb_practicaled_any_s1",
  "increasesympmedical", 
  "increasesympmental", "trauma",
  NULL
)

univariable_cont <- c("agecont", "iesrsum0123_s1", "iesrsum0123_s2", "iesrsum0123_s3")


univariable_list <- c(univariable_ord, univariable_div, univariable_cat)
#univariable_list <- c("ethnicity3")


# loop through variables to create models for each variable
# note that lmer copies the environment into the saved model object so 
# the `models` dataset is very large
# this is compressed when saved

models <- 
  tibble(
    variables = univariable_list,
    variables_strip = str_remove(variables, "\\_s\\d$")
  ) %>%
  left_join(
    bind_rows(
      factor_dct %>% select(R, name, full, vartype) %>% mutate(R=str_remove(R, "\\_s\\d$")),
    ) %>% distinct(R, .keep_all = TRUE),
    by=c("variables_strip"="R")
  ) %>%
  mutate(
    class = map_chr(variables, ~class(data_mod[[.x]])),
    unique = map_int(variables, ~n_distinct(data_mod[.])),
    n = map_int(variables, ~sum(!is.na(data_mod[.]))),
    n_missing = map_int(variables, ~sum(is.na(data_mod[.]))),
    pct_missing = map_dbl(variables, ~mean(is.na(data_mod[.]))),
    dat = map(variables, ~data_mod[!is.na(data_mod[[.]]), ]),
    
    summary_table = map2(list(data_mod), variables, ~summary_table(.x, .y, "ghqsum0123")),
    
    mod.lm_0 = map(dat, ~lmerTest::lmer(data=.x, formula=as.formula("ghqsum0123 ~  (1| ptid)"))),
    mod.lm_s0 = map(dat, ~lmerTest::lmer(data=.x, formula=as.formula("ghqsum0123 ~ survey_chr + (1| ptid)"))),
    mod.lm_s1 = map2(dat, variables, ~lmerTest::lmer(data=.x, formula=as.formula(paste0("ghqsum0123 ~ survey_chr +", .y, "+ (1| ptid)")))),
    mod.lm_s2 = map2(dat, variables, ~lmerTest::lmer(data=.x, formula=as.formula(paste0("ghqsum0123 ~ survey_chr *", .y, "+ (1| ptid)")))),

    R2nakagawa.marg_0 = map_dbl(mod.lm_s0, ~(performance::r2_nakagawa(.)$R2_marginal)),
    R2nakagawa.cond_0 = map_dbl(mod.lm_s0, ~(performance::r2_nakagawa(.)$R2_conditional)),
    R2cor.marg_0 = map_dbl(mod.lm_s0, ~cor(predict(., re.form=~0), .@resp$y)^2),
    R2cor.cond_0 = map_dbl(mod.lm_s0, ~cor(predict(.), .@resp$y)^2),
    R2xu.marg_0 = map_dbl(mod.lm_s0, ~ 1 - var(.@resp$y-predict(., re.form=~0))/var(.@resp$y)),
    R2xu.cond_0 = map_dbl(mod.lm_s0, ~ 1 - var(.@resp$y-predict(.))/var(.@resp$y)),
    
    R2nakagawa.marg_1 = map_dbl(mod.lm_s1, ~(performance::r2_nakagawa(.)$R2_marginal)),
    R2nakagawa.cond_1 = map_dbl(mod.lm_s1, ~(performance::r2_nakagawa(.)$R2_conditional)),
    R2cor.marg_1 = map_dbl(mod.lm_s1, ~cor(predict(., re.form=~0), .@resp$y)^2),
    R2cor.cond_1 = map_dbl(mod.lm_s1, ~cor(predict(.), .@resp$y)^2),
    R2xu.marg_1 = map_dbl(mod.lm_s1, ~ 1 - var(.@resp$y-predict(., re.form=~0))/var(.@resp$y)),
    R2xu.cond_1 = map_dbl(mod.lm_s1, ~ 1 - var(.@resp$y-predict(.))/var(.@resp$y)),
    
    R2nakagawa.marg_2 = map_dbl(mod.lm_s2, ~(performance::r2_nakagawa(.)$R2_marginal)),
    R2nakagawa.cond_2 = map_dbl(mod.lm_s2, ~(performance::r2_nakagawa(.)$R2_conditional)),
    R2cor.marg_2 = map_dbl(mod.lm_s2, ~cor(predict(., re.form=~0), fitted(.))^2),
    R2cor.cond_2 = map_dbl(mod.lm_s2, ~cor(predict(.), .@resp$y)^2),
    R2xu.marg_2 = map_dbl(mod.lm_s2, ~ 1 - var(.@resp$y-predict(., re.form=~0))/var(.@resp$y)),
    R2xu.cond_2 = map_dbl(mod.lm_s2, ~ 1 - var(.@resp$y-predict(.))/var(.@resp$y)),
    
    lrt_0s0 = map2(mod.lm_s0, mod.lm_0, ~anova(.x, .y)),
    lrt_s0s1 = map2(mod.lm_s1, mod.lm_s0, ~anova(.x, .y)),
    lrt_s0s2 = map2(mod.lm_s2, mod.lm_s0, ~anova(.x, .y)),
    lrt_s1s2 = map2(mod.lm_s2, mod.lm_s1, ~anova(.x, .y)),

    pval_0s0 = map_dbl(lrt_0s0, ~.$`Pr(>Chisq)`[2]),
    pval_s0s1 = map_dbl(lrt_s0s1, ~.$`Pr(>Chisq)`[2]),
    pval_s0s2 = map_dbl(lrt_s0s2, ~.$`Pr(>Chisq)`[2]),
    pval_s1s2 = map_dbl(lrt_s1s2, ~.$`Pr(>Chisq)`[2]),
    
  ) #%>%
  #mutate(
  #  mod.lm_0 = map(mod.lm_0, function(x) {
  #    attr(x$terms, ".Environment") <- NULL
  #    return(x)
  #  }),
  #  mod.lm_s0 = map(mod.lm_s0, function(x) {
  #    attr(x$terms, ".Environment") <- NULL
  #    return(x)
  #  }),
  #  mod.lm_s1 = map(mod.lm_s1, function(x) {
  #    attr(x$terms, ".Environment") <- NULL
  #    return(x)
  #  }),
  #  mod.lm_s2 = map(mod.lm_s2, function(x) {
  #    attr(x$terms, ".Environment") <- NULL
  #    return(x)
  #  }),
  #)


models_pvals <- models %>% 
  select(name, vartype, n, n_missing, pct_missing, starts_with("pval"), starts_with("R2"))

## possibly quicker to re-run that compress and reload! so may delete
write_rds(models, file=here::here("outputs", "GHQ", "models_ghq_lmer.rds"), compress="gz")
write_rds(models_pvals, file=here::here("outputs", "GHQ",  "models_ghq_lmer_pvaltable.rds"), compress="gz")



# create plots ------------------------------------------------------------


models_plots <- models %>%
  transmute(
    
    variables, name, full,
    n,
    n_missing,
    pct_missing,
    pval_0s0,
    pval_s0s1,
    pval_s0s2,
    pval_s1s2,
    
    colourtype = map(variables, 
       ~{case_when(
        . %in% univariable_cat ~ "qual",
        . %in% univariable_ord ~ "seq",
        . %in% univariable_div ~ "div",
        . %in% univariable_cont ~ "cont",
        TRUE ~ "qual"
      ) 
    }),
    
    effects_s0 =  map(mod.lm_s0, ~ggemmeans(.x, terms = c("survey_chr"))),
    effects_s1 =  map2(mod.lm_s1, variables, ~ggemmeans(.x, terms = c(.y, "survey_chr"))),
    effects_s2 =  map2(mod.lm_s2, variables, ~ggemmeans(.x, terms = c(.y, "survey_chr"))),
    

    plotraw_hist = pmap(lst(catvar=variables, contvar="ghqsum0123", data=list(data_descr), xlab = "GHQ-12 score"), plotrawdata_hist),
    plotraw_jit = pmap(lst(catvar=variables, contvar="ghqsum0123", data=list(data_descr), xlab = "GHQ-12 score", breaks = list(c(0, 10, 20, 30, 36)), labels = list(c("0", "10", "20", "30", "36"))), plotrawdata_jit),
    ploteffect_1 = pmap(lst(effects=effects_s1, limits = list(c(5, 25)), colourtype = colourtype, xlab="Average GHQ-12 score", title=name), ploteffect),
    ploteffect_2 = pmap(lst(effects=effects_s2, limits = list(c(5, 25)), colourtype = colourtype, xlab="Average GHQ-12 score", title=name), ploteffect),
    ploteffect_1 = align_patches(ploteffect_1),
    ploteffect_2 = align_patches(ploteffect_2),
    
   # plot_legend = pmap(lst(variable=variables, data=dat, colourtype = colourtype), ploteffectlegend),

    # patchwork_plot = pmap(lst(ploteffect_1, ploteffect_2, title=str_wrap(name, 80)), 
    #                       function(ploteffect_1, ploteffect_2, title){
    #   
    #   plot1 = ploteffect_1 #+ theme(legend.position="none", axis.text.y = element_blank())
    #   plot2 = ploteffect_2 #+ theme(legend.position="none", axis.text.y = element_blank())
    # 
    # 
    #   wrap_plots(plot1, plot2) +  
    #     plot_layout(guides = "collect") +
    #     plot_annotation(title = title, theme = theme(plot.title = element_text(hjust = 0.5)))+
    #     plot_layout(
    #       nrow=1
    #     )
    #   
    # }),
    
  )

write_rds(models_plots, file=here::here("outputs", "GHQ", "models_ghq_plots.rds"), compress="gz")


# Save tables --------------------------------------------------------------
#models_plots <- read_rds(file=here::here("outputs", "GHQ", "models_plots.rds"))

write_csv(models_pvals, file = here::here("outputs", "GHQ", "models", "model_ghq_summary_stats.csv"))


models %>%
  select(variables, summary_table) %>%
  pwalk(
    function(variables, summary_table){
      write_csv(summary_table, file = here::here("outputs", "GHQ", "descriptive", "tables", paste0("summary_",variables, ".csv")))
    }
  )


models_plots %>%
  select(variables, effects=effects_s2) %>%
  pwalk(
    function(variables,effects){
      write_csv(effects, file = here::here("outputs", "GHQ", "models", "tables", paste0("model2_",variables, ".csv")))
    }
  )


# Save plots --------------------------------------------------------------


models_plots %>% 
  transmute(
    plot = plotraw_hist,
    facetheight = 1.8,
    units = "cm",
    height = pmap_dbl(list(plot, units, facetheight), function(plot, units, facetheight){plotHeight(plot, units) + plotNpanelrows(plot)*facetheight}),
    width = 20,
    limitsize = FALSE,
    filename = str_c("hist_", variables, ".svg"),
    path = here::here("outputs", "GHQ", "descriptive", "figures"),
  ) %>%
  select(-facetheight) %>%
  pwalk(ggsave)


models_plots %>% 
  transmute(
    plot = plotraw_jit,
    jitheight = 1,
    units = "cm",
    height = pmap_dbl(list(plot, units, jitheight), function(plot, units, jitheight){plotHeight(plot, units) + plotNpanelrows(plot)*plotNyscales(plot)*jitheight}),
    width = 20,
    limitsize = FALSE,
    filename = str_c("jit_", variables, ".svg"),
    path = here::here("outputs", "GHQ", "descriptive", "figures"),
  ) %>%
  select(-jitheight) %>%
  pwalk(ggsave)

models_plots %>% 
  transmute(
    plot = ploteffect_1,
    lineheight = 0.5,
    units = "cm",
    height = pmap_dbl(list(plot, units, lineheight), function(plot, units, lineheight){plotHeight(plot, units) + plotNpanelrows(plot)*plotNyscales(plot)*lineheight}),
    width=20, 
    limitsize=FALSE,
    filename = str_c("model1_", variables, ".svg"),
    path = here::here("outputs", "GHQ", "models", "figures"),
  ) %>%
  select(-lineheight) %>%
  pwalk(ggsave)
  
models_plots %>% 
  transmute(
    plot = ploteffect_2,
    lineheight = 0.5,
    units = "cm",
    height = pmap_dbl(list(plot, units, lineheight), function(plot, units, lineheight){plotHeight(plot, units) + plotNpanelrows(plot)*plotNyscales(plot)*lineheight}),
    width=20, 
    limitsize=FALSE,
    filename = str_c("model2_", variables, ".svg"),
    path = here::here("outputs", "GHQ", "models", "figures"),
  ) %>%
  select(-lineheight) %>%
  pwalk(ggsave)




# R2 plots ----------------------------------------------------------------


#models <- read_rds(path=here::here("figures", "survey-123", "GHQ", "models.rds"))

plot_varexplained <- models %>%
  filter(variables %ni% c("ethnicity", "ethnicity2")) %>%
  mutate(
    name.orderedr2nakagawa = forcats::fct_reorder(name, R2nakagawa.marg_2, .fun=mean, .desc = FALSE),
    #    name.orderedr2xu = forcats::fct_reorder(name, R2xu.marg_1, .fun = mean, .desc = TRUE)
  ) %>%
  ggplot() +
  #geom_point(aes(y=name.orderedr2nakagawa, R2nakagawa.marg_0), alpha=0.7)+
  #geom_point(aes(y=name.orderedr2nakagawa, R2nakagawa.marg_1), colour="blue", alpha=0.8)+
  geom_point(aes(y=name.orderedr2nakagawa, R2nakagawa.marg_2), colour="orange", alpha=0.8)+
  geom_linerange(aes(y=name.orderedr2nakagawa, xmin=0, xmax=R2nakagawa.marg_2), alpha=0.5)+
  geom_text(aes(y=name.orderedr2nakagawa, x=R2nakagawa.marg_2+0.0005, label=scales::label_number(accuracy=0.001)(R2nakagawa.marg_2)), hjust=0, size=3)+
  ggforce::facet_col(facets=vars(vartype), scales="free_y", space="free")+
  coord_cartesian(clip = 'off')+
  labs(
    title="Variation in GHQ-12 explained by each model",
    x=expression("Nagakawa's"~R^2), y=NULL#,
    #caption = 
    #  expression("Nagakawa's "~R^2~"(marginal): the proportion of outcome variation explained by model 0 (black dot), 1 (blue dot), and 2 (orange dot).")
  )+
  scale_x_continuous(limits=c(0,NA), expand=expansion(mult=c(0,.1)))+
  theme_bw()+
  theme(
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    #axis.line.x = element_line(colour = "black"),
    #panel.grid = element_blank(),
    #panel.grid.major.x = element_line(colour="grey", size=0.2),
    panel.grid.major.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    strip.background = element_blank(),
    
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, face= "italic"),
    strip.text.y = element_text(angle = 0),
    legend.position = "left"
  )

ggsave(
  plot = plot_varexplained,
  height =20, width=20, units = "cm",
  limitsize=FALSE,
  filename = "GHQ-12 variance explained model 2.svg",
  path = here::here("outputs", "GHQ", "models")
)