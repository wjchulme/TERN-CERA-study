
# libraries ----

source(here::here("R","0 preliminaries.R"))
source(here::here("R","0 plot functions.R"))
library('tidyverse')
library('viridis')
#library('ggbeeswarm')


# functions ----

Qdata <- function(data, groupvar, contvar){
  
  data %>%
    group_by({{groupvar}}) %>%
    summarise(
      n=n(),
      n_valid=sum(!is.na({{contvar}})),
      pct_valid = n_valid/n,
      Q10=quantile({{contvar}}, 0.1, na.rm=TRUE),
      Q25=quantile({{contvar}}, 0.25, na.rm=TRUE),
      Q50=quantile({{contvar}}, 0.5, na.rm=TRUE),
      Q75=quantile({{contvar}}, 0.75, na.rm=TRUE),
      Q90=quantile({{contvar}}, 0.9, na.rm=TRUE),
      mean=mean({{contvar}}, na.rm=TRUE),
      bsci = list(Hmisc::smean.cl.boot({{contvar}}, conf.int=0.95, B=1000, reps=FALSE)),
      mean.ll = map_dbl(bsci, ~.[2]),
      mean.ul = map_dbl(bsci, ~.[3])
    ) %>% 
    select(-bsci) %>%
    ungroup() 
}





## themes ------------------------------------------------------------------

titlewrapwidth <- 70
ywrapwidth <- 40
theme.size <- 9
geom.text.size <- theme.size * (5/14) * 0.7

#nparticipants <- nrow(data_s1)


bar_theme <- 
  theme_bw(base_size = theme.size) + 
  theme(
    panel.border = element_blank(), #axis.line.x = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(colour="grey"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0, face= "italic")
  )


stack_theme <- 
  theme_bw(base_size = theme.size) + 
  theme(
    panel.border = element_blank(),# axis.line.x = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(colour="grey"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    
    legend.position = "bottom", 
    
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0, face= "italic")
  )



hist_theme <- 
  theme_bw(base_size = theme.size) + 
  theme(
    panel.border = element_blank(), axis.line.x = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour="grey"),
    strip.background = element_blank(),
    
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0, face= "italic")
  )

hist_facet_theme <-
  theme_bw(base_size = theme.size) +
  theme(
    panel.border = element_blank(),
    #axis.line.x = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "lightgrey"),
    strip.background = element_blank(),
    
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0, face = "italic"),
    strip.text = element_text(angle = 0, hjust = 1),
  )


## functions for getting plot data and plots ----------------------------------------------------

plotbardata <- function(data, var, filt=TRUE){
  # function to get data for simple barchart
    data %>% 
    filter(filt) %>%
    select(one_of(var)) %>%
    rename("variable"=all_of(var)) %>%
    group_by(variable) %>%
    tally() %>%
    mutate(
      variable_explicit_na=(fct_explicit_na(variable, na_level="(Missing)")),
      pct=n/sum(n),
      n_valid=if_else(variable_explicit_na=="(Missing)", NA_integer_, n),
      pct_valid = (n_valid/sum(n_valid, na.rm=TRUE)),
      pct_fmt = pct(pct),
      pct_valid_fmt = pct(pct_valid),
    )
}

plotmultibardata <- function(data, var, filt=TRUE){
  # function to get data for simple barchart for variable with multiple possible answers
  
  temp_levels <- factor_dct %>% filter(R==var) %>% pull(short) %>% `[[`(1)
  temp_labels <- factor_dct %>% filter(R==var) %>% pull(level) %>% `[[`(1)
  
  data %>%
    select(starts_with(paste0(var,"_"))) %>%
    summarise_all(
      .funs=(list(n=sum, pct=mean)),
      na.rm=TRUE
    ) %>%
    rename_all(
      ~stringr::str_sub(.,nchar(var)+2)
    ) %>%
    pivot_longer(
      cols=everything(),
      names_to=c("variable", ".value"),
      names_sep="_"
    ) %>%
    #add_row(variable=NA_character_, n=n_na, pct=pct_na) %>%
    mutate(
      variable=factor(variable, levels=temp_levels, labels=temp_labels),
      variable_explicit_na=(fct_explicit_na(variable, na_level="(Missing)")),
      n_valid=if_else(variable_explicit_na=="(Missing)", NA_integer_, n),
      pct_valid = (n_valid/sum(n_valid, na.rm=TRUE)),
      pct_fmt = pct(pct),
      pct_valid_fmt = pct(pct_valid),
    ) %>%
    arrange(variable)
  
}

plotfacethistdata <- function(data, catvar, contvar, filt=TRUE){
  #  function to get data for simple faceted histogram
  plotdata <-
  data %>% 
    filter(filt) %>%
    select(all_of(c(catvar, contvar))) %>%
    rename(variable=all_of(catvar), contvariable=all_of(contvar)) %>%
    mutate(
      variable_explicit_na=(fct_explicit_na(variable, na_level="(Missing)")),
    )
  
  Qdata(plotdata, variable_explicit_na, contvariable)
}

plotbar <- function(data, name, subtitle=NULL, ywrapwidth=Inf, xlim_upper=NULL){
  #  function to plot barchart, taking data from plotbardata or plotmultibardata
  data1 <- data %>%
    mutate(
      variable_explicit_na=factor(str_wrap(variable_explicit_na, ywrapwidth), levels=str_wrap(levels(variable_explicit_na), ywrapwidth))
    )

    ggplot(data1)+
    geom_bar(aes(x=n, y=variable_explicit_na), stat='identity', width=0.8)+
    geom_text(aes(x=n, y=variable_explicit_na, label=pct_fmt), hjust=-0.2, size=geom.text.size)+
    scale_x_continuous(limits=c(0, xlim_upper), breaks=seq(0,10000,1000))+
    scale_y_discrete(limits = rev(levels(data1$variable_explicit_na)))+
    labs(title=name,
         subtitle = subtitle,
         x="Participants", 
         y=NULL)+
    bar_theme+
    NULL
}

plotfacethist <- function(data, catvar, contvar, catname, contname, subtitle=NULL, 
                          ywrapwidth=Inf, breakint=50, titlewrapwidth=40, ylim_upper=NULL){
  #  function to plot faceted histogram, taking data from plotfacethistdata
  
  contdata <- 
    data %>% 
    select(all_of(c(catvar, contvar))) %>%
    rename(variable=all_of(catvar), contvariable=all_of(contvar)) %>%
    mutate(
      variable_explicit_na=(fct_explicit_na(variable, na_level="(Missing)")),
    )
  
  plotdata <- plotfacethistdata(contdata, "variable_explicit_na", "contvariable")

  ggplot()+
    geom_histogram(
      data = contdata %>% filter(!is.na(contvariable)),
      aes(x=contvariable), colour="black", fill="darkgrey", size=1, binwidth=1, boundary = 0.5, closed = "left"
    ) +
    geom_histogram(
     data = contdata %>% filter( is.na(contvariable)),
     aes(x=-2), colour="grey", fill="darkred", size=0.7, binwidth=1, boundary = 0.5, closed = "left"
    )+
    geom_point(data=plotdata, aes(y=-breakint/3, x=Q50), colour='red', size=1, alpha=0.5)+
    geom_linerange(data=plotdata, aes(y=-breakint/3, xmin=Q25, xmax=Q75), colour='red', size=1, alpha=0.5)+
    geom_linerange(data=plotdata, aes(y=-breakint/3, xmin=Q10, xmax=Q90), colour='red', size=0.5, alpha=0.5)+
    geom_hline(yintercept=0)+
    facet_wrap(vars(variable_explicit_na), ncol=1, strip.position="top")+#, space='free_y', scales="free_y")+
    scale_y_continuous(breaks = seq(0, ylim_upper, breakint), limits = c(-(breakint/1.8), NA))+
    labs(
      title=str_wrap(paste0(catname), titlewrapwidth),
      #subtitle = "GHQ-12 score (0-1-2-3)",
      x=contname, y=NULL)+
    hist_facet_theme+
    NULL

}


# plotfacetghqbee <- function(data, name, subtitle=NULL, ywrapwidth=Inf){
#   
#   data1 <- data %>%
#     mutate(
#       variable_explicit_na=factor(str_wrap(variable_explicit_na, ywrapwidth), levels=str_wrap(levels(variable_explicit_na), ywrapwidth))
#     )
#   
#   
#   
#   Qdata <- data1 %>%
#     filter(!is.na(ghqsum_0123)) %>%
#     group_by(variable_explicit_na) %>%
#     summarise(
#       Q10=quantile(ghqsum_0123, 0.1),
#       Q25=quantile(ghqsum_0123, 0.25),
#       Q5=quantile(ghqsum_0123, 0.5),
#       Q75=quantile(ghqsum_0123, 0.75),
#       Q90=quantile(ghqsum_0123, 0.9),
#       mean=mean(ghqsum_0123, na.rm=TRUE),
#     ) %>% ungroup() %>%
#     pivot_longer(cols=-variable_explicit_na, names_to="stat")
#   
#   
#   ggplot(data1)+
#     geom_quasirandom(aes(y=fct_rev(variable_explicit_na), x=ghqsum_0123+runif(length(ghqsum_0123), -0.2, 0.2)), alpha = 0.5, size = 0.1, bandwidth=0.1, varwidth=TRUE, groupOnX=FALSE) +
#     geom_linerange(data= Qdata, aes(y = fct_rev(variable_explicit_na), x = value, ymin=..y..-0.2, ymax=..y..+0.2, colour=stat=="mean"), size=1)+
#     #geom_boxplot(aes(y = fct_rev(variable_explicit_na), x = ghqsum_0123), alpha=0.3, colour='red')+
#     labs(
#       title=str_wrap(paste0(name, " versus GHQ-12 combined score (0-1-2-3)"), titlewrapwidth),
#       #subtitle = "answers assigned to 0, 1, 2, 3",
#       x="GHQ-12 score", y=NULL)+
#     hist_facet_theme+
#     theme(legend.position = "none")+
#     NULL
#   
# }
#plotfacetghqbee(plotfacetghqdata(data_cohort, "age2"), "Age", ywrapwidth=3)




# Import --------------------------------------------------------------------


data_all <- read_rds(file=here::here("processed-data", "data_all.rds"))
data_s1 <- read_rds(file=here::here("processed-data",  "data_s1.rds"))  #%>%
# rename_at(
#   .vars = vars(ends_with("_s1"), -consent_s1),
#   .funs = ~str_remove(., "\\_s1$")
# ) 

factor_dct <- read_rds(file=here::here("processed-data", "factor_dictionary.rds")) #%>%
# filter(
#   !str_detect(R, "\\_s2"),!str_detect(R, "\\_s3"),
# ) %>%
# mutate(
#   R = str_remove(R, "\\_s1")
# )



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# CREATE PLOTS Univariable ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


## bar plots
barvars <- c(
  "grade",
  "gender",
  "age2",
  "region",
  "redeployed",
  # "ghq_conc",
  # "ghq_sleep",
  # "ghq_use",
  # "ghq_capable",
  # "ghq_strain",
  # "ghq_diffs",
  # "ghq_day",
  # "ghq_face",
  # "ghq_depressed",
  # "ghq_loseconf",
  # "ghq_worthless",
  # "ghq_happy",
  "guid_govt",
  "guid_college",
  "guid_trust",
  "guid_dept",
  "guid_sm",
  "guid_blogs",
  "guid_lit",
  "confident_infectiontraining_s1",
  "prepared_suspected_s1",
  "prepared_normal_s1",
  "exposure_s1",
  "exposure_confirmed_s1",
  "increasesympmedical_s1",
  "increasesympmental_s1",
  "perceived_risk_s1",
  "worry_personal_s1",
  "worry_family_s1",
  "selfisolate_s1",
  "redeployed_where_s1",
  "redeployed_where2_s1",
  "redeployed_satisfaction_s1",
  "comb_dept3",
  "comb_specialty2",
  "ghqcase_s1",
  "missedshifts2_s1",
  NULL
)


plotinfo_bar <- 
  factor_dct %>%
  filter(R %in% barvars) %>%
  select(var=R, name=full) %>% 
  mutate(
    data0 = map(var, ~data_s1[,.x]),
  ) %>%
  mutate(
    name = str_wrap(name, titlewrapwidth),
    data = map2(data0, var, ~plotbardata(.x,.y)),
    ywrapwidth = 40,
    plot = pmap(list(data=data, name=name, ywrapwidth=ywrapwidth, xlim_upper=nrow(data_s1)), plotbar)
  )

plotinfo_bar$plot[[4]]

multibarvars <- c(
  "specialty",
  "dept",
  "outbreak",
  "practicaled",
  "ppe.dondof",
  "ppe.fit",
  "ppe.exp",
  "selfisolatereason",
  NULL
)


plotinfo_multibar <- 
  factor_dct %>%
  select(var=R, name=full) %>%
  filter(
    str_starts(var, paste(paste0("^", multibarvars), collapse = "|")),
    #!str_ends(var, fixed("_s2")), !str_ends(var, fixed("_s3"))
  ) %>%
  mutate(
    data0 = map(var, ~data_s1 %>% 
                  select(starts_with(paste0(.x, "_"))) %>% 
                  rename_with(.fn =  ~str_remove(.x, "\\_s1$")) 
                )
  )  %>%
  mutate(
    name = str_wrap(name, titlewrapwidth),
    subtitle = "multiple answers possible",
    data = map2(data0, var, ~plotmultibardata(.x,.y)),
    ywrapwidth=40,
    plot = pmap(list(data=data, name=name, subtitle=subtitle, ywrapwidth=ywrapwidth, xlim_upper = 5000), plotbar)
  )


plotinfo_multibar$plot[[4]]

### GHQ-12


data_s1 %>%
  select(ptid, starts_with("ghq_")) %>%
  mutate_at(
    .vars=vars(starts_with("ghq_")),
    .funs=list(as.integer)
  ) %>% 
  pivot_longer(
    cols = -ptid,
    names_to = "ghq",
    names_pattern = "ghq_?(.*)_",
    values_to = "response"
  )  %>%
  mutate(
    ghq = factor(ghq, levels=temp_levels, labels=temp_labels)
  ) %>%
  filter(!is.na(response)) %>%
  group_by(ghq) %>%
  count(response) %>%
  mutate(
    pct = n/sum(n),
    labelpos = cumsum(pct)-pct/2
  ) %>%
  mutate(
    responsenormalised=factor(response, levels=1:4, labels=c("0 (better)", "1 (same)", "2 (worse)", "3 (much worse)"))
  ) %>%
  arrange(ghq, responsenormalised)


plotinfo_stack <-
  tibble(
    var="ghq",
    name="General Health Questionnaire (GHQ)",
    data=list({
      temp_labels <- factor_dct %>% filter(R=="ghq") %>% pull(level) %>% `[[`(1)
      temp_levels <- factor_dct %>% filter(R=="ghq") %>% pull(short) %>% `[[`(1)
      
      data_s1 %>%
        select(ptid, starts_with("ghq_")) %>%
        mutate_at(
          .vars=vars(starts_with("ghq_")),
          .funs=list(as.integer)
        ) %>% 
        pivot_longer(
          cols = -ptid,
          names_to = "ghq",
          names_pattern = "ghq_?(.*)_",
          values_to = "response"
        )  %>%
        mutate(
          ghq = factor(ghq, levels=temp_levels, labels=temp_labels)
        ) %>%
        filter(!is.na(response)) %>%
        group_by(ghq) %>%
        count(response) %>%
        mutate(
          pct = n/sum(n),
          labelpos = cumsum(pct)-pct/2
        ) %>%
        mutate(
          responsenormalised=factor(response, levels=1:4, labels=c("0 (better)", "1 (same)", "2 (worse)", "3 (much worse)"))
        ) %>%
        arrange(ghq, responsenormalised)
      
    }),
    
    plot = pmap(list(data, name), function(data, name){
      
      ggplot(data)+
        geom_bar(aes(x=pct, y=fct_rev(ghq), fill=responsenormalised), stat="identity", position=position_stack(reverse=TRUE))+
        #geom_text(aes(y=fct_rev(ghq), x=labelpos, label=pct(pct)))+
        scale_fill_viridis(discrete = TRUE, guide = guide_legend(ncol=2))+
        labs(title=name,
             caption = "see GHQ for full questions and answers to each question",
             x=NULL,
             y=NULL,
             fill=NULL)+
        stack_theme +
        NULL
    })
  ) %>%
  add_row(
    var="guid",
    name="How frequently do you access the following sources of information regarding policy and clinical aspects of COVID−19?",
    data=list({
      
      temp_labels <- factor_dct %>% filter(R=="guidetype") %>% pull(level) %>% `[[`(1)
      temp_levels <- factor_dct %>% filter(R=="guidetype") %>% pull(short) %>% `[[`(1)
      
      
      data_s1 %>%
        select(ptid, starts_with("guid_")) %>%
        pivot_longer(
          cols=-ptid,
          names_to="guidetype",
          names_pattern="guid_?(.*)",
          values_to = "response"
        )  %>%
        mutate(
          guidetype = factor(guidetype, levels=temp_levels, labels=temp_labels)
        ) %>%
        #filter(!is.na(response)) %>%
        group_by(guidetype) %>%
        count(response) %>%
        mutate(
          percent = n/sum(n),
          labelpos = cumsum(percent)-percent/2
        ) %>%
        arrange(guidetype, response)
    }),
    
    plot = pmap(list(data, name), function(data, name){
    
        ggplot(data)+
        geom_bar(aes(x=percent, y=fct_rev(guidetype), fill=response), stat="identity", position=position_stack(reverse = TRUE))+
        scale_fill_viridis(discrete = TRUE, na.value="grey")+
        #geom_text(aes(y=fct_rev(guidetype), x=labelpos, label=pct(percent)))+
        guides(fill=guide_legend(ncol=3))+
        labs(title=str_wrap(name, titlewrapwidth),
             x=NULL,
             y=NULL,
             fill=NULL)+
        stack_theme +
        NULL
    })
      
  ) 

plothist_ghq0011 <- data_s1 %>%
  filter(!is.na(ghqsum0011_s1)) %>%
  ggplot() +
  geom_histogram(aes(x=ghqsum0011_s1), colour="white", binwidth=1, boundary = 0.5, closed = "left")+
  #geom_vline(aes(xintercept=11.5), linetype='dotted')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  labs(
    title=str_wrap("GHQ-12 combined score (0-0-1-1)", titlewrapwidth),
    #subtitle = "answers assigned to 0, 0, 1, 1",
    x="GHQ-12 score", y="Respondents")+
  hist_theme+
  NULL


plothist_ghq0123 <- data_s1 %>%
  filter(!is.na(ghqsum0123_s1)) %>%
  ggplot() +
  geom_histogram(aes(x=ghqsum0123_s1), colour="white", binwidth=1, boundary = 0.5, closed = "left")+
  #geom_vline(aes(xintercept=11.5), linetype='dotted')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  labs(
    title=str_wrap("GHQ-12 combined score (0-1-2-3)", titlewrapwidth),
    #subtitle = "answers assigned to 0, 1, 2, 3",
    x="GHQ-12 score", y="Respondents")+
  hist_theme+
  NULL


plotinfo_hist<-
  tibble(
    var=c("ghq0011_s1", "ghq0123_s1"),
    name=c("GHQ-12 combined score (0-0-1-1)", "GHQ-12 combined score (0-1-2-3)"),
    plot=list(plothist_ghq0011,plothist_ghq0123)
  )



## PPE training ####


plotinfo_facetbar <- 
  tibble(
    var="ppe",
    name="What training have you received in regards to PPE since the COVID−19 outbreak was declared"
  ) %>%
  mutate(
    data = list({
      
      
      temptrain_levels <- factor_dct %>% filter(R=="ppe.fit") %>% pull(short) %>% `[[`(1)
      temptrain_labels <- factor_dct %>% filter(R=="ppe.fit") %>% pull(level) %>% `[[`(1)
      
      tempaspect_levels <- c("dondof", "fit", "exp")
      tempaspect_labels <- c("donning and doffing", "fit testing for mask", "exposure to aerosol generating procedure")
      
      data_s1 %>%
        select(starts_with("ppe.")) %>%
        pivot_longer(
          cols=everything(),
          names_to=c("aspect", "training"),
          names_pattern="ppe.?(.*)_(.*)_",
          #names_sep ="_",
          values_to = "response"
        ) %>%
        group_by(aspect, training) %>%
        summarise(
          count=sum(response, na.rm=TRUE),
          percent=mean(response, na.rm=TRUE)
        ) %>%
        ungroup() %>%
        mutate(
          training=factor(training, levels=temptrain_levels, labels=temptrain_labels),
          aspect=factor(aspect, levels=tempaspect_levels, labels=tempaspect_labels)
        ) %>%
        arrange(aspect, training)
      
    }),
    plot = pmap(list(data, name), function(data, name){
                 data %>%
                   ggplot()+
                   geom_bar(aes(x=count, y=training), stat="identity")+
                   facet_wrap(facets=vars(aspect), ncol=1, strip.position="top") +
                   scale_x_continuous(limits=c(0,5000))+
                   geom_text(aes(y=training, x=count, label=pct(percent)), hjust=-0.2, size=geom.text.size)+
                   labs(title=str_wrap(name, titlewrapwidth),
                        subtitle="multiple answers possible",
                        x="Participants",
                        y=NULL)+
                   bar_theme+
                   NULL
                 
          })
  )



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## CREATE PLOTS : var * GHQ-12 #######################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



plotinfo_facethistghq <- factor_dct %>%
  select(var=R, name=full) %>% 
  filter(var %in% barvars) %>%
  mutate(
    data0 = map(var, ~data_s1[,c(.x, "ghqsum0123_s1")]),
    breakint=50,
  ) %>%
  mutate(
    name = str_wrap(name, titlewrapwidth),
    ywrapwidth=40,
    breakint=if_else(is.na(breakint), 50, breakint),
    data = pmap(list(data=data0, contvar="ghqsum0123_s1", catvar=var), plotfacethistdata),
    plot = pmap(list(data=data0, contvar="ghqsum0123_s1", catvar=var, contname="GHQ-12 (0-1-2-3)", catname=name, ywrapwidth=ywrapwidth, breakint=breakint, ylim_upper=5000), plotfacethist),
  )


plotinfo_facethistghq$plot[[2]]

plotfacethistdata(data_s1, "age2" ,"ghqsum0123_s1")
plotfacethist(data_s1, "age2","ghqsum0123_s1", "Age", "GHQ12 (0-1-2-3)", subtitle=NULL, ywrapwidth=30, breakint=50, ylim_upper=5000)

plotfacethist(data=plotinfo_facethistghq$data0[[14]], 
              contvar="ghqsum0123_s1", catvar=plotinfo_facethistghq$var[[14]], 
              contname = "GHQ12 (0-1-2-3)", catname = plotinfo_facethistghq$name[[14]], 
              subtitle=NULL, 
              ywrapwidth=100, breakint=100, ylim_upper=5000,
              titlewrapwidth=70)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## ALIGN PLOTS####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




plotdf_bar <-
  bind_rows(
    plotinfo_bar,
    plotinfo_multibar,
    plotinfo_facetbar
  ) %>%
  mutate(
    tabtype = 1,
    alignedplot = patchwork::align_patches(plot),
    device="png",
    filename = paste0("bar_", var, ".", device),
    units = "cm",
    panelwidth = 7,
    barheight = 0.5,
    #width = pmap_dbl(list(plot, units, panelwidth), function(plot, units, panelwidth){plotWidth(plot, units) + panelwidth}),
    width = 15,
    height = pmap_dbl(list(plot, units, barheight), function(plot, units, barheight){plotHeight(plot, units) + plotNbars(plot)*barheight}), 
  )

plotdf_bar %>% select(-alignedplot)




plotdf_bar <-
  bind_rows(
    plotinfo_bar,
    plotinfo_multibar,
    plotinfo_facetbar
  ) %>%
  mutate(
    tabtype = 1,
    alignedlist =  cowplot::align_plots(plotlist = plot, align="v"),
    alignedgtable = alignedlist,
    alignedplot = map(alignedgtable, cowplot::ggdraw),
    device="png",
    filename = paste0("bar_", var, ".", device),
    units = "cm",
    panelwidth = 7,
    barheight = 0.5,
    #width = pmap_dbl(list(plot, units, panelwidth), function(plot, units, panelwidth){plotWidth(plot, units) + panelwidth}),
    width = 15,
    height = pmap_dbl(list(plot, units, barheight), function(plot, units, barheight){plotHeight(plot, units) + plotNbars(plot)*barheight}), 
  )

plotdf_bar %>% select(-alignedgtable, -alignedplot)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## ALIGN STACK PLOTS ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

plotdf_stack <- plotinfo_stack %>% 
  mutate(
    tabtype = 1,
    alignedlist = cowplot::align_plots(plotlist = plot, align="v"),
    alignedgtable = alignedlist,
    alignedplot = map(alignedgtable, cowplot::ggdraw),
    device="png",
    filename = paste0(var, ".", device),
    units = "cm",
    panelwidth = 7,
    barheight = 0.3,
    #width = pmap_dbl(list(plot, units, panelwidth), function(plot, units, panelwidth){plotWidth(plot, units) + panelwidth}),
    width = 15,
    height = pmap_dbl(list(plot, units, barheight), function(plot, units, barheight){plotHeight(plot, units) + plotNbars(plot)*barheight}),
  )

plotdf_stack %>% select(-alignedgtable, -alignedplot)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## ALIGN HIST PLOTS ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

plotdf_hist <- plotinfo_hist %>%
  mutate(
    tabtype = 1,
    plottype = "hist",
    alignedgtable = NA,
    alignedplot = plot,
    device="png",
    filename = paste0(var, ".", device),
    units = "cm",
    panelwidth = 7,
    barheight = 0.4,
    #width = pmap_dbl(list(plot, units, panelwidth), function(plot, units, panelwidth){plotWidth(plot, units) + panelwidth}),
    width = 15,
    height = pmap_dbl(list(plot, units, barheight), function(plot, units, barheight){plotHeight(plot, units) + plotNbars(plot)*barheight}),
  )


plotdf_hist %>% select(-plot, -alignedgtable, -alignedplot)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## ALIGN FACET PLOTS ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

plotdf_facethistghq <- plotinfo_facethistghq %>%
  mutate(
    tabtype = 2,
    plottype = "facethistghq",
    alignedgtable = NA,
    alignedplot = plot,
    device="png",
    filename = paste0("ghq x ", var, ".", device),
    units = "cm",
    panelwidth = 8,
    panelheight = 2,
    #width = pmap_dbl(list(plot, units, panelwidth), function(plot, units, panelwidth){plotWidth(plot, units) + panelwidth}),
    width = 15,
    height = pmap_dbl(list(plot, units, panelheight), function(plot, units, panelheight){plotHeight(plot, units) + plotNpanelrows(plot)*panelheight}),
  )




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## SAVE PLOTS ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

plotdf <- map_dfr(list(plotdf_bar, plotdf_stack, plotdf_hist), ~.)


plotdf <- map_dfr(list(plotdf_stack), ~.)

tempsave <- 
  rbind(plotdf %>% select(filename, alignedplot, width, height, units)#, 
        #plotdf_facethistghq %>% select(filename, alignedplot, width, height, units)
        ) %>%
  mutate(
    w = pmap(list(filename=filename, plot=alignedplot,
                  path=here::here("outputs", "figures", "survey-1"), 
                  width=width, height=height, units=units, limitsize=FALSE
    ), 
    ggsave)
  )


tempsave <- 
  rbind(
    plotdf %>% select(filename, plot, width, height, units)
  ) %>%
  mutate(
    w = pmap(list(filename=filename, plot=plot,
                  path=here::here("outputs", "figures", "survey-1", "unaligned"), 
                  width=width, height=height, units=units, limitsize=FALSE
    ), 
    ggsave)
  )



write_rds(plotdf %>% select(var, name, data, ywrapwidth, plot, alignedplot, height, tabtype), file=here::here("outputs", "figures", "survey-1", "plotdf.rds"))
write_rds(plotdf_facethistghq %>% select(var, name, data, ywrapwidth, plot, alignedplot, height, tabtype), file=here::here("outputs", "figures", "survey-1", "plotdf_ghq.rds"))


data_grade_seniority <- 
bind_rows(
  data_s1 %>% filter(dept_anaes) %>% mutate(table_dept="Anaesthetics"),
  data_s1 %>% filter(dept_ed) %>% mutate(table_dept="Emergency Medicine"),
  data_s1 %>% filter(dept_icu) %>% mutate(table_dept="Intensive Care"),
)

Qdata_grade_seniority <- data_grade_seniority %>%
  group_by(seniority, table_dept) %>%
  summarise(
    n=n(),
    n_valid=sum(!is.na(ghqsum0123_s1)),
    pct_valid = pct(n_valid/n),
    Q10=quantile(ghqsum0123_s1, 0.1, na.rm=TRUE),
    Q25=quantile(ghqsum0123_s1, 0.25, na.rm=TRUE),
    median=quantile(ghqsum0123_s1, 0.5, na.rm=TRUE),
    Q75=quantile(ghqsum0123_s1, 0.75, na.rm=TRUE),
    Q90=quantile(ghqsum0123_s1, 0.9, na.rm=TRUE),
    mean=mean(ghqsum0123_s1, na.rm=TRUE),
    bsci = list(Hmisc::smean.cl.boot(ghqsum0123_s1, conf.int=0.95, B=1000, reps=FALSE)),
    mean.ll = map_dbl(bsci, ~.[2]),
    mean.ul = map_dbl(bsci, ~.[3])
  ) %>% 
  select(-bsci) %>%
  ungroup() 


plot_grade_seniority <- ggplot(data_grade_seniority) +
  geom_histogram(aes(x=ghqsum0123_s1), colour="black", fill="darkgrey", size=1, binwidth=1, boundary = 0.5, closed = "left")+
  geom_point(data=Qdata_grade_seniority, aes(y=-25/3, x=median), colour='red', size=1, alpha=0.5)+
  geom_linerange(data=Qdata_grade_seniority, aes(y=-25/3, xmin=Q25, xmax=Q75), colour='red', size=1, alpha=0.5)+
  geom_linerange(data=Qdata_grade_seniority, aes(y=-25/3, xmin=Q10, xmax=Q90), colour='red', size=0.5, alpha=0.5)+
  geom_hline(aes(yintercept=0))+
  facet_grid(cols=vars(seniority), rows=vars(table_dept), space='free_y', scales="free_y")+
  scale_y_continuous(breaks = seq(0,5000, 25), limits = c(-(25/2), NA))+
  labs(
    title=str_wrap("GHQ by Seniority and Department", titlewrapwidth),
    x="GHQ-12 (0-1-2-3)", y=NULL,
    caption = "Participants working across multiple departments are including in each band"
  )+
  theme_bw(base_size = theme.size) + 
  theme(
    panel.border = element_blank(), #axis.line.x = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour="lightgrey"),
    panel.grid.major.x = element_line(colour="lightgrey"),
    strip.background = element_blank(),
    
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0, face= "italic"),
    strip.text.x = element_text(angle = 0, hjust=0.5),
    strip.text.y = element_text(angle = 0, hjust=0),
  )+
  NULL
  
write_csv(Qdata_grade_seniority, "table.csv")
ggsave(filename=here::here("outputs", "figures", "survey-1", "grade_seniority.png"), plot = plot_grade_seniority, 
       units="cm", height=20, width=25,
       scale=0.8)
  
##################################################################################
# 
# test <- ggsave(filename=here::here("figures", "gender2.png"), units="cm",
#                plot=plot_gender,
#        width=plotWidth(plot_gender, "cm")+7,
#        height=plotHeight(plot_gender, "cm")+plotNbars(plot_gender)*0.5)

