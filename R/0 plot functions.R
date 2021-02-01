#plot functions

library('tidyverse')

# create a new temporary "package" called "plot_functions" containing functions and other objects that you don't want to appear in the global environment
# the parent environment is the "stats" package
if("plot_functions" %in% search()) detach(plot_functions)

with((plot_functions <- new.env(parent=as.environment("project_functions"))),
     {
       

# descriptive plots for single survey qs ----------------------------------

# function to plot simple barchart
plotbardata <- function(data, var, filt=TRUE){
  data %>% 
    filter(filt) %>%
    select(one_of(var)) %>%
    rename("variable"=var) %>%
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
  
  
  temp_levels <- factor_dct %>% filter(R==var) %>% pull(short) %>% `[[`(1)
  temp_labels <- factor_dct %>% filter(R==var) %>% pull(level) %>% `[[`(1)
  
  data %>%
    filter(filt) %>%
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
    add_row(variable=NA_character_, n=n_na, pct=pct_na) %>%
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



plotbar <- function(data, name, subtitle=NULL, ywrapwidth=Inf){
  
  data1 <- data %>%
    mutate(
      variable_explicit_na=factor(str_wrap(variable_explicit_na, ywrapwidth), levels=str_wrap(levels(variable_explicit_na), ywrapwidth))
    )
  
  ggplot(data1)+
    geom_bar(aes(x=n, y=variable_explicit_na), stat='identity')+
    geom_text(aes(x=n, y=variable_explicit_na, label=pct_fmt), hjust=-0.2, size=geom.text.size)+
    scale_x_continuous(limits=c(0,nparticipants), breaks=seq(0,10000,1000))+
    scale_y_discrete(limits = rev(levels(data1$variable_explicit_na)))+
    labs(title=name,
         subtitle = subtitle,
         x="Respondents", 
         y=NULL)+
    bar_theme+
    NULL
}







colourss <- function(colourtype = "qual"){
  
  if(colourtype == "qual"){
    list(
      #scale_color_brewer(type="qual", palette="Set3")+
      #scale_fill_brewer(type="qual", palette="Set3", guide=FALSE)+
      #ggthemes::scale_color_colorblind(),
      #ggthemes::scale_fill_colorblind(guide=FALSE),
      #rcartocolor::scale_color_carto_d(palette = "Safe"),
      #rcartocolor::scale_fill_carto_d(palette = "Safe", guide=FALSE),
      ggsci::scale_color_simpsons(),
      ggsci::scale_fill_simpsons(guide=FALSE)
    )
  } else if(colourtype == "cont"){
    list(
      scale_colour_viridis(discrete = FALSE),
      scale_fill_viridis(discrete = FALSE, guide = FALSE)
    )
  } else {
    list(
      scale_colour_viridis(discrete = TRUE),
      scale_fill_viridis(discrete = TRUE, guide = FALSE)
    )
  }
}


ploteffectlegend <- function(variable, data, effects, colourtype = "qual", pval_anno=""){
  
  dat = tibble(var=forcats::fct_inorder(levels(data[[variable]]))) %>% mutate(x=n())
  
  str_wrap2 = function(string, width=40){str_wrap(string = string, width=width)}
  
  
  out <- ggplot()+
    geom_point(data = dat, mapping=aes_string(x="x", y="var", colour="var"), alpha=0, size=0.5)+
    scale_y_discrete(limits = rev(levels(dat$var)), 
                     labels = str_wrap2,
                     position = "right")+
    colourss(colourtype)+
    #coord_fixed(xlim=c(0,80), ratio = (80/n_distinct(dat$var))*2)+
    labs(subtitle="var", x="var")+
    theme_void(base_size=10)+
    theme(
      axis.text.y=element_text(hjust=1),
      axis.text.x=element_text(colour='transparent'),
      axis.title.x=element_text(colour='transparent'),
      plot.subtitle = element_text(colour = 'transparent'),
      legend.position="none"
    )
  out
}

plotrawdata_bee <- function(catvar, contvar, data, xlab = "", breaks = c(0, 10, 20, 30, 36), labels = c("0", "10", "20", "30", "36")){
  
  dat = data %>% 
    mutate(
      catvar=data[[catvar]],
      catvar_explicit_na=fct_explicit_na(catvar, na_level="(Missing)"),
      contvar = data[[contvar]],
      contvar_explicit_na = if_else(is.na(contvar), -2, contvar)
      
    )
  
  
  Qdat <- dat %>%
    filter(!is.na(contvar)) %>%
    group_by(survey_chr, catvar_explicit_na) %>%
    summarise(
      Q10=quantile(contvar, 0.1),
      Q25=quantile(contvar, 0.25),
      Q50=quantile(contvar, 0.5),
      Q75=quantile(contvar, 0.75),
      Q90=quantile(contvar, 0.9),
    ) 
  
  dat_jit <- dat %>%
    mutate(
      contvar = contvar + runif(n(), -0.2, 0.2),
      contvar_explicit_na = if_else(is.na(contvar), runif(n(), -2.2, -1.8), contvar)
    )
  
  out <- ggplot()+
    geom_quasirandom(data = dat_jit, mapping=aes(x=contvar_explicit_na, y=catvar_explicit_na, fill=!is.na(contvar)), colour = "transparent", alpha=0.5, size=0.1, bandwidth=0.1, varwidth=TRUE, groupOnX=FALSE)+
    
    geom_linerange(data=Qdat, mapping=aes(y=catvar_explicit_na, xmin = Q10, xmax = Q90), size=0.8, alpha=0.5, colour="darkred")+
    geom_linerange(data=Qdat, mapping=aes(y=catvar_explicit_na, xmin = Q25, xmax = Q75), size=1.6, alpha=0.5, colour="darkred")+
    geom_point(data=Qdat, mapping=aes(x=Q50, y=catvar_explicit_na), size=2, alpha=0.5, fill="darkred", colour = "transparent")+
    
    facet_grid(rows=NULL, cols = vars(survey_chr), scales="free_y", space="free_y")+
    
    labs(x=xlab, y="", colour="")+
    scale_x_continuous(breaks = waiver(), labels = waiver())+
    scale_y_discrete(limits = rev(levels(dat$catvar_explicit_na)))+
    #scale_colour_manual(values= c("TRUE" = "gray", "FALSE" ="black"), guide=FALSE)+
    theme_bw(base_size=12)+
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      
      panel.border = element_blank(), axis.line.x = element_line(colour = "black"),
      panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour="lightgrey", size=0.2),
      panel.grid.minor.x = element_blank(),
      strip.background = element_blank(),
      
      plot.title = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      plot.caption = element_text(hjust = 0, face= "italic"),
      strip.text.y = element_text(angle = 0)
      
    )
  
  out
}


plotrawdata_jit <- function(catvar, contvar, data, xlab = "", breaks = waiver(), labels = waiver()){
  
  dat = data %>% 
    mutate(
      catvar=data[[catvar]],
      catvar_explicit_na=fct_explicit_na(catvar, na_level="(Missing)"),
      contvar = data[[contvar]],
      contvar_explicit_na = if_else(is.na(contvar), -2, contvar)
      
    )
  
  Qdat <- dat %>%
    filter(!is.na(contvar)) %>%
    group_by(survey_chr, catvar_explicit_na) %>%
    summarise(
      Q10=quantile(contvar, 0.1),
      Q25=quantile(contvar, 0.25),
      Q50=quantile(contvar, 0.5),
      Q75=quantile(contvar, 0.75),
      Q90=quantile(contvar, 0.9),
    ) 
  
  out <- ggplot()+
    geom_jitter(data = dat %>% filter(!is.na(contvar)), mapping=aes(x=contvar, y=catvar_explicit_na), height=0.2, width=0.1, alpha=0.2, size=0.5, fill="black", colour = "transparent")+
    geom_jitter(data = dat %>% filter(is.na(contvar)), mapping=aes(x=-2, y=catvar_explicit_na), height=0.2, width=0.1, alpha=0.2, size=0.5, fill="black", colour = "transparent")+
    
    geom_linerange(data=Qdat, mapping=aes(y=catvar_explicit_na, xmin = Q10, xmax = Q90), size=0.8, alpha=0.5, colour="darkred")+
    geom_linerange(data=Qdat, mapping=aes(y=catvar_explicit_na, xmin = Q25, xmax = Q75), size=1.6, alpha=0.5, colour="darkred")+
    geom_point(data=Qdat, mapping=aes(x=Q50, y=catvar_explicit_na), size=2, alpha=0.5, fill="darkred", colour = "transparent")+
    
    facet_grid(rows=NULL, cols = vars(survey_chr), scales="free_y", space="free_y")+
    
    labs(x=xlab, y="", colour="")+
    scale_x_continuous(breaks = breaks, labels = breaks)+
    scale_y_discrete(limits = rev(levels(dat$catvar_explicit_na)))+
    #scale_colour_manual(values= c("TRUE" = "gray", "FALSE" ="black"), guide=FALSE)+
    theme_bw(base_size=12)+
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      
      panel.border = element_blank(), axis.line.x = element_line(colour = "black"),
      panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour="lightgrey", size=0.2),
      panel.grid.minor.x = element_blank(),
      strip.background = element_blank(),
      
      plot.title = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      plot.caption = element_text(hjust = 0, face= "italic"),
      strip.text.y = element_text(angle = 0, hjust=0)
      
    )
  
  out
}



plotrawdata_hist <- function(catvar, contvar, data, xlab = ""){
  
  dat = data %>% 
    mutate(
      catvar = data[[catvar]],
      catvar_explicit_na=fct_explicit_na(catvar, na_level="(Missing)"),
      contvar = data[[contvar]],
      contvar_explicit_na = if_else(is.na(contvar), -2, contvar)
      
    )# %>% 
  #filter(survey==survey)
  
  
  Qdat <- dat %>%
    filter(!is.na(contvar)) %>%
    group_by(survey_chr, catvar_explicit_na) %>%
    summarise(
      Q10=quantile(contvar, 0.1),
      Q25=quantile(contvar, 0.25),
      Q50=quantile(contvar, 0.5),
      Q75=quantile(contvar, 0.75),
      Q90=quantile(contvar, 0.9),
    ) 
  # 
  #   meandat <- dat %>%
  #     filter(!is.na(contvar)) %>%
  #     group_by(survey_chr, catvar_explicit_na) %>%
  #     summarise(
  #       mean=mean(contvar, na.rm=TRUE),
  #       bsci = list(Hmisc::smean.cl.boot(contvar, conf.int=0.95, B=1000, reps=FALSE)),
  #       meanll = map_dbl(bsci, ~.[2]),
  #       meanul = map_dbl(bsci, ~.[3])
  #     ) 
  
  out <-  ggplot()+
    geom_histogram(data = dat %>% filter(!is.na(contvar)), aes(x=contvar), colour="grey", fill="darkgrey", size=0.7, binwidth=1, boundary = 0.5, closed = "left")+
    geom_histogram(data = dat %>% filter(is.na(contvar)), aes(x=-2), colour="grey", fill="darkred", size=0.7, binwidth=1, boundary = 0.5, closed = "left")+
    geom_linerange(data = Qdat, mapping=aes(y=0, xmin = Q10, xmax = Q90), size=0.8, alpha=1, colour='black')+
    geom_linerange(data = Qdat, mapping=aes(y=0, xmin = Q25, xmax = Q75), size=1.6, alpha=0.8, colour='black')+
    geom_point(data = Qdat, mapping=aes(y=0, x=Q50), size=2.5, alpha=0.5, fill='black', colour = "transparent")+
    facet_grid(rows=vars(catvar_explicit_na), cols = vars(survey_chr))+#, scales="free_y", space="free_y")+
    #geom_hline(aes(yintercept=0))+
    #scale_x_continuous(breaks = c(-2, 0, 10, 20, 30, 36), labels = c("(Missing)","0", "10", "20", "30", "36"))+
    labs(x=xlab, y="", colour="")+
    #hist_facet_theme+
    theme_bw(base_size=12)+
    theme(
      panel.border = element_blank(), 
      #axis.line.x = element_line(colour = "grey", size=0.1),
      axis.line.x = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      
      strip.background = element_blank(),
      
      plot.title = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      plot.caption = element_text(hjust = 0, face= "italic"),
      strip.text.y = element_text(angle = 0, hjust=0)
    )+
    NULL
  
  out
}



# plot marginal effects ---------------------------------------------------


ploteffect <- function(effects, limits=NULL, xlab = NULL, title=NULL, colourtype = "qual", pval_anno=NULL){
  
  fitline <- list(
    geom_point(data=effects, mapping=aes(x=predicted, y=x, colour=x), size=2),#, position=position_dodge2(width=0.7, reverse = TRUE))
    geom_linerange(data=effects, mapping=aes(y=x, xmin = conf.low, xmax = conf.high, colour=x), size=0.5, show.legend=FALSE)#, position=position_dodge2(width=0.7, reverse = TRUE))
  )
  
  out <- ggplot()+
    fitline+
    #geom_text(data=tibble(x=70, y=1, text=print_pval(pval_anno,3)), mapping=aes(x=x,y=y, label=text))+
    labs(x=xlab, y="", colour="", 
         title = title,
         subtitle=print_pval(pval_anno,3)
    )+
    facet_grid(rows=vars(group))+
    scale_y_discrete(limits = rev(levels(effects$x)))+
    #scale_x_continuous(limits = limits)+
    coord_cartesian(xlim=limits) +
    colourss(colourtype)+
    theme_bw(base_size=12)+
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      panel.border = element_blank(), 
      axis.line.x = element_line(colour = "black"),
      #axis.line.x = element_line(colour = "black"),
      #panel.grid = element_blank(),
      #panel.grid.major.x = element_line(colour="grey", size=0.2),
      panel.grid.major.y = element_blank(),
      
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      
      strip.background = element_blank(),
      
      plot.title = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, face= "italic"),
      strip.text.y = element_text(angle = 0),
      
      legend.position = "left"
    )
  
  out
}
       
     }
)
attach(plot_functions);rm(plot_functions)
