

library("tidyverse")
library('lubridate')
library('binom')
library("viridis")


# create a new temporary "package" called "project_functions" containing functions and other objects that you don't want to appear in the global environment
# the parent environment is the "stats" package
if("project_functions" %in% search()) detach(project_functions)

with((project_functions <- new.env(parent=as.environment("package:stats"))),
     {
       #list project-specific functions here:
      
       
       #"not in" infix function
       `%ni%` <- Negate(`%in%`)
       
       #
       import::from(magrittr, "%$%")
        
       # function to format percentages to 1dp
       pct <- scales::label_percent(accuracy=0.1, scale=100)
       
       
       specify_decimal <- function(x, k, trim=FALSE) {
         
         fmtd <- format(round(x, k), nsmall = k)
         if (trim) {fmtd <- trimws(fmtd)}
         return(fmtd)
       }
       
       print_est2bracket <- function(x, b1, b2, round=1){
         paste0(specify_decimal(x, round), " (", specify_decimal(b1, round), ", ", specify_decimal(b2, round), ")")
       }  
       
       print_pval <- function(pval, k=3, prefix=FALSE){
         if (prefix) 
           ifelse(pval < 1/(10^k), paste0("p<", 1/(10^k)), paste0("p=", specify_decimal(pval, k = 3)))
         else
           ifelse(pval < 1/(10^k), paste0("<",specify_decimal(1/(10^k), k = 3)), specify_decimal(pval, k = 3))
       }
       
       
       # function that takes a character of the format "1, lev1 | 2, lev2 | 3, lev3..." and returns either the code or the level
       splitlevs <-function(redcapchr, take=c("code", "level")){
         stopifnot(length(redcapchr)==1)
         stopifnot(is.character(redcapchr))
         if(is.na(redcapchr)) 
           return(NA_character_)
         
         split <- stringr::str_split(redcapchr, "\\|")[[1]]
         
         if(take=="code")
           return(as.integer(stringr::str_extract(split, "\\d+")))
         if(take=="level")
           #return(stringr::str_trim(stringr::str_extract(split, "[a-zA-Z](.*)")))
           return(stringr::str_trim(stringr::str_remove(split, "\\d*,\\s")))
       }
       
       fct_case_when <- function(...) {
         args <- as.list(match.call())
         levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
         levels <- levels[!is.na(levels)]
         factor(dplyr::case_when(...), levels=levels)
       }
       
     }
)
attach(project_functions);rm(project_functions)
