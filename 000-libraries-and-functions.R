library(dplyr)
library(tidyr)
library(lubridate)
library(car)
library(knitr)
library(lazyeval)
library(broom)
library(foreign)
library(MplusAutomation)
library(ggplot2)
library(lme4)

read.ppmi <- function(directory, file) {
  data <- read.csv(file = paste0(directory, file))
  colnames(data) <- tolower(colnames(data))
  data

}

# clean.ppmi <- function(df) {
#   pn <- tolower(levels(df$pag_name))[1]
#   df <- df %>%
#     mutate(infodt = parse_date_time(paste0("1/",infodt), "dmy"),
#            orig_entry = parse_date_time(paste0("1/",orig_entry), "dmy"),
#            last_update = parse_date_time(last_update, "ymd HMS"),
#            site_aprv = parse_date_time(paste0("1/",site_aprv), "dmy"),
#            timefr = recode(event_id, "'SC'=-1.5; 'BL'=0; 'V01'=3; 'V02'=6; 'V03'=9;
#                            'V04'=12; 'V05'=18; 'V06'=24; 'V07'=30; 'V08'=36;
#                            'V09'=42; 'V10'=48; 'V11'=54; 'V12'=60; 'PV13'=72;
#                            'V14'=84; 'PV15'=96; 'RS1'=NA; 'ST'=NA; 'U01'=NA;
#                            'PW'=NA"),
#            timefr = as.numeric(levels(timefr)[timefr]) ) %>%
#     rename_(.dots=setNames(list("rec_id"),   paste0(pn,".rec_id"))) %>%
#     rename_(.dots=setNames(list("f_status"), paste0(pn,".f_status"))) %>%
#     rename_(.dots=setNames(list("pag_name"), paste0(pn,".pag_name"))) %>%
#     rename_(.dots=setNames(list("infodt"),   paste0(pn,".infodt"))) %>%
#     rename_(.dots=setNames(list("orig_entry"), paste0(pn,".orig_entry"))) %>%
#     rename_(.dots=setNames(list("last_update"), paste0(pn,".last_update"))) %>%
#     rename_(.dots=setNames(list("query"), paste0(pn,".query"))) %>%
#     rename_(.dots=setNames(list("site_aprv"), paste0(pn,".site_aprv")) ) %>%
#     arrange(patno, event_id)
#   df$event_id <- factor(df$event_id, levels=c("SC", "BL", "V01", "V02", "V03",
#                                               "V04", "V05", "V06", "V07", "V08",
#                                               "V09", "V10", "V11", "V12", "PW",
#                                               "RS1",  "ST", "U01"))
#   df
# }

clean.ppmi <- function(df) {
  baseline.df <- df %>%
    mutate(infodt = parse_date_time(paste0("1/",infodt), "dmy")) %>%
    filter(event_id=="BL") %>%
    rename(baseline.date = infodt) %>%
    select(patno, baseline.date) %>%
    arrange(patno)


  pn <- tolower(levels(df$pag_name))[1]
  df <- df %>%
    left_join(baseline.df, by="patno") %>%
    mutate(infodt = parse_date_time(paste0("1/",infodt), "dmy"),
           orig_entry = parse_date_time(paste0("1/",orig_entry), "dmy"),
           last_update = parse_date_time(last_update, "ymd HMS"),
           site_aprv = parse_date_time(paste0("1/",site_aprv), "dmy"),
           timefr = recode(event_id, "'SC'=-1.5; 'BL'=0; 'V01'=3; 'V02'=6; 'V03'=9;
                           'V04'=12; 'V05'=18; 'V06'=24; 'V07'=30; 'V08'=36;
                           'V09'=42; 'V10'=48; 'V11'=54; 'V12'=60; 'PV13'=72;
                           'V14'=84; 'PV15'=96; 'RS1'=NA; 'ST'=NA; 'U01'=NA;
                           'PW'=NA"),
           timefr = as.numeric(levels(timefr)[timefr]),
           visit = (infodt - baseline.date)/dyears(1)) %>%
    rename_(.dots=setNames(list("rec_id"),   paste0(pn,".rec_id"))) %>%
    rename_(.dots=setNames(list("f_status"), paste0(pn,".f_status"))) %>%
    rename_(.dots=setNames(list("pag_name"), paste0(pn,".pag_name"))) %>%
    rename_(.dots=setNames(list("infodt"),   paste0(pn,".infodt"))) %>%
    rename_(.dots=setNames(list("orig_entry"), paste0(pn,".orig_entry"))) %>%
    rename_(.dots=setNames(list("last_update"), paste0(pn,".last_update"))) %>%
    rename_(.dots=setNames(list("query"), paste0(pn,".query"))) %>%
    rename_(.dots=setNames(list("site_aprv"), paste0(pn,".site_aprv")) ) %>%
    rename_(.dots=setNames(list("visit"), paste0(pn,".visit")) ) %>%
    arrange(patno, event_id) %>%
    select(-baseline.date)

  df$event_id <- factor(df$event_id, levels=c("SC", "BL", "V01", "V02", "V03",
                                              "V04", "V05", "V06", "V07", "V08",
                                              "V09", "V10", "V11", "V12", "PW",
                                              "RS1",  "ST", "U01"))
  df
}


clean.ppmi.random <- function(df) {
  pn <- tolower(levels(df$pag_name))[1]
  df <- df %>%
    mutate(enrolldt = parse_date_time(paste0("1/",enrolldt), "dmy"),
           birthdt = parse_date_time(paste0("1/",birthdt), "dmy"),
           consntdt = parse_date_time(paste0("1/",consntdt), "dmy"),
           orig_entry = parse_date_time(paste0("1/",orig_entry), "dmy"),
           last_update = parse_date_time(last_update, "ymd HMS"),
           site_aprv = parse_date_time(paste0("1/",site_aprv), "dmy")) %>%
    rename_(.dots=setNames(list("rec_id"),   paste0(pn,".rec_id"))) %>%
    rename_(.dots=setNames(list("f_status"), paste0(pn,".f_status"))) %>%
    rename_(.dots=setNames(list("pag_name"), paste0(pn,".pag_name"))) %>%
    rename_(.dots=setNames(list("infodt"),   paste0(pn,".infodt"))) %>%
    rename_(.dots=setNames(list("orig_entry"), paste0(pn,".orig_entry"))) %>%
    rename_(.dots=setNames(list("last_update"), paste0(pn,".last_update"))) %>%
    rename_(.dots=setNames(list("query"), paste0(pn,".query"))) %>%
    rename_(.dots=setNames(list("site_aprv"), paste0(pn,".site_aprv")) ) %>%
    arrange(patno)
  df
}
clean.ppmi.patient <- function(df) {
  pn <- tolower(levels(df$pag_name))[1]
  df <- df %>%
    mutate(enroll_date = parse_date_time(paste0("1/",enroll_date), "dmy")) %>%
    arrange(patno)
  df
}
clean.ppmi.screen <- function(df) {
  pn <- tolower(levels(df$pag_name))[1]
  df <- df %>%
    mutate(consntdt = parse_date_time(paste0("1/",consntdt), "dmy"),
           prjenrdt = parse_date_time(paste0("1/",prjenrdt), "dmy"),
           orig_entry = parse_date_time(paste0("1/",orig_entry), "dmy"),
           last_update = parse_date_time(last_update, "ymd HMS"),
           site_aprv = parse_date_time(paste0("1/",site_aprv), "dmy")) %>%
    rename_(.dots=setNames(list("rec_id"),   paste0(pn,".rec_id"))) %>%
    rename_(.dots=setNames(list("f_status"), paste0(pn,".f_status"))) %>%
    rename_(.dots=setNames(list("pag_name"), paste0(pn,".pag_name"))) %>%
    rename_(.dots=setNames(list("orig_entry"), paste0(pn,".orig_entry"))) %>%
    rename_(.dots=setNames(list("last_update"), paste0(pn,".last_update"))) %>%
    rename_(.dots=setNames(list("query"), paste0(pn,".query"))) %>%
    rename_(.dots=setNames(list("site_aprv"), paste0(pn,".site_aprv")) ) %>%
    arrange(patno)
  df
}



mytablecont <- function(df, uniq.var, title, grp.var=NULL, tabledigits = c( 2, 2, 0, 0, 0, 0 ,0)){
  if(is.null(grp.var)) {
    foo <- df %>%
      summarise_(n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 min      = interp(~quantile(v, probs=0.00, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 `25%`    = interp(~quantile(v, probs=0.25, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 median   = interp(~quantile(v, probs=0.50, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 `75%`    = interp(~quantile(v, probs=0.75, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 max      = interp(~quantile(v, probs=1.00, na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
      kable(digits = c(0, 0, tabledigits),
            caption = paste("Summary statistics for", title),
            align = c(rep("r", 9)),
            col.names = c("N Visit", "N Obs",
                          "Mean", "SD",
                          "Min", "25%", "Median", "75%", "Max"))
  }
  if(!is.null(grp.var)) {
    foo <- df %>%
      group_by_(grp.var) %>%
      summarise_(month    = interp(~first(v), v=as.name("timefr")),
                 n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 min      = interp(~quantile(v, probs=0.00, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 `25%`    = interp(~quantile(v, probs=0.25, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 median   = interp(~quantile(v, probs=0.50, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 `75%`    = interp(~quantile(v, probs=0.75, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 max      = interp(~quantile(v, probs=1.00, na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
      kable(digits = c(0, 0, 0, 0, tabledigits),
            caption = paste("Summary statistics for", title),
            align = c(rep("r", 11)),
            col.names = c("Visit", "Month", "N Visit", "N Obs",
                          "Mean", "SD",
                          "Min", "25%", "Median", "75%", "Max"))
  }
  return(foo)

}



mytableny <- function(df, uniq.var, title, grp.var=NULL, table.col.names=NULL, table.values=NULL){
  table.col.foo <- c("N Visit", "N Obs", "Mean", "SD")
  if(is.null(table.values)) {
    table.values <- c(0, 1)
  }

  if(is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c(table.col.foo, "No", "Yes")
    }
    if(!is.null(table.col.names)) {
      table.col <- c(table.col.foo, table.col.names)
    }
    foo <- df %>%
      summarise_(n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
      kable(digits = c(0, 0, 2, 2, 0, 0),
            caption = paste("Summary statistics for", title),
            align = c(rep("r", 6)),
            col.names = table.col)
  }
  if(!is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, "No", "Yes")
    }
    if(!is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, table.col.names)
    }
    foo <- df %>%
      group_by_(grp.var) %>%
      summarise_(month    = interp(~first(v), v=as.name("timefr")),
                 n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
      kable(digits = c(0, 1, 0, 0, 2, 2, 0, 0),
            caption = paste("Summary statistics for", title),
            align = c(rep("r", 8)),
            col.names = table.col)
  }
  return(foo)

}

mytablecat3 <- function(df, uniq.var, title, grp.var=NULL, table.col.names=NULL, table.values=NULL){
  table.col.foo <- c("N Visit", "N Obs", "Mean", "SD")

  if(is.null(table.values)) {
    table.values <- c(0, 1, 2)
  }

  if(is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c(table.col.foo, "0", "1", "2")
    }
    if(!is.null(table.col.names)) {
      table.col <- c(table.col.foo, table.col.names)
    }
    foo <- df %>%
      summarise_(n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                 v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                 v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                 v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                 v=as.name(uniq.var)),
                 cat3     = interp(~sum(v==table.values[3], na.rm=TRUE),
                                 v=as.name(uniq.var))) %>%
      kable(digits = c(0, 0, 2, 2, 0, 0, 0),
          caption = paste("Summary statistics for", title),
          align = c(rep("r", 7)),
          col.names = table.col)
  }
  if(!is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, "0", "1", "2")
    }
    if(!is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, table.col.names)
    }
    foo <- df %>%
      group_by_(grp.var) %>%
      summarise_(month    = interp(~first(v), v=as.name("timefr")),
                 n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat3     = interp(~sum(v==table.values[3], na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
      kable(digits = c(0, 1, 0, 0, 2, 2, 0, 0, 0),
            caption = paste("Summary statistics for", title),
            align = c(rep("r", 9)),
            col.names = table.col)
  }
  return(foo)
}


mytablecat4 <- function(df, uniq.var, title, grp.var=NULL, table.col.names=NULL, table.values=NULL){
  table.col.foo <- c("N Visit", "N Obs", "Mean", "SD")

  if(is.null(table.values)) {
    table.values <- c(0, 1, 2, 3)
  }

  if(is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c(table.col.foo, "0", "1", "2", "3")
    }
    if(!is.null(table.col.names)) {
      table.col <- c(table.col.foo, table.col.names)
    }
    foo <- df %>%
      summarise_(n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                  v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat3     = interp(~sum(v==table.values[3], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat4     = interp(~sum(v==table.values[4], na.rm=TRUE),
                                 v=as.name(uniq.var))) %>%
    kable(digits = c(0, 0, 2, 2, 0, 0, 0, 0),
          caption = paste("Summary statistics for", title),
          align = c(rep("r", 8)),
          col.names = table.col)
  }
  if(!is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, "0", "1", "2", "3")
    }
    if(!is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, table.col.names)
    }
    foo <- df %>%
      group_by_(grp.var) %>%
      summarise_(month    = interp(~first(v), v=as.name("timefr")),
                 n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat3     = interp(~sum(v==table.values[3], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat4     = interp(~sum(v==table.values[4], na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
    kable(digits = c(0, 1, 0, 0, 2, 2, 0, 0, 0, 0),
          caption = paste("Summary statistics for", title),
          align = c(rep("r", 10)),
          col.names = table.col)
  }
  return(foo)
}

mytablecat5 <- function(df, uniq.var, title, grp.var=NULL, table.col.names=NULL, table.values=NULL){
  table.col.foo <- c( "N Visit", "N Obs", "Mean", "SD")

  if(is.null(table.values)) {
    table.values <- c(0, 1, 2, 3, 4)
  }
  if(is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c(table.col.foo, "0", "1", "2", "3", "4")
    }
    if(!is.null(table.col.names)) {
      table.col <- c(table.col.foo, table.col.names)
    }
    foo <- df %>%
      summarise_(n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat3     = interp(~sum(v==table.values[3], na.rm=TRUE),
                                  v=as.name(uniq.var)),
                 cat4     = interp(~sum(v==table.values[4], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat5     = interp(~sum(v==table.values[5], na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
      kable(digits = c(0, 0, 2, 2, 0, 0, 0, 0 ,0),
            caption = paste("Summary statistics for", title),
            align = c(rep("r", 9)),
            col.names = table.col)
  }
  if(!is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, "0", "1", "2", "3", "4")
    }
    if(!is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, table.col.names)
    }
    foo <- df %>%
      group_by_(grp.var) %>%
      summarise_(month    = interp(~first(v), v=as.name("timefr")),
                 n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat3     = interp(~sum(v==table.values[3], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat4     = interp(~sum(v==table.values[4], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat5     = interp(~sum(v==table.values[5], na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
      kable(digits = c(0, 1, 0, 0, 2, 2, 0, 0, 0, 0 ,0),
            caption = paste("Summary statistics for", title),
            align = c(rep("r", 11)),
            col.names = table.col)
  }
  return(foo)
}

mytablecat6 <- function(df, uniq.var, title, grp.var=NULL, table.col.names=NULL, table.values=NULL){
  table.col.foo <- c("N Visit", "N Obs", "Mean", "SD")

  if(is.null(table.values)) {
    table.values <- c(0, 1, 2, 3, 4, 5)
  }

  if(is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c(table.col.foo, "0", "1", "2", "3", "4", "5")
    }
    if(!is.null(table.col.names)) {
      table.col <- c(table.col.foo, table.col.names)
    }
    foo <-  df %>%
      summarise_(n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat3     = interp(~sum(v==table.values[3], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat4     = interp(~sum(v==table.values[4], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat5     = interp(~sum(v==table.values[5], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat6     = interp(~sum(v==table.values[6], na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
      kable(digits = c(0, 0, 2, 2, 0, 0, 0, 0 ,0, 0),
            caption = paste("Summary statistics for", title),
            align = c(rep("r", 10)),
            col.names = table.col)
  }
  if(!is.null(grp.var)) {
    if(is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, "0", "1", "2", "3", "4", "5")
    }
    if(!is.null(table.col.names)) {
      table.col <- c("Visit", "Month", table.col.foo, table.col.names)
    }
    foo <-  df %>%
      group_by_(grp.var) %>%
      summarise_(month    = interp(~first(v), v=as.name("timefr")),
                 n        = interp(~n()),
                 n.obs    = interp(~sum(!is.na(v)), v=as.name(uniq.var)),
                 mean     = interp(~mean(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 sd       = interp(~sd(v, na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat1     = interp(~sum(v==table.values[1], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat2     = interp(~sum(v==table.values[2], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat3     = interp(~sum(v==table.values[3], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat4     = interp(~sum(v==table.values[4], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat5     = interp(~sum(v==table.values[5], na.rm=TRUE),
                                   v=as.name(uniq.var)),
                 cat6     = interp(~sum(v==table.values[6], na.rm=TRUE),
                                   v=as.name(uniq.var))) %>%
      kable(digits = c(0, 1, 0, 0, 2, 2, 0, 0, 0, 0 ,0, 0),
            caption = paste("Summary statistics for", title),
            align = c(rep("r", 12)),
            col.names = table.col)
  }
  return(foo)
}


my_lmfun <- function(y, df){
  lmmodel = tidy(lm(y ~ year5, data = df))
  lmmodel
}

fit.lm.ppmi <- function(dep.var) {
  do_dots = interp( ~ my_lmfun(y = .[[dep.var]], df = .))
  df <- ppmi %>%
    group_by(patno) %>%
    do_(.dots = do_dots)  %>%
    filter(term=="year5") %>%
    ungroup() %>%
    rename_(.dots=setNames(list("estimate"),   paste0(dep.var,".slpoe"))) %>%
    rename_(.dots=setNames(list("p.value"),   paste0(dep.var,".pvalue"))) %>%
    select(-term, -std.error, -statistic)

  df
}

#### Mplus reporting functions
mplus.model.fit <- function(file) {
  extractModelSummaries(file) %>%
    select(Observations, Parameters, LL, aBIC, CFI, RMSEA_Estimate) %>%
    kable(caption = "Mplus Fit Statistics")
}
mplus.parameter.table <- function(file) {
  extractModelParameters(file)$stdyx.standardized %>%
    filter(grepl("BY", paramHeader)|
             grepl("WITH", paramHeader)|
             grepl("Means", paramHeader)) %>%
    kable(caption = "Standardized Mplus factor loadings and correlations")
}


