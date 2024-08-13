library(tidyverse)
library(lubridate)
library(rpivotTable)
library(cleaninginspectoR)
library(HighFrequencyChecks)
library(rio)
library(koboquest)

# pulluuid
pulluuid <- function (data,logiquetest) {
  data$uuid[which(logiquetest)]
  }

# log template
makeslog <- function (data, logbook, index, question, explanation, new.value = "NULL", action = "check", feedback = "") {
  if (length(index) > 0) {
    if (question == "all") {
      oldval <- "-"
    } else {
        oldval <- as.character(data[[question]][data$uuid%in%index])
    }
    newlog <- data.frame(
      camp = data$camp[data$uuid%in%index],
      organization = data$enum_egency[data$uuid%in%index],
      today = data$today[data$uuid%in%index],
      enumerator = data$enumerator_ID[data$uuid%in%index],
      uuid = index,
      question = question,
      old.value = oldval,
      new.value = new.value,
      issue = explanation,
      action = action,
      resp_phone = data$resp_phone[data$uuid%in%index],
      feedback = feedback)
    bind_rows(logbook,newlog)
  } else{
    logbook
  }
}

initiate_logbook <- function() {
  logbook <- data.frame(
    organization = character(),
    camp= character(),
    today = character(),
    enumerator= character(),
    uuid= character(),
    question = character(),
    old.value = character(),
    new.value = character(),
    issue = character(),
    action = character(),
    feedback = character(),
    resp_phone = character()
  )
  return(logbook)
}

'%!in%' <- Negate('%in%')

# function to detect outliers
detect.outliers <- function(df, method = "sd", n.sd = 3) {
  res <- data.frame()
  for (col in colnames(df)[colnames(df)!=c("uuid","enumerator_ID")]){
    #define columns for the cleaning log
    df.temp <- data.frame(
      uuid = df$uuid,
      camp = df$camp,
      organization = df$enum_egency,
      today = df$today,
      enumerator = df$enumerator_ID,
      new.value = " ",
      action = "AO/FO to confirm",
      resp_phone = df$resp_phone,
      feedback = "",
      value = as.numeric(df[[col]])) %>%
      filter(!is.na(value))
    
    if (method == "sd-linear") {
      df.temp <- df.temp %>%
        mutate(is.outlier.high = value > mean(value, na.rm=T) + n.sd*sd(value, na.rm=T),
               is.outlier.low = value < mean(value, na.rm=T) - n.sd*sd(value, na.rm=T))
    } else if (method == "sd-log") {
      df.temp <- df.temp %>%
        mutate(col.log = log(value),
               is.outlier.high = col.log > mean(col.log, na.rm=T) + n.sd*sd(col.log, na.rm=T),
               is.outlier.low = col.log < mean(col.log, na.rm=T) - n.sd*sd(col.log, na.rm=T))
    } else if (method == "iqr-linear") {
      df.temp <- df.temp %>%
        mutate(is.outlier.high = value > quantile(value, 0.75) + 1.5*IQR(value),
               is.outlier.low = value < quantile(value, 0.25) - 1.5*IQR(value))
    } else if (method == "iqr-log") {
      df.temp <- df.temp %>%
        mutate(col.log = log(value),
               is.outlier.high = col.log > quantile(col.log, 0.75) + 1.5*IQR(col.log),
               is.outlier.low = col.log < quantile(col.log, 0.25) - 1.5*IQR(col.log))
    } else stop("Method unknown")
    df.temp <- df.temp %>% 
      pivot_longer(c("is.outlier.high", "is.outlier.low"), 
                   names_to = "issue", values_to = "is.outlier") %>% 
      filter(is.outlier) %>% 
      mutate(question = col, old.value = value,
             issue = ifelse(issue == "is.outlier.high", "High outlier", "Low outlier")) %>%  
      select(organization, today, enumerator, uuid, question, old.value, new.value, issue, action, feedback, resp_phone)
    res <- rbind(res, df.temp)
  }
  return(res)
}

# Anomyze dataset
anonymise_dataset <- function(data, variables_to_remove=c()){
  
  data_anon <- data[, !(names(data) %in% variables_to_remove)]
  
  data_anon
}

# cleaning log locations
coverage_clog <- function(data, coverage){
  clogs  <- data %>% 
    filter(camp %in% coverage)
}

# time to wait before running next line (in seconds)
wait_time <- function(x){
  p <- proc.time()
  Sys.sleep(x)
  proc.time() -p
}

makesdeletion <- function (data, deletion_log, index, explanation) {
  if (length(index) > 0) {
    newdeletion <- data.frame(
      uuid = index,
      explanation = explanation,
      enumerator = data$enumerator_ID[data$uuid %in% index])
    bind_rows(deletion_log, newdeletion)
  } else{
    deletion_log
  }
}

initiate_deletion <- function() {
  deletion_log <- data.frame(
    uuid = character(),
    explanation = character(),
    enumerator = character()
  )
  return(deletion_log)
}
