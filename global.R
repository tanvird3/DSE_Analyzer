library(openxlsx)
library(TTR)
library(stringr)
library(plotly)
library(quantmod)
library(PerformanceAnalytics)
library(shinythemes)
library(shinyjs)
library(data.table)
library(plyr)
library(dplyr)

options(digits = 5)

# load the virtual env
reticulate::virtualenv_create("python35_env", python = "python3")
reticulate::virtualenv_install("python35_env",
                               packages = c("bdshare"),
                               ignore_installed = T)
reticulate::use_virtualenv("python35_env", required = TRUE)

# read instrument names
inst_name <- fread("Inst.csv")
inst_name <- inst_name$TRADING.CODE
t_default <- which(inst_name == "ACI")

# # read index data
# index <- fread("index.csv")
#
# # format the index data
# index <- mutate(index, DATE = as.Date(DATE, "%d-%m-%y")) %>% arrange(DATE)
