library(data.table)
library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)
library(psych)
library(dygraphs)
library(xts)
# run parallel for Data.table
setDTthreads(threads = 0)
getDTthreads(verbose=TRUE) 


#shinyApp(ui = ui, server = server)
#getwd()
#setwd("/home/ad/Data_science/R_studio/Git/FQC/deploy/")
setwd("C:/Users/DNN/Data_science/Git/FQC/deploy/")
shiny::runApp(host="0.0.0.0",port=4414)

