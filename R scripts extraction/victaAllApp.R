#
# ViCTA Utils 
#
# project to:
#             allow user to extract tabs from excel to single csvs
#             assemble csvs to make a flat file db
#             convert a flat file db to long format for analysis
#
# locations:
#            victa excel files to be put in root of project
#            extracted csv from tabs saved in 'multiplecsvoutput' folder
#            VCD from multiple csvs saved to 'centraldatabaseoutput'
#            long formatted vcd saved to 'longfileoutput' folder
#                  
#
# Jeff Jones
#
# incept date: 2nd June 2022
#
# version date: 08 July 2022
#
# had to amend doextraction on 271123 as it could only cope with 99 reps per ts combo

library(shiny)
library(bslib)
library(readxl)
library(logger)
library(here)
library(data.table)

rm(list = ls())

#set_here()
here::here()
#print(getwd())
#here::dr_here()


source('global.R')
source('doExtraction.R')
source('doAssembleFlatFromCSVs.R')
source('doConvertFlatToLong.R')
source('victaappUI.R')
source('victaappServer.R')
log_threshold(DEBUG)
log_info('Script starting up...')


shinyApp(ui ,server)


