##
#
# source file to convert multiple csv files to vcd database flat file format and save it
#
# list of flat csv files (a single csv is a victa tab output) is supplied by UI
#
##





library(openxlsx)
library(readxl)
library(here)
library(dplyr)


doAssembleFlatFromCSVs <- function(df, filename)
{
  log_info("Starting do Assemble Source to write merged csvs as an excel file")
  #head(df)
  
  df[is.na(df)] = 0 # replaces any na with 0
  # set the new file location to write the output to
  #setwd(outputlocation)
  # write the  large csv file
  df$date = as.Date(x = df$date, format = "%d/%m/%Y")
  df$date = format(df$date, "%d/%m/%Y")
  
  df$vcd_record_id = 0 ;
  for (i in 1:nrow(df))
  {
    df$vcd_record_id[i] = i 
  }
  
  df = relocate(df,vcd_record_id) # move this column to the first column
  
   if (is.null(filename) || filename == "")
     filename = "untitled"
   log_info("creating workbook...")
   log_info("output filename:",filename)
   outputlocation = "centraldatabaseoutput"  
   
   output_file = paste(filename, ".xlsx", sep="")
   output_file = here(outputlocation,output_file)
   

  # #create workbook
   book <- createWorkbook()
  # 
  # ##add sheets
  addWorksheet(book, "DATABASE")
  # addWorksheet(book, "totals")
  # 
  # ##add data
  writeData(book, sheet = "DATABASE", x = df)
  # writeData(book, sheet = "totals", x = total_output)
  # 
  # # Export the file
   saveWorkbook(book, output_file)
  log_info("all done...")
}

