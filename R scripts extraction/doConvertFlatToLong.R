##
#
# source file to convert vcd database flat file to long format 
# for stats analysis and save it as an excel file
#
# source flat VCD file is supplied by user using the UI
#
##





library(openxlsx)
library(readxl)
library(here)
library(stringr)
library(sjmisc)


doConvertFlatToLong <- function(dfdatadb, dbname)
{
  log_info("Starting conversion of central db to long data format")
  df <- dfdatadb
  df$date = as.Date(df$date)
  df$date = format(df$date, "%d/%m/%Y")
  
  # 
  # how many 5s intervals are there? we will calculate this....
  # this is to allow for the different number of intervals between v2 and v3
  
  interval0start = grep("^R0_t_5$", colnames(df)) # the R0 -R3 time intervale data starts after here....
  interval1start = grep("^R1_t_5$", colnames(df))
  interval2start = grep("^R2_t_5$", colnames(df))
  interval3start = grep("^R3_t_5$", colnames(df))
  numintervals = interval1start - interval0start
  log_info("num time intervals: ",numintervals)
  log_info("interval 0 start: ",interval0start)
  log_info("interval 1 start: ",interval1start)
  log_info("interval 2 start: ",interval2start)
  log_info("interval 3 start: ",interval3start)
  # interval0start
  # interval1start
  # interval2start
  # interval3start
  #numintervals
  
  # construct the totals summary tab of the output
  #
  lbltotals = c("vcd_record_id","date_of_test","country","site","experimenter", "strain","treatment","resistance","host","prop_valid","r0_total","r1_total","r2_total","r3_total","top_half_total","bottom_half_total","total","inactive")
  dftotals <- data.frame(matrix(ncol = length(lbltotals), nrow = 0))
  colnames(dftotals)<- lbltotals
  #tmpid = df$prev_key
  tmpid = ""
  vcd_record = df$vcd_record_id
  date = df$date
  country = df$country
  site = df$site
  #country = "UK"
  #site = "LSTM"
  person = df$experimenter
  strain = df$strain
  treatment = df$treatment
  resistance = df$rstatus
  host = df$host_present
  inactive = df$inactive
  pv = df$valid_frames
  t0 = df$r0_total
  t1 = df$r1_total
  t2 = df$r2_total
  t3 = df$r3_total
  thtot = df$tophalftot
  bhtot = df$bottomhalftot
  tot = df$total
  dftotals = data.frame(cbind(vcd_record,date,country,site,person, strain, treatment,resistance, host, pv,t0,t1,t2,t3,thtot,bhtot,tot,inactive))
  dftotals[, 10:17] <- sapply(dftotals[, 10:17], as.numeric) # convert these cols to numeric
  dftotals$vcd_record <- as.numeric(dftotals$vcd_record) 
  
  # construct the main long format tab of the output - it will take some time to do for large datasets...
  #
  #
  numrows = nrow(df) * 4
  print(paste("rows: ",numrows, sep="  "))
  lbllongtotals = c("ID","vcd_record_id","Date of Test","Location","Time","Number_of_Movements","Experimenter","Strain","Treatment","Resistance","Host")
  dflongtotals <- data.frame(matrix(ncol = length(lbllongtotals), nrow = numrows))
  colnames(dflongtotals) <- lbllongtotals
  dfsinglerow <- data.frame(matrix(ncol = length(lbllongtotals), nrow = 0))
  colnames(dfsinglerow) <- lbllongtotals
  locations = c("R0","R1","R2","R3")
  print("Creating long format data - please wait for finish confirmation - : echo every 25th item")
  count = 1 
  for (r in 1:nrow(df))
  {
    if (r %% 25 == 0)
      {
      print(paste(tmpid,r,sep="   "))
      gc()
      }
    tmprow = df[r,]
    tmpid = tmprow$prev_key
    recordnum = tmprow$vcd_record_id
    strain = tmprow$strain
    treatment = tmprow$treatment
    resistance = tmprow$rstatus
    host = tmprow$host_present
    date = tmprow$date
    person = tmprow$experimenter
    validframeprop = tmprow$valid_frames
    r0 = tmprow[,interval0start:(interval1start-1)]
    r1 = tmprow[,interval1start:(interval2start-1)]
    r2 = tmprow[,interval2start:(interval3start-1)]
    r3 = tmprow[,interval3start:ncol(df)]
    for (t in 1:numintervals)
    {
      time = (t)*5
      nummovements = as.numeric(r0[,t])
      location = locations[1]
      dfsinglerow = cbind(tmpid,recordnum,date,location,time,nummovements,person,strain,treatment,resistance, host)
      dflongtotals[count,] <- dfsinglerow
      count = count + 1
      #dflongtotals = rbind(dflongtotals,dfsinglerow)
      nummovements = as.numeric(r1[,t])
      location = locations[2]
      dfsinglerow = cbind(tmpid,recordnum,date,location,time,nummovements,person,strain,treatment,resistance, host)
      dflongtotals[count,] <- dfsinglerow
      count = count + 1
      #dflongtotals = rbind(dflongtotals,dfsinglerow)
      nummovements = as.numeric(r2[,t])
      location = locations[3]
      dfsinglerow = cbind(tmpid,recordnum,date,location,time,nummovements,person,strain,treatment,resistance, host)
      dflongtotals[count,] <- dfsinglerow
      count = count + 1
      #dflongtotals = rbind(dflongtotals,dfsinglerow)
      nummovements = as.numeric(r3[,t])
      location = locations[4]
      dfsinglerow = cbind(tmpid,recordnum,date,location,time,nummovements,person,strain,treatment,resistance, host)
      dflongtotals[count,] <- dfsinglerow
      count = count + 1
      #dflongtotals = rbind(dflongtotals,dfsinglerow)
    }
  }
  print("Finished creating long formatted data")
  # put the column names back
  colnames(dflongtotals) <- lbllongtotals
  colnames(dftotals)<- lbltotals
  
  # change to numeric so they appear nice in excel
  
  dflongtotals$Time <- as.numeric(dflongtotals$Time)
  dflongtotals$Number_of_Movements <- as.numeric(dflongtotals$Number_of_Movements)
  dflongtotals$vcd_record_id <- as.numeric(dflongtotals$vcd_record_id)
  # sort by id, then time, then region
  dflongtotals <- dflongtotals[order(dflongtotals$ID, dflongtotals$Time, dflongtotals$Location),]
  rownames(dflongtotals) <- 1:nrow(dflongtotals)
  # sort by vcd record then time then region
  dflongtotals <- dflongtotals[order(dflongtotals$vcd_record_id, dflongtotals$Time, dflongtotals$Location),]
  rownames(dflongtotals) <- 1:nrow(dflongtotals)
  dflongtotals$ID = paste(dflongtotals$Strain,dflongtotals$Treatment,dflongtotals$Experimenter,dflongtotals$Host,dflongtotals$vcd_record_id, sep = "")

  
  
  # construct the filename from the input file name and create the path to the long format folder
  outputfilename = paste(dbname, "_extract_long_format.xlsx" ,sep="__")
  outputfile = here("longfileoutput",outputfilename)
  #create the workbook
  book <- createWorkbook()
  ##add sheets to the workbook
  addWorksheet(book, "raw data")
  addWorksheet(book, "totals")
  ##add data to the worksheets
  writeData(book, sheet = "raw data", x = dflongtotals)
  writeData(book, sheet = "totals", x = dftotals)
  # write the excel long format file
  #saveWorkbook(book, paste(outputfile, ".xlsx", sep=""))
  saveWorkbook(book, outputfile)
  
}

