#
#  function to actually extract the data from a single victa excel template tab 
#  and return a dataframe of a flat file version
#  This is based on code originally by Mark Fowler.
# 
#  modified by me to utilise UI inputs rather than have user-edited values 
#

library(openxlsx)
library(readxl)
library(here)
library(dplyr)


doExtraction <- function(df, filename, tabname, country, site, expdate, expname, str, trt, rstat, hoststatus, expnotes)
{
  log_info("Starting ViCTA Excel template data extraction function")
  file = filename
  outputlocation = "multiplecsvoutput"  
  filepluspath = here("",file)
  STRAIN = str 
  TREATMENT = trt 
  RESISTANCE = rstat
  EXPERIMENTER_INITIALS = expname
  country = country
  site = site
  sheettab = tabname
  HOST = hoststatus
  DATE = expdate
  explabel = expnotes
  #set manually for test
  # outputlocation = "multiplecsvoutput"
  # file = "test.xlsx"
  #   filepluspath = here("",file)
  # STRAIN = "KS"
  # TREATMENT = "UT"
  # RESISTANCE = "SUSCEPTIBLE"
  # EXPERIMENTER_INITIALS = "JJ"
  # country = "TESTUK"
  # site = "TESTLSTM"
  # sheettab = "E"
  # HOST = "HOST"
  # DATE = "01/01/23"
  # explabel = "TEST"


  # generate the key - new method - uses experimenter initials in key
  #PREF <- paste(substr(STRAIN,1,1), TREATMENT, substr(RESISTANCE,1,1), EXPERIMENTER_INITIALS, sep="")
  PREF <- "test"
  PREF
#  df = read_excel("Malawi UT Data Cone Test Template 22.11.2023.xlsx",sheet = "E",col_names = FALSE)
#  df = read_xlsx("ViCTA Kis-Kdr with host.xlsx",sheet = "A",col_names = FALSE)
  videofilename = df[df[1] == "Video_File_Analysed",] 
  videofilename = videofilename[!(is.na(videofilename[1])), ] # remove na rows
  videofilename = videofilename[,which(unlist(lapply(videofilename, function(x) !all(is.na(x)))))] # remove na cols
  videofilename = rename_at(videofilename,2,~"filename")
  videofilename = select(videofilename,-c(1))
    
  ic = ncol(videofilename)
  ir = nrow(videofilename)
  log_info("num rows filename:" ,ir)
  tmplist <- list()
  tmp = ""
  for(r in 1: ir)
  {
    tmp = ""
    for(c in 1:ic)  # we do this loop in case the filename is split across several columns in the spreadsheet due to incorrect text import
    {
      if (!(is.na(videofilename[r,c])))
        tmp = paste(tmp, videofilename[r,c],sep="_")
    }
    tmplist[r] = tmp 
  }
  outputvideofilenames =  as.data.frame(do.call(rbind, tmplist))  # create df from list
  outputvideofilenames =  as.character(outputvideofilenames$V1)  # create char list from df
  

  
    has_inactivity = FALSE 
  # check if inactivity is present in the results and if so, create storage for it
 
 # inactperstep = df[df[1] == "Inactivity_by_epoch:",]
 # inactperstep = inactperstep[!(is.na(inactperstep$...2)), ]
#  inactperstep$...1 = NULL
#  if (nrow(inactperstep) >0)
 #   has_inactivity = TRUE  else
  #  has_inactivity = FALSE 
 
  if (has_inactivity)
  {
  inactperstep$ID <- as.numeric(formatC(seq.int(nrow(inactperstep)), width=3, flag="0"))
  iter_cols = (ncol(inactperstep)-1)
  for(c in 1:iter_cols)
  {
    colnames(inactperstep)[c] = paste("inactive_t",(c*5),sep="")
  }
  write.csv(inactperstep,"test.csv",col.names = TRUE)
  print(has_inactivity)
  }
  
  
  # build tot activity over time
  totperstep = df[df[1] == "tot_reg_act_per_matrix",]
  totperstep = totperstep[!(is.na(totperstep$...2)), ]
  totperstep$...1 = NULL
  #totperstep = as.numeric(unlist(totperstep))
  #totperstep = lapply(totperstep, as.numeric)
  totperstep$ID <- as.numeric(formatC(seq.int(nrow(totperstep)), width=3, flag="0"))
  iter_cols = (ncol(totperstep)-1)
  #iter_cols = length(totperstep)
  for(c in 1:iter_cols)
  {
    colnames(totperstep)[c] = paste("tot_t",(c*5),sep="_")
  }
  
  
  # build R0 activity over time
  r0_step = df[df[1] == "R0",]
  r0_step = r0_step[!(is.na(r0_step$...2)), ]
  r0_step$...1 = NULL
  r0_step$ID <- as.numeric(formatC(seq.int(nrow(r0_step)), width=3, flag="0"))
  iter_cols = (ncol(r0_step)-1)
  for(c in 1:iter_cols)
  {
    colnames(r0_step)[c] = paste("R0_t",(c*5),sep="_")
  }
  
  # build R1 activity over time
  r1_step = df[df[1] == "R1",]
  r1_step = r1_step[!(is.na(r1_step$...2)), ]
  r1_step$...1 = NULL
  r1_step$ID <- as.numeric(formatC(seq.int(nrow(r1_step)), width=3, flag="0"))
  iter_cols = (ncol(r1_step)-1)
  for(c in 1:iter_cols)
  {
    colnames(r1_step)[c] = paste("R1_t",(c*5),sep="_")
  }
  
  
  # build R2 activity over time
  r2_step = df[df[1] == "R2",]
  r2_step = r2_step[!(is.na(r2_step$...2)), ]
  r2_step$...1 = NULL
  r2_step$ID <- as.numeric(formatC(seq.int(nrow(r2_step)), width=3, flag="0"))
  iter_cols = (ncol(r2_step)-1)
  for(c in 1:iter_cols)
  {
    colnames(r2_step)[c] = paste("R2_t",(c*5),sep="_")
  }
  
  # build R3 activity over time
  r3_step = df[df[1] == "R3",]
  r3_step = r3_step[!(is.na(r3_step$...2)), ]
  r3_step$...1 = NULL
  r3_step$ID <- as.numeric(formatC(seq.int(nrow(r3_step)), width=3, flag="0"))
  iter_cols = (ncol(r3_step)-1)
  for(c in 1:iter_cols)
  {
    colnames(r3_step)[c] = paste("R3_t",(c*5),sep="_")
  }
  
  log_info("useful data from input...")
  
  #EXTRACT USEFUL DATA FROM INPUT
  ##BUILD AREA R0 Data frame
  #r0 = df[df$Field == "R0", ] 
  r0 = df[df[1] == "R0", ] 
  colnames(r0)[2] <- "X__0"
  colnames(r0)[10] <- "X__10"
  r0 = r0[!(is.na(r0$X__10)), ]
  r0$ID <- formatC(seq.int(nrow(r0)), width=3, flag="0")
  
  
  
  ##BUILD AREA R1 Data frame
  r1 = df[df[1] == "R1", ] 
  colnames(r1)[2] <- "X__0"
  colnames(r1)[10] <- "X__10"
  r1 = r1[!(is.na(r1$X__10)), ]
  r1$ID <- formatC(seq.int(nrow(r1)), width=3, flag="0")
  
  
  ##BUILD AREA R2 Data frame
  r2 = df[df[1] == "R2", ] 
  colnames(r2)[2] <- "X__0"
  colnames(r2)[10] <- "X__10"
  r2 = r2[!(is.na(r2$X__10)), ]
  r2$ID <- formatC(seq.int(nrow(r2)), width=3, flag="0")
  
  
  ##BUILD AREA R3 Data frame
  r3 = df[df[1] == "R3", ] 
  colnames(r3)[2] <-  "X__0"
  colnames(r3)[10] <- "X__10"
  r3 = r3[!(is.na(r3$X__10)), ]
  r3$ID <- formatC(seq.int(nrow(r3)), width=3, flag="0")
  
  
  if (has_inactivity)
  {
    totinact= df[df[1] == "Number_of_non-movement_frames:", ] 
    totinact = totinact[!(is.na(totinact[1])), ] # remove na rows
    log_info("num inact: ",nrow(totinact))  
    totinact = totinact[,which(unlist(lapply(totinact, function(x) !all(is.na(x)))))]
    totinact$...1 <- NULL # delete label column
    totinact = as.numeric(totinact$...2) # convert to numeric
  }
  
  ##BUILD proportion of valid frames Data frame
  vf = df[df[1] == "Proportion_of_VALID_frames:", ] 
  vf = vf[!(is.na(vf[1])), ] # remove na rows
  log_info("num vf: ",nrow(vf))  
  vf = vf[,which(unlist(lapply(vf, function(x) !all(is.na(x)))))]
  vf$...1 <- NULL # delete label column
  vf = as.numeric(vf$...2) # convert to numeric
  vf = round(vf, digits=3) # round to 3dp
  #vf =  as.character(vf)  # create char list from df
  
  ##BUILD proportion of non moving Data frame
  nm = df[df[1] == "Number_of_non-movement_frames:", ] 
  nm = nm[!(is.na(nm[1])), ] # remove na rows
  log_info("num non moving: ",nrow(nm))  
  nm = nm[,which(unlist(lapply(nm, function(x) !all(is.na(x)))))]
  nm$...1 <- NULL # delete label column
  nm = as.numeric(nm$...2) # convert to numeric

  
  log_info("RECONFIGURE USEFUL DATA INTO TEMPLATE FORM...")
  #RECONFIGURE USEFUL DATA INTO TEMPLATE FORM
  ##get number of iterations in for loop (1 - count of fields minus area and ID, 2 - number of rows)
  ## DECLARE VARs and DATA FRAMES
  iter_cols = (ncol(r0) - 2)
  iter_rows = nrow(r0)
  
  output =    data.frame()
  output_r0 = data.frame()
  output_r1 = data.frame()
  output_r2 = data.frame()
  output_r3 = data.frame()
  
  for(r in 1:iter_rows)
  {
    for(c in 1:iter_cols)
    { 
      ## location format DF[Row,Col]
      #ID
      output_r0[c+(iter_cols*(r-1)),1] <- paste(PREF, r0[r,iter_cols+2],  sep="") 
      output_r1[c+(iter_cols*(r-1)),1] <- paste(PREF, r0[r,iter_cols+2],  sep="") 
      output_r2[c+(iter_cols*(r-1)),1] <- paste(PREF, r0[r,iter_cols+2],  sep="") 
      output_r3[c+(iter_cols*(r-1)),1] <- paste(PREF, r0[r,iter_cols+2],  sep="") 
      #Location
      output_r0[c+(iter_cols*(r-1)),2] <- r0[r,1]  
      output_r1[c+(iter_cols*(r-1)),2] <- r1[r,1]
      output_r2[c+(iter_cols*(r-1)),2] <- r2[r,1]
      output_r3[c+(iter_cols*(r-1)),2] <- r3[r,1]          
      #time
      output_r0[c+(iter_cols*(r-1)),3] <- (c*5)-5
      output_r1[c+(iter_cols*(r-1)),3] <- (c*5)-5
      output_r2[c+(iter_cols*(r-1)),3] <- (c*5)-5
      output_r3[c+(iter_cols*(r-1)),3] <- (c*5)-5
      #number
      output_r0[c+(iter_cols*(r-1)),4] <- r0[r,c+1]
      output_r1[c+(iter_cols*(r-1)),4] <- r1[r,c+1]
      output_r2[c+(iter_cols*(r-1)),4] <- r2[r,c+1]
      output_r3[c+(iter_cols*(r-1)),4] <- r3[r,c+1]
      #STRAIN
      output_r0[c+(iter_cols*(r-1)),5] <- STRAIN
      output_r1[c+(iter_cols*(r-1)),5] <- STRAIN
      output_r2[c+(iter_cols*(r-1)),5] <- STRAIN
      output_r3[c+(iter_cols*(r-1)),5] <- STRAIN
      #TREATMENT
      output_r0[c+(iter_cols*(r-1)),6] <- TREATMENT
      output_r1[c+(iter_cols*(r-1)),6] <- TREATMENT
      output_r2[c+(iter_cols*(r-1)),6] <- TREATMENT
      output_r3[c+(iter_cols*(r-1)),6] <- TREATMENT
      #RESISTANCE
      output_r0[c+(iter_cols*(r-1)),7] <- RESISTANCE
      output_r1[c+(iter_cols*(r-1)),7] <- RESISTANCE
      output_r2[c+(iter_cols*(r-1)),7] <- RESISTANCE
      output_r3[c+(iter_cols*(r-1)),7] <- RESISTANCE
      #HOST
      output_r0[c+(iter_cols*(r-1)),8] <- HOST
      output_r1[c+(iter_cols*(r-1)),8] <- HOST
      output_r2[c+(iter_cols*(r-1)),8] <- HOST
      output_r3[c+(iter_cols*(r-1)),8] <- HOST
    }
  }
  
  
  
  ##CLEAN TABLE
  colnames(output_r0)[1] <- "ID"
  colnames(output_r1)[1] <- "ID"
  colnames(output_r2)[1] <- "ID"
  colnames(output_r3)[1] <- "ID"
  
  colnames(output_r0)[2] <- "Location"
  colnames(output_r1)[2] <- "Location"
  colnames(output_r2)[2] <- "Location"
  colnames(output_r3)[2] <- "Location"
  
  colnames(output_r0)[3] <- "Time"
  colnames(output_r1)[3] <- "Time"
  colnames(output_r2)[3] <- "Time"
  colnames(output_r3)[3] <- "Time"
  
  colnames(output_r0)[4] <- "Number_of_Movements"
  colnames(output_r1)[4] <- "Number_of_Movements"
  colnames(output_r2)[4] <- "Number_of_Movements"
  colnames(output_r3)[4] <- "Number_of_Movements"
  
  colnames(output_r0)[5] <- "Strain"
  colnames(output_r1)[5] <- "Strain"
  colnames(output_r2)[5] <- "Strain"
  colnames(output_r3)[5] <- "Strain"
  
  colnames(output_r0)[6] <- "Treatment"
  colnames(output_r1)[6] <- "Treatment"
  colnames(output_r2)[6] <- "Treatment"
  colnames(output_r3)[6] <- "Treatment"
  
  colnames(output_r0)[7] <- "Resistance"
  colnames(output_r1)[7] <- "Resistance"
  colnames(output_r2)[7] <- "Resistance"
  colnames(output_r3)[7] <- "Resistance"
  
  colnames(output_r0)[8] <- "Host"
  colnames(output_r1)[8] <- "Host"
  colnames(output_r2)[8] <- "Host"
  colnames(output_r3)[8] <- "Host"
  
  output_r0 = output_r0[!(is.na(output_r0$Number_of_Movements)), ]
  output_r1 = output_r1[!(is.na(output_r1$Number_of_Movements)), ]
  output_r2 = output_r2[!(is.na(output_r2$Number_of_Movements)), ]
  output_r3 = output_r3[!(is.na(output_r3$Number_of_Movements)), ]
  
  
  #COMBINE THE SEPERATE DATAFRAMES AND ORDER APPROPRIATELY
  output <- rbind(output_r0, output_r1, output_r2, output_r3)
  output <- output[order(output$ID, output$Time, output$Location),]
  rownames(output) <- 1:nrow(output)
  
  
  
  log_info("########## CALCULATE REPLICATE TOTALS  ########################...")
  
  ########## CALCULATE REPLICATE TOTALS  ########################
  
  #convert and aggregate area totals
  output_r0$Number_of_Movements = as.numeric(output_r0$Number_of_Movements)
  r0_sum = aggregate(output_r0$Number_of_Movements, by=list(output_r0$ID), FUN=sum)
  colnames(r0_sum)[1] <- "ID"
  colnames(r0_sum)[2] <- "r0_total"
  
  output_r1$Number_of_Movements = as.numeric(output_r1$Number_of_Movements)
  r1_sum = aggregate(output_r1$Number_of_Movements, by=list(output_r1$ID), FUN=sum)
  colnames(r1_sum)[1] <- "ID"
  colnames(r1_sum)[2] <- "r1_total"
  
  output_r2$Number_of_Movements = as.numeric(output_r2$Number_of_Movements)
  r2_sum = aggregate(output_r2$Number_of_Movements, by=list(output_r2$ID), FUN=sum)
  colnames(r2_sum)[1] <- "ID"
  colnames(r2_sum)[2] <- "r2_total"
  
  output_r3$Number_of_Movements = as.numeric(output_r3$Number_of_Movements)
  r3_sum = aggregate(output_r3$Number_of_Movements, by=list(output_r3$ID), FUN=sum)
  colnames(r3_sum)[1] <- "ID"
  colnames(r3_sum)[2] <- "r3_total"
  
  log_info("just before merge totals...")
  
  ## merge totals
  total_output <-  merge(r0_sum,merge(r1_sum,merge(r2_sum,r3_sum,by="ID"), by="ID"), by="ID")
  
  ## calculate total movements for each area
  total_output$total = total_output$r0_total + total_output$r1_total + total_output$r2_total + total_output$r3_total
  
  log_info("new col for filenames...")
  
  
  # create new column for filenames
  total_output$filename <- outputvideofilenames
  #total_output$filename <- videofilename$filename
  
  
  # create new column for date
  total_output$date <- DATE
  
  # create new column with strain
  total_output$strain <- STRAIN
  
  # create new column with treatment
  total_output$treatment <- TREATMENT
  
  # create new column with host status
  total_output$host_present <- HOST
  
  # create new column with res/susc status
  total_output$rstatus <- RESISTANCE
  
  # create new column with experimenter
  total_output$experimenter <- EXPERIMENTER_INITIALS
  
  
  # create a new column for the experiment label
  total_output$experimentlabel <- explabel
  
  # create a new column for the country label
  total_output$country <- country
  
  # create a new column for the site label
  total_output$site <- site
  
  log_info("site is: ",site)
  
  
  
  #create new column for proportion of valid frames
  total_output$valid_frames <- vf

  log_info("just before repop id...")
  
  # repopulate the id column with just run number
  #total_output$ID = 1:nrow(total_output) # don't use this one as it only adds id without leading zero
  # use the one below instead
  total_output$ID <- as.numeric(formatC(seq.int(nrow(total_output)), width=3, flag="0"))
  
  

  # create new column for non moving frames
  if(has_inactivity)
  {
  total_output$tot_inactive = totinact 
  total_output <- merge(total_output,inactperstep, by="ID")
  }
  
  
  log_info("just before r0 prop...")
  
  # create new column with r0 proportion
  total_output$r0prop = total_output$r0_total / total_output$total
  
  # create new column with r1 proportion
  total_output$r1prop = total_output$r1_total / total_output$total
  
  # create new column with r2 proportion
  total_output$r2prop = total_output$r2_total / total_output$total
  
  log_info("just before r3 prop...")
  
  # create new column with r3 proportion
  total_output$r3prop = total_output$r3_total / total_output$total
  
  total_output$tophalftot = total_output$r0_total + total_output$r1_total
  
  total_output$bottomhalftot = total_output$r2_total + total_output$r3_total
  
  total_output$tophalfprop = total_output$tophalftot / total_output$total
  
  total_output$bottomhalfprop = total_output$bottomhalftot / total_output$total
  
  
  
  log_info("use previous key encoding...")
  
  
  # use previous key encoding too
  
  total_output$prev_key <- formatC(seq.int(nrow(r0)), width=3, flag="0")
  total_output$prev_key <- paste(PREF,total_output$prev_key,sep="")
  
  total_output <- merge(total_output,totperstep, by="ID")
  total_output <- merge(total_output,r0_step, by="ID")
  total_output <- merge(total_output,r1_step, by="ID")
  total_output <- merge(total_output,r2_step, by="ID")
  total_output <- merge(total_output,r3_step, by="ID")
  
  # convert char format initial cols to numeric
  total_output$tot_t_5 = as.numeric(total_output$tot_t_5)
  total_output$tot_t_10 = as.numeric(total_output$tot_t_10)
  total_output$tot_t_15 = as.numeric(total_output$tot_t_15)
  
  total_output$R0_t_5 = as.numeric(total_output$R0_t_5)
  total_output$R0_t_10 = as.numeric(total_output$R0_t_10)
  total_output$R0_t_15 = as.numeric(total_output$R0_t_15)
  
  total_output$R1_t_5 = as.numeric(total_output$R1_t_5)
  total_output$R1_t_10 = as.numeric(total_output$R1_t_10)
  total_output$R1_t_15 = as.numeric(total_output$R1_t_15)
  
  total_output$R2_t_5 = as.numeric(total_output$R2_t_5)
  total_output$R2_t_10 = as.numeric(total_output$R2_t_10)
  total_output$R2_t_15 = as.numeric(total_output$R2_t_15)
  
  total_output$R3_t_5 = as.numeric(total_output$R3_t_5)
  total_output$R3_t_10 = as.numeric(total_output$R3_t_10)
  total_output$R3_t_15 = as.numeric(total_output$R3_t_15)
  #EXPORT OUTPUT TO .CSV (automatically named from input file)
  ##manually write csv
  output_file = paste(file, sheettab ,sep="__")
  output_file = paste(output_file, "__WIDE", sep="")
  output_file = paste(output_file, ".csv", sep="")
  output_file = here(outputlocation,output_file)
  log_info("Saving tab as csv")
  write.csv(total_output,output_file, row.names = FALSE)
  
  
  log_info("all done...")
}