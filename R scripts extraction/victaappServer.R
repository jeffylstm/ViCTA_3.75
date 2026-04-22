# server code to handle UI inputs
#
#


library(shiny)

options(shiny.maxRequestSize=190 * 1024 ^2) # increase file upload limit to 120MB
# I needed to do this because our victa excel files are very large



server <- function(input, output, session)
{
  sheets_name <- reactive({
    if (!is.null(input$butFile)) {
      return(excel_sheets(path = input$butFile$datapath))
    } else {
      return(NULL)
    }
  })
  
  
  # reactive to handle click of go button on page to extract tabs from excel file
  data <- reactive(
    {
    req(input$butFile)
    if (!is.null(input$butFile) &&
          (input$comboTabname %in% sheets_name())) {
       read_excel(input$butFile$datapath,
                        sheet = input$comboTabname,
                  col_names = FALSE)
       
      } else {
        return(NULL)
      }
  })
  

  
  # reactive to handle click of go button on page of assemble multiple csvs
  datacsv <- reactive(
    {
      req(input$butCSVFile)
      if(is.null(input$butCSVFile))
      return ()
      else 
      {
        nfiles = nrow(input$butCSVFile) 
        csv = list()
        for (i in 1 : nfiles)
        {
          log_info("name: ",input$butCSVFile$name[i])
          if (i == 1) 
            headerflag = TRUE
          else headerflag = TRUE
          partialdataset <- read.csv(input$butCSVFile$datapath[i], sep = ",", header = headerflag)
          log_info("Ncols: ", ncol(partialdataset))
          if (i ==1)
            fullcsvdataset = partialdataset
          else
            fullcsvdataset = rbind(fullcsvdataset,partialdataset)
        }
        return (fullcsvdataset)
      }
    })
  
  
  
  
  # reactive to handle click of go button on page to extract long format data from central db file
  datadb <- reactive(
    {
      req(input$butLongFile)
      if (!is.null(input$butLongFile))
        {
        dbname = input$butLongFile$name
        read_excel(input$butLongFile$datapath, sheet = "DATABASE",  col_names = TRUE)
      } else {
        return(NULL)
      }
    })
  

  # render a sample of the tab's data to a UI table structure  
  output$tblRawexceldata <- renderTable({
    head(data(), 15)
  })
  
  
  # event to handle click of button on page to assemble csvs into flat db
  observeEvent(input$butStartCSVs,{
    log_info("start assemble csvs clicked")
    dfdatacsv <<- datacsv()
    dbname = input$tfDBFilename
    doAssembleFlatFromCSVs(dfdatacsv, dbname)
  })
  
  # event to handle click of button on first page to extract data from excel tabs
  observeEvent(input$butStart, {
    filename = input$butFile$name
    sheettab = input$comboTabname
    str = input$comboStrain 
    trt = input$comboTreatment 
    rstatus = input$comboResistancestatus
    host = input$comboHost
    country = input$comboCountry
    localsite = input$tfLocalsite
    exp = input$tfExpname
    notes = input$tfExpnotes
    expdate = input$datetest
    df  <<- data()
    head(df)
    # dump to console as a sanity check
    cat("\nFilename:, ",filename,"  Sheet Tab: ",sheettab)
    cat("\nCountry:, ",country,"  Local site: ",localsite)
    cat("\nDate:, ",expdate,"  Experimenter: ",exp)
    cat("\nstrain:, ",str,"  treatment: ",trt, "R status:",rstatus)
    cat("\nHost Status:, ",host,"  Exp Notes: ",notes)
    # actually do the extraction
    # params:   function(df, tabname, country, site, expdate, expname, str, trt, rstat, hoststatus, expnotes)
    doExtraction(df,filename, sheettab, country, localsite, expdate, exp, str, trt, rstatus, host, notes)
  })
  
  
  # event to handle click of button on page to convert db to long format
  observeEvent(input$butStartLong,{
    log_info("start convert to long clicked")
    dfdatadb <<- datadb()
    dbname = input$butLongFile$name
    doConvertFlatToLong(dfdatadb, dbname)
  })
  
  
}
