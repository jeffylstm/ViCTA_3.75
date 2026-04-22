library(shiny)
#
# code to generate HTML UI via shiny mappers
# 
# events (or reactives) are handled by the server code
#



ui = fluidPage(
  theme = bs_theme(version = 4, bootswatch = "darkly"),
  titlePanel("ViCTA Utils"),
  
  navlistPanel(
    "Menu:",
    tabPanel("Extract CSV From From Each Excel Tab",
          #   h3("Extract ViCTA Data From Excel Files")
             
             fluidRow(
               column(4, fileInput('butFile', 'Select ViCTA Data Entry Template', accept = c(".xlsx")),
               ), # end col
               column(6,selectInput('comboTabname','Name of Tab in Spreadsheet', choices = tabs),
               ), # end col
               # column(2,textInput('tfTabname','Name of Tab'),
               #), # end col
             ), # end row
             fluidRow(
               column(4,selectInput('comboCountry', 'Country', countries),
               ), # end col
               column(4,textInput('tfLocalsite','Name of Local Site'),
               ), # end col
             ), # end row
             fluidRow(
               column(4, dateInput("datetest", "Date experiments were done"),
               ), # end col
               column(4,textInput('tfExpname','Initials of Operator'),
               ), # end col
             ), # end row
             fluidRow(
               column(4, selectInput('comboStrain', 'Strain', strains),
               ), # end col
               column(4, selectInput('comboTreatment', 'Treatment', treatments),
               ), # end col
               column(4, selectInput('comboResistancestatus', 'Resistance Status', resistancestatus),
               ), # end col
             ), # end row
             fluidRow(
               column(4,selectInput('comboHost', 'Host Status', hoststatus),
               ), # end col
               column(4,textInput('tfExpnotes','Notes for Experimenter'),
               ), # end col
             ), # end row
             fluidRow(
               column(4, actionButton("butStart", "Extract Data From This Tab"),
               ), # end col
               column(10, tableOutput("tblRawexceldata"),
               ), # end col
             ), # end row
             
    ),
    tabPanel("Create Flat File DB From Multiple CSV Files",
           #  h3("This is the second panel")
             fluidRow(
               column(4, fileInput('butCSVFile', 'Select Multiple CSV Files to Create Flat DB', accept = c(".csv"), multiple = TRUE),
               ), # end col
               column(4,textInput('tfDBFilename','Name of Excel Database to Save'),
               ), # end col
             ), # end row
           fluidRow(
             column(4, actionButton("butStartCSVs", "Convert CSVs to single Flat DB File"),
             ), # end col
             column(10, tableOutput("tblCSVList"),
             ), # end col
           ), # end row
           
    ),
    tabPanel("Export Flat File DB to Long Format DB",
             #h3("This is the third panel")
             fluidRow(
               column(4, fileInput('butLongFile', 'Select Flat DB to Convert To Long', accept = c(".csv"), multiple = FALSE),
               ), # end col
             ), # end row
             fluidRow(
               column(4, actionButton("butStartLong", "Convert Flat DB File to Long Format"),
               ), # end col
               column(10, tableOutput("tblFlatList"),
               ), # end col
             ), # end row
    )
  )
)# end of ui declaration

