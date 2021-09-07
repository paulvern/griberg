library(shiny)
library(leaflet)
library(leaflet.extras)
library(crosstalk)
library(openair)
library(plotly)
library(ggplot2)
library(zoo)
library(dplyr)
library(DT)
library (RCurl)
library(rgdal)
library(rhdf5)
library(stringr)
library(fst)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(raster)
library(gribr)
library(googledrive)
library(gargle)
library(httr)
# Define UI for data download app ----
options(shiny.maxRequestSize = 120 * 1024 ^ 2)
httr::set_config(httr::config(ssl_verifypeer = 0L))
drive_deauth()
google_app <- httr::oauth_app("yourappname",
                              key = "yourkey.apps.googleusercontent.com",
                              secret = "yoursecret")
drive_auth_configure(app = google_app)

setwd("/srv/shiny-server/griberg/www/")
cip <-
  drive_ls("https://drive.google.com/drive/folders/0B7KLnPu6vjdPVGJKR3E4SEluU0U")

today <- format(Sys.Date() - 2, "%Y%m%d")
today_string <- paste0("erg5.", today, "0000.grib")
print(today_string)
getto <- cip$id[cip$name == today_string]
try(drive_download(as_id(getto), overwrite = TRUE))
gribettone <- grib_open(today_string)
grb <- grib_list(gribettone)
grb2 <- grib_list(gribettone, namespace = "mars")
grb <- cbind(grb, grb2)
grb3 <- grib_list(gribettone, namespace = "parameter")
grb <- cbind(grb, grb3)
grb4 <- grib_list(gribettone, namespace = "statistics")
grb <- cbind(grb, grb4)

cs <- NULL
for (i in 1:180) {
  stati <- try(grib_get_message(gribettone, i))
  print (i)
  print(length(stati))
  if (length(stati) > 1) {
    if (i == 1) {
      stati2 <-
        rbind(cs,
              c(
                i,
                stati$typeOfStatisticalProcessing,
                stati$lengthOfTimeRange
              ))
    }
    if (i > 1) {
      if (!is.null(stati$typeOfStatisticalProcessing)) {
        stati2 <-
          rbind(stati2,
                c(
                  i,
                  stati$typeOfStatisticalProcessing,
                  stati$lengthOfTimeRange
                ))
      }
    }
  }
}
stati2 <- as.data.frame(stati2)
colnames(stati2) <- c("n", "tOSP", "lOTR")
grb$n <- rownames(grb)
grb <- grb[, c(-3, -2, -9)]
grb$n <- as.numeric(grb$n)
grb <- left_join(grb, stati2, by = "n")
grb <- grb[, c(26, 27, 1:25)]
grb[] <- lapply(grb, as.character)
gribettone <- brick(today_string)
nomi <- length(names(gribettone))
nomi <- c(1:nomi)
ui = dashboardPage(
  dashboardHeader(title = 'Dati Clima - GRIB',
                  tags$li(
                    a(href = 'http://www.arpae.it',
                      icon("power-off"),
                      title = "Torna ad Arpae"),
                    class = "dropdown"
                  )),
  dashboardSidebar(width = 0),
  
  
  
  # Main panel for displaying outputs ----
  dashboardBody(
    leafletOutput("mappetta", height = 600),
    div(dataTableOutput("stat"), style = "font-size:70%"),
    
    textOutput("latlongu"),
    
    downloadButton('downloadData1', 'Download dataset plottato come csv')
    
  )
  
  
)


server <- function(input, output, session) {
  hide(id = "Inputone")
  
  
  
  output$mappetta <- renderLeaflet({
    need(input$stat_rows_selected != "",
         "Per favore scegli almeno una riga")
    pal <-
      colorNumeric(c("#00FF00", "#FFFF00", "#FF0000"),
                   values(gribettone[[eval(as.numeric(input$stat_rows_selected))]]),
                   na.color = "transparent")
    leaflet(p) %>%
      addTiles() %>%
      addRasterImage(gribettone[[eval(as.numeric(input$stat_rows_selected))]], opacity =
                       0.6, colors = pal) %>%
      addLegend(pal = pal, values = values(gribettone[[eval(as.numeric(input$stat_rows_selected))]]))
    
  })
  
  output$stat <- renderDataTable({
    opti = list(
      paging = TRUE,
      lengthMenu = list(c(10, 25, 100, 500), c('10', '25', '100', '500')),
      autoWidth = TRUE,
      columnDefs = list(list(
        width = '100px', targets = "_all"
      )),
      dom = 'BPfrtip',
      pageLength = 10,
      
      buttons = c('pageLength', 'colvis', 'copy', 'csv'),
      scrollX = TRUE
    )
    datatable(
      grb,
      selection = list(mode = 'single', selected = rownames(grb)[1]),
      options = opti,
      filter = "top",
      extensions = "Buttons"
    )
    
    
  }, selection = "single")
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0(input$cella,
             '_',
             input$daterange[1],
             '_',
             input$daterange[2],
             '.csv')
    },
    content = function(file) {
      write.csv (datasetInput(), file, row.names = FALSE)
    }
  )
}
# Create Shiny app ----
shinyApp(ui, server)
