# Loading packages  
library(shiny)
library(ggplot2)   
library(tidyverse)
library(tidyquant)
library(plotly)
library(scales)
library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(lubridate)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(maps)            
library(mapproj)
library(ggthemes)
library(gganimate)
library(gifski)

######################################################
# AGE16PLUS_TOT Total population age 16 years and over
# AGE18PLUS_TOT Total population age 18 years and over

OhioAgeCountyPop <- 
    read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv") %>% 
    filter(YEAR==12) %>% 
    mutate(county = str_remove(CTYNAME," County")) %>% 
    select(county, POPESTIMATE, 
           AGE16PLUS_TOT,AGE18PLUS_TOT)

head(OhioAgeCountyPop)

######################################################
### Reading the Ohio Vaccine Data (based on: Ohio-vaccine-13apr2021.R)

URL <- "https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv"
OhioVac <- read_csv(file=URL)

OhioVac <- OhioVac %>%
    mutate(Date = ymd(date))

head(OhioVac)

# Adding cumulative counts 
OhioVacCumul <- OhioVac %>%
    group_by(county) %>%
    arrange(Date) %>%
    mutate(cumulVacStart = cumsum(vaccines_started),
           cumulVacCompl = cumsum(vaccines_completed))

head(OhioVacCumul)

# Merging OhioAgeCountyPop and OhioVacCumul

OHVacRate <- merge(OhioVacCumul, 
                   OhioAgeCountyPop, 
                   by.x="county", 
                   by.y="county")

head(OHVacRate)

# Adding percentage (pct) of county populations vaccinated
OHVacRate <- OHVacRate %>% 
    mutate(pct_vac_start = round(cumulVacStart/POPESTIMATE*100,2),
           pct_vac_completed = round(cumulVacCompl/POPESTIMATE*100,2),
           pct_vac_start_18p = round(cumulVacStart/AGE18PLUS_TOT*100,2),
           pct_vac_completed_18p = round(cumulVacCompl/AGE18PLUS_TOT*100,2),
           pct_vac_start_16p = round(cumulVacStart/AGE16PLUS_TOT*100,2),
           pct_vac_completed_16p = round(cumulVacCompl/AGE16PLUS_TOT*100,2))

head(OHVacRate)

# Creating data frame for the MAP
# Creating data frame for the MAP
OHVacRateMAP <- OHVacRate %>% 
  mutate(subregion=county)%>%
  select(subregion, POPESTIMATE, AGE18PLUS_TOT, pct_vac_start, pct_vac_completed, pct_vac_start_18p, pct_vac_completed_18p)

head(OHVacRateMAP)

OHVacRateMAP$subregion<-stringr::str_to_lower(OHVacRateMAP$subregion)
head(OHVacRateMAP)

map.county <- map_data('county')
head(map.county)                     
ohio.county <- subset(map.county, region=="ohio")
head(ohio.county)

OHCountyVacRateMAP <-merge(ohio.county, OHVacRateMAP,
                           by.x="subregion", all = TRUE)

head(OHCountyVacRateMAP)

######################################################################################3

# Vectors with names of pct variables, OH counties, start/completed ...
VacPctOps <- names(OHVacRate)[11:16]

OHCounties <- unique(OHVacRate$county)

TSOps <- names(OHVacRate)[3:4]  # vac started and completed
head(TSOps)

Mapdata <- names(OHCountyVacRateMAP)[7:12]


### Building the Shiny app!

### Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel(title = "Ohio County COVID-19 Vaccination Results"),
    sidebarLayout(
        
        # Sidebar typically used to house input controls
        sidebarPanel(
            selectInput(inputId = "VACvar",
                        label = "Select response to display:",
                        choices = VacPctOps,
                        selected = "pct_vac_start_18p"),
            selectInput(inputId = "County",
                        label = "Select county to highlight:\n
                  (start typing to find your county)",
                        choices = OHCounties,
                        selected = "Butler"),
            selectInput(inputId = "TSvar",
                        label = "Variable Time Series Plot:",
                        choices = TSOps,
                        selected = "vaccines_started"),
            selectInput(inputId = "VACvar2",
                        label = "Compare selected response:",
                        choices = VacPctOps,
                        selected = "pct_vac_start_18p"),
            selectInput(inputId = "VarMap",
                        label = "Ohio Vaccination Map:",
                        choices = Mapdata,
                        selected = "POPESTIMATE"),
            
        ),
        
        # Main panel typically used to display outputs
        mainPanel(
            tabsetPanel(
                tabPanel("Line Graph Cummulative % Vaccinated",
                         plotlyOutput("VacPlot")),
                tabPanel("Bar Graph Cummulate % Vaccinated",
                         plotlyOutput("VacPlot2",
                         height =1500)),
                tabPanel("Vaccine started/completed series",
                         plotlyOutput("TimeSeriesPlot")),
                tabPanel("Ohio Vaccination Mapping",
                         plotlyOutput("Mapdata")),
                tabPanel("References",
                         tags$div(
                             tags$br(),tags$br(),
                             tags$p(" Author: Ibrahim Sanusi "),
                             tags$p(" Date: 05/12/2021"),
                             tags$a("Department Of Chemical and Biomedical Engineering Miami University"),
                             tags$p("References:"),
                             tags$a("https://www.r-bloggers.com/2019/05/how-to-save-and-load-datasets-in-r-an-overview/
                                                 https://shiny.rstudio.com/reference/shiny/0.11/conditionalPanel.html
                                                  https://shiny.rstudio.com/reference/shiny/1.0.4/downloadButton.html",
                                  
                           ),
                           
                            tags$br(),
                             tags$p("The data were obtained from a "),
                             #                        tags$br(),
                             tags$a("CSV data set",
                                    href="https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv"
                             ),
                             tags$br(),tags$br(),
                             tags$p(" downloaded from the "),
                             tags$a("Ohio Department of Health COVID-19 Dashboard",
                                    href="https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards/covid-19-vaccine/covid-19-vaccination-dashboard"
                             ),
                             tags$br(),tags$br(),
                             tags$p(" downloaded from the "),
                             tags$a("United State Census Bureau",
                                    href="https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv"
                             )
                         ))
            )
            
        )
        
    )    # close sidebarLayout
)      # close fluidPage


### Define server behavior for application here
server <- function(input, output) {
    output$VacPlot <- renderPlotly({
        ggplot() +
            geom_line(data=OHVacRate,
                      aes_string(x="date",
                                 y=input$VACvar,
                                 group="county"),
                      color="grey") +
            geom_line(data=filter(OHVacRate,
                                  county==input$County),
                      aes_string(x="date",
                                 y=input$VACvar,
                                 group="county"),
                      color="blue")
    })
    
    output$TimeSeriesPlot <- renderPlotly({
        ggplot() +
            geom_line(data=OHVacRate,
                      aes_string(x="date",
                                 y=input$TSvar,
                                 group="county"),
                      color="grey") +
            geom_line(data=filter(OHVacRate,
                                  county==input$County),
                      aes_string(x="date",
                                 y=input$TSvar,
                                 group="county"),
                      color="blue")
      })
    output$VacPlot2 <- renderPlotly({
        ggplot() +
            geom_col(data=OHVacRate,
                      aes_string(y=paste0('reorder(county,',input$VACvar2,')'), 
                                 x=input$VACvar),
                                 fill="grey") +
            geom_col(data=filter(OHVacRate,
                                  county==input$County),
                      aes_string(y=paste0('reorder(county,',input$VACvar2,')'), 
                                 x=input$VACvar),
                                 fill="blue")
    })
    output$Mapdata <- renderPlotly({
      ggplot()+
        geom_polygon(data=OHCountyVacRateMAP, 
                     aes_string(x="long",y="lat", group=" subregion", fill=input$VarMap))+
        scale_fill_gradient2_tableau(trans = "log2")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        labs(title="", x="", y="")+
        theme(plot.title = element_text(hjust = 0.5))+
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank())+        
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
    })
    

}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)



# References
#  https://www.r-bloggers.com/2019/05/how-to-save-and-load-datasets-in-r-an-overview/
#  https://shiny.rstudio.com/reference/shiny/0.11/conditionalPanel.html
#  https://shiny.rstudio.com/reference/shiny/1.0.4/downloadButton.html
#  https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-agesex.pdf
#  https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv
#  https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv

