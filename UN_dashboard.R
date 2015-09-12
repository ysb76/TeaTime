library(shinydashboard)
library(googleVis)

runApp(
        list(ui = dashboardPage(
                dashboardHeader(title="United Nations 2015"),
                dashboardSidebar(
                        sidebarMenu(
                                menuItem("Dashboard", icon=icon("dashboard")),
                                menuItem("Data Availability", icon = icon("th"), tabName = "dataAvail",
                                        badgeLabel = "new", badgeColor = "green"),
                                menuItem("Population", icon = icon("users"), tabName="popn"),
                                menuItem("Yearly Changes", icon=icon("line-chart"), tabName="motion"),
                                menuItem(uiOutput("regionSelector"), tabName="regionSelector")
#                                menuItem("Graphs", icon=icon("bar-chart")),
#                                menuItem("Data Tables", icon=icon("table"))
                        )
                ),
                
                dashboardBody(
                        
                        #tabsetPanel???
                        tabItems(
                                
                                tabItem(tabName = "dashboard", h2("Millenium Development Goal 6: \nHIV Prevalence"), box()),
                                tabItem(tabName="dataAvail", h2("Available Data"), htmlOutput("gvisCount")),
                                tabItem(tabName="popn", h2("Population"), htmlOutput("gvisPopn")),
                                tabItem(tabName="motion", h2("Motion Chart"), htmlOutput("gvisMotion")),
                                tabItem(tabName = "regionSelector", h2("Individual Country Trends"), 
                                        fluidRow(  
                                                box(title="HIV Prevalence Rates", width="100%", status="primary", solidHeader=TRUE, collapsible=TRUE, 
                                                        plotOutput("hivplot")  
                                                )
                                        )
                                        #fluidRow
                                ) #tabItem
                                        
                        ) )  #tabItems, dashboardBody

        ) #ui
                        
 
        
        , server = function(input, output) {   
                
                source("datamgt.R")
                regionWData = MDGcount[MDGcount$SeriesCode %in% c(747, 748) & MDGcount$Count>0, "Country"]
                setRegions = as.vector(unique(regionWData))
                setRegions = setRegions[order(setRegions)]
                output$regionSelector = renderUI({
                        selectInput("region", "Individual Country Trends", choices = setRegions)
                })
                   
                output$gvisCount = renderGvis({
                        gvisGeoChart(MDGcount[MDGcount$SeriesCode %in% c(579),], locationvar="Country", colorvar="Count", 
                                     options=list(backgroundColor="lightblue", projection="kavrayskiy-vii"))
                })
                
                output$gvisPopn = renderGvis({
                        gvisGeoChart(MDGf[MDGf$SeriesCode %in% c(579) & MDGf$Year==2000,], locationvar="Country", colorvar="Population", 
                                     options=list(backgroundColor="lightblue", displayMode="Markers") )
                })
                
                output$gvisMotion = renderGvis({
                        gvisMotionChart(data=MDGfw, idvar="Country", timevar="Year", 
                                        xvar=names(MDGfw)[6], yvar=names(MDGfw)[7], colorvar="CountryCode", sizevar="Population", chartid="WhatIsThis")          
                })
                
                output$hivplot = renderPlot( {
                        subDat = MDGlong[MDGlong$Country==input$region & MDGlong$SeriesCode %in% c(747, 748),]
                        
                        theme_set = theme_grey()
                        theme_hiv = theme(axis.text=element_text(size=14, face="bold"), 
                                          axis.title.x=element_text(size=14, face="bold"), 
                                          axis.title.y=element_text(size=14, face="bold"),
                                          plot.title=element_text(size=16, face="bold"),
                                          legend.text=element_text(size=12, face="bold"),
                                          legend.title=element_text(size=14, face="bold"))
                        
                        g = ggplot(subDat, 
                                   aes(Year, Value, color=Gender, linetype=Gender)) + 
                                geom_point(aes(x=Year, y=Value), size=5) + theme_hiv + 
                                labs(x="Year", y="HIV Prevalence Rate") + xlim(c(1990,2015)) + ylim(c(0,25)) +
            ggtitle(paste("HIV Prevalence Rates Among 15-49 Year-olds in", input$region)) 
                        
                        print(g)
                }, height=400)  #end renderPlot
                
        }
        )
)