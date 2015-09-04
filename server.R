### ShinyServer function
shinyServer(function(input, output) {
        
        library("googleVis")
        
        # Load data and packages
        source("datamgt.R")
        
        # Include only the regions with at least one data point for HIV prevalence
        # List of regions with at least one data point
        regionWData = MDGcount[MDGcount$SeriesCode %in% c(747, 748) & MDGcount$Count>0, "Country"]
        setRegions = as.vector(unique(regionWData))
        setRegions = setRegions[order(setRegions)]
#        setRegions = setNames(setRegions, setRegions)    #not necessary?
        
        # List of regions to be shown in Shiny app selector window
        output$regionSelector = renderUI({
                selectInput("region", "Select a region", choices = setRegions)
        })
        
        output$region = renderText({input$region})
        
        output$text1 = renderUI({
                line1 = paste("Dots represent the yearly HIV prevalence rates for ",input$region, ".", sep="")
                line2 = paste("Lines represent the mean HIV prevalence across all regions with reported data.", sep="")
                HTML(paste(line1, line2, sep='<br/>'))
        })
        
        dataInput = reactive({
                if(input$variable=="prevrate") seriescode=c(747,748)
                if(input$variable=="condom") seriescode=c(734, 735)
                if(input$variable=="know") seriescode=c(741, 742)
                if(input$variable=="attend") seriescode=726
                subDat = MDGlong[MDGlong$Country==input$region & MDGlong$SeriesCode %in% seriescode,]
                subDat
        })
        
        output$gvis = renderGvis({
                gvisGeoChart(MDGcount, locationvar="Country", colorvar="Count", 
                             options=list(backgroundColor="lightblue") )
        })
        
        output$hivplot = renderPlot( {
                theme_set = theme_grey()
                theme_hiv = theme(axis.text=element_text(size=14, face="bold"), 
                                       axis.title.x=element_text(size=14, face="bold"), 
                                       axis.title.y=element_text(size=14, face="bold"),
                                       plot.title=element_text(size=16, face="bold"),
                                       legend.text=element_text(size=12, face="bold"),
                                       legend.title=element_text(size=14, face="bold"))
                subDat = dataInput()
                
                g = ggplot(subDat, 
                           aes(Year, Value, color=Gender, linetype=Gender)) + 
                        geom_point(aes(x=Year, y=Value), size=5) + theme_hiv + 
                        labs(x="Year", y="HIV Prevalence Rate") + xlim(c(1990,2015)) + ylim(c(0,25))
                        ggtitle(paste("HIV Prevalence Rates Among 15-49 Year-olds in", input$region)) 
                
                print(g)
        }, height=400)  #end renderPlot
        
})  #end shinyServer


