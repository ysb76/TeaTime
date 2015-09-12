### ShinyServer function
shinyServer(function(input, output) {
        
        library("googleVis")
        
        # Load data and packages
        source("datamgt.R")
        
        # Include only the regions with at least one data point for selected SeriesCode
        regionWData = MDGcount[MDGcount$SeriesCode %in% c(579) & MDGcount$Count>0, "Country"]
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
                seriescode= 579
                subDat = MDGf[MDGf$Country==input$region & MDGf$SeriesCode %in% seriescode,]
                subDat
        })
        
        output$gvisCount = renderGvis({
                gvisGeoChart(MDGcount[MDGcount$SeriesCode %in% c(579),], locationvar="Country", colorvar="Count", 
                             options=list(backgroundColor="lightblue", projection="kavrayskiy-vii"))
        })

        output$gvisPopn = renderGvis({
                gvisGeoChart(MDGf[MDGf$SeriesCode %in% c(579) & MDGf$Year==2000,], locationvar="Country", colorvar="Population", 
                     options=list(backgroundColor="lightblue", displayMode="Markers") )
})
       
#JUST A TEST HERE - NOT THE DATA I WANT
        #reorder vars to set default axes (idvar, timevar, xvar, yvar, colorvar, sizevar) 
#        MDGf = MDGf[,c("Country", "Year", "CountryCode", "Value", "Population", "SeriesCode")]
        output$gvisMotion = renderGvis({
#                gvisMotionChart(data=MDGf[MDGf$SeriesCode==579,], idvar="Country", timevar="Year", 
#                                xvar="CountryCode", yvar="Value", colorvar="Region", sizevar="Population", chartid="WhatIsThis")
                gvisMotionChart(data=MDGfw, idvar="Country", timevar="Year", 
                                xvar=names(MDGfw)[6], yvar=names(MDGfw)[7], colorvar="CountryCode", sizevar="Population", chartid="WhatIsThis")
                
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
                
                #maxy = max(MDGlong$Value[MDGlong$SeriesCode==579], na.rm=TRUE)
                g = ggplot(subDat, 
                           aes(Year, Value)) + 
                        geom_point(aes(x=Year, y=Value), size=5) + theme_hiv + 
                        labs(x="Year", y="AIDS Deaths") + xlim(c(1990,2015)) + ylim(c(0,max(subDat$Value))) +
                        ggtitle(paste("AIDS Deaths in ", input$region)) 
                
                print(g)
        }, height=400)  #end renderPlot

#Number of AIDS deaths by region (each year or total?)
        temp = MDGlong[MDGlong$SeriesCode==579 & MDGlong$Year==2013,]  #just AIDS deaths in 2000
        temp2 = table(temp$Value, temp$Region)
        barplot(temp2)
        ddply(temp, .(Region), summarize, sum=sum(Value, na.rm=TRUE))

        temp = MDGlong[MDGlong$SeriesCode==801 & MDGlong$Year==2013,]
        ddply(temp, .(Region), summarize, mean=mean(Value, na.rm=TRUE))
        
})  #end shinyServer


