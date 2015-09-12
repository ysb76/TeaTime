### UI function
shinyUI(pageWithSidebar(
        
        #  setRegions = unique(origDat$Region),
        #  names(setRegions) = setRegions
        
        ###  Application title
        headerPanel("HIV prevalence (UN)"),
        
        ### Sidebar with sliders 
        sidebarPanel(
                # Choose region  - ONLY 2nd one works and displays text; others diplay numeric
                #selectInput(inputId="region", label="Select a region", choices=setRegions),
                uiOutput("regionSelector"),
        
                br(),
        
                # Choose SeriesCode (reflecting question of interest)
                radioButtons(inputId="variable", label="Variable:", 
                             list("HIV prevalence rate" = "prevrate", "Condom use" = "condom", "Knowledge of HIV/AIDS" = "know", 
                                  "School attendance ratio" = "attend"))
        ),
        
        ### Main Panel
        mainPanel(
                
                tabsetPanel(
                        tabPanel("Available Data", htmlOutput("gvisCount")),
                        tabPanel("Population", htmlOutput("gvisPopn")),
                        tabPanel("Motion Chart", htmlOutput("gvisMotion")),
                        tabPanel("Plot", plotOutput("hivplot")),
                        tabPanel("Table", 
                                 h3("Reported HIV prevalence by Year"),
                                 p("(Ages 15-24, genders combined)"), br(),
                                 tableOutput("table")),
                        tabPanel("About", 
                                 p("Describe this app."), br(),
                                 p("More"), br(),
                                 p("More"), br(),
                                 br(),
                                 p("Data was downloaded from ... ")
                                 )
                        
                        )    #tabsetPanel
)      #mainPanel

))