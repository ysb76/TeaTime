#data format for analysis
#obs/year rows; metrics are columns

#EDIT THIS TO POINT TO DIRECTORY WITH YOUR DATAFILE
MDGdat <- read.csv("~/Documents/Research/Software/R/Example Projects/Bryna/UN2015/Data/MDG_Export_20150829x.csv",
                stringsAsFactors=FALSE)

#HIV prevalence rate, 15-49 years old, in national based surveys (women=748 SeriesCode, men=747)
#Condom use at last high-risk sex (women=735, men=734)
#Proportion of age 15-24 with comprehensive knowledge of HIV/AIDS (women=742, men=741)
#Ratio of school-age attendance of orphan to non-orphan children (726)

#subset data to include only relevant variables
MDGsub = MDGdat[MDGdat$SeriesCode %in% c(747, 748, 734, 735, 741, 742, 726),]
#570, 799, 729, 800, 748, 747, 797, 579, 798, 803, 801, 802,735, 734, 741, 742, 726, 781, 782, 622, 805, 765, 804, 808, 806, 807

#Keep cols with X# 
#Do we need Type?  #types = c("Type", paste("Type", seq(1, 25), sep="."))
years = paste("X", seq(1990, 2015), sep="")
MDGwide = MDGsub[,c("CountryCode", "Country", "SeriesCode","MDG", "Series", years)]
names(MDGwide)[6:31] = gsub("X", "", names(MDGwide)[6:31])
MDGwide$Obs = rownames(MDGwide)

#Reshape data wide to long
MDGlong = reshape(MDGwide, varying=c(6:31), direction="long", idvar="Obs",
                  times=1990:2015, timevar="Year", v.name="Value")

MDGlong$Gender[MDGlong$SeriesCode %in% c(747, 734, 741)] = "Male"
MDGlong$Gender[MDGlong$SeriesCode %in% c(748, 735, 742)] = "Female"
MDGlong$Gender[MDGlong$SeriesCode==726] = "All"

#Create a second data farame that counts number of data measurements for each SeriesCode by CountryCode
#MDGcount = ddply(MDGlong, c("CountryCode", "Country", "SeriesCode", "Series", "Gender"), 
#                 function(df) sum( !is.na(df$Value) ) )
MDGcount = ddply(MDGlong, .(CountryCode, Country, SeriesCode, Series, Gender), 
                 summarise, Count=sum( !is.na(Value) ) )




