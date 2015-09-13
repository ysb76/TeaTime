#data format for analysis
#obs/year rows; metrics are columns

#EDIT THIS TO POINT TO DIRECTORY WITH YOUR DATAFILE
MDGdat <- read.csv("~/Documents/Research/Software/R/Example Projects/Bryna/UN2015/Data/MDG_Export_20150829x.csv",
                stringsAsFactors=FALSE)


#INDICATORS IN MDG DATABASE
#SeriesCode value in parentheses
#Current contraceptive use by women - condom (60)
#People living with HIV (67, 68, 69, LB, MP, UB)
#HIV prevalence rate, 15-49 years old, in national based surveys (women=748, men=747)
#AIDS deaths (797, 579, 798, LB, MP, UB)
#HIV incidence rate (803, 801, 802, LB, MP, UB)
#Condom use at last high-risk sex (women=735, men=734)
#Condom use to overall contraceptive use in women (733)
#Proportion of age 15-24 with comprehensive knowledge of HIV/AIDS (women=742, men=741)
#AIDS orphans (622)
#Antiretroviral therapy coverage for HIV-infected (805, 765, 804, LB, MP, UB)
#Percentage of HIV-infected pregnant women with/ART (808, 806, 807, LB, MP, UB)


#subset data to include only relevant variables
MDGsub = MDGdat[MDGdat$SeriesCode %in% 
                c(60, 68, 748, 747, 579, 801, 735, 734, 733, 742, 741, 622, 765, 806),]

#Keep cols with X# 
#Do we need Type?  #types = c("Type", paste("Type", seq(1, 25), sep="."))
years = paste("X", seq(1990, 2015), sep="")
MDGwide = MDGsub[,c("CountryCode", "Country", "SeriesCode","MDG", "Series", years)]
names(MDGwide)[6:31] = gsub("X", "", names(MDGwide)[6:31])
MDGwide$Obs = rownames(MDGwide)

#Reshape data wide to long
MDGlong = reshape(MDGwide, varying=c(6:31), direction="long", idvar="Obs",
                  times=1990:2015, timevar="Year", v.name="Value")

MDGlong$Gender = "All"
MDGlong$Gender[MDGlong$SeriesCode %in% c(747, 734, 741)] = "Male"
MDGlong$Gender[MDGlong$SeriesCode %in% c(748, 735, 742)] = "Female"

#Map countries into regions code (from http://mdgs.un.org/unsd/mdg/Host.aspx?Content=Data/RegionalGroupings.htm)
MDGlong$Region[MDGlong$Country %in% c("Albania", "Andorra", "Australia", "Austria", 
        "Belarus", "Belgium", "Bermuda", "Bosnia and Herzegovina", "Bulgaria", "Canada",
        "Channel Islands", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", 
        "Faeroe Islands", "Finland", "France", "Germany", "Greece", "Greenland", "Hungary",
        "Iceland", "Ireland", "Isle of Man", "Israel", "Italy", "Japan", "Latvia", 
        "Liechetenstein", "Lithuania", "Luxembourg", "Malta", "Monaco", "Montenegro", 
        "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Republic of Moldova", 
        "Romania", "Russian Federation", "San Marino", "Serbia", "Slovakia", "Slovenia",
        "Spain", "Sweden", "Switzerland", "TFYR of Macedonia", "Ukraine", "United Kingdom",
        "United States")] = "Developed"
MDGlong$Region[grepl("Czechoslovakia|European|Liechtenstein|Serbia|Yugoslav|Gibraltar|Miquelon|Soviet", MDGlong$Country)] = "Developed"
MDGlong$Region[MDGlong$Country %in% c("Armenia", "Azerbaijan", "Georgia", "Kazakhstan", 
        "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan")] = 
        "Caucasus and Central Asia"
MDGlong$Region[MDGlong$Country %in% c("Algeria", "Egypt", "Libyan Arab Jamahiriya", 
        "Morocco", "Tunisia", "Western Sahara")] = "Northern Africa"
MDGlong$Region[MDGlong$Country %in% c("Angola", "Benin", "Botswana", "Burkina Faso", 
        "Burundi", "Cameroon", "Cape Verde", "Central African Rep", "Chad", "Comoros", 
        "Congo", "Cote d'Ivoire", "Dem Rep of the Congo", "Djibouti", "Equatorial Guinea", 
        "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
        "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", 
        "Mauritius", "Mayotte", "Mozambique", "Namibia", "Niger", "Nigeria", "Reunion", 
        "Rwanda", "Sao Tome & Principe", "Senegal", "Seychelles", "Sierra Leone", 
        "Somalia", "South Africa", "Sudan", "South Sudan", "Swaziland", "Togo", 
        "Uganda", "United Rep of Tanzania", "Zambia", "Zimbabwe")] = "Sub-Saharan Africa"
MDGlong$Region[grepl("Central African", MDGlong$Country) | 
        grepl("Sao Tome and Principe", MDGlong$Country) |
        grepl("Congo|Sudan|Tanzania|Helena", MDGlong$Country)] = "Sub-Saharan Africa"
MDGlong$Region[MDGlong$Country %in% c("Caribbean", "Anguilla", "Antigua and Barbuda", 
        "Aruba", "Bahamas", "Barbados", "British Virgin Islands", "Cayman Islands", "Cuba", 
        "Dominica", "Dominican Republic", "Grenada", "Guadeloupe", "Haiti", "Jamaica", 
        "Martinique", "Montserrat", "Netherlands Antilles", "Puerto Rico", 
        "Saint Kitts and Nevis", "Saint Lucia", "St Vincent & the Grenadines", 
        "Trinidad and Tobago", "Turks and Caicos Islands", "US Virgin Islands", 
        "Latin America", "Argentina", "Belize", "Bolivia", "Brazil", "Chile", "Colombia", 
        "Costa Rica", "Equador", "El Salvador", "Falkland Is (Malvinas)", "French Guiana", 
        "Guatamala", "Guyana", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", 
        "Suriname", "Uruguay", "Venezuela")] = "Latin America and the Carribbean"
MDGlong$Region[grepl("Virgin Islands", MDGlong$Country) | 
        grepl("Saint Vincent", MDGlong$Country) | grepl("Guatemala|Ecuador|Falkland", MDGlong$Country)] = 
        "Latin America and the Carribbean"
MDGlong$Region[MDGlong$Country %in% c("China", "Hong Kong SAR of China", "Macao SAR of China", 
        "Korea, Dem People's Rep of", "Korea, Rep of", "Mongolia")] = "Eastern Asia"
MDGlong$Region[grepl("China|Korea", MDGlong$Country)] = "Eastern Asia"
MDGlong$Region[MDGlong$Country %in% c("Afghanistan", "Bangladesh", "Bhutan", "India", 
        "Iran (Islamic Republic of)", "Maldives", "Nepal", "Pakistan", "Sri Lanka")] = 
        "Southern Asia"
MDGlong$Region[MDGlong$Country %in% c("Brunei Darussalam", "Cambodia", "Indonesia", 
        "Lao People's Dem Repulic", "Malaysia", "Myanmar", "Philippines", "Singapore", 
        "Thailand", "Timor-Leste", "Viet Nam")] = "South-Eastern Asia"
MDGlong$Region[grepl("Lao", MDGlong$Country)] = "South-Eastern Asia"
MDGlong$Region[MDGlong$Country %in% c("Bahrain", "Iraq", "Jordan", "Kuwait", "Lebanon", 
        "Occupied Palestinian Territory", "Oman", "Qatar", "Saudi Arabia", 
        "Syrian Arab Republic", "Turkey", "United Arab Emirates", "Yemen")] = "Western Asia"
MDGlong$Region[grepl("Palestine", MDGlong$Country)] = "Western Asia"
MDGlong$Region[MDGlong$Country %in% c("American Samoa", "Cook Is", "Fiji", 
        "French Polynesia", "Guam", "Kiribati", "Marshall Islands", 
        "Micronesia (Fed States of)", "Nauru", "Niue", "New Caledonia", 
        "Northern Mariana Is", "Palau", "Papua New Guinea", "Samoa", "Solomon Is", 
        "Tokelau", "Tonga", "Tuvalu", "Vanuatu")] = "Oceania"
MDGlong$Region[grepl("Cook|Micronesia|Solomon|Mariana|Wallis", MDGlong$Country)] = "Oceania"



#Create a second data farame that counts number of data measurements for each SeriesCode by CountryCode
#MDGcount = ddply(MDGlong, c("CountryCode", "Country", "SeriesCode", "Series", "Gender"), 
#                 function(df) sum( !is.na(df$Value) ) )
MDGcount = ddply(MDGlong, .(CountryCode, Country, SeriesCode, Series, Gender), 
                 summarise, Count=sum( !is.na(Value) ) )

MDGcountx = ddply(MDGlong, .(Year, Series), 
                 summarise, Count=sum( !is.na(Value) ) )

MDGcounty = ddply(MDGlong, .(Series), 
                  summarise, Count=sum( !is.na(Value) ) )

#How many measurements are availably by region and year? (for specified SeriesCode)
temp = ddply(MDGlong[MDGlong$SeriesCode==801,], .(Region, Year), summarise, 
             Count=sum(!is.na(Value)) )
temp[temp$Year==2000,]
temp[temp$Year==2013,]
temp2 = MDGlong[MDGlong$Region=="Sub-Saharan Africa" & MDGlong$Year==2009 & MDGlong$SeriesCode==801,]
mean(temp2$Value, na.rm=TRUE)



#GET POPULATION DATA FROM http://esa.un.org/unpd/wpp/DVD/
popn <- read.csv("~/Documents/Research/Software/R/Example Projects/Bryna/UN2015/Data/WPP2015_POPx.csv",
                 stringsAsFactors=FALSE)
#Convert from wide to long format and create the variable "population"
#Keep cols with X# 
years = paste("X", seq(1990, 2015), sep="")
popnwide = popn
names(popnwide)[4:29] = gsub("X", "", names(popnwide)[4:29])
popnwide$Obs = rownames(popnwide)

#Reshape data wide to long
popnlong = reshape(popnwide, varying=c(4:29), direction="long", idvar="Obs",
                   times=1990:2015, timevar="Year", v.name="Population")
#Remove blank spaces from population numbers and convert to numeric
popnlong$Population = gsub(" ", "", popnlong$Population)
popnlong$Population = as.numeric(popnlong$Population)
popnlong = popnlong[,-c(3,4)]  #remove "Region" and "Obs - maybe I don't need them


#MERGE MDG data with population data
MDGp = merge(MDGlong, popnlong, by=c("CountryCode", "Year"))
temp = MDGp[which(MDGp$Country.x != MDGp$Country.y),c("Country.x", "Country.y")]
unique(temp$Country.x)
unique(temp$Country.y)   #Differences are only due to different country name abbreviations - ignorable - take only Country.x
MDGf = MDGp[,c(7, 3, 1, 10, 2, 4, 6, 8:9, 12)]
names(MDGf)[names(MDGf)=="Country.x"] = "Country"


#GET FINANCIAL DATA from http://data.worldbank.org/indicator/SH.XPD.TOTL.ZS (total health expenditore, % of GDP)
#Alternate data is available at http://apps.who.int/gho/data/node.main.78?lang=en but only includes 2000, 2012, and 2013
healthexp <- read.csv("~/Documents/Research/Software/R/Example Projects/Bryna/UN2015/Data/WorldBank_healthexpGDP.csv",
                 stringsAsFactors=FALSE)
years = paste("X", seq(2000, 2013), sep="")
#hexpwide = healthexp[,c("Country.exp", "Series.exp", years)]
names(healthexp)[3:16] = gsub("X", "", names(healthexp)[3:16])
#hexpwide$Obs = rownames(hexpwide)
#Reshape to Country, Series, Year (from wide to long)
healthexp$Obs = rownames(healthexp)
hexplong = reshape(healthexp, varying=c(3:16), direction="long", idvar="Obs",
                  times=2000:2013, timevar="Year", v.name="HealthExpenditure")
#Merge with MDGf
MDGff = merge(MDGf, hexplong, by=c("Country", "Year"), all.x=TRUE)
#Convert to long format (Series, Series.exp will be a single column var SeriesName), (Value.x, Value.y will be a single column var Value)
#names(MDGff)[names(MDGff)=="Series"] = "Series1"
#names(MDGff)[names(MDGff)=="Series.exp"] = "Series2"
#names(MDGff)[names(MDGff)=="Value.x"] = "Value1"
#names(MDGff)[names(MDGff)=="Value.y"] = "Value2"
#MDGff$Obs = rownames(MDGff)
#MDGff = MDGff[,c("Obs", "Country", "Year", "Region", "Series1", "Series2", "Value1", "Value2", "Population")]
#with finance and popn data
#MDGfp = reshape(MDGff, varying=c("Series1", "Series2", "Value1", "Value2"), timevar="Series", idvar="Obs", direction="long", sep="")


#RESHAPE so that Series and SeriesCode are columns - this wil work well with gvisMotionChart
MDGfw = dcast(MDGff, CountryCode + Country + Region + Year + Population + HealthExpenditure ~ Series + SeriesCode, value.var="Value")  #reshape2
MDGfw = MDGfw[,c(1:6, 13, 9, 17, 16, 18, 10, 11, 12, 14, 15, 7, 8)]
orign = names(MDGfw)
names(MDGfw) = c("CountryCode", "Country", "Region", "Year", "Population", "HealthExpenditure",
                 paste("Var", 1:12, ": ", substr(orign[7:18], 1, 60), sep=""))

#TRYING gvisMotion Chart but problem
##TEST
#M = gvisMotionChart(data=MDGf[MDGf$SeriesCode %in% c(579),], idvar="Population", timevar="Year", chartid="WhatIsThis")
#ERROR MSG
#Error in gvisCheckMotionChartData(data, my.options) : 
#        The data must have rows with unique combinations of idvar and timevar.
#Your data has 5902 rows, but idvar and timevar only define 5714 unique rows.
#temp = MDGf[MDGf$SeriesCode==579,]
#test = temp[duplicated(temp[,c("Year", "Population")]),]   #dim(test)=188x10 (note:5902-5714=188 so have the dup problem)
#Not sure why I'm having a problem 
#Test solution
#temp2 = temp[!duplicated(temp[,c("Year", "Population")]),]    #dim(temp2)=5714x10
#M = gvisMotionChart(data=temp2, idvar="Population", timevar="Year", chartid="WhatIsThis")
#plot(M)
#NEW TEST - YES!!  Put Value on y-axis, size of bubble is population (here x-value is CountryCode but that's meaningless)
#########NEED MORE VARIABLES -----  INSTEAD OF SERIESCODE PUT THE VARIABLES AS COLUMNS!!!!!!!!!
#M = gvisMotionChart(data=MDGf[MDGf$SeriesCode==579,], idvar="Country", timevar="Year", chartid="WhatIsThis")


###ADD a graph of y=change in incidence rate versus x=some kind of investment or some gain in knowledge or change in practice
###Maybe (year 2015 - year 2000)

