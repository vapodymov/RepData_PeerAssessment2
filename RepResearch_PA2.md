
# Analysis of Health and Economic Impacts of Severe Weather Events in the U.S.A.

[Valerii Podymov](vapodymov@gmail.com)

September 25, 2015

### Synopsis

The goal of this report is to explore the impact of severe weather on public health and local economy in the U.S. This analysis uses data for the period of the years [1950 to 2011](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) from the U.S. National Oceanic and Atmospheric Administration's (NOAA) [Storm Database](http://www.ncdc.noaa.gov/stormevents/). The analysis of the Top10 severe weather events affecting population health and economies shows that tornadoes, flooding and excessive heat have the greatest impact. The economic consequences are significantly higher for property damage than crop.

### Data Processing

We start with loading the required libraries and raw dataset.


```r
##### Load necessary libraries
library(R.utils)
library(plyr)
```



```r
##### Download and unzip data file
if (!file.exists("storm.csv")) 
{
    if (!file.exists("storm.csv.bz2")) 
    {
        download.file(
            "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
            "storm.csv.bz2")
    }
    bunzip2("storm.csv.bz2", "storm.csv", remove = FALSE)
}
```



```r
#### Load data into R
raw_data <- read.csv("storm.csv")
```


Then we study the structure of the dataset and determine which variables are of our interest.


```r
#### Explore the raw dataset 
str(raw_data)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels "","- 1 N Albion",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels "","- .5 NNW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","$AC",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436774 levels "","-2 at Deer Park\n",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```



```r
##### Subset the data related to health and economic impact analysis
my_columns <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
                "CROPDMG", "CROPDMGEXP")
my_data <- raw_data[my_columns]
```

It is possible to notice that PROPDMGEXP and CROPDMGEXP variables contain numbers, letters and other symbols. According to the [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf), the letters correspond to magnitudes, e.g. "m" = 1e6, "k" = 1e3. Numeric values of PROPDMGEXP and CROPDMGEXP are used to multiply with the PROPDMG and CROPDMG respectively to get the actual damage value. All the other charaters will refer to zero as non-aplicable.


```r
##### Explore the property damage exponent variable
unique(my_data$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```



```r
##### Convert the property damage exponent data to a power of 10
my_data$PROPEXP[my_data$PROPDMGEXP == "K"] <- 10^3
my_data$PROPEXP[my_data$PROPDMGEXP == "M"] <- 10^6
my_data$PROPEXP[my_data$PROPDMGEXP == ""] <- 1
my_data$PROPEXP[my_data$PROPDMGEXP == "B"] <- 10^9
my_data$PROPEXP[my_data$PROPDMGEXP == "m"] <- 10^6
my_data$PROPEXP[my_data$PROPDMGEXP == "0"] <- 1
my_data$PROPEXP[my_data$PROPDMGEXP == "5"] <- 10^5
my_data$PROPEXP[my_data$PROPDMGEXP == "6"] <- 10^6
my_data$PROPEXP[my_data$PROPDMGEXP == "4"] <- 10^4
my_data$PROPEXP[my_data$PROPDMGEXP == "2"] <- 10^2
my_data$PROPEXP[my_data$PROPDMGEXP == "3"] <- 10^3
my_data$PROPEXP[my_data$PROPDMGEXP == "h"] <- 10^2
my_data$PROPEXP[my_data$PROPDMGEXP == "7"] <- 10^7
my_data$PROPEXP[my_data$PROPDMGEXP == "H"] <- 10^2
my_data$PROPEXP[my_data$PROPDMGEXP == "1"] <- 10
my_data$PROPEXP[my_data$PROPDMGEXP == "8"] <- 10^8
```



```r
##### Set invalid exponent data to zero
my_data$PROPEXP[my_data$PROPDMGEXP == "+"] <- 0
my_data$PROPEXP[my_data$PROPDMGEXP == "-"] <- 0
my_data$PROPEXP[my_data$PROPDMGEXP == "?"] <- 0
```



```r
##### Create new variable to store the actual property damage value
my_data$PROPDMGVALUE <- my_data$PROPDMG * my_data$PROPEXP
```



```r
##### Explore the crop damage exponent data variable
unique(my_data$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```



```r
##### Convert the crop damage exponent data to a power of 10
my_data$CROPEXP[my_data$CROPDMGEXP == "M"] <- 10^6
my_data$CROPEXP[my_data$CROPDMGEXP == "K"] <- 10^3
my_data$CROPEXP[my_data$CROPDMGEXP == "m"] <- 10^6
my_data$CROPEXP[my_data$CROPDMGEXP == "B"] <- 10^9
my_data$CROPEXP[my_data$CROPDMGEXP == "0"] <- 1
my_data$CROPEXP[my_data$CROPDMGEXP == "k"] <- 10^3
my_data$CROPEXP[my_data$CROPDMGEXP == "2"] <- 10^2
my_data$CROPEXP[my_data$CROPDMGEXP == ""] <- 1
```



```r
##### Set invalid exponent data to zero
my_data$CROPEXP[my_data$CROPDMGEXP == "?"] <- 0
```



```r
##### Create new variable to store the actual crop damage value
my_data$CROPDMGVALUE <- my_data$CROPDMG * my_data$CROPEXP
```

Then we create four new separate datasets related to public health and economy aggregating data by weather events. 


```r
##### Aggregate data by events
fatalities <- aggregate(FATALITIES ~ EVTYPE, data = my_data, FUN = sum)
injuries <- aggregate(INJURIES ~ EVTYPE, data = my_data, FUN = sum)
prop_dmg <- aggregate(PROPDMGVALUE ~ EVTYPE, data = my_data, FUN = sum)
crop_dmg <- aggregate(CROPDMGVALUE ~ EVTYPE, data = my_data, FUN = sum)
```



```r
##### The number of unique weather event types
num_events <- length(unique(my_data$EVTYPE))
```

Since the EVTYPE variable contains 985 unique events, only Top10 most harmful of them will be selected to visualize results. 


```r
##### Subset Top10 events in each category
top10_fatalities <- fatalities[order(-fatalities$FATALITIES), ][1:10, ]
top10_injuries <- injuries[order(-injuries$INJURIES), ][1:10, ]
top10_prop_dmg <- prop_dmg[order(-prop_dmg$PROPDMGVALUE), ][1:10, ]
top10_crop_dmg <- crop_dmg[order(-crop_dmg$CROPDMGVALUE), ][1:10, ]
```

### Results

The top 10 weather events leading to the largest number of fatalities and injuries in the period from 1950 to 2011 are shown below.


```r
par(mfrow = c(1, 2), mar = c(10, 4, 3, 2), mgp = c(3, 1, 0), cex.main = 0.8)
    
barplot(top10_fatalities$FATALITIES, names.arg = top10_fatalities$EVTYPE, 
        main = "Top10 Weather Events with Highest Fatalities", 
        cex.names = 0.5, las = 3, ylab = "Number of fatalities", log = "y")

barplot(top10_injuries$INJURIES, names.arg = top10_injuries$EVTYPE, 
        main = "Top10 Weather Events with Highest Injuries", 
        cex.names = 0.5, las = 3, ylab = "Number of injuries", log = "y")
```

![Fig. 1 - Top10 weather events most harmful to the U.S. population health](RepResearch_PA2_files/figure-html/unnamed-chunk-17-1.png) 

Tornadoes is the number one cause of fatalities, and excessive heat is on the second place. Tornadoes also take the first place as a cause of injuries with large gap. In both cases events in Top5 relate to the global warming. 

The Top10 weather events leading to the largest cost of property and crop damage (in billion USD) in the period from 1950 to 2011 are shown below.


```r
par(mfrow = c(1, 2), mar = c(10, 4, 3, 2), mgp = c(3, 1, 0), cex.main = 0.7)

barplot(top10_prop_dmg$PROPDMGVALUE/(10^9), names.arg = top10_prop_dmg$EVTYPE, 
        main = "Top10 Weather Events with Highest Property Damage", 
        cex.names = 0.5, las = 3, ylab = "Cost of damage, Bln $", log = "y")

barplot(top10_crop_dmg$CROPDMGVALUE/(10^9), names.arg = top10_crop_dmg$EVTYPE, 
        main = "Top10 Weather Events with Highest Crop Damage", 
        cex.names = 0.5, las = 3, ylab = "Cost of damage, Bln $", log = "y")
```

![Fig. 2 - Top10 weather events most harmful to the U.S. local economies](RepResearch_PA2_files/figure-html/unnamed-chunk-18-1.png) 

Floods is the issue number one for property damage, while drought has the highest impact for crop. Top 5 events seem to be taking place during warm and storm seasons of the year.
