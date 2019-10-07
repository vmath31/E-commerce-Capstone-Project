######################################################################################
##      Capstone - Ecommerce
######################################################################################

## Section 1: Data Understanding, Preparation & EDA
## Section 2: Model Building
## Section 3: Presentation of Results

library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)
install.packages("sqldf")
library(sqldf)

install.packages("readxl")
library(readxl)

library(ggplot2)
library(scales)
install.packages("ggthemes")
library(ggthemes)
install.packages("cowplot")
library(cowplot)
library(zoo)
install.packages("DataCombine")
library(DataCombine)
install.packages("imputeTS")
library(imputeTS)
library(MASS)
install.packages("car")
library(car)
install.packages("DAAG")
library(DAAG)

## Load data from order csv file
baseData <- read.csv("ConsumerElectronics.csv", stringsAsFactors = FALSE)

str(baseData)
# data.frame:	1648824 obs. of  20 variables:
#$ fsn_id                         : chr  "ACCCX3S58G7B5F6P" "ACCCX3S58G7B5F6P" "ACCCX3S5AHMF55FV" "ACCCX3S5AHMF55FV" ...
#$ order_date                     : chr  "2015-10-17 15:11:54" "2015-10-19 10:07:22" "2015-10-20 15:45:56" "2015-10-14 12:05:15" ...
#$ Year                           : int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
#$ Month                          : int  10 10 10 10 10 10 10 10 10 10 ...
#$ order_id                       : num  3.42e+15 1.42e+15 2.42e+15 4.42e+15 4.42e+15 ...
#$ order_item_id                  : num  3.42e+15 1.42e+15 2.42e+15 4.42e+15 4.42e+15 ...
#$ gmv                            : num  6400 6900 1990 1690 1618 ...
#$ units                          : int  1 1 1 1 1 1 1 1 1 1 ...
#$ deliverybdays                  : chr  "\\N" "\\N" "\\N" "\\N" ...
#$ deliverycdays                  : chr  "\\N" "\\N" "\\N" "\\N" ...
#$ s1_fact.order_payment_type     : chr  "COD" "COD" "COD" "Prepaid" ...
#$ sla                            : int  5 7 10 4 6 5 6 5 9 7 ...
#$ cust_id                        : num  -1.01e+18 -8.99e+18 -1.04e+18 -7.60e+18 2.89e+18 ...
#$ pincode                        : num  -7.79e+18 7.34e+18 -7.48e+18 -5.84e+18 5.35e+17 ...
#$ product_analytic_super_category: chr  "CE" "CE" "CE" "CE" ...
#$ product_analytic_category      : chr  "CameraAccessory" "CameraAccessory" "CameraAccessory" "CameraAccessory" ...
#$ product_analytic_sub_category  : chr  "CameraAccessory" "CameraAccessory" "CameraAccessory" "CameraAccessory" ...
#$ product_analytic_vertical      : chr  "CameraTripod" "CameraTripod" "CameraTripod" "CameraTripod" ...
#$ product_mrp                    : int  7190 7190 2099 2099 2099 4044 4044 4044 4044 4044 ...
#$ product_procurement_sla        : int  0 0 3 3 3 5 5 5 5 5 ...

### Order Data attributes
#•	FSN ID: The unique identification of each SKU
#•	Order Date: Date on which the order was placed
#•	Order ID: The unique identification number of each order
#•	Order item ID: Suppose you order 2 different products under the same order, it generates 2 different order Item IDs under the same order ID; orders are tracked by the Order Item ID.
#•	GMV: Gross Merchandise Value or Revenue
#•	Units: Number of units of the specific product sold
#•	Order payment type: How the order was paid – prepaid or cash on delivery
#•	SLA: Number of d ays it typically takes to deliver the product
#•	Cust id: Unique identification of a customer
#•	Product MRP: Maximum retail price of the product
#•	Product procurement SLA: Time typically taken to procure the product


head(baseData)

## load Investment and NPS data
investmentData <- read_excel("Media data and other information.xlsx", sheet = "Media Investment", skip = 2)
View(investmentData)
str(investmentData)

#plotting the investment month wise 
investmentData %>% ggplot(aes(x=Month, y=`Total Investment`)) + geom_bar(stat = 'identity',fill='red') + coord_flip()
### Conclusion:  Maximum investment is done in Month 10 (October 2015) followed by Dec 2015.
# Previous analysis also shows max revenue in same month.
summary(investmentData)


npsData <- read_excel("Media data and other information.xlsx", sheet = "Monthly NPS Score", skip =2, col_names = FALSE)
View(npsData)


#################################
## I. Data cleaning and Prepration
#################################

#1. Convert scientific values in numeric form
baseData$order_id <- format(baseData$order_id,scientific = F)
baseData$order_item_id <- format(baseData$order_item_id,scientific = F)
baseData$cust_id <- format(baseData$cust_id,scientific = F)
baseData$pincode <- format(baseData$pincode,scientific = F)
head(baseData)

#2. \\N chars in deliverybday and deliverycdays, further check these columns
summary.factor(baseData$deliverybdays)
## Out of 1648824 records 1312972 records have \\NA value which is very large port. So we ca delete this column
summary.factor(baseData$deliverycdays)
## Out of 1648824 records 1312971 records have \\NA value which is very large port. So we can delete this column

## Check loaded data against given Product List (product, frequency, percent & total)
nrow(baseData)
#16,48,824   -> matches with given product details sheet

str(baseData)

productDF <- sqldf("select product_analytic_vertical, count() from baseData group by product_analytic_vertical;")
View(productDF)
#####
# VERIFIED: product count = 74 same as in product list. Count is also matching with Frequencies mentioned in provided Data Sheet
######

##  Create a new data frame keeping original DF intact
orderDF <- baseData

## Deleting \\N columns
orderDF$deliverybdays <- NULL
orderDF$deliverycdays <- NULL

head(orderDF)

## Trim spaces from character fields as order_item_id field has spaces in its values
orderDF$order_item_id <- trimws(orderDF$order_item_id)
orderDF$order_id <- trimws(orderDF$order_id)
nrow(orderDF)
##16,48,824

str(orderDF)


#Check for duplicate order_id & order_item_id
sum(duplicated(orderDF, by= c("order_id","order_item_id")))
# 1,04,954

# Remove duplicates
orderDF1 <- orderDF[!duplicated(orderDF, by= c("order_id","order_item_id")),]
nrow(orderDF1)
#15,43,870 = 16,48,824 - 1,04,954


# Check duplicate removal
sqldf("select * from orderDF where order_item_id == '210682941';")
sqldf("select * from orderDF1 where order_item_id == '210682941';")

##Check for NA values in gmv (revenue)
sum(is.na(orderDF1$gmv))
# 4038

#Assuming these NA gmv (Revenue) didn't materailise  and can be removed from analysis
orderDF1 <- orderDF1[!is.na(orderDF1$gmv),]
nrow(orderDF1)
#15,39,832 = 15,43,870 - 4,038

## Check for 0 MRP values
##Check for NA values in gmv (revenue)
sum((orderDF1$product_mrp==0))
# 4912

# Check 0 MRP products' for same fsn_id
mrp_df <- orderDF1[orderDF1$product_mrp==0,]
# 4912 fsn_ids have 0 MRP

mrp_df1 <- sqldf("select fsn_id, product_mrp from orderDF1 where fsn_id in (select distinct fsn_id from mrp_df);")
# 4912 fsn_ids again 

## check if any of these is non-zero
sum(mrp_df1$product_mrp!=0)
# 0 means all of these fsn_ids don't have any non-zero MRPs so we can remove these 0 MRP records

orderDF1 <- orderDF1[orderDF1$product_mrp!=0,]
nrow(orderDF1)
#15,34,920

# Outliers treatment
## Check if gmv > MRP*unit
sum(orderDF1$gmv > orderDF1$product_mrp*orderDF1$units)
#30,300

checkDF <- orderDF1[(orderDF1$gmv > orderDF1$product_mrp*orderDF1$units),]
View(checkDF)
##View: Looks like customers are charged more than MRP in these cases. Let us leave this as it.

sum(orderDF1$units==0)
# 0

sum(is.na(orderDF1$cust_id))
# 0

## Assumptiton:Customer id and Pin Code fileds can be negatiave. Conver these to non-negative.
orderDF1$cust_id <- str_remove_all(orderDF1$cust_id, "-")
orderDF1$pincode <- str_remove_all(orderDF1$pincode, "-")

orderDF1$cust_id <- str_remove_all(orderDF1$cust_id, " ")
orderDF1$pincode <- str_remove_all(orderDF1$pincode, " ")

str(orderDF1)

## Check duration of data. Data duration is July 1015 to June 2016.

unique(orderDF1$Year)
# 2015, 2016
sqldf("select Year, Month, count(*) as orderCount from orderDF1 group by Year, Month;")
#
##   Year Month orderCount
#1  2015     5          1   -> Remove it
#2  2015     6          6   -> Remove it
#3  2015     7      87835
#4  2015     8        157
#5  2015     9     103760
#6  2015    10     205660
#7  2015    11     129447
#8  2015    12     158151
#9  2016     1     143765
#10 2016     2     144071
#11 2016     3     153297
#12 2016     4     134308
#13 2016     5     157017
#14 2016     6     121755
#15 2016     7        602  -> Remove it

orderDF2015 <- subset(orderDF1, Year == 2015 & Month >= 7)
orderDF2016 <- subset(orderDF1, Year == 2016 & Month <= 6)

orderFinalDF <- rbind(orderDF2015, orderDF2016)
# 15,34,311 = 15,34,920 - 609


## Product Super category
unique(orderFinalDF$product_analytic_super_category)
#"CE"
#There is only one value for all records for field product_analytic_super_category so it is of not much help in analysis so remove this columnn
orderFinalDF$product_analytic_super_category <- NULL
str(orderFinalDF)


##Replace time part of order_date
# convert time columns to datetime oject
orderFinalDF$order_date <- as.POSIXlt(orderFinalDF$order_date, format = "%Y-%m-%d %H:%M:%S")
orderFinalDF$order_date <- format(orderFinalDF$order_date, "%Y-%m-%d")

orderFinalDF$product_analytic_category <- as.factor(orderFinalDF$product_analytic_category)
orderFinalDF$product_analytic_sub_category <- as.factor(orderFinalDF$product_analytic_sub_category)

str(orderFinalDF)

########################
## Select order data for three product sub-categories  - camera accessory, home audio and gaming accessory
########################
orderPrepDF1 <- filter(orderFinalDF, product_analytic_sub_category %in% c("CameraAccessory","HomeAudio","GamingAccessory"))
#5,23,804

## Drop unused factors
orderPrepDF2 <- droplevels(orderPrepDF1)

unique(orderPrepDF2$product_analytic_sub_category)
#Levels: CameraAccessory GamingAccessory HomeAudio

str(orderPrepDF2)

### Assign Week number for all days

## Assign week number to each day in a new colunmn week number
## We wre following ISO8601 where week starts on Monday
orderPrepDF2$order_date <- as.Date(orderPrepDF2$order_date)

orderPrepDF2$WeekDay <- weekdays(as.POSIXlt(orderPrepDF2$order_date, format = "%Y-%m-%d"))
orderPrepDF2$Week <- format(as.POSIXlt(orderPrepDF2$order_date, format = "%Y-%m-%d"), "%W")

str(orderPrepDF2)

### Sale Events days

### Sales Events Days (Months)
#Year	Month 	Event Days	Events
#2015	7	      2	Eid & Rathayatra sale 
#2015	8	      6	Indep Day & Raksha Bandhan
#2015	10    	3	Dussera         
#2015	11    	8	Diwali Sale
#2015	12    	7	Christmas & NY Sale
#2016	1     	6	Christmas & NY Sale & Rep D
#2016	2	      6	BED, FHSD, VD
#2016	3	      3	BSD-5
#2016	5     	3	Pacman

### Creating a vector "date" to store the dates of Special Sale Calender as provided in the 'Media data and other information.xlsx'
Event_Dates <- as.Date(c("2015-07-18","2015-07-19","2015-08-15","2015-08-16","2015-08-17","2015-08-28","2015-08-29","2015-08-30","2015-10-15","2015-10-16","2015-10-17",
                         "2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-11-11","2015-11-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26","2015-12-27",
                         "2015-12-28","2015-12-29","2015-12-30","2015-12-31","2016-01-01","2016-01-02","2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01",
                         "2016-02-02","2016-02-14","2016-02-15","2016-02-20","2016-02-21","2016-03-07","2016-03-08","2016-03-09","2016-05-25","2016-05-26","2016-05-27"))


## Add event day flag
orderPrepDF2$eventFlag <- ifelse(as.Date(orderPrepDF2$order_date) %in% Event_Dates, "Y", "N")

## Creating one more column "special_sale_day" which stores which special day it was (like Diwali, Eid etc.)
orderPrepDF2$event='Non-event Day'

orderPrepDF2 <- within(orderPrepDF2, {
  event[as.Date(order_date)  %in% (Event_Dates[1:2])]='Eid & Rath'
  event[as.Date(order_date)  %in% (Event_Dates[3:5])]='Indep Day'
  event[as.Date(order_date)  %in% (Event_Dates[6:8])]='Rakshabandhan'
  event[as.Date(order_date)  %in% (Event_Dates[9:11])]='Daussera'
  event[as.Date(order_date)  %in% (Event_Dates[12:19])]='Diwali'
  event[as.Date(order_date)  %in% (Event_Dates[20:29])]='Christmas & NY'
  event[as.Date(order_date) %in% (Event_Dates[30:32])]='Republic Day'
  event[as.Date(order_date)  %in% (Event_Dates[33:34])]='BED'
  event[as.Date(order_date)  %in% (Event_Dates[35:36])]='Valentine Day'
  event[as.Date(order_date)  %in% (Event_Dates[37:38])]='FHSD'
  event[as.Date(order_date) %in% (Event_Dates[39:41])]='BSD'
  event[as.Date(order_date)  %in% (Event_Dates[42:44])]='Pacman'
})


### Outliers treatment

# Check Units
quantile(orderPrepDF2$units, seq(0,1,.001))  
#99.7%  99.8%  99.9% 100%
#3       3      4      39

table(orderPrepDF2$units)
## 
#1           2      3      4      5      6      7      8      9     10     12     13     15     17     18     19     34     39 
#514025   8125    923    316    207     70     18     22     10     74      3      2      3      2      1      1      1      1 

# Only 0.1% records have units more than 4. Let us remove these 0.1% records as they may skew the analysis
orderPrepDF2 <- subset(orderPrepDF2, units <= 4)
#5,23,389


### Check SLA values
quantile(orderPrepDF2$sla, seq(0,1,.001))  
#99.5% 99.6% 99.7%  99.8%   99.9%  100%
#15     16    16       17     19    60 

table(orderPrepDF2$sla)
# Numbr of records havng SLA more than 15 days are very few ~ approx 1500 orders may be filtered out.
# Also SLA more than 15 days is bit non-realistic from product sellng perspective
orderPrepDF2 <- subset(orderPrepDF2, sla <= 15)
#5,21,280

### Check procurement SLA values
quantile(orderPrepDF2$product_procurement_sla, seq(0,1,.001))  
# there are -1 values , replace them with 0
orderPrepDF2$product_procurement_sla[orderPrepDF2$product_procurement_sla < 0] <- 0
quantile(orderPrepDF2$product_procurement_sla, seq(0,1,.001)) 
table(orderPrepDF2$product_procurement_sla)
#    0      1      2      3      4      5      6      7      8      9     10     12     13     14 
# 22731 100652 179394 102932  35510  74657   1620    124    207      6     63     54    481   2849
##Proc sla seems with 14 days limit which are ok. 

## Remove fileds which are ids like fsn_id, order_id, order_item_id, pin_code  and cust_id as model doesn't depend on such attributes
orderPrepDF2$fsn_id <- NULL
orderPrepDF2$order_id <- NULL
orderPrepDF2$order_item_id <- NULL
orderPrepDF2$cust_id <- NULL
orderPrepDF2$pincode <- NULL

## There is same value in product_analytic_category so we can remove it
orderPrepDF2$product_analytic_category <- NULL

orderPrepDF2$Week <- as.numeric(orderPrepDF2$Week)
##There are week values as 0 so replace them with 53
orderPrepDF2$Week[which(orderPrepDF2$Week == 0)]<- 53

#############
# Investment Data cleaning and preparation

## Replacing NA values
investmentData[is.na(investmentData)] <- 0

##############
## Convert monthly spend data in daily format
#############
investmentData$firstDate <- paste(investmentData$Year,investmentData$Month,"01", sep="-")
investmentData$MonthDays <- days_in_month(as.Date(investmentData$firstDate))


str(investmentData)
## Derive per day expenditure for each channel
investmentData$pd.Total.Investment <- investmentData$`Total Investment`/investmentData$MonthDays
investmentData$pd.TV <- investmentData$TV/investmentData$MonthDays
investmentData$pd.Digital <- investmentData$Digital/investmentData$MonthDays
investmentData$pd.Sponsorship <- investmentData$Sponsorship/investmentData$MonthDays
investmentData$pd.Content.Marketing <- investmentData$`Content Marketing`/investmentData$MonthDays
investmentData$pd.Online.marketing <- investmentData$`Online marketing`/investmentData$MonthDays
investmentData$pd.Affiliates <- investmentData$Affiliates/investmentData$MonthDays
investmentData$pd.SEM <- investmentData$SEM/investmentData$MonthDays
investmentData$pd.Radio <- investmentData$Radio/investmentData$MonthDays
investmentData$pd.Other <- investmentData$Other/investmentData$MonthDays

investmentData %>% mutate(Date = ymd(firstDate)) %>% group_by(Date) %>%
  expand(Date = seq(floor_date(Date, unit = "month"),
                    ceiling_date(Date, unit="month")-days(1), by="day"), Year, Month, MonthDays,firstDate,pd.Total.Investment,
         pd.TV,pd.Digital,pd.Sponsorship,pd.Content.Marketing,pd.Online.marketing,pd.Affiliates,pd.SEM,pd.Radio,pd.Other) %>%
  as.data.frame() -> expInvestmentData

str(expInvestmentData)
expInvestmentData$Week <- format(as.Date(expInvestmentData$Date), "%W")

## group investment data on weekly basis
weekInvGroup <- group_by(expInvestmentData, Week)
weekInvDF <- as.data.frame(summarise(weekInvGroup, w.Total.Investment=sum(pd.Total.Investment),
                                     w.TV=sum(pd.TV),
                                     w.Digital=sum(pd.Digital),
                                     w.Sponsorship=sum(pd.Sponsorship),
                                     w.Content.Marketing=sum(pd.Content.Marketing),
                                     w.Online.marketing=sum(pd.Online.marketing),
                                     w.Affiliates=sum(pd.Affiliates),
                                     w.SEM=sum(pd.SEM),
                                     w.Radio=sum(pd.Radio),
                                     w.Other=sum(pd.Other)))

str(weekInvDF)
# Replace 00 week number with 53 for the days after 52nd week
weekInvDF$Week[which(weekInvDF$Week == '00')]<- '53'

weekInvDF$Week <- as.numeric(weekInvDF$Week)

## Change crores to single currency units
weekInvDF[,2:11] <- weekInvDF[,2:11]*10000000


### NPS data preparation
# Remove first column 
npsData$X__1 <- NULL
## Names the columns properly
colnames(npsData) <- c(1:12)

## Enhance data with Year and month 
npsData <- rbind(npsData, c(2015,2015,2015,2015,2015,2015,2016,2016,2016,2016,2016,2016), c(7,8,9,10,11,12,1,2,3,4,5,6))

## Convert rows to columns
npsData = t(npsData)

## Names columnss
colnames(npsData) <- c("NPS","Year","Month")

## NPS value rounding off to decimal place
npsData[,'NPS']=round(npsData[,'NPS'],1)


###### Merge data from all three sources i.e. order, investment and nps
orderPrepDF3 <- merge(orderPrepDF2, weekInvDF, by = c("Week"), all.x = TRUE)
View(orderPrepDF3)
orderPrepDF3 <- merge(orderPrepDF3, npsData, by = c("Year", "Month"), all.x = TRUE)

### Final NA value checks
sapply(orderPrepDF3, function(x) sum(is.na(x)))
## 0 NAs in all columns

finalFullDF <- orderPrepDF3

### To perfomr EDA at three sub categories level, let us filter out these in separate date frames.
HomeAudioDF <- subset(finalFullDF, product_analytic_sub_category == "HomeAudio")
nrow(HomeAudioDF) # 1,14,836

CameAccDF <- subset(finalFullDF, product_analytic_sub_category == "CameraAccessory")
nrow(CameAccDF)  # 2,18,265

GameAccDF <- subset(finalFullDF, product_analytic_sub_category == "GamingAccessory")
nrow(GameAccDF)   # 1,88,179

################################################
# II. Derived KPIs
###############################################

### Function to derive product level KPIs
addProductKPIs <- function(df){

 # KPI: list_price of product : the price actually charged for a product sold
 df$list_price <- df$gmv/df$units

 # KPI:  promotion offered : calculated by using gmv, mrp & units sold
 df$promotion_offered <- (df$product_mrp - df$list_price)/df$product_mrp


 ## Replace  -ve values ,if any, with 0 as promtion cannot be negative
 df$promotion_offered[which(df$promotion_offered < 0)]<- 0

 #KPI: Payment Mode : convert COD & Prepaid to 1 & 0
 df$payment_Mode <- ifelse(df$s1_fact.order_payment_type == "COD",1,0)

 # KPI: Product Segment : Create three segments based on list price,volume  & MRP products as MASS, PREMIUM and PROMISING
 cluster <- aggregate(cbind(units,list_price, product_mrp)~product_analytic_vertical, df, mean)
 cluster$units_1 <- scale(cluster$units)
 cluster$list_price_1 <- scale(cluster$list_price)
 cluster$product_mrp_1 <- scale(cluster$product_mrp) 
 k1 <- cluster[,-c(1:3)]
 # Perform clustering 
 clust <- kmeans(k1, centers = 3, iter.max = 50, nstart = 50)
 cluster$price_mark <- as.factor(clust$cluster)
 cluster <- cluster[, c(1,8)]
  
 # Adding columns generated from the clustering algorithm to the dataset
 df <- merge(df, cluster, by=c("product_analytic_vertical"), all.x = TRUE)
  
 k2 <- count(df, price_mark)[2]
  
 levels(df$price_mark)[which(k2==max(count(df, price_mark)[2]))] <- "MASS"
 levels(df$price_mark)[which(k2==min(count(df, price_mark)[2]))] <- "PREMIUM"
 levels(df$price_mark)[which(k2!=max(count(df, price_mark)[2]) & k2!=min(count(df, price_mark)[2]))] <- "PROMISING"


 # KPI:  Adstock
 # Assuming  adstock rate = 50%
 adsRrate = 0.50
 
 # Prepare adstock for each spend typr
 dwf <- data.frame(Week=1:53)
 
 for(i in 3:ncol(weekInvDF)){
   
   dwf[[paste0(colnames(weekInvDF)[i],"_adstock")]] <- stats::filter(x=weekInvDF[i], filter=adsRrate, method="recursive")
   
 }
 
 # Merging it with the actual dataset
 df <- merge(df, dwf, by = c("Week"), all.x = TRUE)
 
 
 ## Converting the data into weekly format
 
 # Given data is from Jul 2015 to June-2016. 1st week of Jul 2015 is week number 1 (instead of 26) for adstock processing.
 # Also, for Jan-2016 we'll consider the subsequent week number [i.e week number after Dec-2015 last week] (instead as 1st week)
  df$Week <- ifelse(df$Week>=26, df$Week-25, df$Week+28)
 
 # Filtering out the variables which are not necessary
 df <- subset(df, select = -c(Month,Year,product_analytic_sub_category))
 
 # Creating two vectors which holds the numeric and categorical variables
 col_numeric <- c("Week", "gmv", "units", "sla", "product_mrp", "product_procurement_sla")
 col_factor <- c("product_analytic_vertical", "s1_fact.order_payment_type","WeekDay", "eventFlag","event", "price_mark")
 
 # Convering the continuous variables into numeric format and Categorical variables in to factors
 df[,col_numeric] <- sapply(df[,col_numeric], as.numeric)
 df[,col_factor] <- sapply(df[,col_factor], as.factor)
 
 ## df to hold categorical variables
 df_dummies <- df[,col_factor]  
 
 # Creating dummy variables for categorical attributes
 dummies<- data.frame(sapply(df_dummies, function(x) data.frame(model.matrix(~x-1,data =df_dummies))[,-1]))
 dummies <- as.data.frame(cbind(df[1], dummies))
 
 # Weekly aggregation of  dummy variables
 dummies_aggregate <- aggregate(.~ Week, dummies, sum, na.rm = TRUE)
 
 # Weekly aggregation
 df <- df %>% group_by(Week) %>% summarise(gmv = sum(gmv), units = sum(units), sla = mean(sla), product_mrp = sum(product_mrp),
                                               product_procurement_sla = mean(product_procurement_sla),Total_Investment = mean(w.Total.Investment), 
                                               TV = mean(w.TV), Digital = mean(w.Digital), Sponsorship = mean(w.Sponsorship),
                                               Content_Marketing = mean(w.Content.Marketing), Online_Marketing = mean(w.Online.marketing), 
                                               Affiliates = mean(w.Affiliates),SEM = mean(w.SEM), Radio = mean(w.Radio), Other = mean(w.Other),
                                               NPS_Score = mean(NPS),list_price = sum(list_price), promotion_offered = sum(promotion_offered)/length(week),
                                               TV_adstock= mean(w.TV_adstock),Digital_adstock = mean(w.Digital_adstock), Sponsorship_adstock = mean(w.Sponsorship_adstock),
                                               Content_Marketing_adstock = mean(w.Content.Marketing_adstock),
                                               Online_Marketing_adstock = mean(w.Online.marketing_adstock), Affiliates_adstock = mean(w.Affiliates_adstock),
                                               SEM_adtock = mean(w.SEM_adstock), Radio_adstock = mean(w.Radio_adstock), Other_adstock = mean(w.Other_adstock))
 
 
 # Merging the Dummy and actual data variables in to one data frame 
 df <- merge(df, dummies_aggregate, by = c("Week"), all.x = TRUE)
 
return(df)
}
## Function productKPI ends

### Generate Product KPIs for each of three sub categories
### Calling the "product_features" function for the 3 Product subcategories to create the Engineered variables and
### Also to covet the whole data into weekly format

HomeAudioDF_PDKPI <- addProductKPIs(HomeAudioDF)
GameAccDF_PDKPI <- addProductKPIs(GameAccDF)
CameAccDF_PDKPI <- addProductKPIs(CameAccDF)


### Other/Advanced Engineered KPI's::

advanceKPIs <- function(df){

  ##KPI: Moving average
  f1 = function(x) rollmean(x, k = 2, fill = NA, align = "right")
  f2 = function(x) rollmean(x, k = 3, fill = NA, align = "right")
  f3 = function(x) rollmean(x, k = 4, fill = NA, align = "right")
  
  x <- df[,c("Week", "list_price", "promotion_offered")]
  x <- arrange(x, Week)
  
  x1<-x %>% mutate_each(funs(f1),list_price,promotion_offered) %>% data.frame()
  x2<-x %>% mutate_each(funs(f2),list_price,promotion_offered) %>% data.frame()
  x3<-x %>% mutate_each(funs(f3),list_price,promotion_offered) %>% data.frame()
  
  # Imputing missing or NA values
  x1 <- imputeTS::na.ma(x1, k=2, weighting = "simple")
  x2 <- imputeTS::na.ma(x2, k=3, weighting = "simple")
  x3 <- imputeTS::na.ma(x3, k=4, weighting = "simple")
  
  x1$LP_MA1<-(x1$list_price)
  x1$PO_MA1<-(x1$promotion_offered)
  
  x2$LP_MA2<-(x2$list_price)
  x2$PO_MA2<-(x2$promotion_offered)
  
  x3$LP_MA3<-(x3$list_price)
  x3$PO_MA3<-(x3$promotion_offered)
  
  x4=cbind(x1[,-c(1:3)],x2[,-c(1:3)],x3[,-c(1:3)])
  
  data_1 <- cbind(df, x4[,c(1,3,5,2,4,6)])
  df <- data_1
  
  
  df$inc_LP_MA1<-(df$list_price - df$LP_MA1)/df$LP_MA1
  df$inc_LP_MA2<-(df$list_price - df$LP_MA2)/df$LP_MA2
  df$inc_LP_MA3<-(df$list_price - df$LP_MA3)/df$LP_MA3
  
  df$inc_PO_MA1<-(df$promotion_offered - df$PO_MA1)/df$PO_MA1
  df$inc_PO_MA2<-(df$promotion_offered - df$PO_MA2)/df$PO_MA2
  df$inc_PO_MA3<-(df$promotion_offered - df$PO_MA3)/df$PO_MA3
  
  # Deleting columns
  df$LP_MA1<-NULL
  df$LP_MA2<-NULL
  df$LP_MA3<-NULL
  
  df$PO_MA1<-NULL
  df$PO_MA2<-NULL
  df$PO_MA3<-NULL
  
  
  ##KPI:  Lag Variables [For 'list_price', 'promotion_offered', 'gmv']
  df <- df[with(df, order(Week)),] 
  
  #Lag List Price (different period lags) [Lag of list price by 1st week, 2nd week, 3rd week]
  ddf <- slide(df, Var = "list_price", slideBy = -1)
  ddf <- slide(ddf, Var = "list_price", slideBy = -2)
  ddf <- slide(ddf, Var = "list_price", slideBy = -3)
  
  #Lag Promotional Offer (different period lags) [Lag of discount(Promotional Offer) by 1st week, 2nd week, 3rd week]
  ddf <- slide(ddf, Var = "promotion_offered", slideBy = -1)
  ddf <- slide(ddf, Var = "promotion_offered", slideBy = -2)
  ddf <- slide(ddf, Var = "promotion_offered", slideBy = -3)
  
  #Lag gmv (different period lags) [Lag of gmv by 1st week, 2nd week, 3rd week]
  ddf <- slide(ddf, Var = "gmv", slideBy = -1)
  ddf <- slide(ddf, Var = "gmv", slideBy = -2)
  ddf <- slide(ddf, Var = "gmv", slideBy = -3)
  
  df <- ddf
  
  col1 <- c("list_price-1", "promotion_offered-1", "gmv-1")
  col2 <- c("list_price-2", "promotion_offered-2", "gmv-2")
  col3 <- c("list_price-3", "promotion_offered-3", "gmv-3")
  
  df[, col1] <- imputeTS::na.ma(df[, col1], k=1, weighting = "simple")
  df[, col2] <- imputeTS::na.ma(df[, col2], k=2, weighting = "simple")
  df[, col3] <- imputeTS::na.ma(df[, col3], k=3, weighting = "simple")
  
  
  #Incremental Lags
  #Incremental Lags of List Price by 1 week, 2 week, 3 week
  df$LP_lag_1_per <- (df$list_price - df$`list_price-1`)/df$`list_price-1`
  df$LP_lag_2_per <- (df$list_price - df$`list_price-2`)/df$`list_price-2`
  df$LP_lag_3_per <- (df$list_price - df$`list_price-3`)/df$`list_price-3`
  
  df$LP_lag_1_per <- ifelse(is.na(df$LP_lag_1_per),0,df$LP_lag_1_per)
  df$LP_lag_2_per <- ifelse(is.na(df$LP_lag_2_per),0,df$LP_lag_2_per)
  df$LP_lag_3_per <- ifelse(is.na(df$LP_lag_3_per),0,df$LP_lag_3_per)
  
  #Incremental Lags of Promotional Offer by 1 week, 2 week, 3 week
  df$PO_lag_1_per <- (df$promotion_offered - df$`promotion_offered-1`)/df$`promotion_offered-1`
  df$PO_lag_2_per <- (df$promotion_offered - df$`promotion_offered-2`)/df$`promotion_offered-2`
  df$PO_lag_3_per <- (df$promotion_offered - df$`promotion_offered-3`)/df$`promotion_offered-3`
  
  df$PO_lag_1_per <- ifelse(is.na(df$PO_lag_1_per),0,df$PO_lag_1_per)
  df$PO_lag_2_per <- ifelse(is.na(df$PO_lag_2_per),0,df$PO_lag_2_per)
  df$PO_lag_3_per <- ifelse(is.na(df$PO_lag_3_per),0,df$PO_lag_3_per)
  
  #Incremental Lags of gmv by 1 week, 2 week, 3 week
  df$GMV_lag_1_per <- (df$gmv - df$`gmv-1`)/df$`gmv-1`
  df$GMV_lag_2_per <- (df$gmv - df$`gmv-2`)/df$`gmv-2`
  df$GMV_lag_3_per <- (df$gmv - df$`gmv-3`)/df$`gmv-3`
  
  df$GMV_lag_1_per <- ifelse(is.na(df$GMV_lag_1_per),0,df$GMV_lag_1_per)
  df$GMV_lag_2_per <- ifelse(is.na(df$GMV_lag_2_per),0,df$GMV_lag_2_per)
  df$GMV_lag_3_per <- ifelse(is.na(df$GMV_lag_3_per),0,df$GMV_lag_3_per)
  
  #Removing the columns
  df$`list_price-1` <- NULL
  df$`list_price-2` <- NULL
  df$`list_price-3` <- NULL
  
  df$`promotion_offered-1` <- NULL
  df$`promotion_offered-2` <- NULL
  df$`promotion_offered-3` <- NULL
  
  df$`gmv-1` <- NULL
  df$`gmv-2` <- NULL
  df$`gmv-3` <- NULL
  
  return(df)
  
}


### Aadvance KPI's
HomeAudioFinal <- advanceKPIs(HomeAudioDF_PDKPI)
str(HomeAudioFinal)
GameAccFinal <- advanceKPIs(GameAccDF_PDKPI)
str(GameAccFinal)
CameAccFinal <- advanceKPIs(CameAccDF_PDKPI)
str(CameAccFinal)


####################################################################
#
# III. Perform EDA
#
####################################################################

## 1: Weekly units sale
weekly_units_sold <- aggregate(units~Week, finalFullDF, sum, na.rm=TRUE)

# Plotting Bar graph showing weekly sales
ggplot(weekly_units_sold, aes(x=Week, y=units)) + geom_bar(stat = "identity", fill="red") + 
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_minimal() + 
  labs(x="Week",y="Units") + ggtitle("Weekly Plot") +
  scale_x_continuous(breaks=seq(1,53,1))

# Plotting Line graph showing Weekly orders
ggplot(weekly_units_sold, aes(x=Week, y=units)) + geom_line(size=1, color = "red") + geom_point() + theme_bw() + scale_x_continuous(breaks=seq(1,53,1))

## 2: Units sold by product sub-category
product_units_sold <- aggregate(units~product_analytic_sub_category, finalFullDF, sum, na.rm=TRUE)

# Plotting Bar graph showing units sold for different product sub-category
ggplot(product_units_sold, aes(x=as.factor(product_analytic_sub_category), y=units,fill=as.factor(product_analytic_sub_category))) + geom_bar(stat = "identity", width = 0.4) + 
  geom_text(aes(label=units), vjust=-0.3, size=4) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Sub-Category",y="Units") + 
  ggtitle("Units per Sub-category") +
theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 


## 3: Total weekly orders by product sub-category
weekly_product_units_sold <- aggregate(units~Week + product_analytic_sub_category, finalFullDF, sum, na.rm=TRUE)

# Plotting Bar graph showing units sold for different product sub-category weekly
ggplot(weekly_product_units_sold, aes(x=Week,y=units, fill=as.factor(product_analytic_sub_category))) + geom_bar(stat="identity",position = "stack", width = 4) + 
  theme_hc(base_size = 18, base_family = "sans") + labs(x="Weeks",y="Number of Units Sold") + 
  ggtitle("Weekly Units  by Product Sub-Categories") +
  theme(legend.justification="center",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1,53,1))


# Plotting Line graph showing weekly orders
ggplot(weekly_product_units_sold, aes(x=Week,y=units, group=as.factor(product_analytic_sub_category))) + geom_line(size=1.5, aes(color = product_analytic_sub_category)) + 
  theme_bw() + labs(x="Weeks",y="Units") + ggtitle("Weekly Units by Product Sub-Categories") + 
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1,53,1))

str(finalFullDF)
## 4: Units sold by top 10 Product Vertical from 3 sub categories
product_vertical_units_sold <- aggregate(units~product_analytic_vertical, finalFullDF, sum, na.rm=TRUE)

top_10_product_vertical <- product_vertical_units_sold[order(product_vertical_units_sold$units, decreasing = TRUE),][1:10,]

# Plotting Bar graph showing units sold for top 10 Product Vertical
ggplot(top_10_product_vertical, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Vertical",y="Number of Units Sold") + 
  ggtitle("Units Sold") + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle = 45),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5),legend.position="none") 


## 5:  Aggregated Product units sold on weekdays/weekends
WeekDay_units_sold <- aggregate(units~WeekDay, finalFullDF, sum, na.rm=TRUE)

WeekDay_units_sold$WeekDay <- factor(WeekDay_units_sold$WeekDay,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plotting Bar graph showing units sold for top 10 Product Vertical
ggplot(WeekDay_units_sold, aes(x=as.factor(WeekDay), y=units, fill = as.factor(WeekDay))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Weekdays",y="Number of Units Sold") + 
  ggtitle("Units Sold on Weekdays/Weekend") + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=45),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5),legend.position="none") 


##6: Weekly spent on different channels 
weekly_ad_spent <- finalFullDF[,c(3,17:25)] %>% group_by(Week) %>% summarise_all(funs(unique)) %>% data.frame()

# Plotting Line graph amount spent on different marketing channels
plots <- list()  # new empty list

for (i in 2:10) local({
  i <- i
  p0 <- ggplot(weekly_ad_spent,aes(x=weekly_ad_spent[,1],y=weekly_ad_spent[,i])) + 
    geom_line(size=1, color = "red") + geom_point() + theme_bw() + 
    labs(x="Weeks",y= paste0("Spend on ", colnames(weekly_ad_spent[i])," Ads"))+
    scale_x_continuous(breaks=seq(1,53,2))
  
  plots[[i-1]] <<- p0  # add each plot into plot list
  
})

# Plotting all the graphs
# Note: It takes few seconds to load, please hold in order to come up all the graphs
plot_grid(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],
          plots[[7]],plots[[8]],plots[[9]],align ="h")




## Sub category plots
##CameraAccessory 

## 7: Weekly GMV
CA_weekly_gmv <- aggregate(gmv~Week, CameAccDF, sum, na.rm=TRUE)

# Plotting Line graph showing weekly GMV
ggplot(CA_weekly_gmv, aes(x=Week, y=gmv)) + geom_line(size=1, color = "red") + geom_point() +
  theme_bw() + labs(x="Weeks",y="GMV") + ggtitle("CameraAccessory - Weekly GMV") + 
  scale_x_continuous(breaks=seq(1,53,1))+
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 


 
##8:  Weekly units sold
CA_weekly_units_sold <- aggregate(units~Week, CameAccDF, sum, na.rm=TRUE)

# Plotting Line graph showing weekly sale
ggplot(CA_weekly_units_sold, aes(x=Week, y=units)) + geom_line(size=1, color = "red") + geom_point() +
  theme_bw() + labs(x="Weeks",y="Number of Units Sold") + ggtitle("CameraAccessory - Weekly Units Sold") + 
  scale_x_continuous(breaks=seq(1,53,1))+
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 


## 9: Aggregated units sold for top 10 CameraAccessory Products
CA_product_units_sold <- aggregate(units~product_analytic_vertical, CameAccDF, sum, na.rm=TRUE)

CA_top_10_products <- CA_product_units_sold[order(CA_product_units_sold$units, decreasing = TRUE),][1:10,]

# Ordering the top 10 products based on the number of units sold for display
CA_top_10_products$product_analytic_vertical <- factor(CA_top_10_products$product_analytic_vertical, levels = CA_top_10_products$product_analytic_vertical[order(-CA_top_10_products$units)]) 

# Plotting Bar graph showing units sold for top 10 Products
ggplot(CA_top_10_products, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Vertical",y="Number of Units Sold") + 
  ggtitle("CameraAccessory - Product Units Sold") + theme(legend.position="none", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle = 45, vjust = 0.6),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 

  
##10:  Units sold on big sale days 
CA_special_sale_day_units_sold <- subset(CameAccDF, event != "Non-event Day")

CA_special_sale_day_units_sold <- aggregate(units~event, CA_special_sale_day_units_sold, sum, na.rm=TRUE)


ggplot(CA_special_sale_day_units_sold, aes(x=as.factor(event), y=units, fill = as.factor(event))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Event",y="Units") + 
  ggtitle("Units Sold on Big sale days") + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=45),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5),legend.position="none") 


##Game Accessory 

## 11: Weekly GMV
CA_weekly_gmv <- aggregate(gmv~Week, GameAccDF, sum, na.rm=TRUE)

# Plotting Line graph showing weekly GMV
ggplot(CA_weekly_gmv, aes(x=Week, y=gmv)) + geom_line(size=1, color = "red") + geom_point() +
  theme_bw() + labs(x="Weeks",y="GMV") + ggtitle("Game Accessory - Weekly GMV") + 
  scale_x_continuous(breaks=seq(1,53,1))+
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 



##12:  Weekly units sold
CA_weekly_units_sold <- aggregate(units~Week, GameAccDF, sum, na.rm=TRUE)

# Plotting Line graph showing weekly sale
ggplot(CA_weekly_units_sold, aes(x=Week, y=units)) + geom_line(size=1, color = "red") + geom_point() +
  theme_bw() + labs(x="Weeks",y="Number of Units Sold") + ggtitle("Game Accessory - Weekly Units Sold") + 
  scale_x_continuous(breaks=seq(1,53,1))+
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 


## 13: Aggregated units sold for top 10 Game Accessory Products
CA_product_units_sold <- aggregate(units~product_analytic_vertical, GameAccDF, sum, na.rm=TRUE)

CA_top_10_products <- CA_product_units_sold[order(CA_product_units_sold$units, decreasing = TRUE),][1:10,]

# Ordering the top 10 products based on the number of units sold for display
CA_top_10_products$product_analytic_vertical <- factor(CA_top_10_products$product_analytic_vertical, levels = CA_top_10_products$product_analytic_vertical[order(-CA_top_10_products$units)]) 

# Plotting Bar graph showing units sold for top 10 Products
ggplot(CA_top_10_products, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Vertical",y="Number of Units Sold") + 
  ggtitle("Game Accessory - Product Units Sold") + theme(legend.position="none", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle = 45, vjust = 0.6),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 


##14:  Units sold on big sale days 
CA_special_sale_day_units_sold <- subset(GameAccDF, event != "Non-event Day")

CA_special_sale_day_units_sold <- aggregate(units~event, CA_special_sale_day_units_sold, sum, na.rm=TRUE)


ggplot(CA_special_sale_day_units_sold, aes(x=as.factor(event), y=units, fill = as.factor(event))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Event",y="Units") + 
  ggtitle("Units Sold on Big sale days") + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=45),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5),legend.position="none") 


##Home Audio 

## 15: Weekly GMV
CA_weekly_gmv <- aggregate(gmv~Week, HomeAudioDF, sum, na.rm=TRUE)

# Plotting Line graph showing weekly GMV
ggplot(CA_weekly_gmv, aes(x=Week, y=gmv)) + geom_line(size=1, color = "red") + geom_point() +
  theme_bw() + labs(x="Weeks",y="GMV") + ggtitle("Home Audio - Weekly GMV") + 
  scale_x_continuous(breaks=seq(1,53,1))+
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 



##16:  Weekly units sold
CA_weekly_units_sold <- aggregate(units~Week, HomeAudioDF, sum, na.rm=TRUE)

# Plotting Line graph showing weekly sale
ggplot(CA_weekly_units_sold, aes(x=Week, y=units)) + geom_line(size=1, color = "red") + geom_point() +
  theme_bw() + labs(x="Weeks",y="Number of Units Sold") + ggtitle("Home Audio - Weekly Units Sold") + 
  scale_x_continuous(breaks=seq(1,53,1))+
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 


## 17: Aggregated units sold for top 10 Home Audio Products
CA_product_units_sold <- aggregate(units~product_analytic_vertical, HomeAudioDF, sum, na.rm=TRUE)

CA_top_10_products <- CA_product_units_sold[order(CA_product_units_sold$units, decreasing = TRUE),][1:10,]

# Ordering the top 10 products based on the number of units sold for display
CA_top_10_products$product_analytic_vertical <- factor(CA_top_10_products$product_analytic_vertical, levels = CA_top_10_products$product_analytic_vertical[order(-CA_top_10_products$units)]) 

# Plotting Bar graph showing units sold for top 10 Products
ggplot(CA_top_10_products, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Product Vertical",y="Number of Units Sold") + 
  ggtitle("Home Audio - Product Units Sold") + theme(legend.position="none", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle = 45, vjust = 0.6),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) 


##18:  Units sold on big sale days 
CA_special_sale_day_units_sold <- subset(HomeAudioDF, event != "Non-event Day")

CA_special_sale_day_units_sold <- aggregate(units~event, CA_special_sale_day_units_sold, sum, na.rm=TRUE)


ggplot(CA_special_sale_day_units_sold, aes(x=as.factor(event), y=units, fill = as.factor(event))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 18, base_family = "sans") + labs(x="Event",y="Units") + 
  ggtitle("Units Sold on Big sale days") + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=45),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5),legend.position="none") 


#######
## Enginered KPIs EDA
######


### Camera Accessory
colnames(CameAccFinal)
### gmv vs independent variables
CA <- CameAccFinal[,c(2,17:28,74:88)]

# Plotting the scatter plot of all the Independent variables w.r.t 'gmv'
CA_plots1 <- list()  # new empty list
for (i in 2:28) local({
  i <- i
  iplot <- ggplot(CA,aes(x=CA[,i],y=CA[,1])) + geom_point() + geom_smooth(method = "loess") + theme_bw() + 
    labs(x= paste0("", colnames(CA[i])),y="GMV")
  
  CA_plots1[[i-1]] <<- iplot  # add each plot into plot list
  
})

# Plotting all the graphs
plot_grid(CA_plots1[[1]],CA_plots1[[2]],CA_plots1[[3]],CA_plots1[[4]],CA_plots1[[5]],CA_plots1[[6]],
          CA_plots1[[7]],align ="h")

plot_grid(CA_plots1[[8]],CA_plots1[[9]],CA_plots1[[10]],CA_plots1[[11]],CA_plots1[[12]],CA_plots1[[13]],
          CA_plots1[[14]],align ="h")

plot_grid(CA_plots1[[15]],CA_plots1[[16]],CA_plots1[[17]],CA_plots1[[18]],CA_plots1[[19]],CA_plots1[[20]],
          CA_plots1[[21]],align ="h")

plot_grid(CA_plots1[[22]],CA_plots1[[23]],CA_plots1[[24]],CA_plots1[[25]],CA_plots1[[26]],CA_plots1[[27]],align ="h")



###Gaming Accessory
colnames(GameAccFinal)
### Response Curves ['gmv' w.r.t all the Independent variables]
GA <- GameAccFinal[,c(2,17:27,65:79)]

# Plotting the scatter plot of all the Independent variables w.r.t 'gmv'
GA_plots1 <- list()  # new empty list
for (i in 2:27) local({
  i <- i
  iplot <- ggplot(GA,aes(x=GA[,i],y=GA[,1])) + geom_point() + geom_smooth(method = "loess") + theme_bw() + 
    labs(x= paste0("", colnames(GA[i])),y="GMV")
  
  GA_plots1[[i-1]] <<- iplot  # add each plot into plot list
  
})

# Plotting all the graphs
# Note: It takes few seconds to load, please hold in order to come up all the graphs
plot_grid(GA_plots1[[1]],GA_plots1[[2]],GA_plots1[[3]],GA_plots1[[4]],GA_plots1[[5]],GA_plots1[[6]],
          GA_plots1[[7]],GA_plots1[[8]],GA_plots1[[9]],align ="h")

plot_grid(GA_plots1[[10]],GA_plots1[[11]],GA_plots1[[12]],GA_plots1[[13]],GA_plots1[[14]],GA_plots1[[15]],
          GA_plots1[[16]],GA_plots1[[17]],GA_plots1[[18]],align ="h")

plot_grid(GA_plots1[[19]],GA_plots1[[20]],GA_plots1[[21]],GA_plots1[[22]],GA_plots1[[23]],GA_plots1[[24]],
          GA_plots1[[25]],GA_plots1[[26]],align ="h")


###Home Audio
colnames(HomeAudioFinal)
### ### Response Curves ['gmv' w.r.t all the Independent variables]
HA <- HomeAudioFinal[,c(2,17:27,59:73)]

# Plotting the scatter plot of all the Independent variables w.r.t 'gmv'
HA_plots1 <- list()  # new empty list
for (i in 2:27) local({
  i <- i
  iplot <- ggplot(HA,aes(x=HA[,i],y=HA[,1])) + geom_point() + geom_smooth(method = "loess") + theme_bw() + 
    labs(x= paste0("", colnames(HA[i])),y="GMV")
  
  HA_plots1[[i-1]] <<- iplot  # add each plot into plot list
  
})

# Plotting all the graphs
# Note: It takes few seconds to load, please hold in order to come up all the graphs
plot_grid(HA_plots1[[1]],HA_plots1[[2]],HA_plots1[[3]],HA_plots1[[4]],HA_plots1[[5]],HA_plots1[[6]],
          HA_plots1[[7]],HA_plots1[[8]],HA_plots1[[9]],align ="h")

plot_grid(HA_plots1[[10]],HA_plots1[[11]],HA_plots1[[12]],HA_plots1[[13]],HA_plots1[[14]],HA_plots1[[15]],
          HA_plots1[[16]],HA_plots1[[17]],HA_plots1[[18]],align ="h")

plot_grid(HA_plots1[[19]],HA_plots1[[20]],HA_plots1[[21]],HA_plots1[[22]],HA_plots1[[23]],HA_plots1[[24]],
          HA_plots1[[25]],HA_plots1[[26]],align ="h")








####################################################################
#
# IV. Linear Model - Refer EcommerceModel.R file
#
####################################################################


