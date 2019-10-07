######################################################################################
##      Capstone Ecommerce- R File - Sub Category: Gaming Accessory  Models 
######################################################################################

## Section 4: Model Building

####################### Model Type 1 ###########
##################### Basic Linear Model #######


# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# Remove lag variables and Moving averages variables
str(GameAccFinal)
#data.frame':	50 obs. of  73 variables
colnames(GameAccFinal)
# 18, 19 are list_price and promotion_offered
# Also removing lag and MA variables
LinearGamesDF <- GameAccFinal[,-c(18:19,65:79)]
colnames(LinearGamesDF)



## Scaling the variables
LinearGamesDF[,2:ncol(LinearGamesDF)] <- scale(LinearGamesDF[,2:ncol(LinearGamesDF)])

#write.csv(LinearHomeAudioDF,'LinearHomeAudioDF.csv')

### Step to remove insignificant and correlated variables

# Base intercept model
HABase<- lm(gmv ~ 1 , data= LinearGamesDF)
# Complete model all variables
HAAll<- lm(gmv ~ . , data= LinearGamesDF) 
summary(HAAll)

HAStep <- step(HABase, scope = list(lower = HABase, upper = HAAll), direction = "both", trace = 1, steps = 1000)

SelectedVars <- names(unlist(HAStep[[1]])) 
# Delete Intercepts
SelectedVars <- SelectedVars[!SelectedVars %in% "(Intercept)"]  # remove intercept

show(SelectedVars)
#[1] "units"                                            "product_analytic_vertical.xGamingAccessoryKit"   
#[3] "s1_fact.order_payment_type"                       "Radio"                                           
#[5] "product_analytic_vertical.xJoystickGamingWheel"   "product_analytic_vertical.xGamePad"              
#[7] "event.xEid...Rath"                                "Other_adstock"                                   
#[9] "Affiliates"                                       "WeekDay.xMonday"                                 
#[11] "Affiliates_adstock"                               "price_mark.xPROMISING"                           
#[13] "product_analytic_vertical.xTVOutCableAccessory"   "event.xRepublic.Day"                             
#[15] "event.xPacman"                                    "Online_Marketing_adstock"                        
#[17] "Online_Marketing"                                 "event.xDaussera"                                 
#[19] "Sponsorship_adstock"                              "TV_adstock"                                      
#[21] "event.xFHSD"                                      "event.xBSD"                                      
#[23] "WeekDay.xSaturday"                                "product_mrp"                                     
##[25] "product_analytic_vertical.xGamingMousePad"        "product_analytic_vertical.xGamingChargingStation"
#[27] "product_analytic_vertical.xGamingAdapter"         "product_analytic_vertical.xGamingGun"            
#[29] "product_analytic_vertical.xGamingMouse"                            

### Model Building::

## 1st model based on selected variables
HAModel_L1 <- lm(formula = gmv ~ units + s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
              + event.xEid...Rath + Affiliates + Affiliates_adstock + product_analytic_vertical.xTVOutCableAccessory
              + event.xPacman + Online_Marketing + Sponsorship_adstock + event.xFHSD + WeekDay.xSaturday
              + product_analytic_vertical.xGamingMousePad + product_analytic_vertical.xGamingAdapter
              + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingGun
              + product_analytic_vertical.xGamingChargingStation + product_mrp + event.xBSD
              + TV_adstock + event.xDaussera + Online_Marketing_adstock + event.xRepublic.Day
              + price_mark.xPROMISING + WeekDay.xMonday + Other_adstock + product_analytic_vertical.xGamePad
              + Radio + product_analytic_vertical.xGamingAccessoryKit, data = LinearGamesDF)
summary(HAModel_L1)
#Multiple R-squared:  0.999,	Adjusted R-squared:  0.997 
vif(HAModel_L1)

# remove special event days
HAModel_L1 <- lm(formula = gmv ~ units + s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 + Affiliates + Affiliates_adstock + product_analytic_vertical.xTVOutCableAccessory
                 + Online_Marketing + Sponsorship_adstock 
                 + product_analytic_vertical.xGamingMousePad + product_analytic_vertical.xGamingAdapter
                 + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingGun
                 + product_analytic_vertical.xGamingChargingStation + product_mrp 
                 + TV_adstock + Online_Marketing_adstock
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad
                 + Radio + product_analytic_vertical.xGamingAccessoryKit, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)
#Multiple R-squared:  0.99,	Adjusted R-squared:  0.983

#remove high vif
HAModel_L1 <- lm(formula = gmv ~  s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 + Affiliates + Affiliates_adstock + product_analytic_vertical.xTVOutCableAccessory
                 + Online_Marketing + Sponsorship_adstock 
                 + product_analytic_vertical.xGamingMousePad + product_analytic_vertical.xGamingAdapter
                 + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingGun
                 + product_analytic_vertical.xGamingChargingStation + product_mrp 
                 + TV_adstock + Online_Marketing_adstock
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad
                 + Radio + product_analytic_vertical.xGamingAccessoryKit, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)

#remove high vif
HAModel_L1 <- lm(formula = gmv ~  s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 + Affiliates + product_analytic_vertical.xTVOutCableAccessory
                 + Online_Marketing + Sponsorship_adstock 
                 + product_analytic_vertical.xGamingMousePad + product_analytic_vertical.xGamingAdapter
                 + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingGun
                 + product_analytic_vertical.xGamingChargingStation + product_mrp 
                 + TV_adstock + Online_Marketing_adstock
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad
                 + Radio + product_analytic_vertical.xGamingAccessoryKit, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)

#remove high vif
HAModel_L1 <- lm(formula = gmv ~  s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                  + product_analytic_vertical.xTVOutCableAccessory
                 + Online_Marketing + Sponsorship_adstock 
                 + product_analytic_vertical.xGamingMousePad + product_analytic_vertical.xGamingAdapter
                 + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingGun
                 + product_analytic_vertical.xGamingChargingStation + product_mrp 
                 + TV_adstock + Online_Marketing_adstock
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad
                 + Radio + product_analytic_vertical.xGamingAccessoryKit, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)

#remove high vif
HAModel_L1 <- lm(formula = gmv ~  s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 + product_analytic_vertical.xTVOutCableAccessory
                 + Online_Marketing + Sponsorship_adstock 
                 + product_analytic_vertical.xGamingMousePad + product_analytic_vertical.xGamingAdapter
                 + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingGun
                 + product_analytic_vertical.xGamingChargingStation  
                 + TV_adstock + Online_Marketing_adstock
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad
                 + Radio + product_analytic_vertical.xGamingAccessoryKit, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)
#Residual standard error: 0.143 on 36 degrees of freedom
#Multiple R-squared:  0.986,	Adjusted R-squared:  0.98

# remove high p
HAModel_L1 <- lm(formula = gmv ~  s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 + product_analytic_vertical.xTVOutCableAccessory
                 + Online_Marketing  
                 + product_analytic_vertical.xGamingMousePad 
                 + product_analytic_vertical.xGamingMouse +
                 + product_analytic_vertical.xGamingChargingStation  
                 + TV_adstock + Online_Marketing_adstock
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad
                 + Radio + product_analytic_vertical.xGamingAccessoryKit, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)

# remove high p
HAModel_L1 <- lm(formula = gmv ~  s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 + product_analytic_vertical.xTVOutCableAccessory
                 + Online_Marketing 
                 + product_analytic_vertical.xGamingMouse +
                   + product_analytic_vertical.xGamingChargingStation  
                  + Online_Marketing_adstock
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad
                 + Radio + product_analytic_vertical.xGamingAccessoryKit, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)

# remove high vif 
HAModel_L1 <- lm(formula = gmv ~  s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 + Online_Marketing + product_analytic_vertical.xGamingMouse +
                   + product_analytic_vertical.xGamingChargingStation  
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad
                 + Radio, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)

# remove high p 
HAModel_L1 <- lm(formula = gmv ~  s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                  + product_analytic_vertical.xGamingMouse +
                   + product_analytic_vertical.xGamingChargingStation  
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad
                 + Radio, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)

# remove high p 
HAModel_L1 <- lm(formula = gmv ~  s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 + product_analytic_vertical.xGamingMouse
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)


# remove payment_type as it is not in control of company
HAModel_L1 <- lm(formula = gmv ~  product_analytic_vertical.xJoystickGamingWheel
                 + product_analytic_vertical.xGamingMouse
                 + price_mark.xPROMISING + product_analytic_vertical.xGamePad, data = LinearGamesDF)

summary(HAModel_L1)
vif(HAModel_L1)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                    -5.11e-17   2.76e-02    0.00   1.0000    
#product_analytic_vertical.xJoystickGamingWheel  1.59e-01   3.14e-02    5.07  6.4e-06 ***
#  product_analytic_vertical.xGamingMouse          1.41e-01   4.26e-02    3.30   0.0018 ** 
#  price_mark.xPROMISING                           3.95e-01   4.30e-02    9.18  3.8e-12 ***
#  product_analytic_vertical.xGamePad              4.86e-01   4.01e-02   12.12  3.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.201 on 48 degrees of freedom
#Multiple R-squared:  0.963,	Adjusted R-squared:  0.96 

#product_analytic_vertical.xJoystickGamingWheel         product_analytic_vertical.xGamingMouse 
#1.27                                           2.35 
#price_mark.xPROMISING             product_analytic_vertical.xGamePad 
#2.38                                           2.07 


### Cross-validation - Model 


cv.lm(data = LinearGamesDF, form.lm = HAModel_L1, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)


### Estimating the elasticity coefficients - Model 

elasticity_L <- function(var){
  L_elasticity <- as.numeric(HAModel_L1$coefficients[var]*mean(LinearGamesDF[,var])/mean(LinearGamesDF$gmv))
  return(L_elasticity)
  
} 

L_var_list <- list()

for(i in 2:length(HAModel_L1$coefficients)){
  L_var_list[i-1] <- elasticity_L(names(HAModel_L1$coefficients)[i])
  
}

L_elasticity.outputs <- data.frame(names(HAModel_L1$coefficients[2:length(HAModel_L1$coefficients)]))
L_elasticity.outputs <- cbind(L_elasticity.outputs,do.call(rbind.data.frame, L_var_list))
colnames(L_elasticity.outputs) <- c("Variable","Elasticity")

L_elasticity.outputs$Direction <- ifelse(L_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(L_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Game Accessory - Basic Linear Model") +xlab("Variables")  



################################ Model Type 2 ####################################
###########################   Multiplicative Model  #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# Remove lag variables and Moving averages variables
str(GameAccFinal)
#data.frame':	50 obs. of  73 variables
colnames(GameAccFinal)
# 18, 19 are list_price and promotion_offered
# 59 to 73 are lag and MA variables

GameAccDF <- GameAccFinal[,-c(18:19,65:79)]
colnames(GameAccDF)

## To avoid log (0) issue, replace 0 with '0.00001' in df
GameAccDF[GameAccDF == 0] <- 0.00001

##  Convert values to log for Multiplicative model
GameAccDF <- log(GameAccDF)

## Linear relationship or multicollinearity
MultiGameModel <- lm(gmv~.,GameAccDF)
alias(MultiGameModel)

##colnames(MultiHomeAudioDF)
## Remove linear or multicollinear relationship variables
##MultiHomeAudioDF <- MultiHomeAudioDF[, -c(51:56)]

### Use Stepwise Regression to remove insignificant and correlated variables
MultiGA_base.mod <- lm(gmv ~ 1 , data= GameAccDF)  
MultiGA_all.mod <- lm(gmv ~ . , data= GameAccDF) 
MultiGA_stepMod <- step(MultiGA_base.mod, scope = list(lower = MultiGA_base.mod, upper = MultiGA_all.mod), direction = "both", trace = 1, steps = 1000) 
MultiGA_shortlistedVars <- names(unlist(MultiGA_stepMod[[1]])) 
MultiGA_shortlistedVars <- MultiGA_shortlistedVars[!MultiGA_shortlistedVars %in% "(Intercept)"]  

show(MultiGA_shortlistedVars)
#[1] "units"                                          "s1_fact.order_payment_type"                    
#[3] "product_analytic_vertical.xJoystickGamingWheel" "product_analytic_vertical.xTVOutCableAccessory"
#[5] "product_analytic_vertical.xGamingMouse"         "product_analytic_vertical.xGamingAccessoryKit" 
#[7] "product_analytic_vertical.xGamePad"             "event.xEid...Rath"                             
#[9] "Online_Marketing_adstock"                       "Affiliates"                                    
#[11] "product_analytic_vertical.xMotionController"    "eventFlag"                                     
#[13] "SEM_adtock"                                     "Total_Investment"                              
#[15] "event.xFHSD"                                    "product_analytic_vertical.xGamingSpeaker"      
#[17] "Week"                                           "product_analytic_vertical.xGameControlMount"   
#[19] "WeekDay.xMonday"                                "WeekDay.xWednesday"                            
#[21] "product_analytic_vertical.xGamingMemoryCard"    "event.xChristmas...NY"             

## First Model
MultiGAModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xJoystickGamingWheel
                     + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamePad
                     + Online_Marketing_adstock + product_analytic_vertical.xMotionController
                     + SEM_adtock + event.xFHSD + Week + WeekDay.xMonday + product_analytic_vertical.xGamingMemoryCard
                     + s1_fact.order_payment_type + product_analytic_vertical.xTVOutCableAccessory
                     + event.xEid...Rath + Affiliates + eventFlag + Total_Investment
                     + product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xGameControlMount
                     + WeekDay.xWednesday + event.xChristmas...NY, data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)
##Multiple R-squared:     1,	Adjusted R-squared:     1

## Removing special event days
MultiGAModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xJoystickGamingWheel
                     + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamePad
                     + Online_Marketing_adstock + product_analytic_vertical.xMotionController
                     + SEM_adtock + product_analytic_vertical.xGamingMemoryCard
                     + s1_fact.order_payment_type + product_analytic_vertical.xTVOutCableAccessory+ Affiliates 
                     + product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xGameControlMount
                     , data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)

## Removing high vif 
MultiGAModel_1 <- lm(formula = gmv ~  product_analytic_vertical.xJoystickGamingWheel
                     + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamePad
                     + Online_Marketing_adstock + SEM_adtock + product_analytic_vertical.xGamingMemoryCard
                     + s1_fact.order_payment_type + product_analytic_vertical.xTVOutCableAccessory+ Affiliates 
                     + product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xGameControlMount
                     , data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)

## Removing high vif 
MultiGAModel_1 <- lm(formula = gmv ~  product_analytic_vertical.xJoystickGamingWheel
                      + product_analytic_vertical.xGamePad
                     + Online_Marketing_adstock + SEM_adtock + product_analytic_vertical.xGamingMemoryCard
                     + s1_fact.order_payment_type + product_analytic_vertical.xTVOutCableAccessory+ Affiliates 
                     + product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xGameControlMount
                     , data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)


## Removing high vif 
MultiGAModel_1 <- lm(formula = gmv ~  
                     + product_analytic_vertical.xGamePad
                     + Online_Marketing_adstock + SEM_adtock +
                     + s1_fact.order_payment_type + product_analytic_vertical.xTVOutCableAccessory+ Affiliates 
                     + product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xGameControlMount
                     , data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)



## Removing high vif 
MultiGAModel_1 <- lm(formula = gmv ~  
                      Online_Marketing_adstock + SEM_adtock + s1_fact.order_payment_type + Affiliates 
                     + product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xGameControlMount
                     , data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)

## Removing high p 
MultiGAModel_1 <- lm(formula = gmv ~  
                       Online_Marketing_adstock  + s1_fact.order_payment_type + Affiliates 
                     + product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xGameControlMount
                     , data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)

## Removing high p 
MultiGAModel_1 <- lm(formula = gmv ~  
                       Online_Marketing_adstock  + s1_fact.order_payment_type + Affiliates 
                     + product_analytic_vertical.xGamingSpeaker 
                     , data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)

## Removing high p 
MultiGAModel_1 <- lm(formula = gmv ~  
                     s1_fact.order_payment_type + Affiliates 
                     + product_analytic_vertical.xGamingSpeaker 
                     , data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)


## Removing high p 
MultiGAModel_1 <- lm(formula = gmv ~  
                       s1_fact.order_payment_type + Affiliates 
                     , data = GameAccDF)

summary(MultiGAModel_1)
vif(MultiGAModel_1)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  3.7598     1.4843    2.53    0.014 *  
#  s1_fact.order_payment_type   0.3341     0.0365    9.15  3.0e-12 ***
#  Affiliates                   0.5497     0.1032    5.33  2.4e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.621 on 50 degrees of freedom
#Multiple R-squared:  0.903,	Adjusted R-squared:  0.899 
#F-statistic:  232 on 2 and 50 DF,  p-value: <2e-16

#s1_fact.order_payment_type                 Affiliates 
#2.48                       2.48 

### Cross-validation  - Multiplicative Model 
cv.lm(data = GameAccDF, form.lm = MultiGAModel_1, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating the elasticity coefficients- Model 

elasticity_MultiGA <- function(var){
  MultiGA_elasticity <- as.numeric(MultiGAModel_1$coefficients[var]*mean(GameAccDF[,var])/mean(GameAccDF$gmv))
  return(MultiGA_elasticity)
  
} 

MultiGA_var_list <- list()

for(i in 2:length(MultiGAModel_1$coefficients)){
  MultiGA_var_list[i-1] <- elasticity_MultiGA(names(MultiGAModel_1$coefficients)[i])
  
}

MultiGA_elasticity.outputs <- data.frame(names(MultiGAModel_1$coefficients[2:length(MultiGAModel_1$coefficients)]))
MultiGA_elasticity.outputs <- cbind(MultiGA_elasticity.outputs,do.call(rbind.data.frame, MultiGA_var_list))
colnames(MultiGA_elasticity.outputs) <- c("Variable","Elasticity")

MultiGA_elasticity.outputs$Direction <- ifelse(MultiGA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(MultiGA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust=0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Game Acc - Multiplicative Model") +xlab("Variables")  


#######Model Type 2 Ends########################################################

################################ Model Type 3 ##################################
########################### Distributied Lag Model #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# keeping Week# 1, 2 and 3  lag value of 'gmv' and removing other lag variables and Moving averages variables
str(GameAccFinal)
#data.frame':	50 obs. of  73 variables
colnames(GameAccFinal)
# 18, 19 are list_price and promotion_offered
# 65 to 76 are lag and MA variables
# Keeping GMV_lag_1_per", "GMV_lag_2_per" ,"GMV_lag_3_per"  (column# 77:79)
GamesDF <- GameAccFinal[,-c(18:19,65:76)]
colnames(GamesDF)

## Scaling the variables
GamesDF[,2:ncol(GamesDF)] <- scale(GamesDF[,2:ncol(GamesDF)])

### Step to remove insignificant and correlated variables

# Base intercept model
GamesBase<- lm(gmv ~ 1 , data= GamesDF)
# Complete model all variables
GamesAll<- lm(gmv ~ . , data= GamesDF) 
summary(GamesAll)

GameStep <- step(GamesBase, scope = list(lower = GamesBase, upper = GamesAll), direction = "both", trace = 1, steps = 1000)
DLSelectedVars <- names(unlist(GameStep[[1]])) 

# Remove Intercepts
DLSelectedVars <- DLSelectedVars[!DLSelectedVars %in% "(Intercept)"]  


show(DLSelectedVars)
#[1] "units"                                          "product_analytic_vertical.xMotionController"   
#[3] "product_analytic_vertical.xGamingAccessoryKit"  "s1_fact.order_payment_type"                    
#[5] "Radio"                                          "product_analytic_vertical.xJoystickGamingWheel"
#[7] "product_analytic_vertical.xGamePad"             "event.xEid...Rath"                             
#[9] "GMV_lag_1_per"                                  "Other_adstock"                                 
#[11] "Online_Marketing"                               "WeekDay.xMonday"                               
#[13] "event.xDiwali"                                  "event.xFHSD"                                   
#[15] "event.xRepublic.Day"                            "NPS_Score"                         

### Build model

## Iteration 1:
GamesModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit
                   + Radio + product_analytic_vertical.xGamePad + GMV_lag_1_per
                   + Online_Marketing + event.xDiwali + event.xRepublic.Day
                   + product_analytic_vertical.xMotionController + s1_fact.order_payment_type
                   + product_analytic_vertical.xJoystickGamingWheel + event.xEid...Rath
                   + Other_adstock + WeekDay.xMonday + event.xFHSD + NPS_Score, data = GamesDF)

summary(GamesModel_1)
vif(GamesModel_1)
#Multiple R-squared:  0.997,	Adjusted R-squared:  0.996 

#Remove special event days
GamesModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit
                   + Radio + product_analytic_vertical.xGamePad + GMV_lag_1_per
                   + Online_Marketing  
                   + product_analytic_vertical.xMotionController + s1_fact.order_payment_type
                   + product_analytic_vertical.xJoystickGamingWheel 
                   + Other_adstock + NPS_Score, data = GamesDF)

summary(GamesModel_1)
vif(GamesModel_1)

#Remove high  vif
GamesModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit
                   + Radio + GMV_lag_1_per
                   + Online_Marketing  
                   + product_analytic_vertical.xMotionController + s1_fact.order_payment_type
                   + product_analytic_vertical.xJoystickGamingWheel 
                   + Other_adstock + NPS_Score, data = GamesDF)

summary(GamesModel_1)
vif(GamesModel_1)

#Remove high  vif
GamesModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit
                   + Radio + GMV_lag_1_per
                   + Online_Marketing  
                   + product_analytic_vertical.xMotionController + s1_fact.order_payment_type
                   + product_analytic_vertical.xJoystickGamingWheel 
                   + Other_adstock, data = GamesDF)

summary(GamesModel_1)
vif(GamesModel_1)


#Remove high  p
GamesModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit
                   + Radio + GMV_lag_1_per
                   + Online_Marketing  
                   + product_analytic_vertical.xMotionController + s1_fact.order_payment_type
                   + product_analytic_vertical.xJoystickGamingWheel 
                   , data = GamesDF)

summary(GamesModel_1)
vif(GamesModel_1)


#Remove high  p
GamesModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit
                   + Radio + Online_Marketing  
                   + product_analytic_vertical.xMotionController + s1_fact.order_payment_type
                   , data = GamesDF)

summary(GamesModel_1)
vif(GamesModel_1)

#Remove payment_type as it is not in control of company
GamesModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit
                   + Radio + Online_Marketing  
                   + product_analytic_vertical.xMotionController 
                   , data = GamesDF)

summary(GamesModel_1)
vif(GamesModel_1)

#Remove high p
GamesModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit
                   + Radio+ product_analytic_vertical.xMotionController 
                   , data = GamesDF)

summary(GamesModel_1)
vif(GamesModel_1)

#Remove high p
GamesModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit
                   + product_analytic_vertical.xMotionController 
                   , data = GamesDF)

summary(GamesModel_1)
vif(GamesModel_1)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                   8.38e-17   2.41e-02    0.00        1    
#units                                         6.86e-01   3.86e-02   17.76  < 2e-16 ***
#  product_analytic_vertical.xGamingAccessoryKit 1.77e-01   3.94e-02    4.51  4.1e-05 ***
#  product_analytic_vertical.xMotionController   2.35e-01   3.07e-02    7.65  6.6e-10 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.176 on 49 degrees of freedom
#Multiple R-squared:  0.971,	Adjusted R-squared:  0.969 


### Cross-validation
cv.lm(data = GamesDF, form.lm = GamesModel_1, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)


### Estimating the elasticity coefficients

elasticity_DL <- function(var){
  DL_elasticity <- as.numeric(GamesModel_1$coefficients[var]*mean(GamesDF[,var])/mean(GamesDF$gmv))
  return(DL_elasticity)
  
} 

DL_var_list <- list()

for(i in 2:length(GamesModel_1$coefficients)){
  DL_var_list[i-1] <- elasticity_DL(names(GamesModel_1$coefficients)[i])
  
}

DL_elasticity.outputs <- data.frame(names(GamesModel_1$coefficients[2:length(GamesModel_1$coefficients)]))
DL_elasticity.outputs <- cbind(DL_elasticity.outputs,do.call(rbind.data.frame, DL_var_list))
colnames(DL_elasticity.outputs) <- c("Variable","Elasticity")

DL_elasticity.outputs$Direction <- ifelse(DL_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(DL_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Games Accessory - Distributed Lag Model") +xlab("Variables")  


#################### Model 3: Distributed Lag -End ##########




################################ Model Type 4 ####################################
###########################   Koyck Model  #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# keeping Week# 1 'gmv' and removing other lag variables and Moving averages variables
str(GameAccFinal)
#data.frame':	50 obs. of  73 variables
colnames(GameAccFinal)
# 18, 19 are list_price and promotion_offered
# 59 to 73 are lag and MA variables
# Keeping GMV_lag_1_per"  (column# 71)

KYGamesDF <- GameAccFinal[,-c(18:19,65:70,72:79)]
colnames(KYGamesDF)

## Scaling the variables
KYGamesDF[,2:ncol(KYGamesDF)] <- scale(KYGamesDF[,2:ncol(KYGamesDF)])


### Stepwise Regression to remove insignificant and correlated variables
KMGames_base.mod <- lm(gmv ~ 1 , data= KYGamesDF)  # base intercept only model
KMGames_all.mod <- lm(gmv ~ . , data= KYGamesDF) # full model with all predictors
KMGames_stepMod <- step(KMGames_base.mod, scope = list(lower = KMGames_base.mod, upper = KMGames_all.mod), direction = "both", trace = 1, steps = 1000) 
KMGames_shortlistedVars <- names(unlist(KMGames_stepMod[[1]])) 
KMGames_shortlistedVars <- KMGames_shortlistedVars[!KMGames_shortlistedVars %in% "(Intercept)"]  

show(KMGames_shortlistedVars)
#[1] "units"                                          "product_analytic_vertical.xMotionController"   
#[3] "product_analytic_vertical.xGamingAccessoryKit"  "s1_fact.order_payment_type"                    
#[5] "Radio"                                          "product_analytic_vertical.xJoystickGamingWheel"
#[7] "product_analytic_vertical.xGamePad"             "event.xEid...Rath"                             
#[9] "LP_lag_1_per"                                   "Other_adstock"                                 
#[11] "Online_Marketing"                               "WeekDay.xMonday"                               
#[13] "event.xDiwali"                                  "event.xFHSD"                                   
#[15] "event.xRepublic.Day"                            "NPS_Score"                   

### Model Building::

## Model 1-
KYGModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + Radio
                 + product_analytic_vertical.xGamePad + LP_lag_1_per + Online_Marketing
                 + event.xDiwali + event.xRepublic.Day + product_analytic_vertical.xMotionController
                 + s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 + event.xEid...Rath + Other_adstock + WeekDay.xMonday + NPS_Score + event.xFHSD,
                 data = KYGamesDF)

summary(KYGModel_1)
vif(KYGModel_1)
##Multiple R-squared:  0.997,	Adjusted R-squared:  0.996 

## Remove special event days
KYGModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + Radio
                 + product_analytic_vertical.xGamePad + LP_lag_1_per + Online_Marketing
                 + product_analytic_vertical.xMotionController
                 + s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                + NPS_Score ,
                 data = KYGamesDF)

summary(KYGModel_1)
vif(KYGModel_1)

#remove high vif
KYGModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + Radio
                 + LP_lag_1_per + Online_Marketing
                 + product_analytic_vertical.xMotionController
                 + s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 ,data = KYGamesDF)

summary(KYGModel_1)
vif(KYGModel_1)

#remove high p
KYGModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + Radio
                  + Online_Marketing + product_analytic_vertical.xMotionController
                 + s1_fact.order_payment_type + product_analytic_vertical.xJoystickGamingWheel
                 ,data = KYGamesDF)

summary(KYGModel_1)
vif(KYGModel_1)

#remove high p
KYGModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + Radio
                 + Online_Marketing + product_analytic_vertical.xMotionController
                 + s1_fact.order_payment_type
                 ,data = KYGamesDF)

summary(KYGModel_1)
vif(KYGModel_1)

#remove payment_type as in not in control of company
KYGModel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + Radio
                + Online_Marketing + product_analytic_vertical.xMotionController
                ,data = KYGamesDF)

summary(KYGModel_1)
vif(KYGModel_1)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                    8.20e-17   2.31e-02    0.00    1.000    
#units                                          6.88e-01   3.87e-02   17.77  < 2e-16 ***
#  product_analytic_vertical.xGamingAccessoryKit  2.25e-01   4.27e-02    5.27  3.3e-06 ***
#  Radio                                          6.43e-02   2.67e-02    2.41    0.020 *  
#  Online_Marketing                              -6.07e-02   3.44e-02   -1.77    0.084 .  
#product_analytic_vertical.xMotionController    2.10e-01   3.10e-02    6.76  1.9e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.168 on 47 degrees of freedom
#Multiple R-squared:  0.974,	Adjusted R-squared:  0.972 
#F-statistic:  359 on 5 and 47 DF,  p-value: <2e-16

#units product_analytic_vertical.xGamingAccessoryKit 
#2.76                                          3.36 
#Radio                              Online_Marketing 
#1.31                                          2.18 
#product_analytic_vertical.xMotionController 
#1.77 



### Cross-validation
cv.lm(data = KYGamesDF, form.lm = KYGModel_1, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 

### Estimating the elasticity coefficients
elasticity_KY <- function(var){
  KY_elasticity <- as.numeric(KYGModel_1$coefficients[var]*mean(KYGamesDF[,var])/mean(KYGamesDF$gmv))
  return(KY_elasticity)
  
} 

KY_var_list <- list()

for(i in 2:length(KYGModel_1$coefficients)){
  KY_var_list[i-1] <- elasticity_KY(names(KYGModel_1$coefficients)[i])
}

KY_elasticity.outputs <- data.frame(names(KYGModel_1$coefficients[2:length(KYGModel_1$coefficients)]))
KY_elasticity.outputs <- cbind(KY_elasticity.outputs,do.call(rbind.data.frame, KY_var_list))
colnames(KY_elasticity.outputs) <- c("Variable","Elasticity")

KY_elasticity.outputs$Direction <- ifelse(KY_elasticity.outputs$Elasticity > 0, "Positive", "Negative")

# Plotting the elasticity
ggplot(KY_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Games Accessory - Koyck Model") +xlab("Variables")  

#######Model Type 4 Koyck Model Ends########################################################


################################ Model Type 5 ####################################
###########################   Multiplicative model + Distributed lag  Model  #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# keeping Week# 1,2,3 'gmv' and removing other lag variables and Moving averages variables
str(GameAccFinal)
#data.frame':	50 obs. of  79 variables
colnames(GameAccFinal)
# 18, 19 are list_price and promotion_offered
# 65 to 76 are lag and MA variables
# Keeping GMV_lag"  

MuDiGameDF <- GameAccFinal[,-c(18:19,65:76)]
colnames(MuDiGameDF)


## Replace 0 value  with '0.00001' as log(0) is undefined
MuDiGameDF[MuDiGameDF == 0] <- 0.00001

## Tranform  negative values
MuDiGameDF$GMV_lag_1_per <- 1 + MuDiGameDF$GMV_lag_1_per - min(MuDiGameDF$GMV_lag_1_per)
MuDiGameDF$GMV_lag_2_per <- 1 + MuDiGameDF$GMV_lag_2_per - min(MuDiGameDF$GMV_lag_2_per)
MuDiGameDF$GMV_lag_3_per <- 1 + MuDiGameDF$GMV_lag_3_per - min(MuDiGameDF$GMV_lag_3_per)

## Take log for Multiplicative model
MuDiGameDF <- log(MuDiGameDF)

## Checking the variables for linear relationship or multicollinearity
MuDiGAModel <- lm(gmv~.,MuDiGameDF)
alias(MuDiGAModel)
colnames(MuDiHomeAudioDF)

### Stepwise Regression to remove insignificant and correlated variables
MuDiGA_base.mod <- lm(gmv ~ 1 , data= MuDiGameDF)  # base intercept only model
MuDiGA_all.mod <- lm(gmv ~ . , data= MuDiGameDF) # full model with all predictors
MuDiGA_stepMod <- step(MuDiGA_base.mod, scope = list(lower = MuDiGA_base.mod, upper = MuDiGA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
MuDiGA_shortlistedVars <- names(unlist(MuDiGA_stepMod[[1]])) # get the shortlisted variable.
MuDiGA_shortlistedVars <- MuDiGA_shortlistedVars[!MuDiGA_shortlistedVars %in% "(Intercept)"]  # remove intercept

show(MuDiGA_shortlistedVars)
#[1] "units"                                          "s1_fact.order_payment_type"                    
#[3] "product_analytic_vertical.xJoystickGamingWheel" "product_procurement_sla"                       
#[5] "product_analytic_vertical.xTVOutCableAccessory" "product_analytic_vertical.xGamingMouse"        
#[7] "product_analytic_vertical.xGamingAccessoryKit"  "product_analytic_vertical.xGamePad"            
#[9] "GMV_lag_1_per"                                  "event.xEid...Rath"                             
#[11] "Radio_adstock"                                  "product_analytic_vertical.xMotionController"   
#[13] "eventFlag"                                      "event.xDiwali"                                 
#[15] "Week"                                           "product_analytic_vertical.xGameControlMount"   
#[17] "product_analytic_vertical.xGamingMemoryCard"    "WeekDay.xSaturday"                             
#[19] "product_analytic_vertical.xGamingSpeaker"       "event.xValentine.Day"           


### Model Building::
## Model 1:
MuDiGAmodel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xJoystickGamingWheel
                    + product_analytic_vertical.xTVOutCableAccessory + product_analytic_vertical.xGamingAccessoryKit
                    + GMV_lag_1_per + Radio_adstock + eventFlag + Week + product_analytic_vertical.xGamingMemoryCard
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamePad + event.xEid...Rath
                    + product_analytic_vertical.xMotionController + event.xDiwali 
                    + product_analytic_vertical.xGameControlMount + WeekDay.xSaturday + event.xValentine.Day, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)
#Multiple R-squared:     1,	Adjusted R-squared:     1 

# remove special event days
MuDiGAmodel_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xJoystickGamingWheel
                    + product_analytic_vertical.xTVOutCableAccessory + product_analytic_vertical.xGamingAccessoryKit
                    + GMV_lag_1_per + Radio_adstock + product_analytic_vertical.xGamingMemoryCard
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamePad +
                    + product_analytic_vertical.xMotionController  
                    + product_analytic_vertical.xGameControlMount, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high vif
MuDiGAmodel_1 <- lm(formula = gmv ~  product_analytic_vertical.xJoystickGamingWheel
                    + product_analytic_vertical.xTVOutCableAccessory + product_analytic_vertical.xGamingAccessoryKit
                    + GMV_lag_1_per + Radio_adstock + product_analytic_vertical.xGamingMemoryCard
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamePad +
                      + product_analytic_vertical.xMotionController  
                    + product_analytic_vertical.xGameControlMount, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high vif
MuDiGAmodel_1 <- lm(formula = gmv ~  product_analytic_vertical.xJoystickGamingWheel
                    + product_analytic_vertical.xTVOutCableAccessory + product_analytic_vertical.xGamingAccessoryKit
                    + GMV_lag_1_per + Radio_adstock + product_analytic_vertical.xGamingMemoryCard
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamePad
                    + product_analytic_vertical.xGameControlMount, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high vif
MuDiGAmodel_1 <- lm(formula = gmv ~  product_analytic_vertical.xJoystickGamingWheel
                    + product_analytic_vertical.xTVOutCableAccessory 
                    + GMV_lag_1_per + Radio_adstock + product_analytic_vertical.xGamingMemoryCard
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamePad
                    + product_analytic_vertical.xGameControlMount, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high vif
MuDiGAmodel_1 <- lm(formula = gmv ~  product_analytic_vertical.xJoystickGamingWheel
                    + product_analytic_vertical.xTVOutCableAccessory 
                    + GMV_lag_1_per + Radio_adstock + product_analytic_vertical.xGamingMemoryCard
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse 
                    + product_analytic_vertical.xGameControlMount, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high vif
MuDiGAmodel_1 <- lm(formula = gmv ~  product_analytic_vertical.xJoystickGamingWheel
                    + product_analytic_vertical.xTVOutCableAccessory 
                    + GMV_lag_1_per + Radio_adstock
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse 
                    + product_analytic_vertical.xGameControlMount, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high vif
MuDiGAmodel_1 <- lm(formula = gmv ~ 
                    product_analytic_vertical.xTVOutCableAccessory 
                    + GMV_lag_1_per + Radio_adstock
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse 
                    + product_analytic_vertical.xGameControlMount, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high vif
MuDiGAmodel_1 <- lm(formula = gmv ~ 
                    GMV_lag_1_per + Radio_adstock
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse 
                    + product_analytic_vertical.xGameControlMount, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high p
MuDiGAmodel_1 <- lm(formula = gmv ~ 
                      GMV_lag_1_per + Radio_adstock
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high p
MuDiGAmodel_1 <- lm(formula = gmv ~ 
                    Radio_adstock
                    + product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove high p
MuDiGAmodel_1 <- lm(formula = gmv ~ 
                     product_analytic_vertical.xGamingSpeaker + s1_fact.order_payment_type + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)

# remove payment_type as it is not in control of company
MuDiGAmodel_1 <- lm(formula = gmv ~ 
                      product_analytic_vertical.xGamingSpeaker + product_procurement_sla
                    + product_analytic_vertical.xGamingMouse, 
                    data = MuDiGameDF)

summary(MuDiGAmodel_1)
vif(MuDiGAmodel_1)


#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                9.9480     0.7279   13.67   <2e-16 ***
#  product_analytic_vertical.xGamingSpeaker   0.0508     0.0191    2.65   0.0107 *  
#  product_procurement_sla                    2.8035     0.8123    3.45   0.0012 ** 
#  product_analytic_vertical.xGamingMouse     0.3729     0.0267   13.94   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.707 on 49 degrees of freedom
#Multiple R-squared:  0.877,	Adjusted R-squared:  0.869 
#F-statistic:  116 on 3 and 49 DF,  p-value: <2e-16

#product_analytic_vertical.xGamingSpeaker                  product_procurement_sla   product_analytic_vertical.xGamingMouse 
#1.35                                     1.04                                     1.36 

#----------------------------------------------------------  
### Cross-validation
cv.lm(data = MuDiGameDF, form.lm = MuDiGAmodel_1, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 



### Estimating the elasticity coefficients

as.numeric(MuDiGAmodel_1$coefficients["SEM_adtock"]*mean(MuDiGameDF[,"SEM_adtock"])/mean(MuDiGameDF$gmv))

elasticity_DuMi <- function(var){
  MDHA_elasticity <- as.numeric(MuDiGAmodel_1$coefficients[var]*mean(MuDiGameDF[,var])/mean(MuDiGameDF$gmv))
  return(MDHA_elasticity)
  
} 

MDHA_var_list <- list()

for(i in 2:length(MuDiGAmodel_1$coefficients)){
  MDHA_var_list[i-1] <- elasticity_DuMi(names(MuDiGAmodel_1$coefficients)[i])
  
}

MDHA_elasticity.outputs <- data.frame(names(MuDiGAmodel_1$coefficients[2:length(MuDiGAmodel_1$coefficients)]))
MDHA_elasticity.outputs <- cbind(MDHA_elasticity.outputs,do.call(rbind.data.frame, MDHA_var_list))
colnames(MDHA_elasticity.outputs) <- c("Variable","Elasticity")

MDHA_elasticity.outputs$Direction <- ifelse(MDHA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(MDHA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Game Accessory - Multiplicative and Distributed Lag Model") +xlab("Variables")  



