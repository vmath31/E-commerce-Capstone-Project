######################################################################################
##      Capstone Ecommerce- R File - Sub Category: HomeAudio  Models 
######################################################################################

## Section 4: Model Building

####################### Model Type 1 ###########
##################### Basic Linear Model #######


# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# Remove lag variables and Moving averages variables
str(HomeAudioFinal)
#data.frame':	50 obs. of  73 variables
colnames(HomeAudioFinal)
# 18, 19 are list_price and promotion_offered
# 59 to 73 are lag and MA variables

LinearHomeAudioDF <- HomeAudioFinal[,-c(18:19,59:73)]
colnames(LinearHomeAudioDF)

## Scaling the variables
LinearHomeAudioDF[,2:ncol(LinearHomeAudioDF)] <- scale(LinearHomeAudioDF[,2:ncol(LinearHomeAudioDF)])

#write.csv(LinearHomeAudioDF,'LinearHomeAudioDF.csv')

### Step to remove insignificant and correlated variables

# Base intercept model
HABase<- lm(gmv ~ 1 , data= LinearHomeAudioDF)
# Complete model all variables
HAAll<- lm(gmv ~ . , data= LinearHomeAudioDF) 
summary(HAAll)

HAStep <- step(HABase, scope = list(lower = HABase, upper = HAAll), direction = "both", trace = 1, steps = 1000)

SelectedVars <- names(unlist(HAStep[[1]])) 
# Delete Intercepts
SelectedVars <- SelectedVars[!SelectedVars %in% "(Intercept)"]  # remove intercept

show(SelectedVars)
#[1] "units"                                       "product_mrp"                                 "event.xDaussera"                            
#[4] "Affiliates"                                  "product_analytic_vertical.xDockingStation"   "Digital"                                    
#[7] "product_analytic_vertical.xVoiceRecorder"    "WeekDay.xTuesday"                            "Week"                                       
#[10] "product_analytic_vertical.xDJController"     "product_analytic_vertical.xKaraokePlayer"    "WeekDay.xSunday"                            
#[13] "eventFlag"                                   "event.xValentine.Day"                        "Sponsorship_adstock"                        
#[16] "SEM_adtock"                                  "event.xRepublic.Day"                         "product_analytic_vertical.xFMRadio"         
#[19] "event.xEid...Rath"                           "product_analytic_vertical.xHiFiSystem"       "product_analytic_vertical.xHomeAudioSpeaker"
#[22] "Affiliates_adstock"                          "s1_fact.order_payment_type" 

### Model Building::

## 1st model based on selected variables
HAModel_L1 <- lm(formula = gmv ~ units + product_mrp + event.xDaussera + 
                      Affiliates + product_analytic_vertical.xDockingStation + Digital +
                      product_analytic_vertical.xVoiceRecorder + WeekDay.xTuesday + Week +
                      product_analytic_vertical.xDJController + product_analytic_vertical.xKaraokePlayer + WeekDay.xSunday +                            
                      eventFlag + event.xValentine.Day + Sponsorship_adstock +                       
                      SEM_adtock + event.xRepublic.Day + product_analytic_vertical.xFMRadio +         
                      event.xEid...Rath + product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                      Affiliates_adstock + s1_fact.order_payment_type, data = LinearHomeAudioDF)


summary(HAModel_L1)
#Residual standard error: 0.0276 on 26 degrees of freedom
#Multiple R-squared:     1,	Adjusted R-squared:  0.999 

vif(HAModel_L1)
# product_analytic_vertical.xHomeAudioSpeaker             3294.80
# units   4744.70

## High VIF and Insignificant p-value columns: product_analytic_vertical.xHomeAudioSpeaker and units
## Insignificant p-value columns: Week, eventFlag, event.xEid...Rath , s1_fact.order_payment_type

#2nd model after removing highly correlated variables
HAModel_L2 <- lm(formula = gmv ~ product_mrp + event.xDaussera + 
                   Affiliates + product_analytic_vertical.xDockingStation + Digital +
                   product_analytic_vertical.xVoiceRecorder + WeekDay.xTuesday + Week +
                   product_analytic_vertical.xDJController + product_analytic_vertical.xKaraokePlayer + WeekDay.xSunday +                            
                   eventFlag + event.xValentine.Day + Sponsorship_adstock +                       
                   SEM_adtock + event.xRepublic.Day + product_analytic_vertical.xFMRadio +         
                   event.xEid...Rath + product_analytic_vertical.xHiFiSystem + 
                   Affiliates_adstock + s1_fact.order_payment_type, data = LinearHomeAudioDF)


summary(HAModel_L2)

# Residual standard error: 0.0649 on 28 degrees of freedom
#Multiple R-squared:  0.998,	Adjusted R-squared:  0.996 

vif(HAModel_L2)
# event.xDaussera - 10.11 , product_analytic_vertical.xFMRadio- 11.63


## Remove: high vif

#3rd model after removing insignificant p-value variables
HAModel_L3 <- lm(formula = gmv ~ product_mrp  + 
                   Affiliates + product_analytic_vertical.xDockingStation + Digital +
                   product_analytic_vertical.xVoiceRecorder + WeekDay.xTuesday + Week +
                   product_analytic_vertical.xDJController + product_analytic_vertical.xKaraokePlayer + WeekDay.xSunday +                            
                   eventFlag + event.xValentine.Day + Sponsorship_adstock +                       
                   SEM_adtock + event.xRepublic.Day  +         
                   event.xEid...Rath + product_analytic_vertical.xHiFiSystem + 
                   Affiliates_adstock + s1_fact.order_payment_type, data = LinearHomeAudioDF)

summary(HAModel_L3)
#Residual standard error: 0.0744 on 30 degrees of freedom
# Multiple R-squared:  0.997,	Adjusted R-squared:  0.994 

vif(HAModel_L3)
# all below 10 except for product mrp
# High p-value: week, event.xValentine.Day, event.xRepublic.Day, event.xEid...Rath


#4th model after removing insignificant p-value variables. FM Radio has high value of VIF
HAModel_L4 <- lm(formula = gmv ~ product_mrp  + 
                   Affiliates + product_analytic_vertical.xDockingStation + Digital +
                   product_analytic_vertical.xVoiceRecorder + WeekDay.xTuesday  +
                   product_analytic_vertical.xDJController + product_analytic_vertical.xKaraokePlayer + WeekDay.xSunday +                            
                   eventFlag + event.xValentine.Day + Sponsorship_adstock +                       
                   SEM_adtock + product_analytic_vertical.xHiFiSystem + 
                   Affiliates_adstock + s1_fact.order_payment_type, data = LinearHomeAudioDF)

summary(HAModel_L4)
vif(HAModel_L4)
#Residual standard error: 0.0711 on 33 degrees of freedom
#Multiple R-squared:  0.997,	Adjusted R-squared:  0.995 

#  high-p:    event.xValentine.Day, s1_fact.order_payment_type, 
#  remove eventFlag                     


#5th model after removing insignificant p-value variables
HAModel_L5 <- lm(formula = gmv ~ product_mrp  + 
                   Affiliates + product_analytic_vertical.xDockingStation + Digital +
                   product_analytic_vertical.xVoiceRecorder + WeekDay.xTuesday  +
                   product_analytic_vertical.xDJController + product_analytic_vertical.xKaraokePlayer + WeekDay.xSunday +                            
                   + Sponsorship_adstock +SEM_adtock + product_analytic_vertical.xHiFiSystem + 
                   Affiliates_adstock, data = LinearHomeAudioDF)

summary(HAModel_L5)
vif(HAModel_L5)
#Residual standard error: 0.0753 on 36 degrees of freedom
#Multiple R-squared:  0.996,	Adjusted R-squared:  0.994 

# Remove additional days WeekDay.xTuesday, WeekDay.xSunday

##6th model after removing DEM_adtock
HAModel_L6 <- lm(formula = gmv ~ product_mrp  + 
                   Affiliates + product_analytic_vertical.xDockingStation + Digital +
                   product_analytic_vertical.xVoiceRecorder  +
                   product_analytic_vertical.xDJController + product_analytic_vertical.xKaraokePlayer  +                            
                   + Sponsorship_adstock +SEM_adtock + product_analytic_vertical.xHiFiSystem + 
                   Affiliates_adstock, data = LinearHomeAudioDF)

summary(HAModel_L6)
vif(HAModel_L6)
#Residual standard error: 0.077 on 38 degrees of freedom
#Multiple R-squared:  0.995,	Adjusted R-squared:  0.994 

## Remove products
##7th model after removing DEM_adtock
HAModel_L7 <- lm(formula = gmv ~ product_mrp  +Affiliates +Digital +Sponsorship_adstock +SEM_adtock +
                   Affiliates_adstock, data = LinearHomeAudioDF)

summary(HAModel_L7)
vif(HAModel_L7)
#Residual standard error: 0.11 on 43 degrees of freedom
#Multiple R-squared:  0.989,	Adjusted R-squared:  0.988 

# high p- Sponsorship_adstock
##8th model after removing DEM_adtock
HAModel_L8 <- lm(formula = gmv ~ product_mrp  +Affiliates +Digital + SEM_adtock+
                   Affiliates_adstock, data = LinearHomeAudioDF)

summary(HAModel_L8)
vif(HAModel_L8)

# high p- SEM_adstock
##9th model after removing DEM_adtock
HAModel_L9 <- lm(formula = gmv ~ product_mrp  +Affiliates +Digital +
                   Affiliates_adstock, data = LinearHomeAudioDF)

summary(HAModel_L9)
vif(HAModel_L9)

# high p- Affilates_adstock
##10th model after removing DEM_adtock
HAModel_L10 <- lm(formula = gmv ~ product_mrp  +Affiliates +Digital, data = LinearHomeAudioDF)

summary(HAModel_L10)
vif(HAModel_L10)
#Residual standard error: 0.111 on 46 degrees of freedom
#Multiple R-squared:  0.988,	Adjusted R-squared:  0.988 



# high p- Affilates
##11th model after removing DEM_adtock
HAModel_L11 <- lm(formula = gmv ~ product_mrp  +Digital, data = LinearHomeAudioDF)

summary(HAModel_L11)
vif(HAModel_L11)

#product_mrp  9.75e-01   1.75e-02   55.90   <2e-16 ***
#  Digital      4.35e-02   1.75e-02    2.49    0.016 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.112 on 47 degrees of freedom
#Multiple R-squared:  0.988,	Adjusted R-squared:  0.988 

#Final linear model: HAModel_L11



### Cross-validation - Model 4
cv.lm(data = LinearHomeAudioDF, form.lm = HAModel_L11, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)


### Estimating the elasticity coefficients - Model 4

elasticity_L <- function(var){
  L_elasticity <- as.numeric(HAModel_L11$coefficients[var]*mean(LinearHomeAudioDF[,var])/mean(LinearHomeAudioDF$gmv))
  return(L_elasticity)
  
} 

L_var_list <- list()

for(i in 2:length(HAModel_L11$coefficients)){
  L_var_list[i-1] <- elasticity_L(names(HAModel_L11$coefficients)[i])
  
}

L_elasticity.outputs <- data.frame(names(HAModel_L11$coefficients[2:length(HAModel_L11$coefficients)]))
L_elasticity.outputs <- cbind(L_elasticity.outputs,do.call(rbind.data.frame, L_var_list))
colnames(L_elasticity.outputs) <- c("Variable","Elasticity")

L_elasticity.outputs$Direction <- ifelse(L_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(L_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Basic Linear Model") +xlab("Variables")  



################################ Model Type 2 ####################################
###########################   Multiplicative Model  #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# Remove lag variables and Moving averages variables
str(HomeAudioFinal)
#data.frame':	50 obs. of  73 variables
colnames(HomeAudioFinal)
# 18, 19 are list_price and promotion_offered
# 59 to 73 are lag and MA variables

MultiHomeAudioDF <- HomeAudioFinal[,-c(18:19,59:73)]
colnames(MultiHomeAudioDF)

## To avoid log (0) issue, replace 0 with '0.00001' in df
MultiHomeAudioDF[MultiHomeAudioDF == 0] <- 0.00001

##  Convert values to log for Multiplicative model
MultiHomeAudioDF <- log(MultiHomeAudioDF)

## Linear relationship or multicollinearity
MultiHomeModel <- lm(gmv~.,MultiHomeAudioDF)
alias(MultiHomeModel)

#event.xNon.event.Day           -36705109/22836          -2162028/252955           -473393/101966          -2067060/418097
#event.xPacman                    24655573/4677            1423819/30649             -325322/1135            5399141/40547
#event.xRepublic.Day           -15872257/264657       -16094352/10066973            3493033/18166         -29221281/386681
#event.xValentine.Day    -132261165864/26747993        -95987000/2681099           9674907/133942            -372219/12658
#price_mark.xPREMIUM         2402213371/1214462             171917/11770     -3585042896/71438489         81054293/1604902
#price_mark.xPROMISING       

colnames(MultiHomeAudioDF)
## Remove linear or multicollinear relationship variables
MultiHomeAudioDF <- MultiHomeAudioDF[, -c(51:56)]

### Use Stepwise Regression to remove insignificant and correlated variables
MultiHA_base.mod <- lm(gmv ~ 1 , data= MultiHomeAudioDF)  
MultiHA_all.mod <- lm(gmv ~ . , data= MultiHomeAudioDF) 
MultiHA_stepMod <- step(MultiHA_base.mod, scope = list(lower = MultiHA_base.mod, upper = MultiHA_all.mod), direction = "both", trace = 1, steps = 1000) 
MultiHA_shortlistedVars <- names(unlist(MultiHA_stepMod[[1]])) 
MultiHA_shortlistedVars <- MultiHA_shortlistedVars[!MultiHA_shortlistedVars %in% "(Intercept)"]  

show(MultiHA_shortlistedVars)
#[1] "product_mrp"                                 "Content_Marketing"                           "Radio_adstock"                              
#[4] "Other_adstock"                               "product_analytic_vertical.xDockingStation"   "product_procurement_sla"                    
#[7] "product_analytic_vertical.xFMRadio"          "event.xDaussera"                             "Week"                                       
#[10] "product_analytic_vertical.xHomeAudioSpeaker" "Total_Investment"                            "sla"                                        
#[13] "SEM"                                         "product_analytic_vertical.xDock"             "eventFlag"                                  
#[16] "Digital_adstock"                             "units"                                       "Digital"                                    
#[19] "event.xChristmas...NY"                       "WeekDay.xSunday"                             "event.xBSD"                                 
#[22] "WeekDay.xTuesday"                            "product_analytic_vertical.xDJController"     "event.xEid...Rath"                          
#[25] "product_analytic_vertical.xHiFiSystem"       "TV"                                          "Affiliates_adstock"                         
#[28] "Online_Marketing_adstock"                    "Radio"                                       "event.xFHSD"                                
#[31] "WeekDay.xThursday"                           "Content_Marketing_adstock"                   "event.xDiwali"                              
#[34] "product_analytic_vertical.xSoundMixer"       "Affiliates"  


## First Model
MultiHAModel_1 <- lm(formula = gmv ~ units + product_mrp + Content_Marketing + Radio_adstock +
                       product_analytic_vertical.xDockingStation + product_procurement_sla + product_analytic_vertical.xFMRadio +
                       event.xDaussera + Week + product_analytic_vertical.xHomeAudioSpeaker + Total_Investment+ 
                       sla + SEM + product_analytic_vertical.xDock + eventFlag +                       
                       Digital_adstock +Digital + event.xChristmas...NY + WeekDay.xSunday +                   
                       event.xBSD +  WeekDay.xTuesday + product_analytic_vertical.xDJController +   
                       event.xEid...Rath + product_analytic_vertical.xHiFiSystem + TV + Affiliates_adstock +
                       Online_Marketing_adstock + Radio + event.xFHSD + WeekDay.xThursday + Content_Marketing_adstock + 
                       event.xDiwali + product_analytic_vertical.xSoundMixer + Affiliates, data = MultiHomeAudioDF)


summary(MultiHAModel_1)
vif(MultiHAModel_1)

# Remove special days and Total investment
MultiHAModel_2 <- lm(formula = gmv ~ units + product_mrp + Content_Marketing + Radio_adstock +
                       product_analytic_vertical.xDockingStation + product_procurement_sla + product_analytic_vertical.xFMRadio +
                       product_analytic_vertical.xHomeAudioSpeaker +sla + SEM + product_analytic_vertical.xDock  +                       
                       Digital_adstock +Digital + product_analytic_vertical.xDJController + product_analytic_vertical.xHiFiSystem + TV + Affiliates_adstock +
                       Online_Marketing_adstock + Radio +Content_Marketing_adstock + product_analytic_vertical.xSoundMixer + Affiliates,
                     data = MultiHomeAudioDF)

summary(MultiHAModel_2)
vif(MultiHAModel_2)

##High vif- units

## 3rd Model
MultiHAModel_3 <- lm(formula = gmv ~ product_mrp + Content_Marketing + Radio_adstock +
                     product_analytic_vertical.xDockingStation + product_procurement_sla + product_analytic_vertical.xFMRadio +
                     product_analytic_vertical.xHomeAudioSpeaker +sla + SEM + product_analytic_vertical.xDock  +                       
                     Digital_adstock +Digital + product_analytic_vertical.xDJController + product_analytic_vertical.xHiFiSystem + TV + Affiliates_adstock +
                     Online_Marketing_adstock + Radio +Content_Marketing_adstock + product_analytic_vertical.xSoundMixer + Affiliates,
                   data = MultiHomeAudioDF)


summary(MultiHAModel_3)
vif(MultiHAModel_3)
# high vif->Online_Marketing_adstock

## 4th Model
MultiHAModel_4 <- lm(formula = gmv ~ product_mrp + Content_Marketing + Radio_adstock +
                       product_analytic_vertical.xDockingStation + product_procurement_sla + product_analytic_vertical.xFMRadio +
                       product_analytic_vertical.xHomeAudioSpeaker +sla + SEM + product_analytic_vertical.xDock  +                       
                       Digital_adstock +Digital + product_analytic_vertical.xDJController + product_analytic_vertical.xHiFiSystem + TV + Affiliates_adstock +
                       Radio +Content_Marketing_adstock + product_analytic_vertical.xSoundMixer + Affiliates,
                     data = MultiHomeAudioDF)


summary(MultiHAModel_4)
vif(MultiHAModel_4)
# high vif -> product_mrp

## 5th Model
MultiHAModel_5 <- lm(formula = gmv ~ Content_Marketing + Radio_adstock +
                       product_analytic_vertical.xDockingStation + product_procurement_sla + product_analytic_vertical.xFMRadio +
                       product_analytic_vertical.xHomeAudioSpeaker +sla + SEM + product_analytic_vertical.xDock  +                       
                       Digital_adstock +Digital + product_analytic_vertical.xDJController + product_analytic_vertical.xHiFiSystem + TV + Affiliates_adstock +
                       Radio +Content_Marketing_adstock + product_analytic_vertical.xSoundMixer + Affiliates,
                     data = MultiHomeAudioDF)


summary(MultiHAModel_5)
vif(MultiHAModel_5)
#High vif- Affiliates

MultiHAModel_6 <- lm(formula = gmv ~ Content_Marketing + Radio_adstock +
                       product_analytic_vertical.xDockingStation + product_procurement_sla + product_analytic_vertical.xFMRadio +
                       product_analytic_vertical.xHomeAudioSpeaker +sla + SEM + product_analytic_vertical.xDock  +                       
                       Digital_adstock +Digital + product_analytic_vertical.xDJController + product_analytic_vertical.xHiFiSystem + TV + Affiliates_adstock +
                       Radio +Content_Marketing_adstock + product_analytic_vertical.xSoundMixer,
                     data = MultiHomeAudioDF)


summary(MultiHAModel_6)
vif(MultiHAModel_6)

#high vif -product_analytic_vertical.xHiFiSystem
MultiHAModel_7 <- lm(formula = gmv ~ Content_Marketing + Radio_adstock +
                       product_analytic_vertical.xDockingStation + product_procurement_sla + product_analytic_vertical.xFMRadio +
                       product_analytic_vertical.xHomeAudioSpeaker +sla + SEM + product_analytic_vertical.xDock  +                       
                       Digital_adstock +Digital + product_analytic_vertical.xDJController + TV + Affiliates_adstock +
                       Radio +Content_Marketing_adstock + product_analytic_vertical.xSoundMixer,
                     data = MultiHomeAudioDF)


summary(MultiHAModel_7)
vif(MultiHAModel_7)
#Residual standard error: 0.0384 on 32 degrees of freedom
#Multiple R-squared:  0.999,	Adjusted R-squared:  0.999
#high vif:product_analytic_vertical.xFMRadio

MultiHAModel_8 <- lm(formula = gmv ~ Content_Marketing + Radio_adstock +
                       product_analytic_vertical.xDockingStation + product_procurement_sla  +
                       product_analytic_vertical.xHomeAudioSpeaker +sla + SEM + product_analytic_vertical.xDock  +                       
                       Digital_adstock +Digital + product_analytic_vertical.xDJController + TV + Affiliates_adstock +
                       Radio +Content_Marketing_adstock + product_analytic_vertical.xSoundMixer,
                     data = MultiHomeAudioDF)


summary(MultiHAModel_8)
vif(MultiHAModel_8)
#Residual standard error: 0.0639 on 33 degrees of freedom
#Multiple R-squared:  0.998,	Adjusted R-squared:  0.997 

#high vif: product_analytic_vertical.xDock
MultiHAModel_9 <- lm(formula = gmv ~ Content_Marketing + Radio_adstock +
                       product_analytic_vertical.xDockingStation + product_procurement_sla  +
                       product_analytic_vertical.xHomeAudioSpeaker +sla + SEM   +                       
                       Digital_adstock +Digital + product_analytic_vertical.xDJController + TV + Affiliates_adstock +
                       Radio +Content_Marketing_adstock + product_analytic_vertical.xSoundMixer,
                     data = MultiHomeAudioDF)


summary(MultiHAModel_9)
vif(MultiHAModel_9)
#High vif: Content_Marketing
MultiHAModel_10 <- lm(formula = gmv ~ Radio_adstock +
                        product_analytic_vertical.xDockingStation + product_procurement_sla  +
                        product_analytic_vertical.xHomeAudioSpeaker +sla + SEM   +                       
                        Digital_adstock +Digital + product_analytic_vertical.xDJController + TV + Affiliates_adstock +
                        Radio +Content_Marketing_adstock + product_analytic_vertical.xSoundMixer,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_10)
vif(MultiHAModel_10)
#Residual standard error: 0.0687 on 35 degrees of freedom
#Multiple R-squared:  0.997,	Adjusted R-squared:  0.996 

#high p: product_analytic_vertical.xDJController
MultiHAModel_11 <- lm(formula = gmv ~ Content_Marketing + Radio_adstock +
                        product_analytic_vertical.xDockingStation  + product_analytic_vertical.xFMRadio +
                        product_analytic_vertical.xHomeAudioSpeaker +sla + SEM  +                       
                        Digital + TV + Affiliates_adstock +
                        Radio + product_analytic_vertical.xSoundMixer,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_11)
vif(MultiHAModel_11)
# high vif : product_analytic_vertical.xHomeAudioSpeaker
MultiHAModel_12 <- lm(formula = gmv ~ Content_Marketing + Radio_adstock +
                        product_analytic_vertical.xDockingStation  + product_analytic_vertical.xFMRadio +
                        sla + SEM  +                       
                        Digital + TV + Affiliates_adstock +
                        Radio + product_analytic_vertical.xSoundMixer,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_12)
vif(MultiHAModel_12)
#RResidual standard error: 0.301 on 38 degrees of freedom
#Multiple R-squared:  0.945,	Adjusted R-squared:  0.929

#High p: product_analytic_vertical.xDockingStation
MultiHAModel_13 <- lm(formula = gmv ~ Content_Marketing + Radio_adstock  + product_analytic_vertical.xFMRadio +
                        sla + SEM  + Digital + TV + Affiliates_adstock +
                        Radio + product_analytic_vertical.xSoundMixer,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_13)
vif(MultiHAModel_13)

#High p: SEM
MultiHAModel_14 <- lm(formula = gmv ~ Content_Marketing + Radio_adstock  + product_analytic_vertical.xFMRadio +
                        sla   + Digital + TV + Affiliates_adstock +
                        Radio + product_analytic_vertical.xSoundMixer,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_14)
vif(MultiHAModel_14)

#High p: Radio_adstock
MultiHAModel_15 <- lm(formula = gmv ~ Content_Marketing  + product_analytic_vertical.xFMRadio +
                        sla   + Digital + TV + Affiliates_adstock +
                        Radio + product_analytic_vertical.xSoundMixer,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_15)
vif(MultiHAModel_15)


#High p: Affiliate_adstock
MultiHAModel_16 <- lm(formula = gmv ~ Content_Marketing  + product_analytic_vertical.xFMRadio +
                        sla   + Digital + TV +Radio + product_analytic_vertical.xSoundMixer,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_16)
vif(MultiHAModel_16)

#High p: TV, content marketng
MultiHAModel_17 <- lm(formula = gmv ~  product_analytic_vertical.xFMRadio +
                        sla   + Digital + Radio + product_analytic_vertical.xSoundMixer,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_17)
vif(MultiHAModel_17)

#High p: sla
MultiHAModel_18 <- lm(formula = gmv ~  product_analytic_vertical.xFMRadio +
                        Digital + Radio + product_analytic_vertical.xSoundMixer,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_18)
vif(MultiHAModel_18)

#High p: product_analytic_vertical.xSoundMixer
MultiHAModel_19 <- lm(formula = gmv ~  product_analytic_vertical.xFMRadio +
                        Digital + Radio,
                      data = MultiHomeAudioDF)


summary(MultiHAModel_19)
vif(MultiHAModel_19)


#High p: Radio
MultiHAModel_20 <- lm(formula = gmv ~  product_analytic_vertical.xFMRadio +
                        Digital,
                      data = MultiHomeAudioDF)

summary(MultiHAModel_20)
vif(MultiHAModel_20)

#Residual standard error: 0.295 on 47 degrees of freedom
#Multiple R-squared:  0.935,	Adjusted R-squared:  0.932 


### Cross-validation  - Model 20
cv.lm(data = MultiHomeAudioDF, form.lm = MultiHAModel_19, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating the elasticity coefficients- Model 19

elasticity_MultiHA <- function(var){
  MultiHA_elasticity <- as.numeric(MultiHAModel_19$coefficients[var]*mean(MultiHomeAudioDF[,var])/mean(MultiHomeAudioDF$gmv))
  return(MultiHA_elasticity)
  
} 

MultiHA_var_list <- list()

for(i in 2:length(MultiHAModel_19$coefficients)){
  MultiHA_var_list[i-1] <- elasticity_MultiHA(names(MultiHAModel_19$coefficients)[i])
  
}

MultiHA_elasticity.outputs <- data.frame(names(MultiHAModel_19$coefficients[2:length(MultiHAModel_19$coefficients)]))
MultiHA_elasticity.outputs <- cbind(MultiHA_elasticity.outputs,do.call(rbind.data.frame, MultiHA_var_list))
colnames(MultiHA_elasticity.outputs) <- c("Variable","Elasticity")

MultiHA_elasticity.outputs$Direction <- ifelse(MultiHA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(MultiHA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust=0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Multiplicative Model") +xlab("Variables")  


#######Model Type 2 Ends########################################################


################################ Model Type 3 ##################################
########################### Distributied Lag Model #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# keeping Week# 1, 2 and 3  lag value of 'gmv' and removing other lag variables and Moving averages variables
str(HomeAudioFinal)
#data.frame':	50 obs. of  73 variables
colnames(HomeAudioFinal)
# 18, 19 are list_price and promotion_offered
# 59 to 73 are lag and MA variables
# Keeping GMV_lag_1_per", "GMV_lag_2_per" ,"GMV_lag_3_per"  (column# 71,72,73)

DLHomeAudioDF <- HomeAudioFinal[,-c(18:19,59:70)]
colnames(DLHomeAudioDF)

## Scaling the variables
DLHomeAudioDF[,2:ncol(DLHomeAudioDF)] <- scale(DLHomeAudioDF[,2:ncol(DLHomeAudioDF)])

### Step to remove insignificant and correlated variables

# Base intercept model
DLHABase<- lm(gmv ~ 1 , data= DLHomeAudioDF)
# Complete model all variables
DLHAAll<- lm(gmv ~ . , data= DLHomeAudioDF) 
summary(DLHAAll)

DLHAStep <- step(DLHABase, scope = list(lower = DLHABase, upper = DLHAAll), direction = "both", trace = 1, steps = 1000)
DLSelectedVars <- names(unlist(DLHAStep[[1]])) 

# Remove Intercepts
DLSelectedVars <- DLSelectedVars[!DLSelectedVars %in% "(Intercept)"]  


show(DLSelectedVars)
#[1] "units"                                     "product_mrp"                               "event.xDaussera"                          
#[4] "Affiliates"                                "Online_Marketing_adstock"                  "product_analytic_vertical.xDockingStation"
#[7] "Digital"                                   "product_analytic_vertical.xVoiceRecorder"  "WeekDay.xTuesday"                         
#[10] "Week"                                      "product_analytic_vertical.xDJController"   "GMV_lag_1_per"                            
#[13] "product_analytic_vertical.xKaraokePlayer"  "Sponsorship"                               "GMV_lag_2_per"                            
#[16] "WeekDay.xThursday"                         "product_analytic_vertical.xHiFiSystem"     "event.xEid...Rath"                        
#[19] "GMV_lag_3_per"                             "event.xValentine.Day"                      "event.xFHSD"                              
#[22] "product_analytic_vertical.xDock"           "NPS_Score"                                 "product_analytic_vertical.xSoundMixer"    
#[25] "sla" 

### Build model

## Iteration 1:
DLHAModel_1 <- lm(formula = gmv ~ units + product_mrp + event.xDaussera + 
                   Affiliates + Online_Marketing_adstock + product_analytic_vertical.xDockingStation+ 
                   Digital + product_analytic_vertical.xVoiceRecorder + WeekDay.xTuesday +
                   Week + product_analytic_vertical.xDJController + GMV_lag_1_per + 
                   product_analytic_vertical.xKaraokePlayer + Sponsorship + GMV_lag_2_per +                            
                   WeekDay.xThursday + product_analytic_vertical.xHiFiSystem + event.xEid...Rath + 
                   GMV_lag_3_per + event.xValentine.Day + event.xFHSD + 
                   product_analytic_vertical.xDock + NPS_Score + product_analytic_vertical.xSoundMixer +              
                   sla, data = DLHomeAudioDF)


summary(DLHAModel_1)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                               -0.05413    0.01873   -2.89  0.00806 ** 
#  units                                      0.69729    0.05388   12.94  2.6e-12 ***
#  product_mrp                                0.26266    0.05094    5.16  2.8e-05 ***
#  event.xDaussera                            0.06798    0.01315    5.17  2.7e-05 ***
#  Affiliates                                -0.04264    0.00991   -4.30  0.00024 ***
#  Online_Marketing_adstock                   0.03844    0.00973    3.95  0.00060 ***
#  product_analytic_vertical.xDockingStation  0.05330    0.00983    5.42  1.4e-05 ***
#  Digital                                    0.08335    0.01167    7.14  2.2e-07 ***
#  product_analytic_vertical.xVoiceRecorder  -0.05112    0.01031   -4.96  4.6e-05 ***
#  WeekDay.xTuesday                           0.04113    0.00884    4.65  0.00010 ***
#  Week                                       0.00192    0.00065    2.96  0.00683 ** 
#  product_analytic_vertical.xDJController    0.02794    0.00717    3.90  0.00068 ***
#  GMV_lag_1_per                              0.03136    0.00636    4.93  5.0e-05 ***
#  product_analytic_vertical.xKaraokePlayer  -0.02699    0.00825   -3.27  0.00323 ** 
#  Sponsorship                               -0.05317    0.01080   -4.92  5.1e-05 ***
#  GMV_lag_2_per                              0.02001    0.00555    3.60  0.00142 ** 
#  WeekDay.xThursday                         -0.02240    0.00966   -2.32  0.02919 *  
#  product_analytic_vertical.xHiFiSystem     -0.03055    0.01131   -2.70  0.01247 *  
#  event.xEid...Rath                          0.00874    0.00494    1.77  0.08955 .  -> remove
#  GMV_lag_3_per                              0.01032    0.00515    2.00  0.05651 .  
#  event.xValentine.Day                      -0.02800    0.00770   -3.64  0.00131 ** 
#  event.xFHSD                                0.01712    0.00683    2.51  0.01941 *  
#  product_analytic_vertical.xDock            0.03055    0.01295    2.36  0.02676 *  
#  NPS_Score                                  0.01937    0.01115    1.74  0.09507 .  -> remove
#  product_analytic_vertical.xSoundMixer      0.00890    0.00757    1.18  0.25079    -> remove
#  sla                                       -0.00822    0.00736   -1.12  0.27473    -> remove

#Residual standard error: 0.0287 on 24 degrees of freedom
#Multiple R-squared:     1,	Adjusted R-squared:  0.999 

vif(DLHAModel_1)
#units                               product_mrp                           event.xDaussera 
#172.72                                    154.36                                     10.29 
#Affiliates                  Online_Marketing_adstock product_analytic_vertical.xDockingStation 
#5.84                                      5.63                                      5.75 
#Digital  product_analytic_vertical.xVoiceRecorder                          WeekDay.xTuesday 
#8.10                                      6.33                                      4.65 
#Week   product_analytic_vertical.xDJController                             GMV_lag_1_per 
#5.77                                      3.06                                      2.41 
#product_analytic_vertical.xKaraokePlayer  Sponsorship                             GMV_lag_2_per 
#4.05                                      6.94                                      1.83 
#WeekDay.xThursday     product_analytic_vertical.xHiFiSystem                         event.xEid...Rath 
#5.55                                      7.61                                      1.45 
#GMV_lag_3_per                      event.xValentine.Day                               event.xFHSD 
#1.58                                      3.53                                      2.78 
#product_analytic_vertical.xDock          NPS_Score     product_analytic_vertical.xSoundMixer 
#9.98                                      7.39                                      3.41 
#sla 
#3.22 

## high vif : units
## Iteration 2:
DLHAModel_2 <- lm(formula = gmv ~ product_mrp + event.xDaussera + 
                    Affiliates + Online_Marketing_adstock + product_analytic_vertical.xDockingStation+ 
                    Digital + product_analytic_vertical.xVoiceRecorder + WeekDay.xTuesday +
                    Week + product_analytic_vertical.xDJController + GMV_lag_1_per + 
                    product_analytic_vertical.xKaraokePlayer + Sponsorship + GMV_lag_2_per +                            
                    WeekDay.xThursday + product_analytic_vertical.xHiFiSystem + event.xEid...Rath + 
                    GMV_lag_3_per + event.xValentine.Day + event.xFHSD + 
                    product_analytic_vertical.xDock + NPS_Score + product_analytic_vertical.xSoundMixer +              
                    sla, data = DLHomeAudioDF)


summary(DLHAModel_2)
vif(DLHAModel_2)

#high vif: product_mrp
DLHAModel_3 <- lm(formula = gmv ~ event.xDaussera + 
                    Affiliates + Online_Marketing_adstock + product_analytic_vertical.xDockingStation+ 
                    Digital + product_analytic_vertical.xVoiceRecorder + WeekDay.xTuesday +
                    Week + product_analytic_vertical.xDJController + GMV_lag_1_per + 
                    product_analytic_vertical.xKaraokePlayer + Sponsorship + GMV_lag_2_per +                            
                    WeekDay.xThursday + product_analytic_vertical.xHiFiSystem + event.xEid...Rath + 
                    GMV_lag_3_per + event.xValentine.Day + event.xFHSD + 
                    product_analytic_vertical.xDock + NPS_Score + product_analytic_vertical.xSoundMixer +              
                    sla, data = DLHomeAudioDF)


summary(DLHAModel_3)
vif(DLHAModel_3)

## Remove special event days
DLHAModel_4 <- lm(formula = gmv ~ 
                    Affiliates + Online_Marketing_adstock + product_analytic_vertical.xDockingStation+ 
                    Digital + product_analytic_vertical.xVoiceRecorder +
                    product_analytic_vertical.xDJController + GMV_lag_1_per + 
                    product_analytic_vertical.xKaraokePlayer + Sponsorship + GMV_lag_2_per +                            
                    product_analytic_vertical.xHiFiSystem +GMV_lag_3_per + 
                    product_analytic_vertical.xDock + NPS_Score + product_analytic_vertical.xSoundMixer +              
                    sla, data = DLHomeAudioDF)


summary(DLHAModel_4)
vif(DLHAModel_4)

#Residual standard error: 0.611 on 33 degrees of freedom
#Multiple R-squared:  0.749,	Adjusted R-squared:  0.627


## High p: product_analytic_vertical.xVoiceRecorder, Affiliates
DLHAModel_5 <- lm(formula = gmv ~ 
                    Online_Marketing_adstock + product_analytic_vertical.xDockingStation+ 
                    Digital +
                    product_analytic_vertical.xDJController + GMV_lag_1_per + 
                    product_analytic_vertical.xKaraokePlayer + Sponsorship + GMV_lag_2_per +                            
                    product_analytic_vertical.xHiFiSystem +GMV_lag_3_per + 
                    product_analytic_vertical.xDock + NPS_Score + product_analytic_vertical.xSoundMixer +              
                    sla, data = DLHomeAudioDF)


summary(DLHAModel_5)
vif(DLHAModel_5)

## High p: NPS Score, Digital
DLHAModel_6 <- lm(formula = gmv ~ 
                    Online_Marketing_adstock + product_analytic_vertical.xDockingStation+
                    product_analytic_vertical.xDJController + GMV_lag_1_per + 
                    product_analytic_vertical.xKaraokePlayer + Sponsorship + GMV_lag_2_per +                            
                    product_analytic_vertical.xHiFiSystem +GMV_lag_3_per + 
                    product_analytic_vertical.xDock  + product_analytic_vertical.xSoundMixer +              
                    sla, data = DLHomeAudioDF)


summary(DLHAModel_6)
vif(DLHAModel_6)


## High p: product_analytic_vertical.xDJController, product_analytic_vertical.xKaraokePlayer
DLHAModel_7 <- lm(formula = gmv ~ 
                    Online_Marketing_adstock + product_analytic_vertical.xDockingStation+
                    GMV_lag_1_per  + Sponsorship + GMV_lag_2_per +                            
                    product_analytic_vertical.xHiFiSystem +GMV_lag_3_per + 
                    product_analytic_vertical.xDock  + product_analytic_vertical.xSoundMixer +              
                    sla, data = DLHomeAudioDF)


summary(DLHAModel_7)
vif(DLHAModel_7)


## High p: Online_Marketing_adstock, GMV_lag_3_per
DLHAModel_8 <- lm(formula = gmv ~ 
                    product_analytic_vertical.xDockingStation+
                    GMV_lag_1_per  + Sponsorship + GMV_lag_2_per +                            
                    product_analytic_vertical.xHiFiSystem +
                    product_analytic_vertical.xDock  + product_analytic_vertical.xSoundMixer +              
                    sla, data = DLHomeAudioDF)


summary(DLHAModel_8)
vif(DLHAModel_8)

## High p: Sponsorship,product_analytic_vertical.xSoundMixer
DLHAModel_9 <- lm(formula = gmv ~ 
                    product_analytic_vertical.xDockingStation+
                    GMV_lag_1_per  +GMV_lag_2_per +                            
                    product_analytic_vertical.xHiFiSystem +
                    product_analytic_vertical.xDock  +             
                    sla, data = DLHomeAudioDF)


summary(DLHAModel_9)
vif(DLHAModel_9)

## High p: product_analytic_vertical.xDockingStation
DLHAModel_10 <- lm(formula = gmv ~ 
                    GMV_lag_1_per  +GMV_lag_2_per +                            
                    product_analytic_vertical.xHiFiSystem +
                    product_analytic_vertical.xDock  +             
                    sla, data = DLHomeAudioDF)


summary(DLHAModel_10)
vif(DLHAModel_10)

#Residual standard error: 0.572 on 44 degrees of freedom
#Multiple R-squared:  0.706,	Adjusted R-squared:  0.673 

### Cross-validation
cv.lm(data = DLHomeAudioDF, form.lm = DLHAModel_10, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)


### Estimating the elasticity coefficients

elasticity_DL <- function(var){
  DL_elasticity <- as.numeric(DLHAModel_10$coefficients[var]*mean(DLHomeAudioDF[,var])/mean(DLHomeAudioDF$gmv))
  return(DL_elasticity)
  
} 

DL_var_list <- list()

for(i in 2:length(DLHAModel_11$coefficients)){
  DL_var_list[i-1] <- elasticity_DL(names(DLHAModel_10$coefficients)[i])
  
}

DL_elasticity.outputs <- data.frame(names(DLHAModel_10$coefficients[2:length(DLHAModel_10$coefficients)]))
DL_elasticity.outputs <- cbind(DL_elasticity.outputs,do.call(rbind.data.frame, DL_var_list))
colnames(DL_elasticity.outputs) <- c("Variable","Elasticity")

DL_elasticity.outputs$Direction <- ifelse(DL_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(DL_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Distributed Lag Model") +xlab("Variables")  


#################### Model 3: Distributed Lag -End ##########




################################ Model Type 4 ####################################
###########################   Koyck Model  #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# keeping Week# 1 'gmv' and removing other lag variables and Moving averages variables
str(HomeAudioFinal)
#data.frame':	50 obs. of  73 variables
colnames(HomeAudioFinal)
# 18, 19 are list_price and promotion_offered
# 59 to 73 are lag and MA variables
# Keeping GMV_lag_1_per"  (column# 71)

KYHomeAudioDF <- HomeAudioFinal[,-c(18:19,59:70,72:73)]
colnames(KYHomeAudioDF)



## Scaling the variables
KYHomeAudioDF[,2:ncol(KYHomeAudioDF)] <- scale(KYHomeAudioDF[,2:ncol(KYHomeAudioDF)])


### Stepwise Regression to remove insignificant and correlated variables
KMHA_base.mod <- lm(gmv ~ 1 , data= KYHomeAudioDF)  # base intercept only model
KMHA_all.mod <- lm(gmv ~ . , data= KYHomeAudioDF) # full model with all predictors
KMHA_stepMod <- step(KMHA_base.mod, scope = list(lower = KMHA_base.mod, upper = KMHA_all.mod), direction = "both", trace = 1, steps = 1000) 
KMHA_shortlistedVars <- names(unlist(KMHA_stepMod[[1]])) 
KMHA_shortlistedVars <- KMHA_shortlistedVars[!KMHA_shortlistedVars %in% "(Intercept)"]  

show(KMHA_shortlistedVars)
#[1] "units"                                       "product_mrp"                                 "event.xDaussera"                            
#[4] "Affiliates"                                  "Online_Marketing_adstock"                    "product_analytic_vertical.xDockingStation"  
#[7] "Digital"                                     "product_analytic_vertical.xVoiceRecorder"    "WeekDay.xTuesday"                           
#[10] "Week"                                        "product_analytic_vertical.xDJController"     "GMV_lag_1_per"                              
#[13] "product_analytic_vertical.xKaraokePlayer"    "Sponsorship"                                 "Digital_adstock"                            
#[16] "product_analytic_vertical.xFMRadio"          "product_analytic_vertical.xHomeAudioSpeaker" "product_analytic_vertical.xHiFiSystem"      
#[19] "Sponsorship_adstock"                         "event.xValentine.Day"                        "WeekDay.xThursday"                          
#[22] "event.xFHSD"                                 "event.xEid...Rath"                           "event.xRepublic.Day"                        
#[25] "event.xBSD"




### Model Building::

## Model 1-
KYHModel_1 <- lm(formula = gmv ~ units + product_mrp + event.xDaussera + 
                      Affiliates + Online_Marketing_adstock + product_analytic_vertical.xDockingStation +
                      Digital + product_analytic_vertical.xVoiceRecorder + WeekDay.xTuesday + 
                      Week + product_analytic_vertical.xDJController + Sponsorship + GMV_lag_1_per +
                      product_analytic_vertical.xKaraokePlayer + Digital_adstock + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xHiFiSystem + 
                      Sponsorship_adstock + event.xValentine.Day + WeekDay.xThursday + 
                      event.xFHSD + event.xEid...Rath + event.xRepublic.Day + event.xBSD,
                      data = KYHomeAudioDF)


summary(KYHModel_1)
vif(KYHModel_1)

# Remove special events and days
KYHModel_2 <- lm(formula = gmv ~ units + product_mrp  + 
                   Affiliates + Online_Marketing_adstock + product_analytic_vertical.xDockingStation +
                   Digital + product_analytic_vertical.xVoiceRecorder  + 
                  product_analytic_vertical.xDJController + Sponsorship + GMV_lag_1_per +
                   product_analytic_vertical.xKaraokePlayer + Digital_adstock + 
                   product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xHiFiSystem + 
                   Sponsorship_adstock,
                 data = KYHomeAudioDF)


summary(KYHModel_2)
vif(KYHModel_2)

#high vif: units
KYHModel_3 <- lm(formula = gmv ~  product_mrp  + 
                   Affiliates + Online_Marketing_adstock + product_analytic_vertical.xDockingStation +
                   Digital + product_analytic_vertical.xVoiceRecorder  + 
                   product_analytic_vertical.xDJController + Sponsorship + GMV_lag_1_per +
                   product_analytic_vertical.xKaraokePlayer + Digital_adstock + 
                   product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xHiFiSystem + 
                   Sponsorship_adstock,
                 data = KYHomeAudioDF)


summary(KYHModel_3)
vif(KYHModel_3)


#high vif: product_mrp
KYHModel_4 <- lm(formula = gmv ~ 
                   Affiliates + Online_Marketing_adstock + product_analytic_vertical.xDockingStation +
                   Digital + product_analytic_vertical.xVoiceRecorder  + 
                   product_analytic_vertical.xDJController + Sponsorship + GMV_lag_1_per +
                   product_analytic_vertical.xKaraokePlayer + Digital_adstock + 
                   product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xHiFiSystem + 
                   Sponsorship_adstock,
                 data = KYHomeAudioDF)


summary(KYHModel_4)
vif(KYHModel_4)

#Residual standard error: 0.0608 on 35 degrees of freedom
#Multiple R-squared:  0.997,	Adjusted R-squared:  0.996 


#high p: Online_Marketing_adstock, product_analytic_vertical.xKaraokePlayer
KYHModel_5 <- lm(formula = gmv ~ 
                   Affiliates +product_analytic_vertical.xDockingStation +
                   Digital + product_analytic_vertical.xVoiceRecorder  + 
                   product_analytic_vertical.xDJController + Sponsorship + GMV_lag_1_per + Digital_adstock + 
                   product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xHiFiSystem + 
                   Sponsorship_adstock,
                 data = KYHomeAudioDF)


summary(KYHModel_5)
vif(KYHModel_5)

#high p: Sponsorship, Sponsorship_adstock
KYHModel_6 <- lm(formula = gmv ~ 
                   Affiliates +product_analytic_vertical.xDockingStation +
                   Digital + product_analytic_vertical.xVoiceRecorder  + 
                   product_analytic_vertical.xDJController  + GMV_lag_1_per + Digital_adstock + 
                   product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker +
                   product_analytic_vertical.xHiFiSystem,
                   data = KYHomeAudioDF)


summary(KYHModel_6)
vif(KYHModel_6)

#high p: Sponsorship, Sponsorship_adstock
KYHModel_6 <- lm(formula = gmv ~ 
                   Affiliates +product_analytic_vertical.xDockingStation +
                   Digital + product_analytic_vertical.xVoiceRecorder  + 
                   product_analytic_vertical.xDJController  + GMV_lag_1_per + Digital_adstock + 
                   product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker +
                   product_analytic_vertical.xHiFiSystem,
                 data = KYHomeAudioDF)


summary(KYHModel_6)
vif(KYHModel_6)

#high vif: product_analytic_vertical.xFMRadio
KYHModel_7 <- lm(formula = gmv ~ 
                   Affiliates +product_analytic_vertical.xDockingStation +
                   Digital + product_analytic_vertical.xVoiceRecorder  + 
                   product_analytic_vertical.xDJController  + GMV_lag_1_per + Digital_adstock + 
                  product_analytic_vertical.xHomeAudioSpeaker +product_analytic_vertical.xHiFiSystem,
                 data = KYHomeAudioDF)


summary(KYHModel_7)
vif(KYHModel_7)

#high p: product_analytic_vertical.xHiFiSystem
KYHModel_8 <- lm(formula = gmv ~ 
                   Affiliates +product_analytic_vertical.xDockingStation +
                   Digital + product_analytic_vertical.xVoiceRecorder  + 
                   product_analytic_vertical.xDJController  + GMV_lag_1_per + Digital_adstock + 
                   product_analytic_vertical.xHomeAudioSpeaker,
                 data = KYHomeAudioDF)


summary(KYHModel_8)
vif(KYHModel_8)

#high p: gmv_1
KYHModel_9 <- lm(formula = gmv ~ 
                   Affiliates +product_analytic_vertical.xDockingStation +
                   Digital + product_analytic_vertical.xVoiceRecorder  + 
                   product_analytic_vertical.xDJController + Digital_adstock + 
                   product_analytic_vertical.xHomeAudioSpeaker,
                 data = KYHomeAudioDF)

summary(KYHModel_9)
vif(KYHModel_9)

#high p: 
KYHModel_10 <- lm(formula = gmv ~ 
                   Affiliates +product_analytic_vertical.xDockingStation +
                   Digital   + 
                   product_analytic_vertical.xDJController + Digital_adstock + 
                   product_analytic_vertical.xHomeAudioSpeaker,
                 data = KYHomeAudioDF)

summary(KYHModel_10)
vif(KYHModel_10)

#Residual standard error: 0.0865 on 43 degrees of freedom
#Multiple R-squared:  0.993,	Adjusted R-squared:  0.993 



### Cross-validation
cv.lm(data = KYHomeAudioDF, form.lm = KYHModel_10, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 



### Estimating the elasticity coefficients

elasticity_KY <- function(var){
  KY_elasticity <- as.numeric(KYHModel_10$coefficients[var]*mean(KYHomeAudioDF[,var])/mean(KYHomeAudioDF$gmv))
  return(KY_elasticity)
  
} 

KY_var_list <- list()

for(i in 2:length(KYHModel_10$coefficients)){
  KY_var_list[i-1] <- elasticity_KY(names(KYHModel_10$coefficients)[i])
  
}

KY_elasticity.outputs <- data.frame(names(KYHModel_10$coefficients[2:length(KYHModel_10$coefficients)]))
KY_elasticity.outputs <- cbind(KY_elasticity.outputs,do.call(rbind.data.frame, KY_var_list))
colnames(KY_elasticity.outputs) <- c("Variable","Elasticity")

KY_elasticity.outputs$Direction <- ifelse(KY_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(KY_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Koyck Model") +xlab("Variables")  



#######Model Type 4 Koyck Model Ends########################################################


################################ Model Type 5 ####################################
###########################   Multiplicative model + Distributed lag  Model  #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# keeping Week# 1,2,3 'gmv' and removing other lag variables and Moving averages variables
str(HomeAudioFinal)
#data.frame':	50 obs. of  73 variables
colnames(HomeAudioFinal)
# 18, 19 are list_price and promotion_offered
# 59 to 73 are lag and MA variables
# Keeping GMV_lag_1_per, GMV_lag_2_per, GMV_lag_3_per  (column# 71, 72, 73)

MuDiHomeAudioDF <- HomeAudioFinal[,-c(18:19,59:70)]
colnames(MuDiHomeAudioDF)


## Replace 0 value  with '0.00001' as log(0) is undefined
MuDiHomeAudioDF[MuDiHomeAudioDF == 0] <- 0.00001

## Tranform  negative values
MuDiHomeAudioDF$GMV_lag_1_per <- 1 + MuDiHomeAudioDF$GMV_lag_1_per - min(MuDiHomeAudioDF$GMV_lag_1_per)
MuDiHomeAudioDF$GMV_lag_2_per <- 1 + MuDiHomeAudioDF$GMV_lag_2_per - min(MuDiHomeAudioDF$GMV_lag_2_per)
MuDiHomeAudioDF$GMV_lag_3_per <- 1 + MuDiHomeAudioDF$GMV_lag_3_per - min(MuDiHomeAudioDF$GMV_lag_3_per)

## Take log for Multiplicative model
MuDiHomeAudioDF <- log(MuDiHomeAudioDF)

## Checking the variables for linear relationship or multicollinearity
MuDiHAModel <- lm(gmv~.,MuDiHomeAudioDF)
alias(MuDiHAModel)
## Below varabiles found collienarity with other variables so remove these
#event.xNon.event.Day         -77051/474758         59603/342927          -1192/35177
#event.xPacman                95931/2077105         243857/52415       -162731/223419
#event.xRepublic.Day            -3639/42964       -156657/136937         -26530/55151
#event.xValentine.Day          -17552/19633     -6384984/1361855          65747/59104
#price_mark.xPREMIUM          153349/599458         38041/284020         39245/259052
#price_mark.xPROMISING   7941859/2608113985         -5523/271246            303/66614
#GMV_lag_1_per                    723/20669         65867/265829        -10185/335798
#GMV_lag_2_per                 -16659/38900       -337650/154669             689/25773
#GMV_lag_3_per                   5733/13465          10918/27167      -327373/8875973
colnames(MuDiHomeAudioDF)

## Removing the variables which were showing linear relationship or multicollinearity
MuDiHomeAudioDF <- MuDiHomeAudioDF[, -c(51:59)]


### Stepwise Regression to remove insignificant and correlated variables
MuDiHA_base.mod <- lm(gmv ~ 1 , data= MuDiHomeAudioDF)  # base intercept only model
MuDiHA_all.mod <- lm(gmv ~ . , data= MuDiHomeAudioDF) # full model with all predictors
MuDiHA_stepMod <- step(MuDiHA_base.mod, scope = list(lower = MuDiHA_base.mod, upper = MuDiHA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
MuDiHA_shortlistedVars <- names(unlist(MuDiHA_stepMod[[1]])) # get the shortlisted variable.
MuDiHA_shortlistedVars <- MuDiHA_shortlistedVars[!MuDiHA_shortlistedVars %in% "(Intercept)"]  # remove intercept

show(MuDiHA_shortlistedVars)
#[1] "units"                                     "product_mrp"                               "Content_Marketing"                        
#[4] "SEM_adtock"                                "Radio"                                     "event.xDiwali"                            
#[7] "TV_adstock"                                "product_analytic_vertical.xDockingStation" "Online_Marketing_adstock"                 
#[10] "event.xDaussera"                           "product_procurement_sla"                   "Radio_adstock"                            
#[13] "product_analytic_vertical.xSlingBox"    


### Model Building::

## Model 1:
MuDiHAmodel_1 <- lm(formula = gmv ~ units + product_mrp  + Content_Marketing + 
                    SEM_adtock + Radio + event.xDiwali +
                    TV_adstock + product_analytic_vertical.xDockingStation + Online_Marketing_adstock + 
                    event.xDaussera + product_procurement_sla + Radio_adstock + 
                    product_analytic_vertical.xSlingBox, 
                    data = MuDiHomeAudioDF)


summary(MuDiHAmodel_1)
vif(MuDiHAmodel_1)
#Remove special event days
MuDiHAmodel_2 <- lm(formula = gmv ~ product_mrp  + Content_Marketing +SEM_adtock + Radio  +
                      TV_adstock + product_analytic_vertical.xDockingStation + Online_Marketing_adstock + 
                      product_procurement_sla + Radio_adstock +product_analytic_vertical.xSlingBox, 
                    data = MuDiHomeAudioDF)


summary(MuDiHAmodel_2)
vif(MuDiHAmodel_2)

#high vif: Online_Marketing_adstock
MuDiHAmodel_3 <- lm(formula = gmv ~ product_mrp  + Content_Marketing +SEM_adtock + Radio  +
                      TV_adstock + product_analytic_vertical.xDockingStation + 
                      product_procurement_sla + Radio_adstock +product_analytic_vertical.xSlingBox, 
                    data = MuDiHomeAudioDF)


summary(MuDiHAmodel_3)
vif(MuDiHAmodel_3)

#high p: product_procurement_sla
MuDiHAmodel_4 <- lm(formula = gmv ~ product_mrp  + Content_Marketing +SEM_adtock + Radio  +
                      TV_adstock + product_analytic_vertical.xDockingStation + 
                      Radio_adstock +product_analytic_vertical.xSlingBox, 
                    data = MuDiHomeAudioDF)


summary(MuDiHAmodel_4)
vif(MuDiHAmodel_4)

#high p: radio
MuDiHAmodel_5 <- lm(formula = gmv ~ product_mrp  + Content_Marketing +SEM_adtock  +
                      TV_adstock + product_analytic_vertical.xDockingStation + 
                      Radio_adstock +product_analytic_vertical.xSlingBox, 
                    data = MuDiHomeAudioDF)


summary(MuDiHAmodel_5)
vif(MuDiHAmodel_5)

#high p: product_analytic_vertical.xSlingBox
MuDiHAmodel_6 <- lm(formula = gmv ~ product_mrp  + Content_Marketing +SEM_adtock  +
                      TV_adstock + product_analytic_vertical.xDockingStation + 
                      Radio_adstock, 
                    data = MuDiHomeAudioDF)


summary(MuDiHAmodel_6)
vif(MuDiHAmodel_6)

#high p: TV_adstock
MuDiHAmodel_7 <- lm(formula = gmv ~ product_mrp  + Content_Marketing +SEM_adtock  +
                      product_analytic_vertical.xDockingStation + 
                      Radio_adstock, 
                    data = MuDiHomeAudioDF)


summary(MuDiHAmodel_7)
vif(MuDiHAmodel_7)

#high p: product_analytic_vertical.xDockingStation
MuDiHAmodel_8 <- lm(formula = gmv ~ product_mrp  + Content_Marketing +SEM_adtock + 
                      Radio_adstock, 
                    data = MuDiHomeAudioDF)


summary(MuDiHAmodel_8)
vif(MuDiHAmodel_8)

#high p: Content_Marketing
MuDiHAmodel_9 <- lm(formula = gmv ~ product_mrp  +SEM_adtock + 
                      Radio_adstock, 
                    data = MuDiHomeAudioDF)


summary(MuDiHAmodel_9)
vif(MuDiHAmodel_9)


### Cross-validation
cv.lm(data = MuDiHomeAudioDF, form.lm = MuDiHAmodel_9, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 



### Estimating the elasticity coefficients

as.numeric(MuDiHAmodel_9$coefficients["SEM_adtock"]*mean(MuDiHomeAudioDF[,"SEM_adtock"])/mean(MuDiHomeAudioDF$gmv))

elasticity_DuMi <- function(var){
  MDHA_elasticity <- as.numeric(MuDiHAmodel_9$coefficients[var]*mean(MuDiHomeAudioDF[,var])/mean(MuDiHomeAudioDF$gmv))
  return(MDHA_elasticity)
  
} 

MDHA_var_list <- list()

for(i in 2:length(MuDiHAmodel_9$coefficients)){
  MDHA_var_list[i-1] <- elasticity_DuMi(names(MuDiHAmodel_9$coefficients)[i])
  
}

MDHA_elasticity.outputs <- data.frame(names(MuDiHAmodel_9$coefficients[2:length(MuDiHAmodel_9$coefficients)]))
MDHA_elasticity.outputs <- cbind(MDHA_elasticity.outputs,do.call(rbind.data.frame, MDHA_var_list))
colnames(MDHA_elasticity.outputs) <- c("Variable","Elasticity")

MDHA_elasticity.outputs$Direction <- ifelse(MDHA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(MDHA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Multiplicative and Distributed Lag Model") +xlab("Variables")  



