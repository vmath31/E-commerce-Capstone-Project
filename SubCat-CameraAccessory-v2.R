######################################################################################
##      Capstone Ecommerce- R File - Sub Category: Camere Accessory  Models 
######################################################################################

## Section 4: Model Building

####################### Model Type 1 ###########
##################### Basic Linear Model #######


# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# Remove lag variables and Moving averages variables
str(CameAccFinal)
#data.frame':	52 obs. of  88 variables
colnames(CameAccFinal)
# 18, 19 are list_price and promotion_offered
# 74 to 88 are lag and MA variables

LinearCameAccDF <- CameAccFinal[,-c(18:19,74:88)]
colnames(LinearCameAccDF)

## Scaling the variables
LinearCameAccDF[,2:ncol(LinearCameAccDF)] <- scale(LinearCameAccDF[,2:ncol(LinearCameAccDF)])


### Step to remove insignificant and correlated variables

# Base intercept model
CABase<- lm(gmv ~ 1 , data= LinearCameAccDF)
# Complete model all variables
CAAll<- lm(gmv ~ . , data= LinearCameAccDF) 
summary(CAAll)

CAStep <- step(CABase, scope = list(lower = CABase, upper = CAAll), direction = "both", trace = 1, steps = 1000)

SelectedVars <- names(unlist(CAStep[[1]])) 
# Delete Intercepts
SelectedVars <- SelectedVars[!SelectedVars %in% "(Intercept)"]  # remove intercept

show(SelectedVars)
# [1] "product_mrp"                                     "units"                                           "product_analytic_vertical.xCameraTripod"        
#[4] "product_analytic_vertical.xCameraMount"          "NPS_Score"                                       "product_analytic_vertical.xCameraBattery"       
#[7] "Radio"                                           "product_analytic_vertical.xReflectorUmbrella"    "product_analytic_vertical.xSoftbox"             
#[10] "WeekDay.xSaturday"                               "Online_Marketing"                                "Digital_adstock"                                
#[13] "product_analytic_vertical.xExtensionTube"        "SEM_adtock"                                      "product_analytic_vertical.xTeleconverter"       
#[16] "event.xDaussera"                                 "product_analytic_vertical.xCameraBatteryGrip"    "product_analytic_vertical.xTelescope"           
#[19] "s1_fact.order_payment_type"                      "WeekDay.xThursday"                               "Week"                                           
#[22] "event.xChristmas...NY"                           "Affiliates"                                      "product_analytic_vertical.xStrap"               
#[25] "product_analytic_vertical.xCameraRemoteControl"  "product_analytic_vertical.xCameraBag"            "price_mark.xPROMISING"                          
#[28] "event.xFHSD"                                     "WeekDay.xTuesday"                                "product_analytic_vertical.xCameraHousing"       
#[31] "product_analytic_vertical.xCameraBatteryCharger" "product_analytic_vertical.xCameraEyeCup"         "Content_Marketing"                              
#[34] "event.xDiwali"                                   "product_analytic_vertical.xCameraMicrophone"     "Digital"                                        
#[37] "WeekDay.xMonday"                                 "Radio_adstock"                                   "product_analytic_vertical.xCameraLEDLight"      
#[40] "product_analytic_vertical.xFilter"               "Other_adstock"                                   "TV_adstock"                                     
#[43] "Affiliates_adstock"                              "SEM"                                             "product_analytic_vertical.xFlashShoeAdapter"    
#[46] "Sponsorship"                                     "event.xEid...Rath"                               "product_analytic_vertical.xFlash"               
#[49] "event.xValentine.Day"                            "event.xRakshabandhan"                            "sla"

### Model Building::

## 1st model based on selected variables
CAModel_L1 <- lm(formula = gmv ~ product_mrp+ units+ product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   WeekDay.xSaturday+ Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube+ SEM_adtock + product_analytic_vertical.xTeleconverter+
                   event.xDaussera + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ price_mark.xPROMISING+
                   event.xFHSD+ WeekDay.xTuesday + product_analytic_vertical.xCameraHousing +
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraEyeCup+ Content_Marketing+
                   event.xDiwali+ product_analytic_vertical.xCameraMicrophone+ Digital+
                   WeekDay.xMonday+ Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ Other_adstock+ TV_adstock+
                   Affiliates_adstock+ SEM+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ event.xEid...Rath+ product_analytic_vertical.xFlash+
                   event.xValentine.Day+ event.xRakshabandhan+ sla
                   , data = LinearCameAccDF)


summary(CAModel_L1)
vif(CAModel_L1)
                                  
## REmove special event days
CAModel_L2 <- lm(formula = gmv ~ product_mrp+ units+ product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube+ SEM_adtock + product_analytic_vertical.xTeleconverter+
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ price_mark.xPROMISING+
                   product_analytic_vertical.xCameraHousing +
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraEyeCup+ Content_Marketing+
                   product_analytic_vertical.xCameraMicrophone+ Digital+
                   Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ TV_adstock+
                   Affiliates_adstock+ SEM+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ product_analytic_vertical.xFlash+ sla
                 , data = LinearCameAccDF)


summary(CAModel_L2)
vif(CAModel_L2)

## High vif: units
CAModel_L3 <- lm(formula = gmv ~ product_mrp+ product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube+ SEM_adtock + product_analytic_vertical.xTeleconverter+
                   product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ price_mark.xPROMISING+
                   product_analytic_vertical.xCameraHousing +
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraEyeCup+ Content_Marketing+
                   product_analytic_vertical.xCameraMicrophone+ Digital+
                   Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ TV_adstock+
                   Affiliates_adstock+ SEM+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ product_analytic_vertical.xFlash+ sla
                 , data = LinearCameAccDF)


summary(CAModel_L3)
vif(CAModel_L3)

## High vif: SEM
CAModel_L4 <- lm(formula = gmv ~ product_mrp+ product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube+ SEM_adtock + product_analytic_vertical.xTeleconverter+
                   product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ price_mark.xPROMISING+
                   product_analytic_vertical.xCameraHousing +
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraEyeCup+ Content_Marketing+
                   product_analytic_vertical.xCameraMicrophone+ Digital+
                   Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ TV_adstock+
                   Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ product_analytic_vertical.xFlash+ sla
                 , data = LinearCameAccDF)


summary(CAModel_L4)
vif(CAModel_L4)

## High vif: SEM_adstock
CAModel_L5 <- lm(formula = gmv ~ product_mrp+ product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                   product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ price_mark.xPROMISING+
                   product_analytic_vertical.xCameraHousing +
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraEyeCup+ Content_Marketing+
                   product_analytic_vertical.xCameraMicrophone+ Digital+
                   Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ TV_adstock+
                   Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ product_analytic_vertical.xFlash+ sla
                 , data = LinearCameAccDF)


summary(CAModel_L5)
vif(CAModel_L5)

## High vif: product_mrp
CAModel_L6 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                   product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ price_mark.xPROMISING+
                   product_analytic_vertical.xCameraHousing +
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraEyeCup+ Content_Marketing+
                   product_analytic_vertical.xCameraMicrophone+ Digital+
                   Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ TV_adstock+
                   Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ product_analytic_vertical.xFlash+ sla
                 , data = LinearCameAccDF)


summary(CAModel_L6)
vif(CAModel_L6)

## High vif: Content_Marketing
CAModel_L7 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                   product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ price_mark.xPROMISING+
                   product_analytic_vertical.xCameraHousing +
                   product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraEyeCup+
                   product_analytic_vertical.xCameraMicrophone+ Digital+
                   Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ TV_adstock+
                   Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ product_analytic_vertical.xFlash+ sla
                 , data = LinearCameAccDF)


summary(CAModel_L7)
vif(CAModel_L7)

## High vif: product_analytic_vertical.xCameraBatteryCharger
CAModel_L8 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                   product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ price_mark.xPROMISING+
                   product_analytic_vertical.xCameraHousing +product_analytic_vertical.xCameraEyeCup+
                   product_analytic_vertical.xCameraMicrophone+ Digital+
                   Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ TV_adstock+
                   Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ product_analytic_vertical.xFlash+ sla
                 , data = LinearCameAccDF)


summary(CAModel_L8)
vif(CAModel_L8)

## High vif: price_mark.xPROMISING
CAModel_L9 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                   product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+
                   product_analytic_vertical.xCameraHousing +product_analytic_vertical.xCameraEyeCup+
                   product_analytic_vertical.xCameraMicrophone+ Digital+
                   Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ TV_adstock+
                   Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ product_analytic_vertical.xFlash+ sla
                 , data = LinearCameAccDF)


summary(CAModel_L9)
vif(CAModel_L9)

## High vif: product_analytic_vertical.xCameraBag
CAModel_L10 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                   product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                   Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                   Online_Marketing+ Digital_adstock+
                   product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                   product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                   product_analytic_vertical.xCameraRemoteControl+
                   product_analytic_vertical.xCameraHousing +product_analytic_vertical.xCameraEyeCup+
                   product_analytic_vertical.xCameraMicrophone+ Digital+
                   Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                   product_analytic_vertical.xFilter+ TV_adstock+
                   Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                   Sponsorship+ product_analytic_vertical.xFlash+ sla
                 , data = LinearCameAccDF)


summary(CAModel_L10)
vif(CAModel_L10)

## High vif: product_analytic_vertical.xCameraRemoteControl
CAModel_L11 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                    product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery+
                    Radio + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraBattery+
                    Online_Marketing+ Digital_adstock+
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    product_analytic_vertical.xCameraHousing +product_analytic_vertical.xCameraEyeCup+
                    product_analytic_vertical.xCameraMicrophone+ Digital+
                    Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                    product_analytic_vertical.xFilter+ TV_adstock+
                    Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                    Sponsorship+ product_analytic_vertical.xFlash+ sla
                  , data = LinearCameAccDF)


summary(CAModel_L11)
vif(CAModel_L11)

## High vif: NPS_Score, product_analytic_vertical.xCameraBattery
CAModel_L12 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                    product_analytic_vertical.xCameraMount+
                    Radio + product_analytic_vertical.xReflectorUmbrella +
                    Online_Marketing+ Digital_adstock+
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    product_analytic_vertical.xCameraHousing +product_analytic_vertical.xCameraEyeCup+
                    product_analytic_vertical.xCameraMicrophone+ Digital+
                    Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                    product_analytic_vertical.xFilter+ TV_adstock+
                    Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                    Sponsorship+ product_analytic_vertical.xFlash+ sla
                  , data = LinearCameAccDF)


summary(CAModel_L12)
vif(CAModel_L12)

## High vif: Online_Marketing
CAModel_L13 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                    product_analytic_vertical.xCameraMount+
                    Radio + product_analytic_vertical.xReflectorUmbrella + Digital_adstock+
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    product_analytic_vertical.xCameraHousing +product_analytic_vertical.xCameraEyeCup+
                    product_analytic_vertical.xCameraMicrophone+ Digital+
                    Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                    product_analytic_vertical.xFilter+ TV_adstock+
                    Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                    Sponsorship+ product_analytic_vertical.xFlash+ sla
                  , data = LinearCameAccDF)


summary(CAModel_L13)
vif(CAModel_L13)

## High p: product_analytic_vertical.xFlash, product_analytic_vertical.xCameraEyeCup, product_analytic_vertical.xCameraMicrophone,Digital_adstock, 
CAModel_L14 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                    product_analytic_vertical.xCameraMount+
                    Radio + product_analytic_vertical.xReflectorUmbrella +
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTeleconverter+
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    product_analytic_vertical.xCameraHousing + Digital+
                    Radio_adstock+ product_analytic_vertical.xCameraLEDLight+
                    product_analytic_vertical.xFilter+ TV_adstock+
                    Affiliates_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                    Sponsorship+ sla
                  , data = LinearCameAccDF)


summary(CAModel_L14)
vif(CAModel_L14)

## High p:  
CAModel_L15 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                    product_analytic_vertical.xCameraMount+
                    Radio + product_analytic_vertical.xReflectorUmbrella +
                    product_analytic_vertical.xTeleconverter+
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    product_analytic_vertical.xCameraHousing + Digital+
                    product_analytic_vertical.xFilter+ product_analytic_vertical.xFlashShoeAdapter+
                    Sponsorship+ sla
                  , data = LinearCameAccDF)


summary(CAModel_L15)
vif(CAModel_L15)


## High p:  
CAModel_L16 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod+ 
                    product_analytic_vertical.xCameraMount+
                    product_analytic_vertical.xReflectorUmbrella +
                    product_analytic_vertical.xTeleconverter+
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    product_analytic_vertical.xCameraHousing + Digital+
                    product_analytic_vertical.xFilter+ product_analytic_vertical.xFlashShoeAdapter+sla
                  , data = LinearCameAccDF)


summary(CAModel_L16)
vif(CAModel_L16)

## High p:  
CAModel_L17 <- lm(formula = gmv ~  
                    product_analytic_vertical.xCameraMount +
                    product_analytic_vertical.xTeleconverter+
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    product_analytic_vertical.xCameraHousing +
                    product_analytic_vertical.xFilter+ product_analytic_vertical.xFlashShoeAdapter+sla
                  , data = LinearCameAccDF)


summary(CAModel_L17)
vif(CAModel_L17)

## High p:  
CAModel_L18 <- lm(formula = gmv ~  
                    product_analytic_vertical.xCameraMount +
                    product_analytic_vertical.xCameraBatteryGrip+
                    product_analytic_vertical.xCameraHousing +sla
                  , data = LinearCameAccDF)


summary(CAModel_L18)
vif(CAModel_L18)


### Cross-validation - Model 18
cv.lm(data = LinearCameAccDF, form.lm = CAModel_L18, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)


### Estimating the elasticity coefficients - Model 18

elasticity_L <- function(var){
  L_elasticity <- as.numeric(CAModel_L18$coefficients[var]*mean(LinearCameAccDF[,var])/mean(LinearCameAccDF$gmv))
  return(L_elasticity)
  
} 

L_var_list <- list()

for(i in 2:length(CAModel_L18$coefficients)){
  L_var_list[i-1] <- elasticity_L(names(CAModel_L18$coefficients)[i])
  
}

L_elasticity.outputs <- data.frame(names(CAModel_L18$coefficients[2:length(CAModel_L18$coefficients)]))
L_elasticity.outputs <- cbind(L_elasticity.outputs,do.call(rbind.data.frame, L_var_list))
colnames(L_elasticity.outputs) <- c("Variable","Elasticity")

L_elasticity.outputs$Direction <- ifelse(L_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(L_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Camera Accessory - Basic Linear Model") +xlab("Variables")  



################################ Model Type 2 ####################################
###########################   Multiplicative Model  #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# Remove lag variables and Moving averages variables
str(CameAccFinal)
#data.frame':	52 obs. of  88 variables
colnames(CameAccFinal)
# 18, 19 are list_price and promotion_offered
# 74 to 88 are lag and MA variables

MultiCameAccDF <- CameAccFinal[,-c(18:19,74:88)]
colnames(MultiCameAccDF)


## To avoid log (0) issue, replace 0 with '0.00001' in df
MultiCameAccDF[MultiCameAccDF == 0] <- 0.00001

##  Convert values to log for Multiplicative model
MultiCameAccDF <- log(MultiCameAccDF)

## Linear relationship or multicollinearity
MultiCAModel <- lm(gmv~.,MultiCameAccDF)
alias(MultiCAModel)

#WeekDay.xSunday                   -37219/10567             234905/1376917              26694/55015
#WeekDay.xThursday                144896/169377                  -326/2225     198167945/1213635448
#WeekDay.xTuesday                  -10402/11385            -328239/1426147             59831/195546
#WeekDay.xWednesday              -290841/140689                  -761/5062               3909/22117
#eventFlag                        -642791/13406             1185696/571423         -6964171/1720138
#event.xBSD                                   0                          0                        0
#event.xChristmas...NY            -189977/13697          25029769/29095614           -119509/202929
#event.xDaussera                  1123078/88535                  1439/2277             -60245/66091
#event.xDiwali                       55992/5947                 -4211/3059               -1540/3321
#event.xEid...Rath              -4595789/187025              177104/104013       -31629107/31461089
#event.xFHSD                -165598493/14410847               89797/177884           -888814/833489
#event.xIndep.Day                  -97261/92824               10103/207746          -132424/2927471
#event.xNon.event.Day        -37979694/18737161               28511/151786                1982/3347
#event.xPacman                    387433/192941             119674/1020567             -49923/65966
#event.xRakshabandhan                  994/1291              -5813/1767676             15905/939224
#event.xRepublic.Day          -25380959/1447043                  -887/3805                1349/6951
#event.xValentine.Day             -368490/22559                -6373/21348                 753/2854
#price_mark.xPREMIUM                          0                          0                        0
#price_mark.xPROMISING      



colnames(MultiCameAccDF)
## Remove linear or multicollinear relationship variables
MultiCameAccDF <- MultiCameAccDF[, -c(53:71)]

### Use Stepwise Regression to remove insignificant and correlated variables
MultiCA_base.mod <- lm(gmv ~ 1 , data= MultiCameAccDF)  
MultiCA_all.mod <- lm(gmv ~ . , data= MultiCameAccDF) 
MultiCA_stepMod <- step(MultiCA_base.mod, scope = list(lower = MultiCA_base.mod, upper = MultiCA_all.mod), direction = "both", trace = 1, steps = 1000) 
MultiCA_shortlistedVars <- names(unlist(MultiCA_stepMod[[1]])) 
MultiCA_shortlistedVars <- MultiCA_shortlistedVars[!MultiCA_shortlistedVars %in% "(Intercept)"]  

show(MultiCA_shortlistedVars)
#[[1] "product_mrp"                                     "s1_fact.order_payment_type"                      "product_procurement_sla"                        
#[4] "product_analytic_vertical.xCameraTripod"         "product_analytic_vertical.xCameraBattery"        "product_analytic_vertical.xCameraMount"         
#[7] "units"                                           "Radio"                                           "Other_adstock"                                  
#[10] "Sponsorship_adstock"                             "Content_Marketing_adstock"                       "SEM"                                            
#[13] "Affiliates"                                      "product_analytic_vertical.xReflectorUmbrella"    "product_analytic_vertical.xCameraMicrophone"    
#[16] "Affiliates_adstock"                              "product_analytic_vertical.xCameraHousing"        "Other"                                          
#[19] "product_analytic_vertical.xExtensionTube"        "product_analytic_vertical.xFilter"               "Digital"                                        
#[22] "product_analytic_vertical.xTelescope"            "Content_Marketing"                               "NPS_Score"                                      
#[25] "Online_Marketing"                                "product_analytic_vertical.xCameraEyeCup"         "Digital_adstock"                                
#[28] "Sponsorship"                                     "product_analytic_vertical.xLens"                 "product_analytic_vertical.xSoftbox"             
#[31] "TV"                                              "product_analytic_vertical.xCameraBatteryGrip"    "product_analytic_vertical.xTeleconverter"       
#[34] "product_analytic_vertical.xStrap"                "product_analytic_vertical.xCameraBatteryCharger" "product_analytic_vertical.xCameraFilmRolls"     
#[37] "product_analytic_vertical.xCameraAccessory"      "product_analytic_vertical.xFlash"                "TV_adstock"                                     
#[40] "WeekDay.xMonday"                                 "product_analytic_vertical.xCameraBag"            "sla"                                            
#[43] "Total_Investment"                                "Radio_adstock"                                   "product_analytic_vertical.xFlashShoeAdapter"    
#[46] "Week"                                            "Online_Marketing_adstock"                        "product_analytic_vertical.xCameraRemoteControl" 
#[49] "product_analytic_vertical.xCameraLEDLight"       "SEM_adtock"                                      "WeekDay.xSaturday"


## First Model- removed 'Other' also
MultiCAModel_1 <- lm(formula = gmv ~ product_mrp + s1_fact.order_payment_type + product_procurement_sla +
                       product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                       units + Radio + Other_adstock + 
                       Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                       Affiliates + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                       Affiliates_adstock +product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube +  product_analytic_vertical.xFilter + Digital +   
                       product_analytic_vertical.xTelescope + Content_Marketing + NPS_Score+ 
                       Online_Marketing + product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship + product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox+
                       TV+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                       product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+
                       product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+
                       WeekDay.xMonday+ product_analytic_vertical.xCameraBag+ sla+
                       Total_Investment+ Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       Week+ Online_Marketing_adstock+ product_analytic_vertical.xCameraRemoteControl+
                       product_analytic_vertical.xCameraLEDLight+ SEM_adtock+ WeekDay.xSaturday,
                       data = MultiCameAccDF)


summary(MultiCAModel_1)
vif(MultiCAModel_1)

#remove special days
MultiCAModel_2 <- lm(formula = gmv ~ product_mrp + s1_fact.order_payment_type + product_procurement_sla +
                       product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                       units + Radio  + Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                       Affiliates + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                       Affiliates_adstock +product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube +  product_analytic_vertical.xFilter + Digital +   
                       product_analytic_vertical.xTelescope + Content_Marketing + NPS_Score+ 
                       Online_Marketing + product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship + product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox+
                       TV+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                       product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+
                       product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+
                       product_analytic_vertical.xCameraBag+ sla+
                       Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       Online_Marketing_adstock+ product_analytic_vertical.xCameraRemoteControl+
                       product_analytic_vertical.xCameraLEDLight+ SEM_adtock,
                     data = MultiCameAccDF)


summary(MultiCAModel_2)
vif(MultiCAModel_2)
#high vif: mrp
MultiCAModel_3 <- lm(formula = gmv ~ s1_fact.order_payment_type + product_procurement_sla +
                       product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                       units + Radio  + Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                       Affiliates + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                       Affiliates_adstock +product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube +  product_analytic_vertical.xFilter + Digital +   
                       product_analytic_vertical.xTelescope + Content_Marketing + NPS_Score+ 
                       Online_Marketing + product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship + product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox+
                       TV+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                       product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+
                       product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+
                       product_analytic_vertical.xCameraBag+ sla+
                       Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       Online_Marketing_adstock+ product_analytic_vertical.xCameraRemoteControl+
                       product_analytic_vertical.xCameraLEDLight+ SEM_adtock,
                     data = MultiCameAccDF)


summary(MultiCAModel_3)
vif(MultiCAModel_3)

#high vif: units
MultiCAModel_4 <- lm(formula = gmv ~ s1_fact.order_payment_type + product_procurement_sla +
                       product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                       Radio  + Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                       Affiliates + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                       Affiliates_adstock +product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube +  product_analytic_vertical.xFilter + Digital +   
                       product_analytic_vertical.xTelescope + Content_Marketing + NPS_Score+ 
                       Online_Marketing + product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship + product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox+
                       TV+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                       product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+
                       product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+
                       product_analytic_vertical.xCameraBag+ sla+
                       Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       Online_Marketing_adstock+ product_analytic_vertical.xCameraRemoteControl+
                       product_analytic_vertical.xCameraLEDLight+ SEM_adtock,
                     data = MultiCameAccDF)


summary(MultiCAModel_4)
vif(MultiCAModel_4)

#high vif: Online_Marketing, 
MultiCAModel_5 <- lm(formula = gmv ~ s1_fact.order_payment_type + product_procurement_sla +
                       product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                       Radio  + Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                       Affiliates + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                       Affiliates_adstock +product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube +  product_analytic_vertical.xFilter + Digital +   
                       product_analytic_vertical.xTelescope + Content_Marketing + NPS_Score+ 
                       product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship + product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox+
                       TV+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                       product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+
                       product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+
                       product_analytic_vertical.xCameraBag+ sla+
                       Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       Online_Marketing_adstock+ product_analytic_vertical.xCameraRemoteControl+
                       product_analytic_vertical.xCameraLEDLight+ SEM_adtock,
                     data = MultiCameAccDF)


summary(MultiCAModel_5)
vif(MultiCAModel_5)


#high vif:  
MultiCAModel_6 <- lm(formula = gmv ~ s1_fact.order_payment_type + product_procurement_sla +
                       product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                       Radio  + Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                       Affiliates + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                       Affiliates_adstock +product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube +  product_analytic_vertical.xFilter + Digital +   
                       product_analytic_vertical.xTelescope + Content_Marketing + NPS_Score+ 
                       product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship + product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox+
                       TV+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                       product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+
                       product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+
                       product_analytic_vertical.xCameraBag+ sla+
                       Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       product_analytic_vertical.xCameraRemoteControl+
                       product_analytic_vertical.xCameraLEDLight+ SEM_adtock,
                     data = MultiCameAccDF)


summary(MultiCAModel_6)
vif(MultiCAModel_6)

#high vif:  
MultiCAModel_7 <- lm(formula = gmv ~  product_procurement_sla +
                       product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                       Radio  + Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                       Affiliates + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                       Affiliates_adstock +product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube  + Digital +   
                       product_analytic_vertical.xTelescope + Content_Marketing + NPS_Score+ 
                       product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship + product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox+
                       TV+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                       product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+
                       product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+sla+
                       Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       product_analytic_vertical.xCameraLEDLight+ SEM_adtock,
                     data = MultiCameAccDF)


summary(MultiCAModel_7)
vif(MultiCAModel_7)

#high vif:  
MultiCAModel_8 <- lm(formula = gmv ~  product_procurement_sla +
                       product_analytic_vertical.xCameraTripod  + product_analytic_vertical.xCameraMount +
                       Radio  + Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                       Affiliates + product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                       Affiliates_adstock +product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube  + Digital +Content_Marketing + NPS_Score+ 
                       product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship + product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox+
                       TV+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                       TV_adstock+sla+ Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       product_analytic_vertical.xCameraLEDLight+ SEM_adtock,
                     data = MultiCameAccDF)


summary(MultiCAModel_8)
vif(MultiCAModel_8)

#high vif:  
MultiCAModel_9 <- lm(formula = gmv ~  product_procurement_sla +
                       product_analytic_vertical.xCameraTripod +
                       Radio  + Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                        product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                       Affiliates_adstock +product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube  + Digital  + 
                       product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship  + product_analytic_vertical.xSoftbox+
                       product_analytic_vertical.xTeleconverter+
                       TV_adstock+sla+ Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       product_analytic_vertical.xCameraLEDLight+ SEM_adtock,
                     data = MultiCameAccDF)


summary(MultiCAModel_9)
vif(MultiCAModel_9)
#high vif:  
MultiCAModel_10 <- lm(formula = gmv ~  product_procurement_sla +
                       product_analytic_vertical.xCameraTripod +
                       Radio  + Sponsorship_adstock+ Content_Marketing_adstock+ SEM+ 
                       product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                        product_analytic_vertical.xCameraHousing +                    
                       product_analytic_vertical.xExtensionTube  + Digital  + 
                       product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                       Sponsorship  + product_analytic_vertical.xSoftbox+
                       product_analytic_vertical.xTeleconverter+
                       sla+ Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                       product_analytic_vertical.xCameraLEDLight,
                     data = MultiCameAccDF)


summary(MultiCAModel_10)
vif(MultiCAModel_10)
#High vif
MultiCAModel_11 <- lm(formula = gmv ~  product_procurement_sla +
                        product_analytic_vertical.xCameraTripod +
                        Radio  + Sponsorship_adstock+ Content_Marketing_adstock + 
                        product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraMicrophone +
                        product_analytic_vertical.xCameraHousing +                    
                        product_analytic_vertical.xExtensionTube  + 
                        product_analytic_vertical.xCameraEyeCup + Digital_adstock + 
                        Sponsorship  + product_analytic_vertical.xSoftbox+
                        product_analytic_vertical.xTeleconverter+
                        sla+ Radio_adstock+ product_analytic_vertical.xFlashShoeAdapter+
                        product_analytic_vertical.xCameraLEDLight,
                      data = MultiCameAccDF)


summary(MultiCAModel_11)
vif(MultiCAModel_11)

#high p
MultiCAModel_12 <- lm(formula = gmv ~  product_procurement_sla +
                        product_analytic_vertical.xCameraTripod +
                        Radio  + Sponsorship_adstock+ Content_Marketing_adstock + 
                        product_analytic_vertical.xCameraHousing +                    
                        product_analytic_vertical.xExtensionTube + Digital_adstock + 
                        Sponsorship  +sla+ product_analytic_vertical.xFlashShoeAdapter,
                      data = MultiCameAccDF)


summary(MultiCAModel_12)
vif(MultiCAModel_12)

#high p
MultiCAModel_13 <- lm(formula = gmv ~  product_procurement_sla + product_analytic_vertical.xCameraTripod +
                        Content_Marketing_adstock +Sponsorship  +sla,
                      data = MultiCameAccDF)


summary(MultiCAModel_13)
vif(MultiCAModel_13)


### Cross-validation  - Model 13
cv.lm(data = MultiCameAccDF, form.lm = MultiCAModel_13, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating the elasticity coefficients- Model 9

elasticity_MultiCA <- function(var){
  MultiCA_elasticity <- as.numeric(MultiCAModel_13$coefficients[var]*mean(MultiCameAccDF[,var])/mean(MultiCameAccDF$gmv))
  return(MultiCA_elasticity)
  
} 

MultiCA_var_list <- list()

for(i in 2:length(MultiCAModel_13$coefficients)){
  MultiCA_var_list[i-1] <- elasticity_MultiCA(names(MultiCAModel_13$coefficients)[i])
  
}

MultiCA_elasticity.outputs <- data.frame(names(MultiCAModel_13$coefficients[2:length(MultiCAModel_13$coefficients)]))
MultiCA_elasticity.outputs <- cbind(MultiCA_elasticity.outputs,do.call(rbind.data.frame, MultiCA_var_list))
colnames(MultiCA_elasticity.outputs) <- c("Variable","Elasticity")

MultiCA_elasticity.outputs$Direction <- ifelse(MultiCA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(MultiCA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust=0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Camera Accessory - Multiplicative Model") +xlab("Variables")  

#######Model Type 2 Ends########################################################


################################ Model Type 3 ##################################
########################### Distribuited Lag Model #############################



# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# keeping Week# 1, 2 and 3  lag value of 'gmv' and removing other lag variables and Moving averages variables
str(CameAccFinal)
#data.frame':	52 obs. of  88 variables
colnames(CameAccFinal)
# 18, 19 are list_price and promotion_offered
# 74 to 88 are lag and MA variables
# Keeping GMV_lag_1_per", "GMV_lag_2_per" ,"GMV_lag_3_per"  (column# 86,87,88)

DLCameAccDF <- CameAccFinal[,-c(18:19,74:85)]
colnames(DLCameAccDF)

## Scaling the variables
DLCameAccDF[,2:ncol(DLCameAccDF)] <- scale(DLCameAccDF[,2:ncol(DLCameAccDF)])

### Step to remove insignificant and correlated variables

# Base intercept model
DLCABase<- lm(gmv ~ 1 , data= DLCameAccDF)
# Complete model all variables
DLCAAll<- lm(gmv ~ . , data= DLCameAccDF) 
summary(DLCAAll)

DLCAStep <- step(DLCABase, scope = list(lower = DLCABase, upper = DLCAAll), direction = "both", trace = 1, steps = 1000)
DLSelectedVars <- names(unlist(DLCAStep[[1]])) 

# Remove Intercepts
DLSelectedVars <- DLSelectedVars[!DLSelectedVars %in% "(Intercept)"]  


show(DLSelectedVars)
#[1] "product_mrp"                                     "units"                                          
#[3] "product_analytic_vertical.xCameraTripod"         "product_analytic_vertical.xCameraMount"         
#[5] "NPS_Score"                                       "product_analytic_vertical.xCameraBattery"       
#[7] "Radio"                                           "product_analytic_vertical.xReflectorUmbrella"   
#[9] "product_analytic_vertical.xSoftbox"              "WeekDay.xSaturday"                              
#[11] "Online_Marketing"                                "product_analytic_vertical.xExtensionTube"       
#[13] "product_analytic_vertical.xCameraFilmRolls"      "SEM_adtock"                                     
#[15] "product_analytic_vertical.xTeleconverter"        "event.xDaussera"                                
#[17] "product_analytic_vertical.xCameraBatteryGrip"    "product_analytic_vertical.xTelescope"           
#[19] "WeekDay.xThursday"                               "Week"                                           
#[21] "event.xChristmas...NY"                           "Affiliates"                                     
#[23] "product_analytic_vertical.xStrap"                "GMV_lag_3_per"                                  
#[25] "price_mark.xPROMISING"                           "WeekDay.xTuesday"                               
#[27] "event.xEid...Rath"                               "event.xPacman"                                  
#[29] "WeekDay.xWednesday"                              "event.xFHSD"                                    
#[31] "product_analytic_vertical.xCameraBag"            "event.xValentine.Day"                           
#[33] "product_analytic_vertical.xFlashShoeAdapter"     "Radio_adstock"                                  
#[35] "Content_Marketing_adstock"                       "Sponsorship"                                    
#[37] "Other_adstock"                                   "event.xRakshabandhan"                           
#[39] "product_analytic_vertical.xCameraMicrophone"     "event.xIndep.Day"                               
#[41] "product_analytic_vertical.xCameraEyeCup"         "Sponsorship_adstock"                            
#[43] "product_analytic_vertical.xCameraAccessory"      "product_analytic_vertical.xCameraBatteryCharger"
#[45] "event.xRepublic.Day"                             "WeekDay.xSunday"                                
#[47] "product_analytic_vertical.xCameraRemoteControl"  "WeekDay.xMonday"                                
#[49] "product_analytic_vertical.xCameraHousing"        "product_analytic_vertical.xCameraLEDLight"      
#[51] "sla" 

### Build model

## Iteration 1: Removed Other_Adstock also
DLCAModel_1 <- lm(formula = gmv ~ units + product_mrp + product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ 
                    NPS_Score + product_analytic_vertical.xCameraBattery + Radio+ 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + WeekDay.xSaturday +
                    Online_Marketing +product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ 
                    SEM_adtock+ product_analytic_vertical.xTeleconverter+ event.xDaussera+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ WeekDay.xThursday+
                    Week+ event.xChristmas...NY+ Affiliates+ product_analytic_vertical.xStrap+ GMV_lag_3_per+
                    price_mark.xPROMISING+ WeekDay.xTuesday+ event.xEid...Rath+ event.xPacman+WeekDay.xWednesday+ event.xFHSD+
                    product_analytic_vertical.xCameraBag+ event.xValentine.Day+ product_analytic_vertical.xFlashShoeAdapter+
                    Radio_adstock+ Content_Marketing_adstock+ Sponsorship+ event.xRakshabandhan+ 
                    product_analytic_vertical.xCameraMicrophone+ event.xIndep.Day+ product_analytic_vertical.xCameraEyeCup+ 
                    Sponsorship_adstock+ product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xCameraBatteryCharger+
                    event.xRepublic.Day+ WeekDay.xSunday+ product_analytic_vertical.xCameraRemoteControl+ WeekDay.xMonday+
                    product_analytic_vertical.xCameraHousing+ product_analytic_vertical.xCameraLEDLight+ sla
                    , data = DLCameAccDF)


summary(DLCAModel_1)
vif(DLCAModel_1)


## Remove special days
DLCAModel_2 <- lm(formula = gmv ~ units + product_mrp + product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ 
                    NPS_Score + product_analytic_vertical.xCameraBattery + Radio+ 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox  +
                    Online_Marketing +product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ 
                    SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                     Affiliates+ product_analytic_vertical.xStrap+ GMV_lag_3_per+
                    price_mark.xPROMISING+ 
                    product_analytic_vertical.xCameraBag+ + product_analytic_vertical.xFlashShoeAdapter+
                    Radio_adstock+ Content_Marketing_adstock+ Sponsorship+ + 
                    product_analytic_vertical.xCameraMicrophone+ + product_analytic_vertical.xCameraEyeCup+ 
                    Sponsorship_adstock+ product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xCameraBatteryCharger+
                    product_analytic_vertical.xCameraRemoteControl+ +
                    product_analytic_vertical.xCameraHousing+ product_analytic_vertical.xCameraLEDLight+ sla
                  , data = DLCameAccDF)


summary(DLCAModel_2)
vif(DLCAModel_2)

#High vif
DLCAModel_3 <- lm(formula = gmv ~ units + product_mrp + product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ 
                    NPS_Score + product_analytic_vertical.xCameraBattery + Radio+ 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox  +
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ 
                    SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    Affiliates+ product_analytic_vertical.xStrap+ GMV_lag_3_per+
                    price_mark.xPROMISING+ 
                    product_analytic_vertical.xCameraBag+ + product_analytic_vertical.xFlashShoeAdapter+
                    Radio_adstock+ Content_Marketing_adstock+ Sponsorship+ + 
                    product_analytic_vertical.xCameraMicrophone+ + product_analytic_vertical.xCameraEyeCup+ 
                    Sponsorship_adstock+ product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xCameraBatteryCharger+
                    product_analytic_vertical.xCameraRemoteControl+ +
                    product_analytic_vertical.xCameraHousing+ product_analytic_vertical.xCameraLEDLight+ sla
                  , data = DLCameAccDF)


summary(DLCAModel_3)
vif(DLCAModel_3)

#High vif
DLCAModel_4 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ 
                    NPS_Score + product_analytic_vertical.xCameraBattery + Radio+ 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox  +
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ 
                    SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    Affiliates+ product_analytic_vertical.xStrap+ GMV_lag_3_per+ price_mark.xPROMISING+ 
                    product_analytic_vertical.xCameraBag+ + product_analytic_vertical.xFlashShoeAdapter+
                    Radio_adstock+ Sponsorship+ + 
                    product_analytic_vertical.xCameraMicrophone+ + product_analytic_vertical.xCameraEyeCup+ 
                    Sponsorship_adstock+ product_analytic_vertical.xCameraAccessory+
                    product_analytic_vertical.xCameraRemoteControl+ +
                    product_analytic_vertical.xCameraHousing+ product_analytic_vertical.xCameraLEDLight+ sla
                  , data = DLCameAccDF)


summary(DLCAModel_4)
vif(DLCAModel_4)

#High vif
DLCAModel_5 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ 
                    NPS_Score  + Radio+ 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox  +
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ 
                    SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+
                    Affiliates+ product_analytic_vertical.xStrap+ GMV_lag_3_per + 
                    product_analytic_vertical.xCameraBag+ + product_analytic_vertical.xFlashShoeAdapter+
                    Radio_adstock+ Sponsorship+ + 
                    product_analytic_vertical.xCameraMicrophone+ + product_analytic_vertical.xCameraEyeCup+ 
                    Sponsorship_adstock+ product_analytic_vertical.xCameraAccessory+
                    product_analytic_vertical.xCameraHousing+ product_analytic_vertical.xCameraLEDLight+ sla
                  , data = DLCameAccDF)


summary(DLCAModel_5)
vif(DLCAModel_5)

#High vif
DLCAModel_6 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ Radio+ 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox  +
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ 
                    SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+
                    product_analytic_vertical.xStrap+ GMV_lag_3_per + 
                    product_analytic_vertical.xCameraBag+ + product_analytic_vertical.xFlashShoeAdapter+
                    Radio_adstock+ Sponsorship+ + 
                    product_analytic_vertical.xCameraMicrophone+ + product_analytic_vertical.xCameraEyeCup+ 
                    Sponsorship_adstock+ product_analytic_vertical.xCameraAccessory+
                    product_analytic_vertical.xCameraHousing+ product_analytic_vertical.xCameraLEDLight+ sla
                  , data = DLCameAccDF)


summary(DLCAModel_6)
vif(DLCAModel_6)

#High p
DLCAModel_7 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ Radio+ 
                    product_analytic_vertical.xReflectorUmbrella   +
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ 
                    SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+
                    product_analytic_vertical.xStrap+ GMV_lag_3_per + 
                    product_analytic_vertical.xCameraBag+ + product_analytic_vertical.xFlashShoeAdapter+
                    Radio_adstock + product_analytic_vertical.xCameraEyeCup+ 
                    Sponsorship_adstock+
                    product_analytic_vertical.xCameraHousing+ product_analytic_vertical.xCameraLEDLight+ sla
                  , data = DLCameAccDF)


summary(DLCAModel_7)
vif(DLCAModel_7)

#High p
DLCAModel_8 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ Radio+ 
                    product_analytic_vertical.xReflectorUmbrella   +
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ 
                    SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+
                    product_analytic_vertical.xStrap+ GMV_lag_3_per + 
                    product_analytic_vertical.xCameraBag+
                    Sponsorship_adstock+ product_analytic_vertical.xCameraLEDLight
                  , data = DLCameAccDF)


summary(DLCAModel_8)
vif(DLCAModel_8)

#High p
DLCAModel_9 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ Radio+ 
                    product_analytic_vertical.xCameraFilmRolls+ 
                    product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+
                    product_analytic_vertical.xStrap+ GMV_lag_3_per + 
                    product_analytic_vertical.xCameraBag
                  , data = DLCameAccDF)


summary(DLCAModel_9)
vif(DLCAModel_9)


#High p
DLCAModel_10 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ 
                    product_analytic_vertical.xCameraFilmRolls+ 
                    product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip + 
                    product_analytic_vertical.xCameraBag
                  , data = DLCameAccDF)


summary(DLCAModel_10)
vif(DLCAModel_10)

#High p
DLCAModel_11 <- lm(formula = gmv ~  product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ 
                     product_analytic_vertical.xTeleconverter + 
                     product_analytic_vertical.xCameraBag
                   , data = DLCameAccDF)


summary(DLCAModel_11)
vif(DLCAModel_11)


### Cross-validation
cv.lm(data = DLCameAccDF, form.lm = DLCAModel_11, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)


### Estimating the elasticity coefficients

elasticity_DL <- function(var){
  DL_elasticity <- as.numeric(DLCAModel_11$coefficients[var]*mean(DLCameAccDF[,var])/mean(DLCameAccDF$gmv))
  return(DL_elasticity)
  
} 

DL_var_list <- list()

for(i in 2:length(DLCAModel_11$coefficients)){
  DL_var_list[i-1] <- elasticity_DL(names(DLCAModel_11$coefficients)[i])
  
}

DL_elasticity.outputs <- data.frame(names(DLCAModel_11$coefficients[2:length(DLCAModel_11$coefficients)]))
DL_elasticity.outputs <- cbind(DL_elasticity.outputs,do.call(rbind.data.frame, DL_var_list))
colnames(DL_elasticity.outputs) <- c("Variable","Elasticity")

DL_elasticity.outputs$Direction <- ifelse(DL_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(DL_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Camera Accessory - Distributed Lag Model") +xlab("Variables")  


#################### Model 3: Distributed Lag -End ##########




################################ Model Type 4 ####################################
###########################   Koyck Model  #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# keeping Week# 1 'gmv' and removing other lag variables and Moving averages variables
str(CameAccFinal)
#data.frame':	52 obs. of  88 variables
colnames(CameAccFinal)
# 18, 19 are list_price and promotion_offered
# 74 to 88 are lag and MA variables
# Keeping GMV_lag_1_per" (column# 86)

KYCameAccDF <- CameAccFinal[,-c(18:19,74:85,87:88)]
colnames(KYCameAccDF)


## Scaling the variables
KYCameAccDF[,2:ncol(KYCameAccDF)] <- scale(KYCameAccDF[,2:ncol(KYCameAccDF)])


### Stepwise Regression to remove insignificant and correlated variables
KMCA_base.mod <- lm(gmv ~ 1 , data= KYCameAccDF)  # base intercept only model
KMCA_all.mod <- lm(gmv ~ . , data= KYCameAccDF) # full model with all predictors
KMCA_stepMod <- step(KMCA_base.mod, scope = list(lower = KMCA_base.mod, upper = KMCA_all.mod), direction = "both", trace = 1, steps = 1000) 
KMCA_shortlistedVars <- names(unlist(KMCA_stepMod[[1]])) 
KMCA_shortlistedVars <- KMCA_shortlistedVars[!KMCA_shortlistedVars %in% "(Intercept)"]  

show(KMCA_shortlistedVars)
# [1] "product_mrp"                                    "units"                                          "product_analytic_vertical.xCameraTripod"       
#[4] "product_analytic_vertical.xCameraMount"         "NPS_Score"                                      "product_analytic_vertical.xCameraBattery"      
#[7] "Radio"                                          "product_analytic_vertical.xReflectorUmbrella"   "product_analytic_vertical.xSoftbox"            
#[10] "WeekDay.xSaturday"                              "Online_Marketing"                               "Digital_adstock"                               
#[13] "product_analytic_vertical.xExtensionTube"       "product_analytic_vertical.xCameraFilmRolls"     "SEM_adtock"                                    
#[16] "product_analytic_vertical.xTeleconverter"       "event.xDaussera"                                "product_analytic_vertical.xCameraBatteryGrip"  
#[19] "product_analytic_vertical.xTelescope"           "s1_fact.order_payment_type"                     "WeekDay.xThursday"                             
#[22] "Week"                                           "event.xChristmas...NY"                          "Affiliates"                                    
#[25] "product_analytic_vertical.xStrap"               "product_analytic_vertical.xCameraRemoteControl" "product_analytic_vertical.xCameraBag"          
#[28] "price_mark.xPROMISING"                          "event.xFHSD"                                    "WeekDay.xTuesday"                              
#[31] "GMV_lag_1_per"                                  "product_analytic_vertical.xCameraEyeCup"        "product_analytic_vertical.xCameraHousing"      
#[34] "event.xPacman"                                  "WeekDay.xWednesday"                             "event.xNon.event.Day"                          
#[37] "product_analytic_vertical.xFlash"               "product_analytic_vertical.xFlashShoeAdapter"    "product_analytic_vertical.xFilter"             
#[40] "Online_Marketing_adstock"                       "event.xEid...Rath"                              "WeekDay.xSunday"                               
#[43] "TV_adstock"                                     "Other"                                          "WeekDay.xMonday"                               
#[46] "Radio_adstock"                                  "TV"                                             "Total_Investment"                              
#[49] "event.xIndep.Day"                               "Other_adstock"                                  "sla"  


### Model Building:Koyck

## Model 1- Ignore Other, Other_adstock, Week
KYCAModel_1 <- lm(formula = gmv ~ units + product_mrp + product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ WeekDay.xSaturday+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    event.xDaussera+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ s1_fact.order_payment_type+ WeekDay.xThursday+ 
                    Week+ event.xChristmas...NY+ Affiliates+ product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ 
                    price_mark.xPROMISING+ event.xFHSD+ WeekDay.xTuesday+ GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    event.xPacman+ WeekDay.xWednesday+ event.xNon.event.Day+ product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+ event.xEid...Rath+ WeekDay.xSunday+ TV_adstock+ WeekDay.xMonday+
                    Radio_adstock+ TV+ Total_Investment+ event.xIndep.Day+ sla,
                      data = KYCameAccDF)


summary(KYCAModel_1)
vif(KYCAModel_1)

# remove high vif: units
KYCAModel_2 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ WeekDay.xSaturday+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    event.xDaussera+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ s1_fact.order_payment_type+ WeekDay.xThursday+ 
                    Week+ event.xChristmas...NY+ Affiliates+ product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ 
                    price_mark.xPROMISING+ event.xFHSD+ WeekDay.xTuesday+ GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    event.xPacman+ WeekDay.xWednesday+ event.xNon.event.Day+ product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+ event.xEid...Rath+ WeekDay.xSunday+ TV_adstock+ WeekDay.xMonday+
                    Radio_adstock+ TV+ Total_Investment+ event.xIndep.Day+ sla,
                  data = KYCameAccDF)


summary(KYCAModel_2)
vif(KYCAModel_2)

# remove high vif: Affiliates
KYCAModel_3 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ WeekDay.xSaturday+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    event.xDaussera+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ s1_fact.order_payment_type+ WeekDay.xThursday+ 
                    Week+ event.xChristmas...NY+ product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ 
                    price_mark.xPROMISING+ event.xFHSD+ WeekDay.xTuesday+ GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    event.xPacman+ WeekDay.xWednesday+ event.xNon.event.Day+ product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+ event.xEid...Rath+ WeekDay.xSunday+ TV_adstock+ WeekDay.xMonday+
                    Radio_adstock+ TV+ Total_Investment+ event.xIndep.Day+ sla,
                  data = KYCameAccDF)


summary(KYCAModel_3)
vif(KYCAModel_3)

# remove high vif: mrp
KYCAModel_4 <- lm(formula = gmv ~ product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ WeekDay.xSaturday+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    event.xDaussera+ product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ s1_fact.order_payment_type+ WeekDay.xThursday+ 
                    Week+ event.xChristmas...NY+ product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ 
                    price_mark.xPROMISING+ event.xFHSD+ WeekDay.xTuesday+ GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    event.xPacman+ WeekDay.xWednesday+ event.xNon.event.Day+ product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+ event.xEid...Rath+ WeekDay.xSunday+ TV_adstock+ WeekDay.xMonday+
                    Radio_adstock+ TV+ Total_Investment+ event.xIndep.Day+ sla,
                  data = KYCameAccDF)


summary(KYCAModel_4)
vif(KYCAModel_4)


# remove events, special days, week, total investment
KYCAModel_5 <- lm(formula = gmv ~ product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ SEM_adtock+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ s1_fact.order_payment_type+ 
                    product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ 
                    price_mark.xPROMISING+ GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+ TV_adstock+
                    Radio_adstock+ TV+ sla,
                  data = KYCameAccDF)


summary(KYCAModel_5)
vif(KYCAModel_5)


# high vif:SEM_adtock
KYCAModel_6 <- lm(formula = gmv ~ product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ s1_fact.order_payment_type+ 
                    product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ 
                    price_mark.xPROMISING+ GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+ TV_adstock+
                    Radio_adstock+ TV+ sla,
                  data = KYCameAccDF)


summary(KYCAModel_6)
vif(KYCAModel_6)


# high vif: price_mark.xPROMISING
KYCAModel_7 <- lm(formula = gmv ~ product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ s1_fact.order_payment_type+ 
                    product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraRemoteControl+ product_analytic_vertical.xCameraBag+ 
                    GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+ TV_adstock+
                    Radio_adstock+ TV+ sla,
                  data = KYCameAccDF)


summary(KYCAModel_7)
vif(KYCAModel_7)

# high vif: product_analytic_vertical.xCameraBag, TV_adstock
KYCAModel_8 <- lm(formula = gmv ~ product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ NPS_Score+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ s1_fact.order_payment_type+ 
                    product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraRemoteControl+ 
                    GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+
                    Radio_adstock+ TV+ sla,
                  data = KYCameAccDF)


summary(KYCAModel_8)
vif(KYCAModel_8)

# high vif
KYCAModel_9 <- lm(formula = gmv ~ product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTelescope+ s1_fact.order_payment_type+ 
                    product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraRemoteControl+ 
                    GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+
                    Radio_adstock+ TV+ sla,
                  data = KYCameAccDF)


summary(KYCAModel_9)
vif(KYCAModel_9)

# high vif: product_analytic_vertical.xTelescope, product_analytic_vertical.xCameraRemoteControl
KYCAModel_10 <- lm(formula = gmv ~ product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraMount+ product_analytic_vertical.xCameraBattery + 
                    Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ Online_Marketing+ Digital_adstock+ 
                    product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xCameraFilmRolls+ product_analytic_vertical.xTeleconverter+ 
                    product_analytic_vertical.xCameraBatteryGrip+ s1_fact.order_payment_type+ 
                    product_analytic_vertical.xStrap+ 
                    GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                    product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                    product_analytic_vertical.xFilter+ Online_Marketing_adstock+
                    Radio_adstock+ TV+ sla,
                  data = KYCameAccDF)


summary(KYCAModel_10)
vif(KYCAModel_10)

# high vif
KYCAModel_11 <- lm(formula = gmv ~ product_analytic_vertical.xCameraTripod  + 
                     Radio+ product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xSoftbox+ Online_Marketing+ Digital_adstock+ 
                     product_analytic_vertical.xExtensionTube+ product_analytic_vertical.xTeleconverter+ 
                     product_analytic_vertical.xCameraBatteryGrip+ s1_fact.order_payment_type+ 
                     product_analytic_vertical.xStrap+ 
                     GMV_lag_1_per+ product_analytic_vertical.xCameraEyeCup+ product_analytic_vertical.xCameraHousing+
                     product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                     product_analytic_vertical.xFilter+ Online_Marketing_adstock+
                     Radio_adstock+ TV+ sla,
                   data = KYCameAccDF)


summary(KYCAModel_11)
vif(KYCAModel_11)

# high p: products
KYCAModel_12 <- lm(formula = gmv ~ product_analytic_vertical.xCameraTripod  + 
                     Radio+ product_analytic_vertical.xReflectorUmbrella+ Online_Marketing+ Digital_adstock+ 
                     product_analytic_vertical.xTeleconverter+ 
                     product_analytic_vertical.xCameraBatteryGrip+ s1_fact.order_payment_type+ 
                     GMV_lag_1_per+ product_analytic_vertical.xCameraHousing+
                     product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                     product_analytic_vertical.xFilter+ Online_Marketing_adstock+
                     Radio_adstock+ TV+ sla,
                   data = KYCameAccDF)


summary(KYCAModel_12)
vif(KYCAModel_12)

# high p
KYCAModel_13 <- lm(formula = gmv ~
                     Radio+ product_analytic_vertical.xReflectorUmbrella+ Online_Marketing+ Digital_adstock+ 
                     product_analytic_vertical.xTeleconverter+ 
                     product_analytic_vertical.xCameraBatteryGrip+ s1_fact.order_payment_type+ 
                     product_analytic_vertical.xCameraHousing+
                     product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                     product_analytic_vertical.xFilter+ Online_Marketing_adstock+
                     Radio_adstock+ TV+ sla,
                   data = KYCameAccDF)


summary(KYCAModel_13)
vif(KYCAModel_13)

# high p
KYCAModel_14 <- lm(formula = gmv ~
                     Radio+ Online_Marketing+ Digital_adstock+ 
                     product_analytic_vertical.xTeleconverter+ 
                     product_analytic_vertical.xCameraBatteryGrip+ 
                     product_analytic_vertical.xCameraHousing+
                     product_analytic_vertical.xFlash+ product_analytic_vertical.xFlashShoeAdapter+ 
                     product_analytic_vertical.xFilter+ Online_Marketing_adstock+
                     Radio_adstock+ TV+ sla,
                   data = KYCameAccDF)


summary(KYCAModel_14)
vif(KYCAModel_14)


# high p: 
KYCAModel_15 <- lm(formula = gmv ~
                     Radio+ Online_Marketing+ Digital_adstock+ 
                     product_analytic_vertical.xTeleconverter+ 
                     product_analytic_vertical.xCameraBatteryGrip+ 
                     product_analytic_vertical.xCameraHousing+
                     product_analytic_vertical.xFlash+ 
                     product_analytic_vertical.xFilter+
                     Radio_adstock+ TV+ sla,
                   data = KYCameAccDF)


summary(KYCAModel_15)
vif(KYCAModel_15)

# high p: 
KYCAModel_16 <- lm(formula = gmv ~
                     Radio+ Online_Marketing+ Digital_adstock+ 
                     product_analytic_vertical.xTeleconverter+ 
                     product_analytic_vertical.xCameraBatteryGrip+ 
                     product_analytic_vertical.xCameraHousing+
                     product_analytic_vertical.xFlash+ 
                     product_analytic_vertical.xFilter+
                     TV,
                   data = KYCameAccDF)


summary(KYCAModel_16)
vif(KYCAModel_16)

# high p: 
KYCAModel_17 <- lm(formula = gmv ~
                     Radio+ Online_Marketing+ Digital_adstock+ 
                     product_analytic_vertical.xTeleconverter+ 
                     product_analytic_vertical.xCameraBatteryGrip+ 
                     product_analytic_vertical.xCameraHousing+
                     product_analytic_vertical.xFlash+ 
                     product_analytic_vertical.xFilter,
                   data = KYCameAccDF)


summary(KYCAModel_17)
vif(KYCAModel_17)

# high p: 
KYCAModel_18 <- lm(formula = gmv ~
                     Online_Marketing+ Digital_adstock+ 
                     product_analytic_vertical.xTeleconverter+ 
                     product_analytic_vertical.xCameraBatteryGrip+ 
                     product_analytic_vertical.xCameraHousing+
                     product_analytic_vertical.xFlash+ 
                     product_analytic_vertical.xFilter,
                   data = KYCameAccDF)


summary(KYCAModel_18)
vif(KYCAModel_18)

### Cross-validation
cv.lm(data = KYCameAccDF, form.lm = KYCAModel_18, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 



### Estimating the elasticity coefficients


elasticity_KY <- function(var){
  KY_elasticity <- as.numeric(KYCAModel_18$coefficients[var]*mean(KYCameAccDF[,var])/mean(KYCameAccDF$gmv))
  return(KY_elasticity)
  
} 

KY_var_list <- list()

for(i in 2:length(KYCAModel_11$coefficients)){
  KY_var_list[i-1] <- elasticity_KY(names(KYCAModel_18$coefficients)[i])
  
}

KY_elasticity.outputs <- data.frame(names(KYCAModel_18$coefficients[2:length(KYCAModel_18$coefficients)]))
KY_elasticity.outputs <- cbind(KY_elasticity.outputs,do.call(rbind.data.frame, KY_var_list))
colnames(KY_elasticity.outputs) <- c("Variable","Elasticity")

KY_elasticity.outputs$Direction <- ifelse(KY_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(KY_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Camer Accessory - Koyck Model") +xlab("Variables")  



#######Model Type 4 Koyck Model Ends########################################################


################################ Model Type 5 ####################################
###########################   Multiplicative model + Distributed lag  Model  #############################

# For modeling let us remove derived variables
# list_price, promotion_offered are gmv dependent
# keeping Week# 1,2,3 'gmv' and removing other lag variables and Moving averages variables
str(CameAccFinal)
#data.frame':	52 obs. of  88 variables
colnames(CameAccFinal)
# 74 to 88 are lag and MA variables
# Keeping GMV_lag_1_per, GMV_lag_2_per, GMV_lag_3_per  (column# 86, 87, 88)

MuDiCameAccDF <- CameAccFinal[,-c(18:19,74:85)]
colnames(MuDiCameAccDF)


## Replace 0 value  with '0.00001' as log(0) is undefined
MuDiCameAccDF[MuDiCameAccDF == 0] <- 0.00001

## Tranform  negative values
MuDiCameAccDF$GMV_lag_1_per <- 1 + MuDiCameAccDF$GMV_lag_1_per - min(MuDiCameAccDF$GMV_lag_1_per)
MuDiCameAccDF$GMV_lag_2_per <- 1 + MuDiCameAccDF$GMV_lag_2_per - min(MuDiCameAccDF$GMV_lag_2_per)
MuDiCameAccDF$GMV_lag_3_per <- 1 + MuDiCameAccDF$GMV_lag_3_per - min(MuDiCameAccDF$GMV_lag_3_per)

## Take log for Multiplicative model
MuDiCameAccDF <- log(MuDiCameAccDF)

## Checking the variables for linear relationship or multicollinearity
MuDiCAModel <- lm(gmv~.,MuDiCameAccDF)
alias(MuDiCAModel)
## Below varabiles found collienarity with other variables so remove these
#eWeekDay.xSunday                 -418362/621949                            -582751/67598            
#WeekDay.xThursday               275375/2407787                              30241/23349            
#WeekDay.xTuesday                439115/2791473                            207631/224911            
#WeekDay.xWednesday               82229/2332402                              -2066/13383            
#eventFlag                          -9281/11887                            -416671/16627            
#event.xBSD                       506461/299327                                        0            
#event.xChristmas...NY          -296497/1307957                          -1774121/227865            
#event.xDaussera                  25761/1375031                              23839/16982            
#event.xDiwali                          971/627                             280437/13061            
#event.xEid...Rath                -79151/255759                         -12220596/609269            
#event.xFHSD                         -1781/5948                             -88295/19274            
#event.xIndep.Day                   -1339/58224                               -1091/1754            
#event.xNon.event.Day    -7774997757/8743685939                            -259337/23144            
#event.xPacman                 -3854700/2306537                              -24257/5574            
#event.xRakshabandhan                    14/517                             98396/597243            
#event.xRepublic.Day          -13834716/9298645                              -16762/1619            
#event.xValentine.Day                 1937/4076                             149663/31085            
#price_mark.xPREMIUM                          1                                        0            
#price_mark.xPROMISING          -96229/10172165                           46464/11496695
colnames(MuDiCameAccDF)

## Removing the variables which were showing linear relationship or multicollinearity
MuDiCameAccDF <- MuDiCameAccDF[, -c(53:71)]
colnames(MuDiCameAccDF)

### Stepwise Regression to remove insignificant and correlated variables
MuDiCA_base.mod <- lm(gmv ~ 1 , data= MuDiCameAccDF)  # base intercept only model
MuDiCA_all.mod <- lm(gmv ~ . , data= MuDiCameAccDF) # full model with all predictors
MuDiCA_stepMod <- step(MuDiCA_base.mod, scope = list(lower = MuDiCA_base.mod, upper = MuDiCA_all.mod), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
MuDiCA_shortlistedVars <- names(unlist(MuDiCA_stepMod[[1]])) # get the shortlisted variable.
MuDiCA_shortlistedVars <- MuDiCA_shortlistedVars[!MuDiCA_shortlistedVars %in% "(Intercept)"]  # remove intercept

show(MuDiCA_shortlistedVars)
#[1] "product_mrp"                                     "s1_fact.order_payment_type"                      "product_procurement_sla"                        
#[4] "product_analytic_vertical.xCameraTripod"         "product_analytic_vertical.xCameraBattery"        "product_analytic_vertical.xCameraMount"         
#[7] "units"                                           "Radio"                                           "Other_adstock"                                  
#[10] "Sponsorship_adstock"                             "Content_Marketing_adstock"                       "SEM"                                            
#[13] "Affiliates"                                      "product_analytic_vertical.xReflectorUmbrella"    "product_analytic_vertical.xCameraMicrophone"    
#[16] "Affiliates_adstock"                              "product_analytic_vertical.xCameraHousing"        "Other"                                          
#[19] "product_analytic_vertical.xExtensionTube"        "product_analytic_vertical.xFilter"               "Digital"                                        
#[22] "product_analytic_vertical.xTelescope"            "Content_Marketing"                               "NPS_Score"                                      
#[25] "Online_Marketing"                                "product_analytic_vertical.xCameraEyeCup"         "Digital_adstock"                                
#[28] "Sponsorship"                                     "product_analytic_vertical.xLens"                 "product_analytic_vertical.xSoftbox"             
#[31] "TV"                                              "product_analytic_vertical.xCameraBatteryGrip"    "product_analytic_vertical.xTeleconverter"       
#[34] "product_analytic_vertical.xStrap"                "product_analytic_vertical.xCameraBatteryCharger" "product_analytic_vertical.xCameraFilmRolls"     
#[37] "product_analytic_vertical.xCameraAccessory"      "product_analytic_vertical.xFlash"                "TV_adstock"                                     
#[40] "WeekDay.xMonday"                                 "GMV_lag_2_per"                                   "GMV_lag_1_per"                                  
#[43] "Total_Investment"                                "product_analytic_vertical.xFlashShoeAdapter"     "Online_Marketing_adstock"                       
#[46] "SEM_adtock"                                      "sla"                                             "product_analytic_vertical.xCameraLEDLight"      
#[49] "GMV_lag_3_per"                                   "Week"                                            "Radio_adstock"    


### Model Building::

## Model 1: Removed Other and other related variables
MuDiCAmodel_1 <- lm(formula = gmv ~ product_mrp + s1_fact.order_payment_type  + product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                    units + Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      Affiliates + product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                    Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + Digital+
                    product_analytic_vertical.xTelescope+ Content_Marketing+ NPS_Score+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                      product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+ 
                      product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+                                    
                    WeekDay.xMonday+ GMV_lag_2_per+ GMV_lag_1_per+                                  
                    Total_Investment+ product_analytic_vertical.xFlashShoeAdapter+ Online_Marketing_adstock+                       
                    SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+       
                    GMV_lag_3_per+ Week+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_1)
vif(MuDiCAmodel_1)

#High vif:units
MuDiCAmodel_2 <- lm(formula = gmv ~ product_mrp + s1_fact.order_payment_type  + product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                      Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      Affiliates + product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                      Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + Digital+
                      product_analytic_vertical.xTelescope+ Content_Marketing+ NPS_Score+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                      product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+ 
                      product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+                                    
                      WeekDay.xMonday+ GMV_lag_2_per+ GMV_lag_1_per+                                  
                      Total_Investment+ product_analytic_vertical.xFlashShoeAdapter+ Online_Marketing_adstock+                       
                      SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+       
                      GMV_lag_3_per+ Week+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_2)
vif(MuDiCAmodel_2)


#Remove events
MuDiCAmodel_3 <- lm(formula = gmv ~ product_mrp + s1_fact.order_payment_type  + product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                      Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      Affiliates + product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                      Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + Digital+
                      product_analytic_vertical.xTelescope+ Content_Marketing+ NPS_Score+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                      product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+ 
                      product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+                                    
                      GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+ Online_Marketing_adstock+                       
                      SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_3)
vif(MuDiCAmodel_3)

# high vif
MuDiCAmodel_4 <- lm(formula = gmv ~ s1_fact.order_payment_type  + product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                      Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      Affiliates + product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                      Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + Digital+
                      product_analytic_vertical.xTelescope+ Content_Marketing+ NPS_Score+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                      product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+ 
                      product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+                                    
                      GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+ Online_Marketing_adstock+                       
                      SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_4)
vif(MuDiCAmodel_4)

# high vif
MuDiCAmodel_5 <- lm(formula = gmv ~ s1_fact.order_payment_type  + product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                      Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                      Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + Digital+
                      product_analytic_vertical.xTelescope+ Content_Marketing+ NPS_Score+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                      product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+ 
                      product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+                                    
                      GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+ Online_Marketing_adstock+                       
                      SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_5)
vif(MuDiCAmodel_5)

# high vif
MuDiCAmodel_6 <- lm(formula = gmv ~   product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                      Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                      Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + Digital+
                      product_analytic_vertical.xTelescope+ Content_Marketing+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                      product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+ 
                      product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+                                    
                      GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+ Online_Marketing_adstock+                       
                      SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_6)
vif(MuDiCAmodel_6)

# high vif
MuDiCAmodel_7 <- lm(formula = gmv ~   product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod + product_analytic_vertical.xCameraBattery + product_analytic_vertical.xCameraMount +
                      Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                      Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + Digital+
                      product_analytic_vertical.xTelescope+ Content_Marketing+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                      product_analytic_vertical.xStrap+ product_analytic_vertical.xCameraBatteryCharger+ product_analytic_vertical.xCameraFilmRolls+ 
                      product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+                                    
                      GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+                       
                      SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_7)
vif(MuDiCAmodel_7)

# high vif
MuDiCAmodel_8 <- lm(formula = gmv ~   product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod  + product_analytic_vertical.xCameraMount +
                      Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                      Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube  + Digital+
                      product_analytic_vertical.xTelescope+ Content_Marketing+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                      product_analytic_vertical.xCameraFilmRolls+ 
                      product_analytic_vertical.xCameraAccessory+ product_analytic_vertical.xFlash+ TV_adstock+                                    
                      GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+                       
                      SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_8)
vif(MuDiCAmodel_8)

# high vif
MuDiCAmodel_9 <- lm(formula = gmv ~   product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod  + product_analytic_vertical.xCameraMount +
                      Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                      Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube  + Digital+
                      product_analytic_vertical.xTelescope+ Content_Marketing+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+
                      product_analytic_vertical.xCameraFilmRolls+TV_adstock+                                    
                      GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+                       
                      SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_9)
vif(MuDiCAmodel_9)

# high vif
MuDiCAmodel_10 <- lm(formula = gmv ~   product_procurement_sla + 
                      product_analytic_vertical.xCameraTripod  + product_analytic_vertical.xCameraMount +
                      Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                      product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                      Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                      product_analytic_vertical.xExtensionTube  + Digital+ Content_Marketing+ 
                      Online_Marketing+ product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                      Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                      TV + product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+TV_adstock+                                    
                      GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+                       
                      SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                    data = MuDiCameAccDF)


summary(MuDiCAmodel_10)
vif(MuDiCAmodel_10)


# high vif
MuDiCAmodel_11 <- lm(formula = gmv ~   product_procurement_sla + 
                       product_analytic_vertical.xCameraTripod   +
                       Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                       product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                       Affiliates_adstock+ product_analytic_vertical.xCameraHousing+ 
                       product_analytic_vertical.xExtensionTube  + Digital+ Content_Marketing+ 
                       product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                       Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                       product_analytic_vertical.xCameraBatteryGrip+ product_analytic_vertical.xTeleconverter+TV_adstock+                                    
                       GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+                       
                       SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                     data = MuDiCameAccDF)


summary(MuDiCAmodel_11)
vif(MuDiCAmodel_11)

# high vif
MuDiCAmodel_12 <- lm(formula = gmv ~   product_procurement_sla + 
                       product_analytic_vertical.xCameraTripod   +
                       Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                       product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                        product_analytic_vertical.xCameraHousing+ 
                       product_analytic_vertical.xExtensionTube  + Digital+ Content_Marketing+ 
                       product_analytic_vertical.xCameraEyeCup+ Digital_adstock+ 
                       Sponsorship+ product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                       product_analytic_vertical.xTeleconverter+                                    
                       GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+                       
                       SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                     data = MuDiCameAccDF)


summary(MuDiCAmodel_12)
vif(MuDiCAmodel_12)

# high vif
MuDiCAmodel_13 <- lm(formula = gmv ~   product_procurement_sla + 
                       product_analytic_vertical.xCameraTripod   +
                       Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                       product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                       product_analytic_vertical.xCameraHousing+ 
                       product_analytic_vertical.xExtensionTube  + Digital+ 
                       product_analytic_vertical.xCameraEyeCup+ 
                       product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                       product_analytic_vertical.xTeleconverter+                                    
                       GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+                       
                       SEM_adtock+ sla+ product_analytic_vertical.xCameraLEDLight+ GMV_lag_3_per+ Radio_adstock,
                     data = MuDiCameAccDF)


summary(MuDiCAmodel_13)
vif(MuDiCAmodel_13)

# high p
MuDiCAmodel_14 <- lm(formula = gmv ~   product_procurement_sla + 
                       product_analytic_vertical.xCameraTripod   +
                       Radio  + Sponsorship_adstock + Content_Marketing_adstock + SEM + 
                       product_analytic_vertical.xReflectorUmbrella+ product_analytic_vertical.xCameraMicrophone+
                       product_analytic_vertical.xCameraHousing+ 
                       product_analytic_vertical.xExtensionTube  + Digital+ 
                       product_analytic_vertical.xCameraEyeCup+ 
                       product_analytic_vertical.xLens+ product_analytic_vertical.xSoftbox+
                       product_analytic_vertical.xTeleconverter+                                    
                       GMV_lag_2_per+ GMV_lag_1_per+ product_analytic_vertical.xFlashShoeAdapter+                       
                       SEM_adtock+ sla+ Radio_adstock,
                     data = MuDiCameAccDF)


summary(MuDiCAmodel_14)
vif(MuDiCAmodel_14)

# high p
MuDiCAmodel_15 <- lm(formula = gmv ~   product_procurement_sla +  product_analytic_vertical.xCameraTripod   +
                      Content_Marketing_adstock +  product_analytic_vertical.xCameraMicrophone+
                       product_analytic_vertical.xCameraHousing+product_analytic_vertical.xCameraEyeCup+ 
                       product_analytic_vertical.xLens+ product_analytic_vertical.xTeleconverter+ sla+ Radio_adstock,
                     data = MuDiCameAccDF)


summary(MuDiCAmodel_15)
vif(MuDiCAmodel_15)

# high p
MuDiCAmodel_16 <- lm(formula = gmv ~   product_procurement_sla +  product_analytic_vertical.xCameraTripod   +
                       Content_Marketing_adstock+ product_analytic_vertical.xCameraHousing+ 
                       product_analytic_vertical.xLens + sla+ Radio_adstock,
                     data = MuDiCameAccDF)


summary(MuDiCAmodel_16)
vif(MuDiCAmodel_16)

# high p
MuDiCAmodel_17 <- lm(formula = gmv ~   product_procurement_sla +  product_analytic_vertical.xCameraTripod   +
                       Content_Marketing_adstock+ product_analytic_vertical.xCameraHousing+ 
                       product_analytic_vertical.xLens +Radio_adstock,
                     data = MuDiCameAccDF)


summary(MuDiCAmodel_17)
vif(MuDiCAmodel_17)



### Cross-validation
cv.lm(data = MuDiCameAccDF, form.lm = MuDiCAmodel_17, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 



### Estimating the elasticity coefficients


elasticity_DuMi <- function(var){
  MDCA_elasticity <- as.numeric(MuDiCAmodel_17$coefficients[var]*mean(MuDiCameAccDF[,var])/mean(MuDiCameAccDF$gmv))
  return(MDCA_elasticity)
} 

MDCA_var_list <- list()

for(i in 2:length(MuDiCAmodel_17$coefficients)){
  MDCA_var_list[i-1] <- elasticity_DuMi(names(MuDiCAmodel_17$coefficients)[i])
  
}

MDCA_elasticity.outputs <- data.frame(names(MuDiCAmodel_17$coefficients[2:length(MuDiCAmodel_17$coefficients)]))
MDCA_elasticity.outputs <- cbind(MDCA_elasticity.outputs,do.call(rbind.data.frame, MDCA_var_list))
colnames(MDCA_elasticity.outputs) <- c("Variable","Elasticity")

MDCA_elasticity.outputs$Direction <- ifelse(MDCA_elasticity.outputs$Elasticity > 0, "Positive", "Negative")


# Plotting the elasticity
ggplot(MDCA_elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() + 
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("Camera Accessory - Multiplicative and Distributed Lag Model") +xlab("Variables")  



