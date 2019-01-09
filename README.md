# Predict-Car-Price-using-Linear-Regression
Predict Price of various cars depending of various using Linear Regression - Machine Learning in R

You are required to model the price of cars with the available independent variables. It will be used by the management to understand how exactly the prices vary with the independent variables. They can accordingly manipulate the design of the cars, the business strategy etc. to meet certain price levels. Further, the model will be a good way for the management to understand the pricing dynamics of a new market.

#### I have created a frontend based application using php to run the finalized model. Please view the below video

[![Watch the video](https://github.com/yatinkode/Predict-Car-Price-using-Linear-Regression/blob/main/images/image.png)](https://dms.licdn.com/playback/C5105AQHJv2DHuc9M5Q/cd832393b7694826be8f82b65e941084/feedshare-mp4_500-captions-thumbnails/1507940118923-hysdc8?e=1547056800&v=beta&t=arsyUiDoK_lqSX_OO1rr4ghYXB1ukUb7fLiSnbFusVg)

## R code to create Linear regression Model
### Loading required libraries
```R
load.libraries <- c('tidyr','dplyr','MASS','car','ggplot2','reshape2')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
```

### Loading Data
```R
#load dataframe from file
carprice<-read.csv("CarPrice_Assignment.csv")
```
### Data Understanding
```R
str(carprice)

summary(carprice)

sum(is.na(carprice))    #no NA values in dataframe
```

### Data Cleaning
```R
#we ignore car id since it is not going to help us in any way in our regression


#Checking levels of categorical variables

#Checking symboling variable
unique(carprice$symboling)            #3  1  2  0 -1 -2

carprice$symboling<-factor(carprice$symboling)   #Levels: -2 -1 0 1 2 3

#Checking CarName variable

unique(carprice$CarName)        #Car name is divided into 2 parts "carcompany carmodel" we only need car company for regression test so we extract car company here

#Separating out car company from car names
carprice<-separate(carprice,CarName,into="company",sep=" ",remove=T)

#Checking unique car companies
unique(carprice$company)

#we have found that some companies are in upper case, so changing case to lower case
carprice$company<-tolower(carprice$company)

#toyota and toyouta are same, correct spelling is toyota
carprice$company<-gsub(pattern="toyouta",x=carprice$company,replacement="toyota")

#porsche and porcshce are same, correct spelling is porsche
carprice$company<-gsub(pattern="porcshce",x=carprice$company,replacement="porsche")

#maxda and mazda are same, correct spelling is mazda
carprice$company<-gsub(pattern="maxda",x=carprice$company,replacement="mazda")

#vokswagen,vw and volkswagen are same, correct spelling is volkswagen
carprice$company<-gsub(pattern="vokswagen|vw",x=carprice$company,replacement="volkswagen")

#Checking the unique car companies after doing the changes
unique(carprice$company)

#"alfa-romero" "audi"        "bmw"         "chevrolet"   "dodge"       "honda"       "isuzu"      
#"jaguar"      "mazda"       "buick"       "mercury"     "mitsubishi"  "nissan"      "peugeot"    
#"plymouth"    "porsche"     "renault"     "saab"        "subaru"      "toyota"      "volkswagen"   "volvo"

carprice$company<-factor(carprice$company)  #22 Levels: alfa-romero audi bmw buick chevrolet dodge honda isuzu jaguar mazda mercury ... volvo



#Checking fueltype variable
unique(carprice$fueltype)            #Levels: diesel gas

#checking aspiration variable
unique(carprice$aspiration)          #Levels: std turbo

#checking doornumber variable
unique(carprice$doornumber)          #Levels: four two

#checking carbody variable
unique(carprice$carbody)             #Levels: convertible hardtop hatchback sedan wagon

#checking drivewheel variable
unique(carprice$drivewheel)          #Levels: 4wd fwd rwd

#checking enginelocation variable
unique(carprice$enginelocation)      #Levels: front rear


#checking enginetype variable
unique(carprice$enginetype)          #Levels: dohc dohcv l ohc ohcf ohcv rotor

#dohcv and dohc are same , dohc is correct abbr.
carprice$enginetype<-gsub(pattern="dohcv",x=carprice$enginetype,replacement="dohc")

#change to factor
carprice$enginetype<-factor(carprice$enginetype)   #Levels: dohc l ohc ohcf ohcv rotor

#checking cylindernumber variable
unique(carprice$cylindernumber)      #Levels: eight five four six three twelve two

#checking fuelsystem variable
unique(carprice$fuelsystem)          #Levels: 1bbl 2bbl 4bbl idi mfi mpfi spdi spfi

#mfi and mpfi are same , mpfi is correct abbr.
carprice$fuelsystem<-gsub(pattern="mfi",x=carprice$fuelsystem,replacement="mpfi")

carprice$fuelsystem<-factor(carprice$fuelsystem)   #Levels: 1bbl 2bbl 4bbl idi mpfi spdi spfi
```
### Checking continuous variables vs price
```R
#Making a correlation matrix of all continuous variables including price
cormat <- carprice[, c(10:14,17,19:26)]

str(cormat)
cormat <- round(cor(cormat),2)
head(cormat)

#Function to convert lower triangle to NA
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)


ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#F55F22", high = "#259B55", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))+ coord_fixed()

ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1,title.position = "top", title.hjust = 0.5))+
  ggtitle("Correlation plot of continuous variables in Car Prices dataset")
```
[![data](https://github.com/yatinkode/Predict-Car-Price-using-Linear-Regression/blob/main/images/corrplot.png)]

#### Creating derived variable
```R
# From correlation plot we get to know, there is higher positive correlation between 

#1. citympg and highwaympg are almost nearer to each other so we take mean on citympg and highwaympg and create variable mpg
carprice$mpg<-as.numeric(apply(carprice[,24:25],1,mean))
carprice<-within(carprice,rm(citympg,highwaympg))


#2. carlength and curbweight (0.88). So we can create a single variable as a ratio (carweight/Carlength)
carprice$wtbylen<-as.numeric(carprice$curbweight/carprice$carlength)

#Also we need to remove curbweight and carlength variables
carprice<-within(carprice,rm(carlength,curbweight))
```
#### Creating dummy variables
```R
dummy_symboling<- data.frame(model.matrix( ~symboling, data = carprice))                   #create dummy variables for car symboling
dummy_symboling <- dummy_symboling[,-1]                                                     #remove x -intercept column from dummy variables
carprice1 <- cbind(within(carprice, rm(symboling)), dummy_symboling)                                        #attach dummy variables with main data frame

dummy_company <- data.frame(model.matrix( ~company, data = carprice))                   #create dummy variables for car companies
dummy_company <- dummy_company[,-1]                                                     #remove x -intercept column from dummy variables
carprice1 <- cbind(within(carprice1, rm(company)), dummy_company)                                        #attach dummy variables with main data frame

levels(carprice1$fueltype)<-c(0,1)                                                      #converting 1 for gas and 0 for diesel in fueltype
carprice1$fueltypegas<- as.numeric(levels(carprice1$fueltype))[carprice1$fueltype]      #convert levels to number and store it in a column fueltypegas
carprice1<-within(carprice1, rm(fueltype))                                              #remove fueltype column

levels(carprice1$aspiration)<-c(0,1)                                                            #converting 1 for turbo and 0 for std in aspiration
carprice1$aspirationturbo<- as.numeric(levels(carprice1$aspiration))[carprice1$aspiration]      #convert levels to number and store it in a column aspirationturbo
carprice1<-within(carprice1, rm(aspiration))                                                    #remove aspiration column

levels(carprice1$doornumber)<-c(0,1)                                                            #converting 1 for two and 0 for four in doornumber
carprice1$doornumbertwo<- as.numeric(levels(carprice1$doornumber))[carprice1$doornumber]        #convert levels to number and store it in a column doornumbertwo
carprice1<-within(carprice1, rm(doornumber))                                                    #remove doornumber column

dummy_carbody <- data.frame(model.matrix( ~carbody, data = carprice))                   #create dummy variables for car bodies
dummy_carbody <- dummy_carbody[,-1]                                                     #remove x -intercept column from dummy variables
carprice1 <- cbind(within(carprice1, rm(carbody)), dummy_carbody)                                       #attach dummy variables with main data frame

dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = carprice))                   #create dummy variables for car bodies
dummy_drivewheel <- dummy_drivewheel[,-1]                                                     #remove x -intercept column from dummy variables
carprice1 <- cbind(within(carprice1, rm(drivewheel)), dummy_drivewheel)                                          #attach dummy variables with main data frame

levels(carprice1$enginelocation)<-c(1,0)                                                             #converting 1 for front and 0 for rear in enginelocation
carprice1$enginefront<- as.numeric(levels(carprice1$enginelocation))[carprice1$enginelocation]        #convert levels to number and store it in a column enginelocationfront
carprice1<-within(carprice1, rm(enginelocation))                                                     #remove enginelocation column

dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = carprice))                   #create dummy variables for car bodies
dummy_enginetype <- dummy_enginetype[,-1]                                                     #remove x -intercept column from dummy variables
carprice1 <- cbind(within(carprice1, rm(enginetype)), dummy_enginetype)                                          #attach dummy variables with main data frame

dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = carprice))                   #create dummy variables for cylindernumber
dummy_cylindernumber <- dummy_cylindernumber[,-1]                                                     #remove x -intercept column from dummy variables
carprice1 <- cbind(within(carprice1, rm(cylindernumber)) , dummy_cylindernumber)                      #attach dummy variables with main data frame

dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = carprice))                   #create dummy variables for cylindernumber
dummy_fuelsystem <- dummy_fuelsystem[,-1]                                                     #remove x -intercept column from dummy variables
carprice1 <- cbind(within(carprice1, rm(fuelsystem)) , dummy_fuelsystem)                      #attach dummy variables with main data frame

#Since car_Id is of no use in regression we can remove it
carprice1<-within(carprice1, rm(car_ID))
```

### Split data into train and test
```R
set.seed(100)
trainindices= sample(1:nrow(carprice1), 0.7*nrow(carprice1))

train = carprice1[trainindices,]
test = carprice1[-trainindices,]
```

### Modelling
```R
#Backward selection process. make model with all independent variables
model_1<-lm(price~.,data=train)
summary(model_1)
#Multiple R-squared:  0.981,	Adjusted R-squared:  0.9686 

#StepAIC method to get relevant variables
step<-stepAIC(model_1, direction = "both")

step

#Created model from StepAIC recommendation
model_2<-lm(formula = price ~ carwidth + enginesize + stroke + peakrpm + 
              wtbylen + symboling3 + companybmw + companybuick + companydodge + 
              companyhonda + companyjaguar + companymazda + companymercury + 
              companymitsubishi + companynissan + companypeugeot + companyplymouth + 
              companyrenault + companysaab + companysubaru + companytoyota + 
              companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + drivewheelrwd + enginefront + 
              enginetypeohc + enginetypeohcv + enginetyperotor + cylindernumberfive + symboling1, data = train)


summary(model_2)
#Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9729 

vif(model_2)
# Since vif of carbodysedan is 17.796421 which is the highest and pvalue  is 0.017510 which is very insignificant, so we drop carbodysedan

model_3<-lm(formula = price ~ carwidth + enginesize + stroke + peakrpm + 
              wtbylen + symboling3 + companybmw + companybuick + companydodge + 
              companyhonda + companyjaguar + companymazda + companymercury + 
              companymitsubishi + companynissan + companypeugeot + companyplymouth + 
              companyrenault + companysaab + companysubaru + companytoyota + 
              companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginefront + 
              enginetypeohc + enginetypeohcv + enginetyperotor + cylindernumberfive + symboling1, data = train)

summary(model_3)
#Multiple R-squared:  0.9783,	Adjusted R-squared:  0.9717 
#The adjusted rsquared is not changed much, so the model looks good till now

vif(model_3)
#wtbylen has pvalue 0.003235  and very high VIF of 10.971204 , so we will try removing it in the next model

model_4<-lm(formula = price ~ carwidth + enginesize + stroke + peakrpm + 
              symboling3 + companybmw + companybuick + companydodge + 
              companyhonda + companyjaguar + companymazda + companymercury + 
              companymitsubishi + companynissan + companypeugeot + companyplymouth + 
              companyrenault + companysaab + companysubaru + companytoyota + 
              companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginefront + 
              enginetypeohc + enginetypeohcv + enginetyperotor + cylindernumberfive + symboling1, data = train)

summary(model_4)
#Multiple R-squared:  0.9765,	Adjusted R-squared:  0.9696

vif(model_4)
#removing enginetypeohc with pvalue 0.012796 and vif of 5.652061 

model_5<-lm(formula = price ~ carwidth + enginesize + stroke + peakrpm + 
              symboling3 + companybmw + companybuick + companydodge + 
              companyhonda + companyjaguar + companymazda + companymercury + 
              companymitsubishi + companynissan + companypeugeot + companyplymouth + 
              companyrenault + companysaab + companysubaru + companytoyota + 
              companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginefront + 
              enginetypeohcv + enginetyperotor + cylindernumberfive + symboling1, data = train)

summary(model_5)
#Multiple R-squared:  0.9751,	Adjusted R-squared:  0.9682 

vif(model_5)
#removing enginetypeohcv   with  p-value 0.624891 and vif 2.668468    

model_6<-lm(formula = price ~ carwidth + enginesize + stroke + peakrpm + 
              symboling3 + companybmw + companybuick + companydodge + 
              companyhonda + companyjaguar + companymazda + companymercury + 
              companymitsubishi + companynissan + companypeugeot + companyplymouth + 
              companyrenault + companysaab + companysubaru + companytoyota + 
              companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginefront + enginetyperotor + cylindernumberfive + 
              symboling1, data = train)

summary(model_6)
#Multiple R-squared:  0.9751,	Adjusted R-squared:  0.9684

#symboling3   pvalue is 0.477570 and vif is 2.406865 

vif(model_6)
#remove symboling3

model_7<-lm(formula = price ~ carwidth + enginesize + stroke + peakrpm + 
              companybmw + companybuick + companydodge + 
              companyhonda + companyjaguar + companymazda + companymercury + 
              companymitsubishi + companynissan + companypeugeot + companyplymouth + 
              companyrenault + companysaab + companysubaru + companytoyota + 
              companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginefront + enginetyperotor + cylindernumberfive + 
              symboling1, data = train)

summary(model_7)
#Multiple R-squared:  0.9749,	Adjusted R-squared:  0.9685 

vif(model_7)
#remove cylindernumberfive  of p-value 0.049170 and vif of 2.182294

model_8<-lm(formula = price ~ carwidth + enginesize + stroke + peakrpm + 
              companybmw + companybuick + companydodge + 
              companyhonda + companyjaguar + companymazda + companymercury + 
              companymitsubishi + companynissan + companypeugeot + companyplymouth + 
              companyrenault + companysaab + companysubaru + companytoyota + 
              companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginefront + enginetyperotor + symboling1, data = train)

summary(model_8)
#Multiple R-squared:  0.9741,	Adjusted R-squared:  0.9677  

vif(model_8)
#remove peakrpm  p-value is 0.001360 and vif 2.674326

model_9<-lm(formula = price ~ carwidth + enginesize + stroke + 
              companybmw + companybuick + companydodge + 
              companyhonda + companyjaguar + companymazda + companymercury + 
              companymitsubishi + companynissan + companypeugeot + companyplymouth + 
              companyrenault + companysaab + companysubaru + companytoyota + 
              companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginefront + enginetyperotor + symboling1, data = train)

summary(model_9)
#Multiple R-squared:  0.9716,	Adjusted R-squared:  0.965 

vif(model_9)
#remove drivewheelrwd p-value 0.006832 and vif 3.685328 

model_10<-lm(formula = price ~ carwidth + enginesize + stroke + 
               companybmw + companybuick + companydodge + 
               companyhonda + companyjaguar + companymazda + companymercury + 
               companymitsubishi + companynissan + companypeugeot + companyplymouth + 
               companyrenault + companysaab + companysubaru + companytoyota + 
               companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
               carbodywagon + enginefront + enginetyperotor + symboling1, data = train)

summary(model_10)
#Multiple R-squared:  0.9697,	Adjusted R-squared:  0.963  

vif(model_10)
#remove companyhonda  of    p-value 0.083597 and  vif 2.405263

model_11<-lm(formula = price ~ carwidth + enginesize + stroke + 
               companybmw + companybuick + companydodge + 
               companyjaguar + companymazda + companymercury + 
               companymitsubishi + companynissan + companypeugeot + companyplymouth + 
               companyrenault + companysaab + companysubaru + companytoyota + 
               companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
               carbodywagon + enginefront + enginetyperotor + symboling1, data = train)

summary(model_11)
#Multiple R-squared:  0.9689,	Adjusted R-squared:  0.9623

vif(model_11)
#remove symboling1  having  p-value   0.077507 and vif 1.57129

model_12<-lm(formula = price ~ carwidth + enginesize + stroke + 
               companybmw + companybuick + companydodge + 
               companyjaguar + companymazda + companymercury + 
               companymitsubishi + companynissan + companypeugeot + companyplymouth + 
               companyrenault + companysaab + companysubaru + companytoyota + 
               companyvolkswagen + aspirationturbo + carbodyhardtop + carbodyhatchback + 
               carbodywagon + enginefront + enginetyperotor, data = train)

summary(model_12)
#Multiple R-squared:  0.9681,	Adjusted R-squared:  0.9616 

vif(model_12)
#remove    carbodyhatchback  p-value  0.219238 vif 1.5358


model_13<-lm(formula = price ~ carwidth + enginesize + stroke + 
               companybmw + companybuick + companydodge + 
               companyjaguar + companymazda + companymercury + 
               companymitsubishi + companynissan + companypeugeot + companyplymouth + 
               companyrenault + companysaab + companysubaru + companytoyota + 
               companyvolkswagen + aspirationturbo + carbodyhardtop +
               carbodywagon + enginefront + enginetyperotor, data = train)

summary(model_13)
#Multiple R-squared:  0.9677,	Adjusted R-squared:  0.9615 

vif(model_13)
# remove   carbodyhardtop  p-value  0.238045   vif 1.390827


model_14<-lm(formula = price ~ carwidth + enginesize + stroke + 
               companybmw + companybuick + companydodge + companyjaguar + companymazda + companymercury + 
               companymitsubishi + companynissan + companypeugeot + companyplymouth + 
               companyrenault + companysaab + companysubaru + companytoyota + 
               companyvolkswagen + aspirationturbo +carbodywagon + enginefront +enginetyperotor,data =train)

summary(model_14)
#Multiple R-squared:  0.9716,	Adjusted R-squared:  0.9665  

vif(model_14)
# remove   companysaab having p-value  0.580496 and vif 1.351276

model_15<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick + 
               companydodge + companyjaguar + companymazda + companymercury + 
               companymitsubishi + companynissan + companypeugeot + companyplymouth + 
               companyrenault + companysubaru + companytoyota + companyvolkswagen + aspirationturbo +
               carbodywagon + enginefront + enginetyperotor, data = train)

summary(model_15)
#Multiple R-squared:  0.9672,	Adjusted R-squared:  0.9615

vif(model_15)
#  remove  companyvolkswagen  p-value  0.026306 vif 1.261448

model_16<-lm(formula = price ~ carwidth + enginesize + stroke + 
               companybmw + companybuick + companydodge + companyjaguar + companymazda + companymercury + 
               companymitsubishi + companynissan + companypeugeot + companyplymouth + 
               companyrenault + companysubaru + companytoyota + aspirationturbo +
               carbodywagon + enginefront + enginetyperotor, data = train)

summary(model_16)
#Multiple R-squared:  0.9659,	Adjusted R-squared:  0.9603   

vif(model_16)
# remove companyrenault    p-value  0.062271 vif 1.190991

model_17<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick + companydodge + 
               companyjaguar + companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companysubaru + companytoyota + aspirationturbo +
               carbodywagon + enginefront + enginetyperotor, data = train)

summary(model_17)
#Multiple R-squared:  0.9649,	Adjusted R-squared:  0.9594 

vif(model_17)
#Most insignificant p-value foung is of companynissan , p-value is 0.002006 vif 1.269498

model_18<-lm(formula = price ~ carwidth + enginesize + stroke +companybmw + companybuick + 
               companydodge + companyjaguar + companymazda + companymercury + companymitsubishi + 
               companypeugeot +companyplymouth + companysubaru + companytoyota + 
               aspirationturbo +carbodywagon + enginefront + enginetyperotor, data = train)

summary(model_18)
#Multiple R-squared:  0.962,	Adjusted R-squared:  0.9565  

vif(model_18)
# remove carbodywagon  having  p-value  0.910410  and  vif 1.148776


model_19<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick + companydodge + 
               companyjaguar + companymazda + companymercury + companymitsubishi + companypeugeot + companyplymouth + 
               companysubaru + companytoyota + aspirationturbo +enginefront + enginetyperotor, data = train)

summary(model_19)
#Multiple R-squared:  0.962,	Adjusted R-squared:  0.9569

vif(model_19)
# remove companyplymouth  having  p-value  0.025098 and vif 1.120359

model_20<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick + companydodge + companyjaguar+
               companymazda + companymercury + companymitsubishi + companypeugeot + companysubaru + companytoyota + 
               aspirationturbo +enginefront + enginetyperotor, data = train)

summary(model_20)
#Multiple R-squared:  0.9605,	Adjusted R-squared:  0.9554

vif(model_20)
# remove  companymazda  having p-value 0.001529 and vif 1.44038

model_21<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick + companydodge + 
               companyjaguar + companymercury + companymitsubishi + companypeugeot + companysubaru + companytoyota + 
               aspirationturbo +enginefront + enginetyperotor, data = train)

summary(model_21)
#Multiple R-squared:  0.9572,	Adjusted R-squared:  0.9521   

vif(model_21)
# remove  companypeugeot having  p-value 0.00124 and vif 1.213973 

model_22<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick + companydodge + 
               companyjaguar + companymercury + companymitsubishi +companysubaru + companytoyota + 
               aspirationturbo +enginefront + enginetyperotor, data = train)

summary(model_22)
#Multiple R-squared:  0.9535,	Adjusted R-squared:  0.9484  

vif(model_22)
# remove  companytoyota having  p-value 0.004403 and vif 1.180727 

model_23<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick + companydodge + 
               companyjaguar + companymercury + companymitsubishi +companysubaru + aspirationturbo +
               enginefront + enginetyperotor, data = train)

summary(model_23)
#Multiple R-squared:  0.9575,	Adjusted R-squared:  0.9532  

vif(model_23)
# remove  companydodge and   p-value 0.254 and vif 1.080967 

model_24<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick +companyjaguar + companymercury + 
               companymitsubishi +companysubaru + aspirationturbo +enginefront + enginetyperotor, data = train)

summary(model_24)
#Multiple R-squared:  0.9553,	Adjusted R-squared:  0.9511 

vif(model_24)
# remove  companymitsubishi having  p-value 0.0264 and vif 1.0571


model_25<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick +companyjaguar + companymercury + 
               companysubaru + aspirationturbo +enginefront + enginetyperotor, data = train)


summary(model_25)
#Multiple R-squared:  0.9527,	Adjusted R-squared:  0.9487 

vif(model_25)
#Remove companymercury having p-value 0.382 and vif 1.052109

model_26<-lm(formula = price ~ carwidth + enginesize + stroke + companybmw + companybuick +companyjaguar +
               companysubaru + aspirationturbo +enginefront + enginetyperotor, data = train)

summary(model_26)
#Multiple R-squared:  0.9477,	Adjusted R-squared:  0.9437 

vif(model_26)
#All variables are in 3 star. So we have got the nearly perfect model

###################################################################Final Model ##############################################################

#Call:
#lm(formula = price ~ carwidth + enginesize + stroke + companybmw + 
#    companybuick + companyjaguar + companysubaru + aspirationturbo + 
#    enginefront + enginetyperotor, data = train)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-4058.9 -1218.9  -394.2   942.5  4850.9 
#
#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     -37298.78    8595.60  -4.339 2.82e-05 ***
#carwidth          1017.62     142.11   7.161 5.04e-11 ***
#enginesize          98.33       8.91  11.035  < 2e-16 ***
#stroke           -4618.45     688.83  -6.705 5.35e-10 ***
#companybmw        8073.50    1176.13   6.864 2.36e-10 ***
#companybuick      6528.61     995.12   6.561 1.11e-09 ***
#companyjaguar     9403.51    1706.89   5.509 1.82e-07 ***
#companysubaru    -4234.38     945.49  -4.479 1.61e-05 ***
#aspirationturbo   2039.60     465.34   4.383 2.37e-05 ***
#enginefront     -15139.97    1396.28 -10.843  < 2e-16 ***
#enginetyperotor   5876.60    1224.88   4.798 4.27e-06 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1938 on 132 degrees of freedom
#Multiple R-squared:  0.9477,	Adjusted R-squared:  0.9437 
#F-statistic: 239.1 on 10 and 132 DF,  p-value: < 2.2e-16
```
### Model Validation
```R
#get the price by applying model_26
Predict_1<-predict(model_26,within(test, rm(price)))

#Create a new column in test data with predicted price values
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
#0.8364287
#Our model works 83.64~ 84% on the test data correctly



#Get difference between actual price and predicted price
test$error <-  test$price - test$test_price


#Plotting white noise (Difference between actual and predicted price)
ggplot(test, aes(price, error)) + geom_point()+labs(title = "White noise or error")+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))
#As seen in the plot the error is randomly distributed
```
[![data](https://github.com/yatinkode/Predict-Car-Price-using-Linear-Regression/blob/main/images/noise.png)]

```R
#Plotting line graph of Actual vs Predicted Price
ggplot()+geom_line(data=test,aes(y=price,x= seq_len(length(test_price)),color="red"),size=1)+
  geom_line(data=test,aes(y=test_price,x= seq_len(length(test_price)),color="blue"),size=1)+
  labs(x = " ", y = "Price", title = "Predicted Price vs Actual Price")+
  theme(axis.line = element_line(colour = "black"))+
  scale_color_discrete(name = "Price", labels = c("Actual Price", "Predicted Price"))
```
[![data](https://github.com/yatinkode/Predict-Car-Price-using-Linear-Regression/blob/main/images/actualpred.png)]
