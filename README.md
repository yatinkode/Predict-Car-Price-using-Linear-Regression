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
