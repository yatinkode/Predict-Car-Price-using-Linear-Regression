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
