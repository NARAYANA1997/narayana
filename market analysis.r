#LOading data
salesData <- read.csv("C:/Users/venka/OneDrive/Desktop/MINI PROJECT/salesData.csv")


#install packages and load packages
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)

# Structure of dataset
str(salesData, give.attr = FALSE)
summary(salesData)

# Visualization of correlations
salesData %>% select_if(is.numeric) %>%
  select(-id) %>%
  cor() %>% corrplot()


# Frequent stores
ggplot(salesData) +
  geom_boxplot(aes(x = mostFreqStore, y = salesThisMon))

# Preferred brand
ggplot(salesData) +
  geom_boxplot(aes(x = preferredBrand, y = salesThisMon))

# Model specification using lm
salesSimpleModel <- lm(salesThisMon ~ salesLast3Mon, 
                       data = salesData)
# Looking at model summary
summary(salesSimpleModel)
plot(salesSimpleModel)

ggplot(salesData,aes(salesLast3Mon,salesThisMon))+
  geom_point()+
  geom_smooth(method=lm,se=FALSE)+
  xlab("saleslast3months")+
  ylab("salesthismonth")

#Multiple linear regression

MultipleLM <- lm(salesThisMon ~ salesLast3Mon+id+nItems+mostFreqStore+
                   mostFreqCat+nCats+preferredBrand+nBrands+nPurch+salesLast3Mon+
                   salesThisMon+daysSinceLastPurch+meanItemPrice+meanShoppingCartValue+
                   customerDuration,data=salesData)
summary(MultipleLM)
library(rms)
vif(MultipleLM)

# Estimating the full model
salesModel1 <- lm(salesThisMon ~ . - id, 
                  data = salesData)

# Checking variance inflation factors
vif(salesModel1)

# Estimating new model by removing information on brand
salesModel2 <- lm(salesThisMon ~ . - id-preferredBrand-nBrands, 
                  data = salesData)

# Checking variance inflation factors
vif(salesModel2)
library(stats)
AIC(MultipleLM)
library(MASS)
stepAIC(MultipleLM)

# getting an overview of new data
salesData2_4 <- read.csv("C:/Users/venka/OneDrive/Desktop/MINI PROJECT/salesData2_4.csv")
head(salesData2_4)
summary(salesData2_4)
# predicting sales
predSales5 <- predict(salesModel2, newdata = salesData2_4)

# calculating mean of future sales
mean(predSales5 ,na.rm=TRUE)


