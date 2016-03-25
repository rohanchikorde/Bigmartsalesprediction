##The data scientists at BigMart have collected 2013 sales data for 1559 products across 10 stores in 
##different cities. Also, certain attributes of each product and store have been defined. 
##The aim is to build a predictive model and find out the sales of each product at a particular store.
##Using this model, BigMart will try to understand the properties of products and stores which play a key
##role in increasing sales.Please note that the data may have missing values as some stores might not 
##report all the data due to technical glitches. Hence, it will be required to treat them accordingly.

library(dplyr)
library(ggplot2)
library(lubridate)
library(plyr)

data<- read.csv("Bigmart.csv",sep = ",")
View(data)
str(data)

## Scatter plots.
### Plotting Item_type vs Item_MRP
ggplot(data, aes(x=Item_Type,y = Item_MRP))+
  geom_bar(stat = "Identity", width = 0.5)

## Plotting Item_Fat_Content vs Item_Weight
ggplot(data, aes(x= Item_Fat_Content, y = Item_Weight))+
  geom_bar(stat= "Identity")


## Plotting Outlet_Location_Type vs Item_Outlet_Sales. 
ggplot(data, aes(x= Outlet_Location_Type, y = Item_Outlet_Sales, fill = Outlet_Type ))+
  geom_bar(stat = "Identity")
####Observations:
#### 1.The tier 1 is occupied by Super market 1 entirely with a Very few grocery store
#### 2. The tier 2 is entirely occupied with Supermarket type 1 stores.
#### 3. The Tier 3 market has all other outlets.
#### 4. Sales Tier 3 > Tier 2 > Tier 1

## Plotting Outlet_Location_Type vs Item_Outlet_Sales and segreegating them into Outlet_Identifier
ggplot(data, aes(x= Outlet_Location_Type, y = Item_Outlet_Sales, fill = Outlet_Identifier ))+
  geom_bar(stat = "Identity", position = "dodge")
####Observations:
#### 1. The tier 1  has OUT019 , OUT046, OUT049
#### 2. The tier 2 has OUT017, OUT035, OUT045
#### 3. The tier 3 has the remaining 4 outlets

## Plotting Outlet_Location_Type vs Item_Outlet_Sales and segreegating them into Outlet_Identifier
ggplot(data, aes(y= Outlet_Size, x = Outlet_Location_Type, fill = Outlet_Type ))+
  geom_bar(stat = "Identity", position = "dodge")

ggplot(data, aes(x= Outlet_Size, y = Item_Outlet_Sales, fill = Outlet_Type ))+
  geom_bar(stat = "Identity", position = "dodge")

ggplot(data, aes(y= Outlet_Location_Type, x = Outlet_Type, fill = Outlet_Size ))+
  geom_bar(stat = "Identity", position = "dodge")
#### Observations:
#### The outlet size of some supermarket type1 and Grocery store were missing and we need to find them.
#### The outlets opened in 1998, 2002, 2007 were not known.
#### The outlets opened in 1998 is in tier 3 and the outlets opened in 2002 and 2007 are in tier2.
#### The outlets opened in 1998 are grocery stores and Supermarkets type1 in 2002 and 2007.



## Plotting Item_Weight and Item_Type using ggplot
ggplot(data,aes(x=Item_Type , y = Item_Identifier))+
  geom_bar(stat = "Identity")





## Data Cleaning 
## Problems.

### 1. Converting low fat and LF to Low Fat
data$Item_Fat_Content <- revalue(data$Item_Fat_Content, c("LF"="Low Fat", "low fat"="Low Fat", "reg"="Regular"))


### 2 . Changing the Outlet_Establishment_year from Integer to Factor
data$Outlet_Establishment_Year <- as.factor(data$Outlet_Establishment_Year)

### 3. Finding the missing values in Item_Weight

r<- data[0,]
data$Item_Identifier <- as.integer(data$Item_Identifier)
for (i in 1:1559){
  m1<- filter(data, Item_Identifier == i)
  m2 <- filter(m1, is.na(Item_Weight)== T)
  m3 <- filter(m1, is.na(Item_Weight)== F)
  f1 <- nrow(m2)
  f2 <- nrow(m3)
  if (f1>=1 & f2 >= 1){
    m2$Item_Weight[1:f1] <- mean(m3$Item_Weight)
  } else{
    m2 <- m2
    m3 <- m3
  }
  r <- rbind(r,m2,m3)
  m2 <- Null
  m3 <- Null
}

## there are 4 missing values in the Item_Weight yet and we will check, Weather this can be removed or not.

## Test data
datatest<- read.csv("Bigmarttest.csv",sep = ",")

##cleaning datatest as usually done for training set.
### 1. Converting low fat and LF to Low Fat
datatest$Item_Fat_Content <- revalue(datatest$Item_Fat_Content, c("LF"="Low Fat", "low fat"="Low Fat", "reg"="Regular"))


### 2 . Changing the Outlet_Establishment_year from Integer to Factor
datatest$Outlet_Establishment_Year <- as.factor(datatest$Outlet_Establishment_Year)

### 3. Finding the missing values in Item_Weight

rtest<- datatest[0,]
datatest$Item_Identifier <- as.integer(datatest$Item_Identifier)
for (i in 1:1543){
  m1<- filter(datatest, Item_Identifier == i)
  m2 <- filter(m1, is.na(Item_Weight)== T)
  m3 <- filter(m1, is.na(Item_Weight)== F)
  f1 <- nrow(m2)
  f2 <- nrow(m3)
  if (f1>=1 & f2 >= 1){
    m2$Item_Weight[1:f1] <- mean(m3$Item_Weight)
  } else{
    m2 <- m2
    m3 <- m3
  }
  rtest <- rbind(rtest,m2,m3)
  m2 <- Null
  m3 <- Null
}

m1<- filter(datatest, Item_Identifier == 1200)
