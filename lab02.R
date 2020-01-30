# Load neccessary libraries
library(tidyverse)

# Load the data set
ameslist <- read.table(
  "https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
  header = TRUE,
  sep = ","
)
# ameslist <- read.csv("ames.csv")

typeof(ameslist)
unique(ameslist$GarageType)

# Create a matrix to transform GarageType factor data into numeric data
# https://stackoverflow.com/questions/5616210/model-matrix-with-na-action-null
options(na.action='na.pass')
GarageTemp = model.matrix(~ GarageType - 1, data=ameslist)

# Combine the numeric matrix and full data frame together
ameslist <- cbind(
  ameslist, 
  GarageTemp
)

# Create the GarageOutside column in ameslist
ameslist$GarageOutside <- ifelse(
  ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 
  1, 
  0
)

# Check for any NA values in GarageOutside, and change them to a 0
ameslist$GarageOutside <- ifelse(
  is.na(ameslist$GarageOutside),
  0,
  ameslist$GarageOutside
)

# Check that the above NA value changes worked
unique(ameslist$GarageOutside)

################################## Exercise 1 ##################################

### 1 ###
x <- sapply(ameslist, class)
y <- sapply(x, function(z) z == "integer")
names(y) <- NULL
Ames <- ameslist[ , y]

Ames$Id <- NULL
Ames$MasVnrArea <- NULL
Ames$MiscVal <- NULL

### 2 ###
scatterMatrix <- pairs(
  SalePrice ~ OverallQual + OverallCond + YearBuilt + FullBath 
    + BedroomAbvGr + TotalBsmtSF + Fireplaces + GarageCars + GarageArea + TotRmsAbvGrd
    + YearRemodAdd + LotArea,
  data=Ames
)

### 3 ###
correlateVariables <- Ames[ , c("LotArea", "OverallQual", "OverallCond", "YearBuilt", "FullBath", 
                                "BedroomAbvGr", "TotalBsmtSF", "Fireplaces", "GarageCars", "GarageArea", 
                                "TotRmsAbvGrd", "YearRemodAdd")]
correlates <- cor(correlateVariables)
correlates

### 4 ###
plot(
  x=Ames$GrLivArea,
  y=Ames$SalePrice,
  xlab="Ground Floor Living Area in Square Feet",
  ylab="Sale Price in US Dollars"
)


x <- Ames$GrLivArea
y <- Ames$SalePrice

Sxy = sum((x - mean(x)) * (y - mean(y)))
Sxx = sum((x - mean(x)) ^ 2)
Syy = sum((y - mean(y)) ^ 2)

beta_1_hat = Sxy / Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)

abline(beta_0_hat, beta_1_hat, col="blue")

################################################################################



################################################################################

### 1 ###
# Create the GarageInside column in ameslist
ameslist$GarageInside <- ifelse(
  ameslist$GarageTypeDetchd == 0 & ameslist$GarageTypeCarPort == 0, 
  1, 
  0
)

# Check for any NA values in GarageOutside, and change them to a 0
ameslist$GarageInside <- ifelse(
  is.na(ameslist$GarageInside),
  0,
  ameslist$GarageInside
)

indoorGarageSLR <- lm(
  SalePrice ~ GarageInside - 1,
  data=ameslist
)
summary(indoorGarageSLR)

### 2 ###
options(na.action='na.omit')

fullAmesModel <- lm(
  SalePrice ~ .,
  data=Ames
)
summary(fullAmesModel)

### 3 ###

# https://data.library.virginia.edu/diagnostic-plots/
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(fullAmesModel)
par(mfrow=c(1,1)) # Change back to 1 x 1

### 4 ###
fullAmesModel2 <- lm(
  SalePrice ~ . + LotArea:BedroomAbvGr,
  data=Ames
)
summary(fullAmesModel2)

fullAmesModel3 <- lm(
  SalePrice ~ . + BedroomAbvGr:TotRmsAbvGrd,
  data=Ames
)
summary(fullAmesModel3)

fullAmesModel4 <- lm(
  SalePrice ~ . + YearBuilt*YrSold,
  data=Ames
)
summary(fullAmesModel4)

### 5 ###

fullAmesModel5 <- lm(
  SalePrice ~ . - LotArea + log2(LotArea),
  data=Ames
)
summary(fullAmesModel5)
# Lot Area became more significant when log2'd

fullAmesModel6 <- lm(
  SalePrice ~ . + (OverallCond)^2 + (OverallQual)^2,
  data=Ames
)
summary(fullAmesModel6)

fullAmesModel7 <- lm(
  SalePrice ~ . + sqrt(BedroomAbvGr),
  data=Ames
)
summary(fullAmesModel7)