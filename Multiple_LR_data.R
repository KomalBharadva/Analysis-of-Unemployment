setwd("D:/NCI/SDA/CA1/Datasets")
getwd()

library(dplyr)
library(tidyverse)
# Reading CSV files from local directory for multiple regression

# 1- Unemployment data which is the target or dependent variable
Unemployment_data <- read.csv("Unemployment_in_thousands.csv", check.names=FALSE)
head(Unemployment_data)
str(Unemployment_data)

## Renaming columns:
Unemployment_data <- Unemployment_data %>% 
  rename(
    Country = "Country or Area",
    Value_Footnotes = "Value Footnotes",
    Unemployment_value = Value
  )

## Filtering columns:
Unemployment_data<-
  Unemployment_data %>%
  filter(Sex=="Total men and women") %>%
  filter(Coverage=="Total coverage") %>%
  filter(Year == "2004") %>%
  filter(Type=="Unemployed") %>%
  filter(Source=="Labour force survey")
  
## Removing unnecessary columns:
Unemployment_data <- subset(Unemployment_data, select = -c(Coverage, Source, SourceID, Type, Value_Footnotes, Sex, Year))

## To reset row names:
rownames(Unemployment_data) <- NULL
head(Unemployment_data)
dim(Unemployment_data)

# 2- Population data which is the independent variable
Population_data <- read.csv("Total_population_in_thousands.csv", check.names=FALSE)
head(Population_data)
dim(Population_data)
str(Population_data)

## Renaming columns:
Population_data <- Population_data %>% 
  rename(
    Country = "Country or Area",
    Population_value = Value,
    Year = "Year(s)"
  )

## Filtering columns:
Population_data <- Population_data %>%
  filter(Variant=="Medium") %>%
  filter(Year == "2004")
  
## Removing unnecessary columns:
Population_data <- subset(Population_data, select = -c(Variant, Year))

## To reset row names:
rownames(Population_data) <- NULL
head(Population_data)
dim(Population_data)

# 3- Education GDP data which is the independent variable
Edu_GDP_data <- read.csv("Education_GDP_in_percent.csv", check.names=FALSE)
head(Edu_GDP_data)
str(Edu_GDP_data)

## Renaming columns:
Edu_GDP_data <- Edu_GDP_data %>% 
  rename(
    Country = "Reference Area",
    Edu_GDP_value = "Observation Value",
    Age_Group = "Age group",
    Units = "Units of measurement",
    Year = "Time Period"
  )

## Filtering columns:
Edu_GDP_data <- Edu_GDP_data %>%
  filter(Year== "2004")
  
## Removing unnecessary columns:
Edu_GDP_data <- subset(Edu_GDP_data, select = -c(Sex, Age_Group, Units, Year))

## To reset row names:
rownames(Edu_GDP_data) <- NULL
head(Edu_GDP_data)
dim(Edu_GDP_data)

# 4- R&D GDP data which is the independent variable
RnD_GDP_data <- read.csv("R&D_GDP_in_percent.csv", check.names=FALSE)
head(RnD_GDP_data)
str(RnD_GDP_data)

## Renaming columns:
RnD_GDP_data <- RnD_GDP_data %>% 
  rename(
    Country = "Reference Area",
    RnD_GDP_value = "Observation Value",
    Age_Group = "Age group",
    Units = "Units of measurement",
    Year = "Time Period"
  )

## Filtering columns:
RnD_GDP_data <- RnD_GDP_data %>%
  filter(Year == "2004")
  
## Removing unnecessary columns:
RnD_GDP_data <- subset(RnD_GDP_data, select = -c(Sex, Age_Group, Units, Year))

## To reset row names:
rownames(RnD_GDP_data) <- NULL
head(RnD_GDP_data)
dim(RnD_GDP_data)

# Merging all the above dataframes by Country Column
merge1 <- merge(RnD_GDP_data,Population_data,by=c("Country"))
merge2 <- merge(merge1,Edu_GDP_data,by=c("Country"))
final_data <- merge(merge2,Unemployment_data,by=c("Country"))

head(final_data)
dim(final_data)
str(final_data)

write.csv(final_data, "sample_data.csv", row.names=FALSE)
library(caret)
# Preparing data for modelling and Normalizing between 0 and 1
preprocessed_data <- preProcess(final_data[,c(2:4,5)], method=c("range"))
norm_data <- predict(preprocessed_data, final_data[,c(2:4,5)])
summary(norm_data)
head(norm_data)
str(norm_data)
dim(norm_data)
norm_data$Unemployment_value <- log(norm_data$Unemployment_value)
head(norm_data)

write.csv(norm_data, "final_dataset.csv", row.names=FALSE)


# Lets check pair plots
pairs(norm_data)
pairs(norm_data, panel = panel.smooth)

# Applying Multiple Linear Regression algorithm:
model1<-lm(Unemployment_value~Population_value*RnD_GDP_value*Edu_GDP_value 
           +I(Population_value^2)+I(RnD_GDP_value^2) +I(Edu_GDP_value^2) 
           ,data = norm_data)
model2<-step(model1)
summary(model2)
plot(model2)

# Checking Durbin-Watson test for Independence of Errors:
durbinWatsonTest(model2)
vif(model2)

# Checking Cook's distance and Influence plot for influential data points:
cooks.distance(model2)
influencePlot(model = model2, scale =3, main="Influence Plot")

# Checking NCV test for heteroscedasticity:
library(car)
ncvTest(model2)

#Checking White-test for verification of heteroscedasticity
library(vars) 
library(het.test)
model_test <- VAR(norm_data, p = 1) 
whites.htest(model_test)

