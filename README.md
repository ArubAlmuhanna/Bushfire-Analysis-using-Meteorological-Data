# Bushfire Analysis using Meteorological Data : Project Overview 

In this project:
- Conducted data exploration.
- Tested and treated outliers.
- Built a linear regression model.
- Enhanced the linear regression model using varies teqniques.
- Built a Random Forest model.
- Built a Boosted Tree model.
- Compared the three models and chose the best one in terms of performance. 
- Highlghted the variable importance using Exploratory Data Analysis (EDA) and Boosted Tree model.

# Code and Resources Used

**Programming Language:** R 3.5.1 in Jupyter Notebook
**Libraries used:**
- psych
- ggplot2
- reshape2
- car
- corrplot
- GGally
- gridExtra
- grid
- olsrr
- randomForest
- gbm

# Dataset 
- The dataset is the Forest Fire Dataset provided by UCI Machine Learning Repository.
- The dataset contains 517 fire instances(rows), each of which have 13 columns.
- The first 12 columns corresponding to the attributes (e.g., spatial coordinates, month, day, four fire indices, and other meteorological data) 
- The last column contains the burned area, the variable that I will predict. 


# Exploratory Data Analysis (EDA) 
Proior to developing my models, I performed a general exploration of the dataset. The exploration inculded statsitcal and graphical exploration in addition to exploring possible correlation between variables. 

Below are some highlights of the EDA process: 

![Bushfire Analysis using Meteorological Data ipynb](https://user-images.githubusercontent.com/67848891/101126882-81ecde80-3650-11eb-92d4-fa5f0b6b314a.png)

![](https://user-images.githubusercontent.com/67848891/101126980-ac3e9c00-3650-11eb-9293-84c656fa1aeb.png)

<a href="url"><img src="https://user-images.githubusercontent.com/67848891/101126980-ac3e9c00-3650-11eb-9293-84c656fa1aeb.png" height="400" width="400" ></a>


# Model Building
Prior to building the modeld, the below steps were completed:  
- Features selection based on EDA. 
- The dataset was split into train dataset 80% and a test dataset 20%. 

I tried three different models and evaluated them based on their Root Mean Square Error (RMSE). I chose RSME because it is a good measure of accuracy and relatively easy to interpret.

# Model Performance 
The Boosted Tree model outperformed the other models on the test and validation sets.

- Linear Regression: RSME = 16.644
- Random Forest: RSME = 18.03
- Boosted Tree: RSME = 22.71

