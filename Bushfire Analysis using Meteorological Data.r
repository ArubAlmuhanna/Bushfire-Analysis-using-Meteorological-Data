#turn off warnings to avoid any warngings when running the codes 
warn = getOption("warn")
options(warn=-1)
options(warn=warn)

#import required libraries

library(psych) #For advanced statistcs (describe())
library(ggplot2) #For plots
library(reshape2) #For melt() function 
library("car") #For normality test plots
library(corrplot) #For correlation matrix plot
library(GGally) #To plot the correlogram
library(gridExtra) #To design a layout for plots
library(grid) #To design a layout for plots
library(olsrr) #For Cook's Distance function 
library(randomForest) #For Random Forst model 
library(gbm) #For Boosted Tree model 

#importing the dataset
forestFire = read.csv('forestfires.csv')

#view the dataset structure
cat("The Bush Fire dataset has", nrow(forestFire),
    "records, each with", ncol(forestFire),'attributes')

#view the attributes names 
cat("The attributes names are:\n", attributes(forestFire)$names)

#view the attributes names and samples 
cat("The dataset structure is:\n\n")
str(forestFire)

#inspect the first few records
cat("\nThe first few records in the dataset:")
head(forestFire)

#inspect the last few records
cat("\nThe last few records in the dataset:")
tail(forestFire)

#view the statistical summary 
cat("Basic statistics for each attribute are:")
summary(forestFire)

#use describe() function to view descriptive statistcs
round(describe(forestFire[, -c(3,4)]), 3)

#To view all the lables on x-axis
par(las=2)
#plot the boxplot for all the attributes 
boxplot(forestFire, main='Boxplot of Forest Fire',col = 'blue')

#prepare the data to be plotted
#melt all columns excluding the response variable area
meltedDF <- melt(as.data.frame(forestFire[,-13]))

#create the side by side boxplot using ggplot and facet_wrap
ggplot(meltedDF,aes(x = variable,y = value)) + 
  labs(title='Boxplots of Forest Fire',size=14)+
  facet_wrap(~variable) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=24, face="bold"))

#create the plots layout 
par(mfrow = c(2,4)) 

#for loop to iterate through each column in the dataset 
#excluding variables of type factor and the response variable area
for (i in 1:(length(forestFire))){
    if(class(forestFire[[i]])!='factor') {
        boxplot(forestFire[,i], 
        main = names(forestFire[i]), 
        type="l", col = 'purple')}
}
#add title 
mtext("Boxplots of Numerical Variables", 
      outer = TRUE, line = -30,cex = 1.5)

#Find the mean of each attribute then round the values
#sort the values in an ascending order
sort(round(sapply(forestFire[,5:13],mean),3))

#Find the median of each attribute then round the values
#sort the values in an ascending order
sort(round(sapply(forestFire[,5:13],median),3))

sort(round(apply(forestFire[,5:13], 2, var),3))

#comparing the standard deviations of the attributes
sort(round(apply(forestFire[,5:13], 2, sd),3))

#viewing the ranges of the numerical attributes 
Range<-sapply(forestFire[,5:13],range)
#create names for each row
row.names(Range)<-c('Minimum Value','Maximum Value')
#view the dataframe
Range

#creating Boxplot elements for the numerical attributes using fivenum() function 
fivenumForest<-sapply(forestFire[,5:13],fivenum)
#creating row names to distinguesh the rows
row.names(fivenumForest)<-c('minimum', 'lower-hinge',
                            'median', 'upper-hinge', 'maximum')
#view the dataframe
fivenumForest

#returns the same output with the corresponding Quarter percentage 
sapply(forestFire[,5:13],quantile)

#view the sorted Interquartile Range for each numerical attribute
IQR<-t(data.frame(sort(sapply(forestFire[,5:13],IQR))))
#create a name for the row
row.names(IQR)<-('IQR')
#diaplay the data frame
IQR

#to view the number of fires per month 
monthTable<-sort(table(forestFire$month))
monthTable

#to view the precentage of fires per month
round(prop.table(monthTable),3)*100

#to view the number of fires per day 
dayTable<-sort(table(forestFire$day))
dayTable

#to view the precentage of fires per month
round(prop.table(dayTable),3)*100

#view the for the respopnse variable area
boxplot.stats(forestFire$area)

#plot a boxplot of the response variable area 
boxplot(forestFire$area,col='blue',
        main='Area Boxplot',xlab='Area in Hectar',
        yaxt = "n", horizontal=TRUE)
#change the sequence of value on y-axisb
axis(2, at = seq(0, 1000, 100), las = 2)

#melt the dataset to change the form from wide to long 
#use geom_histogram to plot histograms 
#use facet_wrap to plot the variables side by side 

ggplot(melt(forestFire),aes(x=value)) + 
geom_histogram(col='blue') + 
facet_wrap(~variable,ncol=3, scales = "free")+
theme(strip.text = element_text(size=20))

#temp histogram
p1<-ggplot(aes(x=temp), data = forestFire) +
    geom_histogram(color = I('black'), fill = "red") + 
    ggtitle('temp Distribution')

#RH histogram
p2<-ggplot(aes(x=RH), data = forestFire) +
    geom_histogram(color = I('black'), fill = "red") + 
    ggtitle('RH Distribution')

#wind histogram
p3<-ggplot(aes(x=wind), data = forestFire) +
    geom_histogram(color = I('black'), fill = "red") + 
    ggtitle('wind Distribution')

#rain histogram
p4<-ggplot(aes(x=rain), data = forestFire) +
    geom_histogram(color = I('black'), fill = "red") + 
    ggtitle('rain Distribution')



#arrange the plots
grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = textGrob("Distribution of Weather Inputs",
                            gp=gpar(fontsize=25,font=3)))

#FFMC histogram
p1<-ggplot(aes(x=FFMC), data = forestFire) +
    geom_histogram(color = I('black'), fill = "blue") + 
    ggtitle('FFMC Distribution')

#DMC histogram
p2<-ggplot(aes(x=DMC), data = forestFire) +
    geom_histogram(color = I('black'), fill = "blue") + 
    ggtitle('DMC Distribution')

#DC histogram
p3<-ggplot(aes(x=DC), data = forestFire) +
    geom_histogram(color = I('black'), fill = "blue") + 
    ggtitle('DC Distribution')

#ISI histogram
p4<-ggplot(aes(x=ISI), data = forestFire) +
    geom_histogram(color = I('black'), fill = "blue") + 
    ggtitle('ISI Distribution')

#arrange the plots
grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = textGrob("Distribution of FWI Inputs",
                            gp=gpar(fontsize=25,font=3)))

hist(forestFire$area,col='light green',xlab='area',
     main='Histogram of Area')
lines(density(forestFire$area), col = "red", lwd = 3)

#create a new column logarea to hold the values of the log transformation of area
forestFire$logarea=ifelse(forestFire$area>0,
                          log(forestFire$area),NA)

#view a subset of the dataframe 
tail(forestFire[,c('area','logarea')])

ggplot(forestFire, aes(x=logarea))+
geom_histogram(aes(y=..density..),colour='black', 
               fill='light green')+
stat_function(fun=dnorm, 
              args=list(mean=mean(forestFire$logarea,na.rm=TRUE),
              sd=sd(forestFire$logarea,na.rm=TRUE)),col='red')

#create a vector to store the value of the sorted months
monthSorted  = factor(forestFire$month, 
                      levels=c("jan","feb","mar","may","jun","jul",
                                "aug","sep","oct","nov","dec"))

#bar plot of the sorted months 
#indicating the number of fires each month 
barplot(table(monthSorted),col="light blue",
        xlab="Month",ylab="No. Fires")

#count per month in percentage 
monthPercent<-round(prop.table(monthTable),3)*100

#plot a bar plot of the sorted months 
#indicating the number of fires each month 
barplot(monthPercent,col="light blue",
        xlab="Month",ylab="Percentage of Fires")

#create a vector to store the value of the sorted days
daySorted  = factor(forestFire$day, 
            levels=c("mon","tue","wed","thu","fri","sat","sun"))


#plot a bar plot of the sorted months 
#indicating the number of fires each month 
barplot(table(daySorted),col="light green",
        xlab="Day",ylab="No. Fires")

#count per month in percentage 
dayPercent<-round(prop.table(dayTable),3)*100


#plot a bar plot of the sorted months indicating the number of fires each month 
barplot(dayPercent,col="light green",
        xlab="Month",ylab="Percentage of Fires")

pairs(forestFire[,c(1,2,seq(5,12))],panel = panel.smooth,cex=0.2)

#fint the correlation and round the number 
corMatrix<-round(cor(forestFire[,-c(3,4,14)]),3)
corMatrix

corrplot(corMatrix,method='number',
         col=c("red", "orange",'dark green'),type='lower')

corrplot(corMatrix,method='circle', type='lower')

ggpairs(forestFire[,-c(3,4,14)],
        title="correlogram with ggpairs()") +
theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=24, face="bold"))

#create vector that contains the column names 
fwiVarLst <- c("FFMC","DMC","DC","ISI")
#use the vector to create a subset of the original dataset 
fwiVar <- forestFire[fwiVarLst]
#find the correlation
cor(fwiVar)

#create vector that contains the column names 
weatherVarLst <- c("RH","temp","wind","rain")
#use the vector to create a subset of the original dataset 
weatherVar <- forestFire[weatherVarLst]
#find the correlation
cor(weatherVar)

cov(forestFire$area,forestFire$temp)

cov(forestFire$area,forestFire$rain)

cov(fwiVar)

cov(weatherVar)

#sort the month 
monthSorted  = factor(forestFire$month, 
               levels=c("jan","feb","mar","may","jun","jul",
                        "aug","sep","oct","nov","dec"))

#plot the graph of month and area
#convert the scale of the y axis for better presentation
ggplot(forestFire, aes(monthSorted, forestFire$area)) + 
geom_boxplot() + coord_trans(y = "sqrt") + 
labs(x = "Month", y = "The Burned Area",
     title ="The Burned Area By Different Months")+
     theme(plot.title = element_text(size=24, face="bold"))

#plot the graph of month and temprature 
#convert the scale of the y axis for better presentation
ggplot(forestFire, aes(monthSorted, forestFire$temp)) + 
geom_boxplot() + coord_trans(y = "sqrt") + 
labs(x = "Month", y = "The Temprature Level",
     title ="The Temprature Level in Different Months")+
theme(plot.title = element_text(size=20, face="bold"))

#rearrange the days 
daySorted  = factor(forestFire$day, 
            levels=c("mon","tue","wed","thu","fri","sat","sun"))

#plot the graph of day and area
#convert the scale of the y axis for better presentation
ggplot(forestFire, aes(daySorted, forestFire$area)) + 
geom_boxplot() + coord_trans(y = "sqrt") + 
labs(x = "Day", y = "The Burned Area",
     title ="The Burned Area By Different Days")+
     theme(plot.title = element_text(size=24, face="bold"))



#fit the basic linear model using the original dataset
#and excluding the column logarea
outlierMod=lm(area~.,data=subset(forestFire, select = -c(logarea)))

#apply the cook's distance method on the model 
cooksDistance=cooks.distance(outlierMod)


#plot the values of cook's distance 
plot(cooksDistance, pch="*", cex=2,
    main="Influential Points by Cooks Distance",
    cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)

#setting the cutoff line according to the rule 
#(4 times above the mean)
abline(h = 4*mean(cooksDistance, na.rm=T), col="red") 

#add and place labels to the plot
text(x=1:length(cooksDistance)+1, y=cooksDistance+0.01,
     labels=ifelse(cooksDistance>4*mean(cooksDistance, na.rm=T), 
     names(cooksDistance),""), 
     col="red") 

#extract the highly influential data points
infPoints=which(cooksDistance>4*mean(cooksDistance,na.rm=T))

#count the number of influential data points
cat('The total number of high infulential points is:',
    length(infPoints),'points')

#view the highly influential points 
#exclude logarea attribute 
forestFire[infPoints, -14]


#remove the influential points from the dataframe
forestFire=forestFire[-infPoints, ]

#the structure of the new dataframe 
cat('The new data structure after removing
     the high influence data points is:'
    ,nrow(forestFire),'rows x', ncol(forestFire), 'columns')

modelFeatures=c('X','Y','temp','RH','wind','rain')

#Set.seed is used to maintain the same output of the split 
set.seed(2020)

#create a subset of the selected variables 
forestFireSub=subset(forestFire,select=c(modelFeatures,'area'))

#--shuffle the dataframe--#
shuffledData <- forestFireSub[sample(503),]


#splitting the new dataset into 80:20 ratio 

#select 80% of the total rows 
split= sample(nrow(shuffledData), size=0.8*nrow(shuffledData),
              replace=FALSE)

#create a training dataset 
#selecting 80% from the original dataset
train= shuffledData[split,]


#remove 80% from the original dataset, the remaining is 20%
test= shuffledData[-split,modelFeatures]

#find the true value of the response variable 
trueArea= shuffledData[-split,'area']

cat('The structure of the training dataset is',(nrow(train)),
    'rows and',ncol(train),'columns')  #size of training set

cat('\n\nThe structure of the testing dataset is',(nrow(test)),
    'rows and',ncol(test),'columns')  #size of training set

#length of the true response variable
cat('\nThe length of the true value of the response variable is',
    length(trueArea)) 

#fit the first  Basic Model using the selected features 
linearMod1 <- lm(area~., data = train)

#the model results using summary 
summary(linearMod1)

#Set.seed is used to maintain the same output of the split 
set.seed(2021)

#--shuffle the dataframe--#
shuffledData2 <- forestFire[sample(503),]


#splitting the new dataset into 80:20 ratio

#select 80% of the total rows 
split2= sample(nrow(shuffledData2), size=0.8*nrow(shuffledData2),
               replace=FALSE)

#create a training dataset
#selecting 80% from the original dataset
train2= shuffledData2[split2,]
train2=subset(train2, select=-logarea)


#remove 80% from the original dataset, the remaining is 20%
test2= shuffledData2[-split2,]
test2=subset(test2, select=-c(area,logarea))

#find the true value of the response variable 
trueArea2= shuffledData2[-split2,'area']

cat('The structure of the training dataset is',(nrow(train2)),
    'rows and',ncol(train2),'columns')  #size of training set

cat('\n\nThe structure of the testing dataset is',(nrow(test2)),
    'rows and',ncol(test2),'columns')  #size of training set

#length of the true response variable
cat('\nThe length of the true value of the response variable is',
    length(trueArea2)) 

#fit the full model using all the predictiors in train2
fullMod = lm(area~., data = train2)

#view the full model's summary 
summary(fullMod)

#apply step() function on the full model and call it reducedMod 
reducedMod = step(fullMod, direction="backward")

#view the reduced model'summary 
summary(reducedMod)

#VIF test to explore the multicollinearity between the predictors 
vif(step(reducedMod))

#fit the model with excluding RH, wind, and rain 
CorrMod <- lm(area~., data=subset(train, 
                                     select=c( -RH, -wind, -rain )))

#view the model's summary
summary(CorrMod)

#interaction effect using the symbols * and : 
#fit the model
InteractionMod <- lm(area~(X+Y)*temp+X:Y:temp,data=train)

#view the model's summary
summary(InteractionMod)

#fit the Random Forest Model using all the predictors 
randomForestMod = randomForest(area~.,data=train, ntree=1000,
                               mtry=6, importance=TRUE)

#display the model 
randomForestMod

#view the importance measures produced by randomForest Model
importance(randomForestMod)

#plot visualisation of variables importance 
varImpPlot(randomForestMod)

#fit the Boosted Tree model
boostMod=gbm(area~.,data=train,distribution='gaussian',
                n.trees = 10000, shrinkage = 0.01, 
                interaction.depth = 4)

#print the model's summary
summary(boostMod)

#the interaction effect model will be used for prediction 
predLinear = predict(CorrMod ,test)

#find the RMSE 
mseLinear = mean((trueArea-predLinear)^2)
#find the RMSE by taking the square root of MSE 
cat('The RMSE for the Linear model is:', sqrt(mseLinear))

#plot 
plot(trueArea, predLinear, 
    main='Linear Regression: Predicted Values vs. Actual Values',
    xlab='Actual Value', ylab='Predicted Value',
    cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#fit a line 
abline(lm(predLinear~trueArea),col='red')

#the RandomForst Model is used to predict the value of area
predRanForest= predict(randomForestMod, test)

#find the MSE 
mseRanForst = mean((trueArea-predRanForest)^2)
#find the RMSE by taking the square root of MSE 
cat('The RSME for the Random Forest is:', sqrt(mseRanForst))

#plot 
plot(trueArea, predRanForest, 
    main='Random Forest: Predicted Values vs. Actual Values',
    xlab='Actual Values', ylab='Predicted Values',
    cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)


#the Boosted Tree Model is used to predict the value of area
predBoost= predict(boostMod, test, n.trees=1000)

#find the MSE 
mseBoost = mean((trueArea-predBoost)^2)
#find the RMSE by taking the square root of MSE 
cat('The RSME for the Boosted Tree is:', sqrt(mseBoost))

#plot 
plot(trueArea, predBoost, 
    main='Boosted Tree: Predict Value vs. Actural Value',
    xlab='Actual Value', ylab='Predicted Value',
    cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)


summary(boostMod)


