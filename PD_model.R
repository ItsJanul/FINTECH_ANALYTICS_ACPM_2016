#PROBABILITY OF DEFAULT MODEL V2.0
#Created by Janul and Amir
#Fintech Analytics (INTA-GB.2320.10)
#FALL 2016

#LIBRARIES-----------------------------------------------------------------
library(lubridate)
library(tidyverse)
library(caTools)

#change # of rows to print on tibble
options(tibble.print_max = 1000, tibble.print_min = 200)

#FUNCTIONS-----------------------------------------------------------------

#FUNCTION TO CALCULATE DENSITY ESTIMATES  
densityMap	<-	function(x,outcome,k=25,plot=T,xaxis="breaks",spar=0.5)	
{	
  df =data.frame(x, outcome)
  df<-na.omit(df)
  
  breaks<-(0:k)/k	
  qs<-quantile(df$x, breaks)	
  buckets<-cut(df$x, qs)
  
  raw.curve<-tapply(X=df$outcome , IND=buckets , FUN=mean)	
  curve<-smooth.spline(breaks[-1],raw.curve,spar=spar)	
  pred<-predict(curve,breaks[-1])
  
  if(plot)	
  {	
    plot(breaks[-1],raw.curve,
         main="Density Plot",
         xlab=xaxis,
         ylab="PD")
    lines(pred,col="red",lwd=3)
  }	
  
  baseline<-mean(outcome)	
  cutoff<-qs[-1]
  PD<-pred$y	
  map<-data.frame(cutoff=cutoff,PD=PD)
  
  return(list(baseline=baseline,map=map))	
}

#FUNCTION TO USE DENSITY ESTIMATE TO MAP A VARIABLE TO A PD
applyDensityMap	<-	function(x,	map, forceZero = F)	
{
  output <- vector("double", length(x)) 
  cutoff <- map$map$cutoff
  PD <- map$map$PD
  len <- length(cutoff)
  
  
  for (i in seq_along(x)) {
    if (is.na(x[i]))
      pd <- NA
    else{
      lb <- sum(cutoff < x[i])
      if (lb == 0)
        pd <- PD[1]
      else	if (lb >= len)
        pd <- PD[len]
      else
      {
        ub <- lb + 1
        num <- x[i] - cutoff[lb]
        den <- cutoff[ub] - cutoff[lb]
        wt.ub <- num / den
        wt.lb <- 1 - wt.ub
        pd<-wt.lb*PD[lb]+wt.ub*PD[ub]	
      }
    }
    
    
    if (forceZero){
      if (pd>0 || is.na(pd)) output[i]<- pd
      else output[i]<- 0
    }
    else{output[i]<- pd}
  }
  return(output)
}

#FUNCTION TO TRANSFORM VARIABLE TO PD
transformVar <- function(x, outcome, k=25, plot=F, xaxis="breaks", forceZero = F)
{
  fit<- densityMap(x, outcome, k=k, plot=plot, xaxis=xaxis)
  output<- applyDensityMap(x, fit, forceZero)
  return(list(baseline=fit[[1]], map=fit[[2]], transformedVar=output, buckets=k))
}

#FUNCTION TO PUSH TRANFORM VARIABLES TO TRAIN df
pushTfrm2TRAIN<- function(df, debt, lev, liq, prof, size){
  df$debtT<- debt[[3]]
  df$levT<- lev[[3]]
  df$liqT<- liq[[3]]
  df$profT<- prof[[3]]
  df$sizeT<- size[[3]]
  
  return(df)
}

#FUNCTION TO TRANSFORM VARIBLES TO TEST df
pushTfrm2TEST<- function(df, debt, lev, liq, prof, size, naRemove= F){
  df$debtT<- applyDensityMap(df$noi_int ,debt, forceZero = T)
  df$levT<- applyDensityMap(df$lev ,lev, forceZero = T)
  df$liqT<- applyDensityMap(df$rbc1rwaj ,liq, forceZero = T)
  df$profT<- applyDensityMap(df$roaptx ,prof, forceZero = T)
  df$sizeT<- applyDensityMap(df$asset5 ,size, forceZero = T)
  
  if(naRemove){
    df<- df %>%
      filter(!is.na(profT)) %>%
      filter(!is.na(rbc1rwaj))
  }
  
  return(df)
}

#AUTO-FIT DENSITY ESTIMATE
autoCal<- function(x, outcome, k=25, plot=T, xaxis="breaks")
{
  output<-transformVar(x , outcome, k , plot , xaxis)
  
  while(summary(output[[3]])[[1]]<0){
    k=k+5
    output<-transformVar(x , outcome, k , plot , xaxis)
    print(summary(output[[3]])[[1]])
    print(k)
  }
  return(output)
}

#THE WALKFORWARD FUNCTION: SEND THE DATAFRAME IT DOES THE REST
#THIS FUNCTION CALCULATES NEW FITS ACCORDING TO ADDITIONAL DATA,
#REMOVES ALL NA's FROM TEST SET
#RETURNS A LIST OF (YEAR, MODEL, TEST DATA, TRAIN DATA, RAW PREDICT,
#CLEAN PREDICT, ROC FOR MODEL)
walkForwardDf<- function(df, yearX=2007, yearOutofSample=1){
  
  i=1
  output<-list()
  
  while(yearX<year(max(df$repdte.adjust))){
    
    train.data<- df %>%
      filter(year(repdte.adjust)<yearX) %>%
      select(ID, repdte.adjust, Default.Date, roaptx, lev, noi_int, rbc1rwaj, asset5, default.flag)
    
    test.data<- df %>%
      filter(year(repdte.adjust)==(yearX+yearOutofSample)) %>%
      select(ID, repdte.adjust, Default.Date, roaptx, lev, noi_int, rbc1rwaj, asset5, default.flag)
    
    ##calculating new fits
    #PROFIT
    profit.fit<-transformVar(train.data$roaptx, 
                             train.data$default.flag, 
                             k=50, plot = F, xaxis = "roaptx")
    
    #MEASURE OF LEVERAGE: 
    leverage.fit<-transformVar(train.data$lev, 
                               train.data$default.flag, 
                               k=35, plot = T, xaxis = "leverage", forceZero = T)
    
    #MEASURES OF DEBT COVERAGE: 
    debt.fit<-transformVar(train.data$noi_int, 
                           train.data$default.flag, 
                           k=60, plot = T, xaxis = "debt_coverage")
    
    #MEASURE OF LIQUIDITY: 
    liquidity.fit<- transformVar(train.data$rbc1rwaj, 
                                 train.data$default.flag, 
                                 k=30, plot = T, xaxis = "liquidity", forceZero = T)
    
    #MEASURE OF SIZE: 
    size.fit<-transformVar(train.data$asset5, 
                           train.data$default.flag,
                           k=11, plot = T, xaxis = "assets")
    
    #TRANSFORMING VARIABLES
    train.data<- pushTfrm2TRAIN(train.data, debt.fit, leverage.fit, liquidity.fit, profit.fit, size.fit)
    test.data.RAW<- pushTfrm2TEST(test.data, debt.fit, leverage.fit, liquidity.fit, profit.fit, size.fit, naRemove = F)
    test.data.clean<- pushTfrm2TEST(test.data, debt.fit, leverage.fit, liquidity.fit, profit.fit, size.fit, naRemove = T)
    
    #CREATING NEW MODEL AND CALCULATIONS
    model<-glm(default.flag ~ debtT + levT + liqT + profT + sizeT,family=binomial(link="logit"),
                       data=train.data, na.action=na.exclude)
    
    #Clean dataset passed, no NA's the length of the predict vector may differ
    predict.clean<-predict(model , newdat = test.data.clean , type="response",na.action=na.pass)
    
    #RAW dataset all NA's passed, this can't be used to calculate ROC due to NA inclusion.
    predict.raw<-predict(model , newdat = test.data.RAW , type="response",na.action=na.pass)
    
    rocAUC<-colAUC(data.frame(model= predict.clean), test.data.clean$default.flag, plotROC=TRUE, alg=c("ROC"))
    
    #step forward in list and return year, training data, test data, model, predictClean, predictRAW 
    output[[i]]<-list(year=yearX, train=train.data, test=test.data.clean, model= model, predictClean=predict.clean, 
                      predictRAW=predict.raw, rocAUC= rocAUC,
                      fit= list(prof=profit.fit,lev=leverage.fit, debt=debt.fit, size=size.fit, liq=liquidity.fit))
    
    yearX=yearX+yearOutofSample  
    i= i+1
  }
  return(output)
}

#FUNCTION FOR DEFAULT TIMELINE
defaultTimeline <-function (df)
    #need to send dates as Default.Date and bank ids as ID
{
    output <- df %>%
      filter(!is.na(Default.Date)) %>%
      group_by(yearx= year(Default.Date)) %>%
      summarise(number_banks = n_distinct(ID)) %>%
      ggplot(., aes(x = yearx, y = number_banks)) + 
          geom_area() + 
          ggtitle("Bank Defaults") + 
          labs(x = "Year", y = "Total Defaults")
    return(output)
}



#ITEM2: RUNNING CODE--------------------------------------------------------------

#creating a tibble called bankdata
bankdata<-as_data_frame(bankdata.in.new)
head(bankdata, 10)

#Cleaning tibble and reading all date columns as dates.
#Stepping forward report date by 3 months
#Creating default flags
bankdata<- bankdata %>%
  mutate(Default.Date= mdy(Default.Date)) %>%
  mutate(repdte.adjust= repdte %m+% months(3)) %>%
  mutate(days.to.default= ifelse (is.na(Default.Date), NA, 
                                  Default.Date-repdte.adjust)) %>%
  mutate(default.flag= ifelse (is.na(days.to.default), 0, ifelse(days.to.default >0 & days.to.default <= 365.25, 1, 0)))

#Here I am creating variables I know I will need
##Pre-tax Net operating income over interest expense- "idpretx/eintexp" create variable
bankdata$noi_int<-bankdata$idpretx/(bankdata$eintexp+1)

##Total asset over total liability- "asset/liability" create variable
bankdata$lev<-bankdata$asset/(bankdata$liab+1)

#Viewing defaults volume in dataset
##need to send dates as Default.Date and bank ids as ID
defaultTimeline(bankdata)

#Noticed that defaults started occuring around 2007 want to get some of that data in the training sample
#Setting training set according to time 2007
train.data<- bankdata %>%
  filter(year(repdte.adjust)<2007) %>%
  select(ID, repdte.adjust, Default.Date, roaptx, lev, noi_int, rbc1rwaj, asset5, default.flag)

#My out of sample will only be 1 year, I plan to step it foward.
test.data<- bankdata %>%
  filter(year(repdte.adjust)==2008) %>%
  select(ID, repdte.adjust, Default.Date, roaptx, lev, noi_int, rbc1rwaj, asset5, default.flag)


#TRANSFORMING VARIABLES TO A MAP----------------

#MEASURE OF PROFITABILITY: 
#Pre-tax return on assets- "roaptx" 
#We run the autoCal which tries to find the best fit, I chose 50 buckets.
profit.fit<-transformVar(train.data$roaptx, 
                    train.data$default.flag, 
                    k=50, plot = T, xaxis = "profitability", forceZero = T)

#MEASURE OF LEVERAGE: 
#Total asset over total liability- "asset/liability"
#We run the autoCal we find that it solves at k=25 but at k=35 I get more for the curve 
leverage.fit<-autoCal(train.data$lev, 
                      train.data$default.flag, 
                      k=25, plot = T, xaxis = "leverage")

#So we force 0 on any PD below zero with the transformVar forceZero set to true
leverage.fit<-transformVar(train.data$lev, 
                           train.data$default.flag, 
                           k=35, plot = T, xaxis = "leverage", forceZero = T)

#MEASURES OF DEBT COVERAGE: 
#Pre-tax Net operating income over interest expense- "idpretx/eintexp"
#We run autoCal which works extremely well on this datasample. It buckets at 50.
debt.fit<-autoCal(train.data$noi_int, 
                  train.data$default.flag, 
                  k=60, plot = T, xaxis = "debt_coverage")

#MEASURE OF LIQUIDITY: 
#Tier 1 Capital over Total risk-weight assets- "rbc1rwaj"

#We run autoCal and it solves at k=10, but the curve doesn't capture all the information
##note if you start at k=25 autoCal WILL NOT SOLVE due to the shape of the curve
liquidity.fit<- autoCal(train.data$rbc1rwaj, 
                        train.data$default.flag, 
                        k=20, plot = T, xaxis = "liquidity")

#I force k=30 with forceZero set to true to remove the negative min's is very small
liquidity.fit<- transformVar(train.data$rbc1rwaj, 
                             train.data$default.flag, 
                             k=30, plot = T, xaxis = "liquidity", forceZero = T)

#MEASURE OF SIZE: 
#Average assets- "asset5"
#With more bucketing than 11 the fit stops being monotonically decreasing
#While k=11 is not the best fit, it creates the smoothest line without losing information.
size.fit<-autoCal(train.data$asset5, 
                  train.data$default.flag,
                  k=11, plot = T, xaxis = "assets")

#PUSHING ALL TRANSFORMED VARIABLES ONTO BOTH DATA SETS-----
train.data<- pushTfrm2TRAIN(train.data, debt.fit, leverage.fit, liquidity.fit, profit.fit, size.fit)
test.data<- pushTfrm2TEST(test.data, debt.fit, leverage.fit, liquidity.fit, profit.fit, size.fit)
test.data.clean<- pushTfrm2TEST(test.data, debt.fit, leverage.fit, liquidity.fit, profit.fit, size.fit, naRemove = T)

#CREATING GLM FOR OUR VARIABLES----------------

#this is our multi-variate model (all 5 are included)
multi.variate<-glm(default.flag ~ debtT + levT + liqT + profT + sizeT,family=binomial(link="logit"),
                    data=train.data, na.action=na.exclude)

predict.multi.variate<-predict(multi.variate , newdat=test.data.clean , type="response",na.action=na.pass)

#this is our scalable model, with the variables (leverage and assets) with no NA's
scalable.model<-glm(default.flag ~ levT + sizeT ,family=binomial(link="logit"),
                    data=train.data, na.action=na.exclude)

predict.scalable<-predict(scalable.model , newdat=test.data.clean , type="response",na.action=na.pass)

#comparing ROC curves we see that our multiVariate model is much more powerfull and accurate than our scalable model
output.Scale<-colAUC(data.frame(scalable=predict.scalable, multi_variate= predict.multi.variate), 
                     test.data.clean$default.flag, plotROC=TRUE, alg=c("ROC"))

#LAST STEP----
#I would like to now use our multi-variable model but step it forward starting in 2005 and 
#see if the addition of incremental years make it more powerfull, over here I call the "do all function"
#walkForwardDf DOES EVERYTHING WE JUST DID ABOVE BY ITSELF AND WALKS FORWARD A YEAR
#NOTE this function is set on the number of baskets for variable transformation it no longer autoCals
#it returns a large list of important variables I would recommend reading over the function
allModels<-walkForwardDf(bankdata, yearX=2007)

#From the AUCs collected of all the walk forward models 2005, 2006, 2007, 2008, and 2009 I will choose the 2009 
#model since it has the highest AUC from the 5:

paste("Year: ", allModels[[1]]$year, " rocAUC: ", round(allModels[[1]]$rocAUC, 5))
paste("Year: ", allModels[[2]]$year, " rocAUC: ", round(allModels[[2]]$rocAUC, 5))
paste("Year: ", allModels[[3]]$year, " rocAUC: ", round(allModels[[3]]$rocAUC, 5))
paste("Year: ", allModels[[4]]$year, " rocAUC: ", round(allModels[[4]]$rocAUC, 5))
paste("Year: ", allModels[[5]]$year, " rocAUC: ", round(allModels[[5]]$rocAUC, 5))

#"Year:  2005  rocAUC:  0.79439"
#"Year:  2006  rocAUC:  0.78278"
#"Year:  2007  rocAUC:  0.89672"
#"Year:  2008  rocAUC:  0.947"
#"Year:  2009  rocAUC:  0.97373"


#ITEM 4: MODEL TO GENERATE PD's---------
model2009<-allModels[[5]]$model

#The following functions will be used to predict clean and RAW pds for the new dataset:


holdout.data <- pushTfrm2TEST(holdout.data, 
                          allModels[[5]]$fit$debt, 
                          allModels[[5]]$fit$lev, 
                          allModels[[5]]$fit$liq, 
                          allModels[[5]]$fit$prof, 
                          allModels[[5]]$fit$size)

holdout.data.clean <- pushTfrm2TEST(holdout.data, 
                                allModels[[5]]$fit$debt, 
                                allModels[[5]]$fit$lev, 
                                allModels[[5]]$fit$liq, 
                                allModels[[5]]$fit$prof, 
                                allModels[[5]]$fit$size, naRemove = T)


predict.holdout.raw<- predict(model2009 , newdat=holdout.data , type="response",na.action=na.pass)
predict.holdout.clean<- predict(model2009 , newdat=holdout.data.clean , type="response",na.action=na.pass)



