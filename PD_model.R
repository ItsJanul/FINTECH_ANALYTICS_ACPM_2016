#PROBABILITY OF DEFAULT MODEL V1.0
#Created by Janul and Amir
#Fintech Analytics (INTA-GB.2320.10)
#FALL 2016

#LIBRARIES-----------------------------------------------------------------
library(lubridate)
library(tidyverse)
library(microbenchmark)

#change # of rows to print on tibble
options(tibble.print_max = 1000, tibble.print_min = 200)

#FUNCTIONS-----------------------------------------------------------------

#FUNCTION TO CALCULATE AUC ROC FOR 2 DIFFERENT SUBSET OF A SINGLE DATASET
subROC <- function(x,	split.val,	split.on,	score,	outcome)
{
  len <- nrow(x)
  idx <- 1:len
  
  filt <- split.on	<=	split.val
  
  score.1 <- score[filt]
  outcome.1 <- outcome[filt]
  idx.1 <- idx[filt]
  score.2 <- score[!filt]
  outcome.2 <- outcome[!filt]
  idx.2 <- idx[!filt]
  
  #use favorite AUC function here
  AUC.1 <- colAUC(data.frame(score.1), outcome.1,	alg = c(ROC))
  AUC.2 <- colAUC(data.frame(score.2), outcome.2, alg = c(ROC))
  
  AUCs<-c(AUC.1,AUC.2)
  #	AUCs<-ifelse(AUCs<0.5,1-AUCs,AUCs)
  ret<-list(AUCs=AUCs,	idx.1=idx.1,	idx.2=idx.2)
    
      
  return(ret)
}

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
transformVar <- function(x, outcome, k=25, plot=T, xaxis="breaks", forceZero = F)
{
  fit<- densityMap(x, outcome, k=k, plot=plot, xaxis=xaxis)
  output<- applyDensityMap(x, fit, forceZero)
  return(list(baseline=fit[[1]], map=fit[[2]], transformedVar=output, buckets=k))
}

#FUNCTION TO PUSH TRANFORM VARIABLES TO df
pushTfrm2DF<- function(df, debt, lev, liq, prof, size){
  df$debtT<- debt[[3]]
  df$levT<- lev[[3]]
  df$liqT<- liq[[3]]
  df$profT<- prof[[3]]
  df$sizeT<- size[[3]]
  
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


#RUNNING CODE--------------------------------------------------------------

#creating a tibble called bankdata
bankdata<-as_data_frame(bankdata.in.new)
head(bankdata)


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
profit.fit<-autoCal(train.data$roaptx, 
                    train.data$default.flag, 
                    k=50, plot = F, xaxis = "roaptx")

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

#I force k=30 with forceZero set to true the negative min is very small
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

#PUSHING ALL TRANSFORMED VARIABLES ONTO train.data
train.data<- pushTfrm2DF(train.data, debt.fit, leverage.fit, liquidity.fit, profit.fit, size.fit)

#CREATING GLM FOR OUR VARIABLES----------------
my.first.model<-glm(default.flag ~ debtT + levT + liqT + profT + sizeT,family=binomial(link="logit"),
                    data=train.data, na.action=na.exclude)

summary(my.first.model)
round(summary(my.first.model$fitted.values),5)

nondefault.records<-my.first.model$y==0 #actual non-defaulted firms
default.records<- my.first.model$y==1 #actual defaults
n.ndef<-sum(nondefault.records, na.rm=T)
n.def<-sum(default.records,na.rm=T)

ndef.PDs<- my.first.model$fitted.values[nondefault.records] #model estimates for non-defaulted firms
def.PDs<- my.first.model$fitted.values[default.records] #model estimates for defaulted firms

W<-wilcox.test(x=def.PDs,y=ndef.PDs,paired=FALSE)
aprox.AUC<- W$statistic/(n.ndef*n.def)
aprox.AUC
