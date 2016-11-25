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
applyDensityMap	<-	function(x,	map)	
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
    #if (pd>0 || is.na(pd)) output[i]<- pd
    #else output[i]<- 0
    output[i]<- pd
  }
  return(output)
}

#FUNCTION TO TRANSFORM VARIABLE TO PD
transformVar <- function(x, outcome, k=25, plot=T, xaxis="breaks")
{
  fit<- densityMap(x, outcome, k=k, xaxis=xaxis)
  output<- applyDensityMap(x, fit)
  return(list(baseline=fit[[1]], map=fit[[2]], transformedVar=output, buckets=k))
}

#AUTO-FIT DENSITY ESTIMATE
autoCal<- function(x, outcome, k=25, plot=T, xaxis="breaks")
{
  output<-transformVar(x, outcome, k = k, plot = plot, xaxis = xaxis)
  
  while(summary(output[[3]])[[1]]<0){
    k=k+1
    output<-transformVar(x, outcome, k = k, plot = plot, xaxis = xaxis)
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


#All date adjustments and flags are below
bankdata<- bankdata %>%
  mutate(Default.Date= mdy(Default.Date)) %>%
  mutate(repdte.adjust= repdte %m+% months(3)) %>%
  mutate(days.to.default= ifelse (is.na(Default.Date), NA, 
                                  Default.Date-repdte.adjust)) %>%
  mutate(default.flag= ifelse (is.na(days.to.default), 0, ifelse(days.to.default >0 & days.to.default <= 365.25, 1, 0)))


#Setting training set
set.seed(1);
training.sample.naive<-runif(dim(bankdata)[1])<0.70
training.sample.date<-(year(bankdata$repdte.adjust)<2005)


#TESTING DIFFERENT VARIABLES

#MEASURE OF PROFITABILITY: 
#Pre-tax return on assets- "roaptx" 
profit.fit<-autoCal(bankdata$roaptx, bankdata$default.flag, plot = T, xaxis = "roaptx")

#MEASURE OF LEVERAGE: 
#Total asset over total liability- "asset/liability"
bankdata$lev<-bankdata$asset/(bankdata$liab+1)
leverage.fit<-densityMap(bankdata$lev, bankdata$default.flag, xaxis = "leverage")

#MEASURES OF DEBT COVERAGE: 
#Pre-tax Net operating income over interest expense- "idpretx/eintexp"
bankdata$noi_int<-bankdata$idpretx/(bankdata$eintexp+1)

test1<-transformVar(bankdata$noi_int, bankdata$default.flag, k=50, xaxis = "debt_coverage")
summary(test1)

#MEASURE OF LIQUIDITY: 
#Tier 1 Capital over Total risk-weight assets- "rbc1rwaj"
liquidity.fit<- densityMap(bankdata$rbc1rwaj, bankdata$default.flag, xaxis = "liquidity")

#MEASURE OF SIZE: 
#Average assets- "asset5"
size.fit<- densityMap(bankdata$asset5, bankdata$default.flag, xaxis = "size")

test.fit<-transformVar2(bankdata$noi_int, bankdata$default.flag, k=k,plot = F, xaxis = "debt_coverage")

microbenchmark(var1= transformVar(bankdata$rbc1rwaj, bankdata$default.flag, k=k, xaxis = "debt_coverage"),
               var2=transformVar2(bankdata$rbc1rwaj, bankdata$default.flag, k=k,plot = F, xaxis = "debt_coverage"),
               times = 5)


