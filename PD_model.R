#PROBABILITY OF DEFAULT MODEL V1.0
#Created by Janul and Amir
#Fintech Analytics (INTA-GB.2320.10)
#FALL 2016

#LIBRARIES-----------------------------------------------------------------
library(lubridate)
bankdata.in.new$adjust_repdte<- bankdata.in.new$repdte %month+% 3
bankdata.in.new$days.to.default<- ifelse(bankdata.in.new$Default.Flag, bankdata.in.new$Default.Date-bankdata.in.new$adjust_repdte)
bankdata.in.new$Default.Date<- as.Date(bankdata.in.new$Default.Date, "%m/%d/%y")
bankdata.in.new$days.to.default<- bankdata.in.new$Default.Date-bankdata.in.new$adjust_repdte
bankdata.in.new$default.in.cur.year<- bankdata.in.new$days.to.default >0 & bankdata.in.new$days.to.default < 366
bankdata.in.new$Default.Flag<- ifelse(bankdata.in.new$default.in.cur.year, 1, 0)

#FUNCTIONS--------------------------------------------------------------------------------------------------------------------------



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


#FUNCTION TO CREATE CALIBRATION CURVE MAPPING TO A DEFAULT RATE
estimateCalibCurve	<-	function(x,outcome,k=25,plot=T,spar=0.5)	
{	
  breaks<-(0:k)/k	
  qs<-quantile(x,breaks)	
  buckets<-cut(x,qs)
  
  raw.curve<-tapply(X=outcome,IND=buckets,FUN=mean)	
  curve<-smooth.spline(breaks[-1],raw.curve,spar=spar)	
  pred<-predict(curve,breaks[-1])
  
  if(plot)	
  {	
    plot(breaks[-1],raw.curve)	
    lines(pred,col="red",lwd=3)	
  }	
  
  baseline<-mean(outcome)	
  cutoff<-qs[-1]
  PD<-pred$y	
  map<-data.frame(cutoff=cutoff,PD=PD)
  
  return(list(baseline=baseline,map=map))	
}


#FUNCTION TO USE CALIBRATION CURVE TO MAP A VARIABLE TO A DEFAULT RATE
applyCalibCurve	<-	function(x,	map,	baseline=NULL)	
{
  cutoff<-map$map$cutoff	
  PD<-map$map$PD	
  len<-length(cutoff)
  lb<-sum(cutoff<x)	
  if(lb==0)	pd<-PD[1]	
  else	if(lb>=len)	pd<-PD[len]
  else	#interpolate	
  {	
    ub<-lb+1	
    num<-x-cutoff[lb]	
    den<-cutoff[ub]-cutoff[lb]	
    wt.ub<-num/den	
    wt.lb<-1-wt.ub	
    pd<-wt.lb*PD[lb]+wt.ub*PD[ub]	
  }	
  
  if(is.null(baseline))	p.star<-pd	
  else	
  {	
    piS<-map$baseline	
    piT<-baseline	
    num<-pd-(pd*piS)	
    den<-piS-(pd*piS)+(pd*piT)-(piS*piT)	
    p.star<-piT*num/den	
  }	
  return(p.star)	
}

#DATA VIZ FOR ANALYSIS OF VARIABLES----------------------------------------------------------------------------------------------

#Measures of profitability: 
#Pre-tax return on assets- "roaptx" 
plot(bankdata.in.new$roaptx, bankdata.in.new$Default.Flag, pch = 16, xlab = "roaptx", ylab = "PD")

#Net operating income over assets- "noijy"
plot(bankdata.in.new$noijy, bankdata.in.new$Default.Flag, pch = 16, xlab = "noijy", ylab = "PD")

#Measures of leverage: 
#Total debt over total assets- "liab/asset"
plot(bankdata.in.new$liab/bankdata.in.new$asset, 
     bankdata.in.new$Default.Flag, pch = 16, xlab = "liab/asset", ylab = "PD")

#Total debt over Net operating income- "liab/idpretx"
plot(bankdata.in.new$liab/bankdata.in.new$idpretx, 
     bankdata.in.new$Default.Flag, pch = 16, xlab = "debt/NOPAT", ylab = "PD")

#Average equity over total debt- "eq5/liab" Net loans&leases over core deposits- "idlncorr"
plot(bankdata.in.new$eq5/bankdata.in.new$liab, 
     bankdata.in.new$Default.Flag, pch = 16, xlab = "avg_equity/total_debt", ylab = "PD")

#Common tier 1 equity over risk-adjusted assets- "rbct1cer"

#Measures of debt coverage: 
#Pre-tax Net operating income over interest expense- "idpretx/eintexp"
#Net income minus cash dividends over interest expense- "(netinc-eqcdiv)/eintexp"

#Measure of liquidity: 
#Tier 1 Capital over Total risk-weight assets- "rbc1rwaj"

#Measures of size: Adjusted average assets- "avassetj" 
#Average assets- "asset5"


#RUNNING CODE--------------------------------------------------------------------------------------------------------------------

#Adding default flags to bank data
bankdata.in.new$Default.Flag =ifelse(is.na(bankdata.in.new$Default.Date), 0, 1)

#adjust for repdte date 4 month lag
bankdata.in.new$repdte_adjust<- bankdata.in.new$repdte %m+% months(3)

#Total assets
model3<- glm(bankdata.in.new$Default.Flag ~ bankdata.in.new$asset , family= binomial(link = logit),na.action = na.exclude)

#plot model
plot(bankdata.in.new$asset, bankdata.in.new$Default.Flag, pch = 16, xlab = "Asset", ylab = "PD")
xweight <- seq(1, 1768657000, 1768657000/721110)
yweight <- predict(model3, list(wt = xweight),type="response")
lines(xweight, yweight)



