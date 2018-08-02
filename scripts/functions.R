library(classInt)
library(data.table)
library(dplyr)
library(lfe)
library(multcomp)
library(plotrix)
library(RColorBrewer)
library(readr)
library(reshape2)
library(sampling)
library(sp)
library(splines)


#write a function to generate lags. data need to be sorted
shift <- function(x,shift_by,y) {
  ana <- rep(NA,abs(shift_by))
  if (shift_by > 0 ) {
    out <- c(x[(1+shift_by):length(x)],ana)
    ck <- c(y[(1+shift_by):length(y)],ana) 
    out[y!=ck] <- NA } #fills in NAs when you inappropriately drag observations across cells
  else if (shift_by < 0 ) {
    out <- c(ana,x[1:(length(x)+shift_by)])
    ck <- c(ana,y[1:(length(y)+shift_by)])
    out[y!=ck] <- NA }
  else 
    out <- x
  return(out)      
}

#define a function for easy string pasting
"%&%"<-function(x,y)paste(x,y,sep="")  


# DEFINE FUNCTIONS THAT RUN THE REGRESSIONS AND GENERATE CURATED OUTPUT

# function to fit uninteracted and interacted models
fitmod <- function(data=dta,y="rate_adj", vb = "tmean+prec",  fe="fipsmo + stateyear", cl="fips", inter="no", intvar="", name="") {
  if (inter=="yes") {
    data$above <- data[,intvar] > median(unlist(data[,intvar]),na.rm=T)  #define interaction variables
    data$tmeanabove <- data$tmean*data$above
    data$precabove <- data$prec*data$above
    vb <- "tmean + tmeanabove + prec + precabove"
  }
  fmla <- as.formula(paste0(y,"~",vb, " | ",fe," | 0 | ",cl))
  mod <- felm(fmla , data=data, weights=data$popw)
  coef <- summary(mod)$coefficients
  if (inter=="no") {
    mean <- weighted.mean(unlist(data[,y]),data$popw,na.rm=T)
    out <- data.frame(est=coef["tmean",1],se=coef["tmean",2],mean=mean,var=name,group="none",N=mod$N)
  } else {
    mns <- data %>% group_by(above) %>% summarise(mean=weighted.mean(rate_adj,popw,na.rm=T))
    lincom <- summary(glht(mod,linfct="tmean + tmeanabove = 0"))$test
    lc <- c(lincom$coefficients, lincom$sigma)
    r1 <- data.frame(est=coef["tmean",1],se=coef["tmean",2],mean=mns[1,2],var=name,group="below",N=mod$N/2)
    r2 <- data.frame(est=lc[1],se=lc[2],mean=mns[2,2],var=name,group="above",N=mod$N/2)
    out <- rbind(r1,r2)
  }
  out
}

# function to run regs on data where lags have been generated, and to report the sum of the lagged coeff with standard error
# defaults are set to run with US data with arguments for changing to mexico data
runreg <- function (lag=0,lead=0,y="rate_adj",temp="tmean",prec="prec",data=dta,weights=dta$popw,fe="fipsmo + stateyear",cl="fips") {
  if (lead==0) {
    ll <- c(paste0(temp,"_lag",0:lag),paste0(prec,"_lag",0:lag)) }
  else {
    ll <- c(paste0(temp,"_lead",lead:1),paste0(temp,"_lag",0:lag),paste0(prec,"_lead",lead:1),paste0(prec,"_lag",0:lag)) }
  lgs <- paste(ll,collapse="+")
  fmla <- as.formula(paste0(y," ~",lgs, " | ",fe," | 0 | ",cl))
  mod <- felm( fmla, data=data,weights=weights)
  coef <- summary(mod)$coefficients
  mn <- weighted.mean(unlist(data[,y]),weights,na.rm=T)
  out <- list()
  vars = 1:(lag+lead+1)  #CPI
  vars1 = (1+lead):(lag+lead+1)  #lag coefficients only, CPI
  r1 <- data.frame(est=coef[vars,1],se=coef[vars,2],mean=mn,var="leadlag",group=c("t+1","t","t-1"),N=mod$N)
  r2 <- data.frame(est=sum(mod$coefficients[vars1,1]), se=sqrt(sum(vcov(mod)[vars1,vars1])),mean=mn,var="leadlag",group="combined",N=mod$N  )
  out <- rbind(r1,r2)
}




#create function that takes bootstrap sample and then runs regression with it
run_reg_bstrap <- function(data, clustername){
  #srswr = simple random sampling with replacement
  sub_data <- getdata(data, cluster(data = data, clustername = clustername, size = n_clust, method = "srswr")) 
  #run reg but then just pull out sum of relevant coefficients
  output <- sum(summary(felm(fmla, weights = sub_data$popw, data = sub_data))$coefficients[c("tmean","tmean_lag1"),"Estimate"]) 
  return(output)
}#end function
