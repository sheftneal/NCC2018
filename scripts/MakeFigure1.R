source("scripts/functions.R")


### Bootstrap CI for main specification ###

      #Load US data
      data_us <- read.csv("inputs/SuicideData_US.csv")

      ###########################################################
      #The below code has been commented out because it
      #takes ~30 minutes to run. The stored data
      #from thesese bootstrap runs will be read-in below.
      #Uncomment out this section to run bootstraps and replace
      #file `BootstrapMainModel_us.csv' with updated data.      
      ###########################################################      
      # run bootstrap of base model for US (county-month + state-year FE). this takes a while. 
      # cc <- unique(data_us$fips)
      # set.seed(1)
      #     out <- c()
      #     for (i in 1:1000) {
      #       tryCatch( { samp <- data.frame(fips=sample(cc,length(cc),replace=T))  #use tryCatch() because occasionally weird samples throw an error
      #           subdata <- inner_join(data_us,samp)
      #           reg <- summary(felm(rate_adj ~ tmean + prec | fipsmo + stateyear, weights = subdata$popw, data = subdata))$coefficients[c("tmean"),"Estimate"]
      #           out <- c(out,reg)
      #       }, error=function(e){})
      #       print(i)
      #       }
      #     write_csv(data.frame(x = 1:length(out), est = out),path = "inputs/bootstrap_runs/BootstrapMainModel_us.csv")
    
  
  
      #Load Mexico data      
      data_mex <- read.csv('inputs/SuicideData_Mexico.csv')
      
      ###########################################################
      #The below code has been commented out because it
      #takes ~30 minutes to run. The stored data
      #from thesese bootstrap runs will be read-in below.
      #Uncomment out this section to run bootstraps and replace
      #file `BootstrapMainModel_mex.csv' with updated data.      
      ########################################################### 
      # # run bootstrap of base model for Mex. also takes a while. 
      #   cc <- unique(data_mex$id)
      #   set.seed(1)
      #   out <- c()
      #   for (i in 1:1000) {
      #     tryCatch( { samp <- data.frame(id=sample(cc,length(cc),replace=T))   #use tryCatch() because occasionally weird samples throw an error
      #         subdata <- inner_join(data_mex,samp)
      #         reg <- summary(felm(rate_adj ~ tmean + prec | id + statemonth + state_year, weights = subdata$popw, data = subdata))$coefficients[c("tmean"),"Estimate"]
      #         out <- c(out,reg)
      #     }, error=function(e){})
      #     print(i)
      #   }
      #   write_csv(data.frame(x = 1:length(out), est = out),path="inputs/bootstrap_runs/BootstrapMainModel_mex.csv")



### Run alternative specifications ###
              
        #US
          data_us$yearmonth <- data_us$yr*100+data_us$month
            xx=-20:30
      
          mod1 <- felm(rate_adj ~ tmean + prec | fipsmo + stateyear | 0 | fips , data=data_us, weights=data_us$popw)  # stateyear FE
            yy = data.frame(xx,stateyear=coef(mod1)[1]*xx)
          mod2 <- felm(rate_adj ~ tmean + prec | fipsmo + stateyear | 0 | fips , data=data_us)  # state-year no weights
            yy = data.frame(yy,noweight=coef(mod2)[1]*xx)
          mod3 <- felm(rate_adj ~ tmean + prec | fipsmo + year | 0 | fips , data=data_us, weights=data_us$popw)  # year FE
            yy = data.frame(yy,year=coef(mod3)[1]*xx)
          mod4 <- felm(rate_adj ~ tmean + prec + as.factor(state)*yr | fipsmo + year | 0 | fips , data=data_us, weights=data_us$popw)  # year FE + time trend
            yy = data.frame(yy,yearTT=coef(mod4)[1]*xx)
          mod5 <- felm(rate_adj ~ tmean + prec | fipsmo + yearmonth | 0 | fips , data=data_us, weights=data_us$popw)  # year-month FE
            yy = data.frame(yy,yearmonth=coef(mod5)[1]*xx)
          mod6 <- felm(rate_adj ~ poly(tmean,3,raw=T) + prec | fipsmo + stateyear | 0 | fips , data=data_us, weights=data_us$popw)  # stateyear FE, polynomial
            yy = data.frame(yy,poly=as.numeric(t(as.matrix(coef(mod6)[1:3]))%*%t(matrix(nrow=length(xx),ncol=3,data=poly(xx,3,raw=T)))))
          mod7 <- felm(rate_adj ~ ns(tmean,knots=c(0,10,20)) + prec | fipsmo + stateyear | 0 | fips , data=data_us, weights=data_us$popw)  # stateyear FE, spline 3 knots
           yy = data.frame(yy,spline=as.numeric(t(as.matrix(coef(mod7)[1:4]))%*%t(matrix(nrow=length(xx),ncol=4,data=ns(xx,knots=c(0,10,20))))))
          mod8 <- felm(rate_adj ~ ns(tmean,df=8) + prec | fipsmo + stateyear | 0 | fips , data=data_us, weights=data_us$popw)  # stateyear FE
           yy = data.frame(yy,spline7=as.numeric(t(as.matrix(coef(mod8)[1:8]))%*%t(matrix(nrow=length(xx),ncol=8,data=ns(xx,df=8)))))
          

        # Mexico
          data_mex$yearmonth <- data_mex$year*100+data_mex$month
            xx=5:35
          
          modata_mex1 <- felm(rate_adj ~ tmean + prec | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)
            yym = data.frame(xx,stateyear=coef(modata_mex1)[1]*xx)
          modata_mex2 <- felm(rate_adj ~ tmean + prec | id + statemonth + state_year | 0 | id , data=data_mex)  #no weights
            yym = data.frame(yym,noweights=coef(modata_mex2)[1]*xx)
          modata_mex3 <- felm(rate_adj ~ tmean + prec | id + statemonth + year | 0 | id , data=data_mex, weights=data_mex$popw)  #year FE
            yym = data.frame(yym,year=coef(modata_mex3)[1]*xx)
          modata_mex4 <- felm(rate_adj ~ tmean + prec + as.factor(state)*year | id + statemonth + year | 0 | id , data=data_mex, weights=data_mex$popw)  #state TT
           yym = data.frame(yym,yearTT=coef(modata_mex4)[1]*xx)
          modata_mex5 <- felm(rate_adj ~ tmean + prec | id + statemonth + yearmonth | 0 | id , data=data_mex, weights=data_mex$popw)  #year-month
            yym = data.frame(yym,yearmonth=coef(modata_mex5)[1]*xx)
          modata_mex6 <- felm(rate_adj ~ poly(tmean,3,raw=T) + prec | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)  #polynomial
            yym = data.frame(yym,poly=as.numeric(t(as.matrix(coef(modata_mex6)[1:3]))%*%t(matrix(nrow=length(xx),ncol=3,data=poly(xx,3,raw=T)))))
          modata_mex7 <- felm(rate_adj ~ ns(tmean,knots=c(10,20,30)) + prec | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)  #polynomial
            yym = data.frame(yym,spline=as.numeric(t(as.matrix(coef(modata_mex7)[1:4]))%*%t(matrix(nrow=length(xx),ncol=4,data=ns(xx,knots=c(10,20,30))))))
          modata_mex8 <- felm(rate_adj ~ ns(tmean,df=8) + prec | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)  #polynomial
            yym = data.frame(yym,spline7=as.numeric(t(as.matrix(coef(modata_mex8)[1:8]))%*%t(matrix(nrow=length(xx),ncol=8,data=ns(xx,df=8)))))



### Plot Figure ###
        
      pdf(file="outputs/raw_figures/Figure1.pdf",height=8,width=10)
            par(mfrow=c(1,2))
        
        
        # US panel
            xx=-20:30
            nn <- dim(yy)[2]
            
            #calculate weighted mean for base rate
            br <- weighted.mean(data_us$rate_adj,data_us$popw)  
            
            #read-in bootstrapped runs from above to get CI
            boot <- read.csv("inputs/bootstrap_runs/BootstrapMainModel_us.csv")
              xx=-20:30
              est <- as.matrix(boot$est)%*%matrix(xx,ncol=length(xx))
              est <- est - est[,which(xx==10)] 
              ci <- apply(est,2,function(x) quantile(x,probs=c(0.025,0.975)))/br*100
              yyr <- yy
              for (i in 2:nn) {
                yyr[,i] <- (yy[,i] - yy[yy$xx==10,i])/br*100
              }
              
            #plot response curves + ci 
            plot(1,type="n",las=1,ylim=c(-50,50),xlim=c(-20,40))
                polygon(c(xx,rev(xx)),c(ci[1,],rev(ci[2,])),col="lightblue",border = NA) #ci
            lines(xx,yyr[,2],lwd=2)
           
             abline(h=0,lty=2,col="black",lwd=0.5)
           
            #alternative specs
            clz = c(NA,NA,rep("orange",2),rep("red",2),rep("blue",3))
            lp = c(NA,NA,rep(c(1,2),3),3)
          
            for (i in 3:nn) {
              lines(xx,yyr[,i],col=clz[i],lty=lp[i],lwd=2)
            }
            
            #plot hist below
              bb = 50
              ht <- hist(data_us$tmean,breaks=bb,plot=F)
              bb <- length(ht$breaks)  #hist() doesn't follow your exact instructions sometimes...
              rect(ht$breaks[1:(bb-1)],-50,ht$breaks[2:bb],-50+ht$counts/max(ht$counts)*10,col="lightblue")
            
            text(30,yyr[dim(yyr)[1],2:nn],names(yyr)[2:nn],pos=4,cex=0.5)
              
               
                 
        # mexico panel
        xx=5:35
        nn <- dim(yym)[2]

        #calculate weighted mean for base rate
        br <- weighted.mean(data_mex$rate_adj,data_mex$popw,na.rm=T)
        
        #read-in bootstrapped runs from above to get CI
        boot <- read.csv("inputs/bootstrap_runs/BootstrapMainModel_mex.csv")
          est <- as.matrix(boot$est)%*%matrix(xx,ncol=length(xx))
          est <- est - est[,which(xx==20)] 
          ci <- apply(est,2,function(x) quantile(x,probs=c(0.025,0.975)))/br*100
          yyr2 <- yym
          for (i in 2:nn) {
                yyr2[,i] <- (yym[,i] - yym[yym$xx==20,i])/br*100
          }
          
        #plot response curves + ci 
          plot(1,type="n",las=1,ylim=c(-50,50),xlim=c(0,40))
            polygon(c(xx,rev(xx)),c(ci[1,],rev(ci[2,])),col="lightblue",border = NA)
            lines(xx,yyr2[,2],lwd=2)
        
        abline(h=0,lty=2,col="black",lwd=0.5)
        
        #alternative specs
          clz = c(NA,NA,rep("orange",2),rep("red",2),rep("blue",3))
          lp = c(NA,NA,rep(c(1,2),3),3)
        
            for (i in 3:nn) {
              lines(xx,yyr2[,i],col=clz[i],lty=lp[i],lwd=2)
            }
        
        #plot hist below
            bb = 40
            ht <- hist(data_mex$tmean,breaks=bb,plot=F)
            bb <- length(ht$breaks)  #hist() doesn't follow your exact instructions sometimes...
            rect(ht$breaks[1:(bb-1)],-50,ht$breaks[2:bb],-50+ht$counts/max(ht$counts)*10,col="lightblue")
            
        
        text(35,yyr2[dim(yyr2)[1],2:nn],names(yyr2)[2:nn],pos=4,cex=0.5)
        
    dev.off()
