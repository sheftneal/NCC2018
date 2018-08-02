source("scripts/functions.R")

  # Read in data and collapse to monthly means
      dta <- readRDS("inputs/twitter_data.Rds")
        dtaw <- dta %>% group_by(cbsa,month,year) %>% summarize(depress=weighted.mean(depress,depress.c), depress2 = weighted.mean(depress2,depress2.c), depress.c = mean(depress.c), tmean=mean(tmean),ppt = sum(ppt), cbsaXmonth= first(cbsaXmonth), stateXy=first(stateXy), state=first(state), stateXmonth= first(stateXmonth))
        dtaw <- as.data.frame(dtaw)
      
    #generate month of sample
        dtaw$mos <- dtaw$month+12*(dtaw$year-2014)
        
        dtaw <- dplyr::arrange(dtaw,cbsa,year,month)  #sort data and generate lags and leads
        for (i in c(-3:0)) {  #generate lags
          dtaw[,paste0('tmean_lag',abs(i))] <- shift(dtaw$tmean,i,dtaw$cbsa)
          dtaw[,paste0('ppt_lag',abs(i))] <- shift(dtaw$ppt,i,dtaw$cbsa)
        }
        for (i in c(1)) {  #generate leads
          dtaw[,paste0('tmean_lead',abs(i))] <- shift(dtaw$tmean,i,dtaw$cbsa)
          dtaw[,paste0('ppt_lead',abs(i))] <- shift(dtaw$ppt,i,dtaw$cbsa)
        }
        
      
      #bootstrap the main model for both codings
      # run bootstrap of base model for US (county-month + state-year FE)
          cc <- unique(dtaw$cbsa)
          set.seed(1)
          out1 <- out2 <- c()
          for (i in 1:1000) {
            tryCatch( { samp <- data.frame(cbsa=sample(cc,length(cc),replace=T))  #get some weird samples that screw stuff up
            subdata <- inner_join(dtaw,samp)
            reg1 <- summary(felm(depress ~ tmean + ppt | cbsa + stateXmonth + stateXy, data=subdata, weights = subdata$depress.c))$coefficients[c("tmean"),"Estimate"]
            out1 <- c(out1,reg1)
            reg2 <- summary(felm(depress2 ~ tmean + ppt | cbsa + stateXmonth + stateXy, data=subdata, weights = subdata$depress.c))$coefficients[c("tmean"),"Estimate"]
            out2 <- c(out2,reg2)
            }, error=function(e){})
            if (i%in%seq(100,900,100)) {print(i)}
          }
          
          boot_runs <- list(out1, out2)
          
      
      xx=-10:30
      mods <- list()
      
      #full sample, coding 1
          mod <- felm(depress ~ tmean + ppt | cbsa + stateXmonth + stateXy | 0 | cbsa, data=dtaw, weights=dtaw$depress.c)
              yy = data.frame(xx,base=coef(mod)[1]*xx)
          mod <- felm(depress ~ tmean + ppt | cbsa + month + year | 0 | cbsa, data=dtaw, weights=dtaw$depress.c)
              yy = data.frame(yy,year=coef(mod)[1]*xx)
          mod <- felm(depress ~ tmean + ppt + as.factor(state)*mos | cbsa + stateXmonth | 0 | cbsa, data=dtaw, weights=dtaw$depress.c)
              yy = data.frame(yy,mos=coef(mod)[1]*xx)
          mod <- felm(depress ~ tmean + ppt | cbsaXmonth + stateXy | 0 | cbsa, data=dtaw, weights=dtaw$depress.c)
              yy = data.frame(yy,stateyear=coef(mod)[1]*xx)
          mod <- felm(depress ~ tmean + ppt | cbsa + stateXmonth + stateXy | 0 | cbsa, data=dtaw)
              yy = data.frame(yy,noweight=coef(mod)[1]*xx)
          mod <- felm(depress ~ tmean + ppt | cbsaXmonth + stateXy | 0 | cbsa, data=dtaw)
              yy = data.frame(yy,stateyearnoweight=coef(mod)[1]*xx)
          mods[[1]] <- yy
      
      #coding 2
          mod <- felm(depress2 ~ tmean + ppt | cbsa + stateXmonth + stateXy | 0 | cbsa, data=dtaw,weights=dtaw$depress.c)
              yy = data.frame(xx,base=coef(mod)[1]*xx)
          mod <- felm(depress2 ~ tmean + ppt | cbsa + month + year | 0 | cbsa, data=dtaw, weights=dtaw$depress.c)
              yy = data.frame(yy,year=coef(mod)[1]*xx)
          mod <- felm(depress2 ~ tmean + ppt + as.factor(state)*mos | cbsa + stateXmonth | 0 | cbsa, data=dtaw, weights=dtaw$depress.c)
              yy = data.frame(yy,mos=coef(mod)[1]*xx)
          mod <- felm(depress2 ~ tmean + ppt | cbsaXmonth + stateXy | 0 | cbsa, data=dtaw, weights=dtaw$depress.c)
              yy = data.frame(yy,stateyear=coef(mod)[1]*xx)
          mod <- felm(depress2 ~ tmean + ppt | cbsa + stateXmonth + stateXy | 0 | cbsa, data=dtaw)
              yy = data.frame(yy,noweight=coef(mod)[1]*xx)
          mod <- felm(depress2 ~ tmean + ppt | cbsaXmonth + stateXy | 0 | cbsa, data=dtaw)
              yy = data.frame(yy,stateyearnoweight=coef(mod)[1]*xx)
          mods[[2]] <- yy
      
      
      # now make plot
          brs <- c(mean(dtaw$depress),mean(dtaw$depress2))
          colz <- c(NA,"black","red","orange","blue", "black","blue")
          ltyz <- c(rep(1,5),2,2)
      
      #get values for histogram
          bb = 50
          ht <- hist(dtaw$tmean,breaks=bb,plot=F)
          bb <- length(ht$breaks)  
      
      
      
      
      
      
      pdf(file="outputs/raw_figures/Figure4.pdf",height=5,width=10)
      
          par(mfrow=c(1,2))
          
          for (mm in 1:2) {
            yy <- mods[[mm]]
            nn <- dim(yy)[2]
            boot <- boot_runs[[mm]]
            est <- as.matrix(boot)%*%matrix(xx,ncol=length(xx))
            est <- est - est[,which(xx==10)] 
            ci <- apply(est,2,function(x) quantile(x,probs=c(0.025,0.975)))/brs[mm]*100
            yyr <- yy
            
            for (i in 2:nn) {
              yyr[,i] <- (yy[,i] - yy[yy$xx==10,i])/brs[mm]*100
            }
           
            pmin = -20
            
            plot(1,type="n",las=1,ylim=c(pmin,20),xlim=c(-10,30),xlab="average monthly temperature (C)",ylab="% change in depressive tweets")
            abline(h=0,lty=2,col="black",lwd=0.5)
            polygon(c(xx,rev(xx)),c(ci[1,],rev(ci[2,])),col="lightblue",border = NA)
            for (i in 3:nn) {
              lines(xx,yyr[,i],col=colz[i],lty=ltyz[i])
            }
            
            lines(xx,yyr[,2],col="black",lwd=2)
            
            rect(ht$breaks[1:(bb-1)],pmin,ht$breaks[2:bb],pmin+ht$counts/max(ht$counts)*5,col="grey90")
              nms <- yyr[dim(yyr)[1],2:dim(yyr)[2]]
              text(30,nms,names(nms),cex=0.5,pos=4)  #which model is which for labeling
          }
          
      dev.off()
      
