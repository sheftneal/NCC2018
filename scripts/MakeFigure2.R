source("scripts/functions.R")


###################################################
# Load US data and add in data from other sources
###################################################

  #load and sort US data
    data_us <- read.csv("inputs/SuicideData_US.csv") %>% arrange(fips,year,month)  #sort data

  #generate lags and leads
      for (i in c(-1:0)) {  #generate lags
        data_us[,paste0('tmean_lag',abs(i))] <- shift(data_us$tmean,i,data_us$fips)
        data_us[,paste0('prec_lag',abs(i))] <- shift(data_us$prec,i,data_us$fips)
      }
      for (i in c(1)) {  #generate leads
        data_us[,paste0('tmean_lead',abs(i))] <- shift(data_us$tmean,i,data_us$fips)
        data_us[,paste0('prec_lead',abs(i))] <- shift(data_us$prec,i,data_us$fips)
      }

  # income data from the BEA, Implicit GDP deflator data from the ST Louis Fed  https://fred.stlouisfed.org/series/GDPDEF, downloaded July 19, 2017
    inc <- read.csv("inputs/income/BEA_Income_1969_2014.csv")
        inc <- inc[inc$LineCode==3,]
        ii <- melt(inc,id.vars=c('GeoFIPS'),measure.vars=c(paste0('X',1969:2014)))
        ii <- arrange(ii,GeoFIPS,variable)
        ii$year <- substr(ii$variable,2,5)
        ii$fips <- as.numeric(as.character(ii$GeoFIPS))
        ii$income <- as.numeric(ii$value)
        ii$year <- as.numeric(ii$year)
    def <- read.csv("inputs/income/GDP_deflator.csv")  #deflator
        def$year <- as.numeric(substr(def$DATE,1,4))
        def <- def %>% group_by(year) %>% summarize(GDPd = mean(GDPDEF))
        def$GDPd <- 100/def$GDPd
        ii <- left_join(ii,def)
        ii$incomeD <- ii$income*ii$GDPd
        ii <- ii[,c('fips','year','incomeD')]
    
    data_us <- left_join(data_us,ii,by=c("fips"="fips","yr"="year"))
        data_us$incomeD <- data_us$incomeD/1000  #rescale income so coefficients are legibile

  # air conditioning data from Barreca et al 2016
      ac <- read_rds("inputs/BarrecaEtAl2016_ACData.rds")
        ac <- ac[,c("year","stfips","i2_ac")]
        ac <- ac %>% group_by(stfips,year) %>% summarise(AC=mean(i2_ac,na.rm=T))
        names(ac)[names(ac)=="stfips"] <- "state"
      data_us <- left_join(data_us,ac)

  # generate average temperature
      avgtemp <- data_us %>% group_by(fips) %>% summarize(avgtemp = weighted.mean(tmean,popw,na.rm=T))
      data_us <- left_join(data_us,avgtemp)

  # guns data
      gun <- read_rds("inputs/OkoroEtAl2005_GunData.rds")
      data_us <- left_join(data_us,gun[,c("state_fips","anygun")],by=c("state"="state_fips"))

    # merge in gender/violent data
      gend <- read_rds("inputs/SuicideCountyMonth_Gender_AgeAdj_FullPeriod.rds")
      data_us <- left_join(data_us,gend)

    # CDC_UCD data
      cdc <- read_rds("inputs/CDC_UCD_forAnalysis.rds")
      cdc$rate_adj <- as.numeric(as.character(cdc$ageadjustedrate))
      popw <- cdc %>% group_by(fips) %>% summarise(popw = mean(population,na.rm=T))
      cdc <- left_join(cdc,popw,by="fips")






#####################################################
# Load Mexico data and add in data from other sources
#####################################################
      
      #load and sort Mexico data
      data_mex <- read.csv('inputs/SuicideData_Mexico.csv') %>% arrange(id, year, month)
      yrs=1990:2010
      
      #generate lags and leads
      for (i in c(-1:0)) {  #generate lags
        data_mex[,paste0('tmean_lag',abs(i))] <- shift(data_mex$tmean,i,data_mex$id)
        data_mex[,paste0('prec_lag',abs(i))] <- shift(data_mex$prec,i,data_mex$id)
      }
      for (i in c(1)) {  #generate leads
        data_mex[,paste0('tmean_lead',abs(i))] <- shift(data_mex$tmean,i,data_mex$id)
        data_mex[,paste0('prec_lead',abs(i))] <- shift(data_mex$prec,i,data_mex$id)
      }
      

      # generate avg temp
        avgtemp <- data_mex %>% group_by(id) %>% summarise(avgtemp = mean(tmean,na.rm=T))
        data_mex <- left_join(data_mex,avgtemp,by="id")
        
      # generate gender-specific rates
        data_mex$rate_f <- (data_mex$suicides_female/(data_mex$population/2))*100000
        data_mex$rate_m <- (data_mex$suicides_male/(data_mex$population/2))*100000
        

      


###################################################
# Run regressions
###################################################
        
  resU <- resM <- list()  #lists to save results

    # US
        resU[[1]] <- fitmod(data=data_us,name="main")  # full sample, US
        resU[[2]] <- fitmod(data=cdc,fe="fips + year", cl="fips",name="cdc")  #CDC annual data, US
        resU[[3]] <- fitmod(data=data_us, inter="yes", intvar="avgtemp",name="temp")
        resU[[4]] <- fitmod(data=data_us, inter="yes", intvar="incomeD",name="income")
        resU[[5]] <- fitmod(data=data_us, inter="yes", intvar="AC",name="ac")
        resU[[6]] <- fitmod(data=data_us, inter="yes", intvar="anygun", name="gun")
        resU[[7]] <- fitmod(data=data_us, inter="yes", intvar="popw", name="pop")
        resU[[8]] <- runreg(lag=1,lead=1,y="rate_adj",data=data_us,weights=data_us$popw)  #generate leads and lags
        resU[[9]] <- fitmod(data=data_us[data_us$year>1978,], y="rate_adj_malenonviol", name="male, nonviolent")  #type of suicide data appear to start in 1979
        resU[[10]] <- fitmod(data=data_us[data_us$year>1978,], y="rate_adj_maleviol", name="male, violent")
        resU[[11]] <- fitmod(data=data_us[data_us$year>1978,], y="rate_adj_femalenonviol", name="female, nonviolent")
        resU[[12]] <- fitmod(data=data_us[data_us$year>1978,], y="rate_adj_femaleviol", name="female, violent")

  
    
    # mexico
        resM[[1]] <- fitmod(data=data_mex,fe="id + statemonth + state_year",cl="id",name="main")  
        resM[[2]] <- fitmod(data=data_mex, fe="id + statemonth + state_year",cl="id",inter="yes", intvar="avgtemp",name="temp")
        resM[[3]] <- fitmod(data=data_mex,fe="id + statemonth + state_year",cl="id", inter="yes", intvar="incearn",name="income")
        resM[[4]] <- fitmod(data=data_mex, fe="id + statemonth + state_year",cl="id",inter="yes", intvar="ac",name="ac")
        resM[[5]] <- fitmod(data=data_mex, fe="id + statemonth + state_year",cl="id",inter="yes", intvar="popw",name="pop")
        resM[[6]] <- runreg(lag=1,lead=1,y="rate_adj",data=data_mex,weights=data_mex$popw, fe="id + statemonth + state_year",cl="id")  #lag and lead
        resM[[7]] <- fitmod(data=data_mex,y="rate_f", fe="id + statemonth + state_year",cl="id",name="female") #female
        resM[[8]] <- fitmod(data=data_mex,y="rate_m", fe="id + statemonth + state_year",cl="id",name="male")  #male



    #reorganize results
        
      us <- do.call("rbind",resU)
      us$country <- "US"
      mex <- do.call("rbind",resM)
      mex$country <- "Mex"
      
      data <- rbind(us,mex)

      #order rows for plot
      plotorder <- c(which(data$country=="US" & data$var=="main"),
                     which(data$country=="US" & data$var=="cdc"),
                     which(data$country=="Mex" & data$var=="main"),
                     which(data$var=="temp"),
                     which(data$var=="income"),
                     which(data$var=="ac"),
                     which(data$var=="pop"),
                     which(data$var%in%c("male, nonviolent","male, violent","female, nonviolent","female, violent")),
                     which(data$country=="Mex" & data$var=="male"),
                     which(data$country=="Mex" & data$var=="female"),
                     which(data$var=="gun"),
                     which(data$country=="US" & data$group%in%c("t+1","t","t-1","combined")),
                     which(data$country=="Mex" & data$group%in%c("t+1","t","t-1","combined"))
      )
      data <- data[plotorder,]
      data <- data.frame(data,n=dim(data)[1]:1)
      

      
################################################################
# Plot 
################################################################            
      
      #specify plot features
          tp <- data$est/data$mean*100
          cilo <- tp - 1.96*data$se/data$mean*100
          cihi <- tp + 1.96*data$se/data$mean*100
          n=length(tp):1
          xx <- seq(-2,4,1)
          
          br <- data$mean
          br[2] <- br[2]/12  #convert annual rate in CDC data into monthly rate for comparison
          
          nn <- n
          
          colz = rep("black",length(tp))
          colz[data$country=="Mex"] <- "white"
          
          txt1 <- c("Full sample","By average temperature",  "By income", "By AC penetration","By population", "By sex and suicide type", "By gun ownership", "Placebo and displacement tests")
          txt2 <- c("US, monthly 1981-2004 (NHCS)","US, annual 1999-2013 (CDC)","Mexico, 1990-2010", "Below median temperature","Above median temperature","Below median temperature","Above median temperature","Below median income", "Above median income", "Below median income", "Above median income","Below median penetration", "Above median penetration", "Below median penetration", "Above median penetration", "Below median population", "Above median population", "Below median population", "Above median population", "Male, nonviolent", "Male, violent", "Female, nonviolent", "Female, violent", "Male", "Female", "Below median ownership", "Above median ownership", "next month temperature (t+1)", "current month temperature (t)", "previous month temperature (t-1)","current + previous", "next month temperature (t+1)", "current month temperature (t)", "previous month temperature (t-1)","current + previous")
          
          ci <-  formatC(round(tp,2),digits=2,format="f")%&%" ("%&%formatC(round(cilo,2),digits=2,format="f")%&%","%&%formatC(round(cihi,2),digits=2,format="f")%&%")" #this makes sure lenght of character string is the same
          
          lz <- c(8.5,10.5,16.5, 20.5, 24.5, 28.5, 32.5)  #horizontal break lines to divide up the different subgroups
          
      
      #plot  
          
      pdf(file="outputs/raw_figures/Figure2.pdf",width=9,height=9,useDingbats = F)
           
            par(mgp=c(2,0.8,0),mar=c(5,4,4,3),lend=1)
            
            plot(1,type="n",xlim=c(-9,7),ylim=c(1,dim(data)[1]),axes=F,ylab="",xlab="% change in monthly suicide rate")
              
            abline(v=xx,lwd=0.5,col="grey",lty=2)
            abline(v=0,lwd=0.5)
            segments(cilo,nn,cihi,nn)
            
            points(tp,nn,pch=21,bg=colz,cex=1.2)
            
            abline(h=lz,lwd=0.75,lty=1,col="grey") #with subgroup ordering
            axis(1,at=xx,labels=xx)
            
            text(-9.5,nn,txt2,cex=0.65,pos=4)
            text(-5.5,nn,format(data$N,big.mark = ","),pos=4,cex=0.65)  #this adds comma in number
            text(-4,nn,formatC(round(br,2),digits=2,format="f"),cex=0.65,pos=4)
            text(6.5,nn,ci,cex=0.65)
      
      dev.off()
      
      
      
      #write out heterogeneity results to be used in later plot
      write_csv(data, path = "outputs/HeterogeneityFigureEstimates.csv")
      
      