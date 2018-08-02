source("scripts/functions.R")

##################################
###### Run 1,000 bootstraps #####
##################################


      #load and sort US data
        data_us <- read.csv("inputs/SuicideData_US.csv")%>% arrange(fips,year,month)  #sort data
        
      #generate lags and leads  
        for (i in c(-1:0)) {  #generate lags
          data_us[,paste0('tmean_lag',abs(i))] <- shift(data_us$tmean,i,data_us$fips)
          data_us[,paste0('prec_lag',abs(i))] <- shift(data_us$prec,i,data_us$fips)
        }
        for (i in c(1)) {  #generate leads
          data_us[,paste0('tmean_lead',abs(i))] <- shift(data_us$tmean,i,data_us$fips)
          data_us[,paste0('prec_lead',abs(i))] <- shift(data_us$prec,i,data_us$fips)
        }

     
        ###########################################################
        #The below code has been commented out because it
        #takes ~20 minutes to run. The stored data
        #from thesese bootstrap runs will be loaded below.
        #Uncomment out this section to run bootstraps and replace
        #csv files with updated data.      
        ########################################################### 
        #   
        #   # #define regression equation
        #       fmla <- as.formula("rate_adj ~ tmean + tmean_lag1 + tmean_lead1 + prec + prec_lag1 + prec_lead1 | fipsmo + stateyear")
        #       n_clust <- length(unique(data_us$fips))
        # 
        #   #bootstrap 1,000x
        #     est_us <- sapply(1:1000, function(x){run_reg_bstrap(data_us, "fips")})
        #     write_csv(data.frame(est_us), path = "inputs/bootstrap_runs/BootstrapProjections_us.csv")
        # 
        #   
        # #Mexico
        #   
        #     #load mex data
        #       data_mex <- read.csv('inputs/SuicideData_Mexico.csv')
        #       yrs=1990:2010
        #     
        #     # generate avg temp
        #       avgtemp <- data_mex %>% group_by(id) %>% summarise(avgtemp = mean(tmean,na.rm=T))
        #       data_mex <- left_join(data_mex,avgtemp,by="id")
        #       
        #     # generate gender-specific rates
        #       data_mex$rate_f <- (data_mex$suicides_female/(data_mex$population/2))*100000
        #       data_mex$rate_m <- (data_mex$suicides_male/(data_mex$population/2))*100000
        #     
        #     #sort and generate lags and leads
        #       data_mex <- dplyr::arrange(data_mex,id,year,month)
        #       for (i in c(-1:0)) {  #generate lags
        #         data_mex[,paste0('tmean_lag',abs(i))] <- shift(data_mex$tmean,i,data_mex$id)
        #         data_mex[,paste0('prec_lag',abs(i))] <- shift(data_mex$prec,i,data_mex$id)
        #       }
        #       for (i in c(1)) {  #generate leads
        #         data_mex[,paste0('tmean_lead',abs(i))] <- shift(data_mex$tmean,i,data_mex$id)
        #         data_mex[,paste0('prec_lead',abs(i))] <- shift(data_mex$prec,i,data_mex$id)
        #       }
        #       
        #       n_clust <- length(unique(data_mex$id))
        #       fmla <- as.formula("rate_adj ~ tmean + tmean_lag1 + tmean_lead1 + prec + prec_lag1 + prec_lead1 | id + statemonth + state_year")
        #       
        #       
        #   
        #   #apply function 1,000x 
        #     est_mex <- sapply(1:1000, function(x){run_reg_bstrap(data_mex, "id")})
        #     write_csv(data.frame(est_mex), path = "inputs/bootstrap_runs/BootstrapProjections_mex.csv")
          
          
        
        
################################
###### Prepare plot data #####
################################

    #load bootstrap runs from above        
        est_us <- read.csv("inputs/bootstrap_runs/BootstrapProjections_us.csv")
        est_mex <- read.csv("inputs/bootstrap_runs/BootstrapProjections_mex.csv")
        
    #base rates (per 100,000 per month)
        data <- read.csv('outputs/HeterogeneityFigureEstimates.csv')  #fig 2 data to get historical base rate
        bmex <- data$mean[data$country=="Mex"&data$var=="main"]
        
    # generate US base rate for more recent period
        data <- read.csv("inputs/SuicideData_US.csv") 
        data$yr <- as.numeric(as.character(data$year))
        yrs <- 1990:2004
        ind <- data$yr%in%yrs
        data <- data[ind,]
        bus <- weighted.mean(data$rate_adj,data$popw)
        
    
    #temperature projections (pop-weighted average for country as a whole)
        Tus <- read.csv('inputs/projections/TemperatureProjections_us.csv') %>% as_data_frame()
        Tmex <- read.csv('inputs/projections/TemperatureProjections_mex.csv')%>% as_data_frame()
        
        quantile(Tus$deltaT,probs=c(0.025,0.5,0.975))  #statistics reported in paper
        quantile(Tmex$deltaT,probs=c(0.025,0.5,0.975))  #statistics reported in paper
        
    
      dUS <- as.matrix(est_us)%*%matrix(Tus$deltaT,nrow=1)/bus*100
      dmex <- as.matrix(est_mex)%*%matrix(Tmex$deltaT,nrow=1)/bmex*100
    
    # make vectors for boxplot, limiting to 95% CI
        bxu <- c(dUS)
        qu <- quantile(bxu,probs=c(0.025,0.5,0.975))
        bxu[bxu<qu[1]] <- qu[1]
        bxu[bxu>qu[3]] <- qu[3]
        bxm <- c(dmex)
        qm <- quantile(bxm,probs=c(0.025,0.5,0.975))
        bxm[bxm<qm[1]] <- qm[1]
        bxm[bxm>qm[3]] <- qm[3]
        print(qu)
        print(qm)
    
    # read in impact estimates from other studies
        imp <- read_csv('inputs/OtherSucideStudies.csv') %>% as.data.frame()
    
    
    #define a transparent blue color 
        alpha = 0.2
        cll <- apply(sapply("lightblue", col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 
        
    # collate rate estimates for plotting
        att <- 1:(dim(imp)[1]+2)
        est <- c(imp$estimate,c(qu[2],qm[2]))  #put together the estimates from the other studies, and median estimates from ours
        cilo <- c(imp$loCI,c(qu[1],qm[1]))  #same with 95% CI
        cihi <- c(imp$hiCI,c(qu[3],qm[3]))
        ub = 40  #ylim max for excess death plots
        
    
    
##################################
########### Plot #################
##################################    

      pdf(file="outputs/raw_figures/Figure5.pdf",width=15,height=5,useDingbats = F)
             
          par(mfrow=c(1,3))
                
               
          #Panel (a) 
               
              plot(1,type="n",xlim=c(-12,8),ylim=c(0,length(att)+1),axes=F,xlab="% change in suicide rate",ylab="",yaxs='i',cex.lab=1.5)
                  abline(v=seq(-12,8,4),lty=2,lwd=0.8,col="grey")
                  abline(v=0,lwd=2)
                  axis(1,at=seq(-12,8,4),labels=seq(-12,8,4),cex.axis=1.5)
                  segments(cilo,att,cihi,att,lwd=3)
                  points(est,att,pch=21,bg=c(rep("black",dim(imp)[1]),"orange","lightblue"),cex=3)
                  text(-12,att,c(as.character(imp$study),"US","Mexico"),cex=0.5)
                  mtext("Change in suicide rate", side=3, adj=0, line=0.5, cex=1.5, font=2)  #add title, left justified
                  
                  
                  
          
          
          #panels (b) and (c)
          
              #  CALCULATE PROJECTED EXCESS SUICIDES BETWEEN 2000 AND 2050
              #   ASSUMPTIONS:
              #     1. LINEAR WARMING to 2050
              #     2. BASELINE SUICIDE RATE STAYS THE SAME 
              #     3. UN MEDIUM VARIANT POPULATION
              #     4. EFFECTS ARE CONSTANT ACROSS THE AGE DISTRIBUTION. (consistent with evidence in US)
              
                  # population data, from world population prospects, 2015 revision.  http://esa.un.org/unpd/wpp/Download/Standard/Population/
                      pp <- read_csv('inputs/projections/PopulationProjections.csv')  #projections, medium variant
                      ph <- read_csv('inputs/projections/PopulationProjections_Historical.csv')  #historical estimates
                      popc <- list()  #list to fill
                 
                 # make a 2 item list of population 2000-2050.  first getting historical data
                      p1 <- ph[ph[,3]=="United States of America",paste0(2000:2015)]
                      popc[[1]] <- as.numeric(gsub(" ","",unlist(p1)))*1000
                      p2 <- ph[ph[,3]=="Mexico",paste0(2000:2015)]
                      popc[[2]] <-  as.numeric(gsub(" ","",unlist(p2)))*1000
                      
                  #add in 2016-2050
                      p1 <- pp[pp[,3]=="United States of America",paste0(2016:2050)]
                      popc[[1]] <- c(popc[[1]],as.numeric(gsub(" ","",unlist(p1)))*1000)
                      p2 <- pp[pp[,3]=="Mexico",paste0(2016:2050)]
                      popc[[2]] <- c(popc[[2]],as.numeric(gsub(" ","",unlist(p2)))*1000)
                      
                      yr=2000:2050
                  
                  # excess suicides in US
                      dUS <- as.matrix(est_us)%*%matrix(Tus$deltaT,nrow=1)*12  #additional deaths per 100,000 per year by 2050 in US
                  
                  # assume this rate increases linearly between 2000 and 2050 due to linear temperature increase
                  # looping over years, calculating effect for each bootstrap x model
                      ex <- array(dim=c(dim(dUS),length(yr)))
                      for (y in 1:length(yr)) {
                        z <- (yr[y]-2000)/(2050-2000)
                        ex[,,y] <- dUS*z*popc[[1]][y]/100000
                      }
                      exs <- apply(ex,c(1,2),sum)  #total cumulative deaths
                      exc_US <- exs
                      
                  # define colors
                      alpha = 0.03
                      cll <- apply(sapply("orange", col2rgb)/255, 2, 
                                   function(x) 
                                     rgb(x[1], x[2], x[3], alpha=alpha)) 
                  
                  # define 2050 distributions for boxplotting
                      bxu <- c(exs)
                      qu <- quantile(bxu,probs=c(0.025,0.5,0.975))
                      bxu[bxu<qu[1]] <- qu[1]
                      bxu[bxu>qu[3]] <- qu[3]
                      print(qu)
                      
                  #define median for each year
                      med <- apply(ex,3,median)
                  
            #Panel (b)
                  plot(yr,cumsum(ex[1,1,])/1000,type="l",ylim=c(-8,ub),col=cll,las=1,ylab="excess suicides (cumulative, thousands)",xlab="year",lwd=0.3,xlim=c(2000,2052),cex.axis=1.5,cex.lab=1.5,axes=F)
                      for (j in 1:dim(ex)[1]) {
                        for (i in 1:dim(ex)[2]) {
                          lines(yr,cumsum(ex[j,i,])/1000,col=cll,lwd=0.3)
                        }}
                      
                  lines(yr,cumsum(med)/1000,col="black",lwd=2)
                  abline(h=0,lty=2)
                  boxplot(bxu/1000,horizontal=F,range=0,at=2052,add=T,col="orange",axes=F,boxwex=3,lty=1)
                  
                  mtext("Excess suicides by 2050, US", side=3, adj=0, line=0.5, cex=1.5, font=2)  #add title, left justified
                  axis(1,at=seq(2000,2050,10),labels=seq(2000,2050,10),cex.axis=1.5,las=1)
                  axis(2,at=seq(-10,ub,10),labels=seq(-10,ub,10),cex.axis=1.5,las=1)
                  
                  
                  # now same for mexico
                      dmex <- as.matrix(est_mex)%*%matrix(Tmex$deltaT,nrow=1)*12
                  
                  # assume this rate increases linearly between 2000 and 2050 due to linear temperature increase
                      ex <- array(dim=c(dim(dmex),length(yr)))
                      for (y in 1:length(yr)) {
                        z <- (yr[y]-2000)/(2050-2000)
                        ex[,,y] <- dmex*z*popc[[1]][y]/100000
                      }
                      exs <- apply(ex,c(1,2),sum)
                      exc_MX <- exs
                      
                  # define colors
                      alpha = 0.03
                      cll <- apply(sapply("lightblue", col2rgb)/255, 2, 
                                   function(x) 
                                     rgb(x[1], x[2], x[3], alpha=alpha)) 
                      
                  # define 2050 distributions for boxplotting
                      bxu <- c(exs)
                      qm <- quantile(bxu,probs=c(0.025,0.5,0.975))
                      bxu[bxu<qm[1]] <- qm[1]
                      bxu[bxu>qm[3]] <- qm[3]
                      print(qm)
                  
                  #define median for each year
                     med <- apply(ex,3,median)
                  
                  
          #Panel (c)
                  
                  plot(yr,cumsum(ex[1,1,])/1000,type="l",ylim=c(-8,ub),col=cll,las=1,ylab="excess suicides (cumulative, thousands)",xlab="year",lwd=0.3,xlim=c(2000,2052),cex.axis=1.5,cex.lab=1.5,axes=F)
                  
                  for (j in 1:dim(ex)[1]) {
                    for (i in 1:dim(ex)[2]) {
                      lines(yr,cumsum(ex[j,i,])/1000,col=cll,lwd=0.3)
                    }}
                  
                  lines(yr,cumsum(med)/1000,col="black",lwd=2)
                  abline(h=0,lty=2)
                  
                  boxplot(bxu/1000,horizontal=F,range=0,at=2052,add=T,col="lightblue",axes=F,boxwex=3,lty=1)
                  
                  mtext("Excess suicides by 2050, Mexico", side=3, adj=0, line=0.5, cex=1.5, font=2)  #add title, left justified
                  axis(1,at=seq(2000,2050,10),labels=seq(2000,2050,10),cex.axis=1.5,las=1)
                  axis(2,at=seq(-10,ub,10),labels=seq(-10,ub,10),cex.axis=1.5,las=1)
                  
        dev.off()
          
          
          
          
          
##################################
####  Numbers cited in text ######
##################################         
          
          print(qu)  
          print(qm)
          
          # calculate confidence interval of overall deaths (Mex + US)
          # we assume that reg parameters are uncorrelated between countries, but climate projections are
          nn <- which(Tus$model %in% Tmex$model)  # US has one more GCM than Mex, restricting to overlap
          ss <- exc_MX + exc_US[,nn]
          quantile(ss,probs=c(0.025,0.5,0.975))
          mean(ss)
          
          
          #total suicides in 2015.  data from here: http://www.who.int/gho/mental_health/suicide_rates/en/
          #mex: 5 per 100,000 127m
          #US: 14.3 per 100,000  320m
          tot = 5/100000*127000000 + 14.3/100000*320000000
          #share of global in US+MEx, 
          tot/788000