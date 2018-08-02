source("scripts/functions.R")

##########################################
######## Panels (a) and (b)   ############
##########################################


###### Panel (a) ###### 

# load US data and interact with time effects
data_us <- read.csv("inputs/SuicideData_US.csv")

    yrs <- 1968:2004
    data_us$year <- as.factor(data_us$year)

    #run model
      mod <- felm(rate_adj ~ tmean*year + prec*year  | fipsmo + stateyear | 0 | fips , data=data_us, weights=data_us$popw)
        coef <- summary(mod)$coefficients
        vars <- grep("tmean",rownames(coef))
        vcov <- vcov(mod)[vars,vars]

      #pull out and proces results  
        coef <- coef[vars,]
          b <- coef[1,1]
          se <- coef[1,2]
  
        for (k in 2:length(vars)) {
          b = c(b,coef[k,1]+coef[1,1])
          covv <- vcov[c(1,k),c(1,k)]
          se = c(se,sqrt(sum(covv)))
        }
  
        cilo <- b - 1.96*se
        cihi <- b + 1.96*se
  
        res <- data.frame(yr=yrs,b,se,cilo,cihi)
        avg <- data_us %>% group_by(yr) %>% summarize(avg = weighted.mean(rate_adj,popw))
        res <- left_join(res,avg,by="yr")
        
        mod <- felm(rate_adj ~ tmean + prec | fipsmo +stateyear | 0 | fips , data=data_us, weights=data_us$popw)
        br <- weighted.mean(data_us$rate_adj,data_us$popw)
        eff <- coef(mod)[1]/br*100


###### Panel (b) ###### 
        
     
    # load Mexico data
      data_mex <- read.csv('inputs/SuicideData_Mexico.csv')
        data_mex$yr <- as.factor(data_mex$year)
        yrs=1990:2010

    #run time interacted model
    mod <- felm(rate_adj ~ tmean*yr + prec*yr  | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)
      coef <- summary(mod)$coefficients
      vars <- grep("tmean",rownames(coef))
      vcov <- vcov(mod)[vars,vars]
      
    #pull out and proces results  
      coef <- coef[vars,]
        b <- coef[1,1]
        se <- coef[1,2]
      
      for (k in 2:length(vars)) {
        b = c(b,coef[k,1]+coef[1,1])
        covv <- vcov[c(1,k),c(1,k)]
        se = c(se,sqrt(sum(covv)))
      }
      
      cilo <- b - 1.96*se
      cihi <- b + 1.96*se
      
      resm <- data.frame(year=yrs,b,se,cilo,cihi)
        avg <- data_mex %>%  group_by(year) %>% summarize(avg = weighted.mean(rate_adj,popw,na.rm=T)) 
        resm <- left_join(resm,avg,by="year")
      
      mod <- felm(rate_adj ~ tmean + prec | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)
        br <- weighted.mean(data_mex$rate_adj,data_mex$popw,na.rm=T)
        effm <- coef(mod)[1]/br*100

      
      
      
# Plot panels (a) and (b)
      
    pdf(file="outputs/raw_figures/Figure3_a_b.pdf",height=5,width=10,useDingbats = F)
        par(mfrow=c(1,2))
    
    #(a)
        plot(1,type = "n",las=1,ylim=c(0,1.5),xlim=c(1968,2004),ylab="% change in rate",xlab="year")  #dividing through by base rate
          abline(h=eff,col="red",lty=2)
          segments(res$yr,res$cilo/res$avg*100,res$yr,res$cihi/res$avg*100,lwd=1,col="grey")
        points(res$yr,res$b/res$avg*100,pch=19,cex=1.3)
      
    #(b)      
        plot(t,type="n",las=1,ylim=c(0,5),xlim=c(1988,2012),ylab="% change in rate",xlab="year")  #dividing through by base rate
          abline(h=effm,col="red",lty=2)
          segments(resm$year,resm$cilo/resm$avg*100,resm$year,resm$cihi/resm$avg*100,lwd=1,col="grey")
        points(resm$year,resm$b/resm$avg*100,pch=19,cex=1.3)
    
      dev.off()



#######################################################################################
### Panel c:  map of suicide/temperature effects by state in the US + Mex
########################################################################################

        
      #bootstrap state regressions
             
            #load US data
            data_us <- read.csv("inputs/SuicideData_US.csv")
            
            #load mex data
            data_mex <- read.csv('inputs/SuicideData_Mexico.csv')

            
      ###########################################################
      #The below code has been commented out because it
      #takes several hours to run. The stored data
      #from thesese bootstrap runs will be loaded below.
      #Uncomment out this section to run bootstraps and replace
      #file `Figure3_bootstrap_runs.RData' with updated data.      
      ###########################################################                    
      #   
      #   B <- 1000
      #     n1 <- nrow(data_us)
      #     n2 <- nrow(data_mex)
      #   
      #   data_us$bstrap_id <- 1:nrow(data_us)
      #   data_mex$bstrap_id <- 1:nrow(data_mex)
      #   
      #   bstrap1_main <- rep(NA, B); names(bstrap1_main)<-paste("b",1:B,sep="")
      #   bstrap1_state <- matrix(nrow = length(unique(data_us$state)), ncol = B)
      #       rownames(bstrap1_state)<-sort(unique(data_us$state))
      #       colnames(bstrap1_state)<-paste("b",1:B,sep="")
      #   
      #   bstrap2_main <- rep(NA,B); names(bstrap2_main)<-paste("b",1:B,sep="")
      #   bstrap2_state <- matrix(nrow = length(unique(data_mex$state)), ncol = B)
      #       rownames(bstrap2_state)<-sort(unique(data_mex$state))
      #       colnames(bstrap2_state)<-paste("b",1:B,sep="")
      #   
      #   
      #   us_states <- data.frame(state = sort(unique(data_us$state)))
      #   
      #   #bootstrap 
      #   for (b in 1:B){
      #   
      #     data_us.sample = left_join(data.frame(bstrap_id = sample(x = data_us$bstrap_id, size = n1, replace = TRUE)), data_us, "bstrap_id")
      #         while(length(unique(data_us.sample$state))<49){data_us.sample = left_join(data.frame(bstrap_id = sample(x = data_us$bstrap_id, size = n1, replace = TRUE)), data_us, "bstrap_id")}
      #     data_mex.sample = left_join(data.frame(bstrap_id = sample(x = data_mex$bstrap_id, size = n2, replace = TRUE)), data_mex, "bstrap_id")
      #         while(length(unique(data_mex.sample$state))<32){data_mex.sample = left_join(data.frame(bstrap_id = sample(x = data_mex$bstrap_id, size = n2, replace = TRUE)), data_mex, "bstrap_id")}
      #   
      #   
      #     #us
      #     mod1_main <-  felm(rate_adj ~ tmean + prec | fipsmo + stateyear, data=data_us.sample, weights= data_us.sample$popw)
      #     mod1_state <- felm(rate_adj ~ tmean*as.factor(state) + prec*as.factor(state) | fipsmo + stateyear, data= data_us.sample, weights= data_us.sample$popw)
      #   
      #     #mex
      #     mod2_main <- felm(rate_adj ~ tmean + prec | id + statemonth + state_year, data=data_mex.sample, weights=data_mex.sample$popw)
      #     mod2_state <- felm(rate_adj ~ tmean*as.factor(state) + prec*as.factor(state) | id + statemonth + state_year , data=data_mex.sample, weights=data_mex.sample$popw)
      #   
      #     bstrap1_main[b] <- as.numeric(summary(mod1_main)$coefficients["tmean","Estimate"])
      #         coef <- summary(mod1_state)$coefficients[grep(x = rownames(summary(mod1_state)$coefficients), pattern = "tmean"),"Estimate"]
      #         coef[2:length(coef)] <- coef[1] + coef[2:length(coef)]
      #     bstrap1_state[,b] <- as.numeric(coef)
      #   
      #     bstrap2_main[b] <- as.numeric(summary(mod2_main)$coefficients["tmean","Estimate"])
      #         coef <- summary(mod2_state)$coefficients[grep(x = rownames(summary(mod2_state)$coefficients), pattern = "tmean"),"Estimate"]
      #         coef[2:length(coef)] <- coef[1] + coef[2:length(coef)]
      #     bstrap2_state[,b] <- as.numeric(coef)
      #   
      #     #if(round(b/25)==b/25){print(b)}
      #     print(b)
      #   }
      # write_csv(data.frame(bstrap1_main), path = "inputs/bootstrap_runs/BootstrapStateEffects_main_us.csv")
      # write_csv(data.frame(bstrap2_main), path = "inputs/bootstrap_runs/BootstrapStateEffects_main_mex.csv")     
      # write_csv(data.frame(bstrap1_state), path = "inputs/bootstrap_runs/BootstrapStateEffects_state_us.csv")   
      # write_csv(data.frame(bstrap2_state), path = "inputs/bootstrap_runs/BootstrapStateEffects_state_mex.csv") 
      ###########################################################                       
        
        
        
    ### US ###
      
    #load bootstrap runs and plot      
        bstrap1_main <- read.csv("inputs/bootstrap_runs/BootstrapStateEffects_main_us.csv")[,1]
        bstrap1_state <- read.csv("inputs/bootstrap_runs/BootstrapStateEffects_state_us.csv") %>% as.matrix()
        bstrap2_main <- read.csv("inputs/bootstrap_runs/BootstrapStateEffects_main_mex.csv")[1,]
        bstrap2_state <- read.csv("inputs/bootstrap_runs/BootstrapStateEffects_state_mex.csv") %>% as.matrix()
            
            
        #load shapefile:
        usa <- readRDS("inputs/map_boundaries/USA_adm1.rds")
        
        #load fips info
        fips <- read.csv("inputs/US_FIPS_Codes.csv",header = T, skip = 1)[,c("State","FIPS.State")]
            fips <- fips[(!duplicated(fips)),]
            names(fips)<- c("NAME_1","FIPS"); rownames(fips)<-1:nrow(fips)	
        usa@data <- left_join(usa@data, fips, by = "NAME_1") %>% arrange(plotOrder)
        
        
        # US Regression        
            mod <- felm(rate_adj ~ tmean*as.factor(state) + prec*as.factor(state) | fipsmo + stateyear | 0 | fips , data=data_us, weights=data_us$popw)  # stateyear FE
        
        #pull out coefficients we care about
        coef <- summary(mod)$coefficients
          coef <- coef[grep(x = rownames(coef), pattern = "tmean"),]
          coef <- data.frame(state = as.integer(substr(rownames(coef),23,24)), coef = coef[,"Estimate"])
          coef[1,"state"]<-1
          coef[2:nrow(coef),"coef"] <- coef[2:nrow(coef),"coef"] + coef[1,"coef"] #everything relative to omitted temp coefficient
          rownames(coef)<-1:nrow(coef)
        
        
        # get base rates, pop weighted
            br <- data_us %>% dplyr::group_by(state) %>% dplyr::summarise(avg=weighted.mean(rate_adj,popw,na.rm=T))  
        
        #calculate % effects = (coef/base rate)*100
            coef <- left_join(coef, br, by = "state")
        
        
        #get tstats
            st <- sort(unique(data_us$state))
            all.equal(rownames(bstrap1_state), as.character(st))
            test_stat <- bstrap1_state 
            
            for(i in 1:nrow(test_stat)){test_stat[i,] = bstrap1_state[i,] - bstrap1_main}
              pos <- apply(test_stat, 1, function(x){sum(x>0)/length(x)})
              neg <- apply(test_stat, 1, function(x){sum(x<0)/length(x)})
            
            coef$siggy <- (pos > 0.95 | pos < 0.05)
            
            for (s in st){
              data_us[,paste("temp_x_fe",s, sep="")] <- as.numeric(data_us$state==s)*data_us$tmean
              data_us[,paste("prec_x_fe",s, sep="")] <- as.numeric(data_us$state==s)*data_us$prec
                        }
            
            st <- st[st!=1]
            
            coef$plot <- (coef$coef/coef$avg)*100
            names(coef)[c(1,5)]<-c("FIPS","plotValue")
            usa@data <- left_join(usa@data, coef[,c("FIPS","plotValue","siggy")], by = "FIPS") %>% arrange(plotOrder)
            
            
        
        
      ### MEXICO ###
            
        #read in shapefile and data
          mex <- readRDS("inputs/map_boundaries/MEX_adm1.rds")
          data_mex <- read.csv('inputs/SuicideData_Mexico.csv')
        
        # run regression
          mod <- felm(rate_adj ~ tmean*as.factor(state) + prec*as.factor(state) | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)
        
        
        #pull out coefficients we care about
            coef <- summary(mod)$coefficients
            coef <- coef[grep(x = rownames(coef), pattern = "tmean"),]
            coef <- data.frame(state = unlist(lapply(strsplit(rownames(coef), ")"), function(x){x[2]})) , coef = coef[,"Estimate"])
            coef$state <- as.character(coef$state)
            coef[1,"state"]<-"Aguascalientes"
            coef[2:nrow(coef),"coef"] <- coef[2:nrow(coef),"coef"] + coef[1,"coef"]
            rownames(coef)<-1:nrow(coef)
            
        brm <- data_mex %>% group_by(state) %>% summarise(avg=weighted.mean(rate_adj,popw,na.rm=T))  # base rates, pop weighted
        
        #calculate the values we want to plot
        coef <- left_join(coef, brm,"state")
        coef$plot <- (coef$coef/coef$avg)*100
        names(coef)[c(1,4)]<-c("NAME_1","plotValue")
        
        
        
        
        #merge plot values with shapefile
        #fix names that don't match (issue with accents)
        coef$NAME_1[coef$NAME_1%in%mex@data$NAME_1==F] <- c("México","Michoacán","Nuevo León","Querétaro","San Luis Potosí","Yucatán")
        
        test_stat <- bstrap2_state 
        for(i in 1:nrow(test_stat)){test_stat[i,] = bstrap2_state[i,] - bstrap2_main}
        pos <- apply(test_stat, 1, function(x){sum(x>0)/length(x)})
        neg <- apply(test_stat, 1, function(x){sum(x<0)/length(x)})
        
        coef$siggy <- (pos > 0.95 | pos < 0.05)
        
        
        
        mex@data <- left_join(mex@data, coef, by = "NAME_1") %>% arrange(plotOrder)
        
        
        
        
        
  ########## PLOT ###########
        
        
        ### do two different color pals, 1 positive, 1 negative, and manually set 0s. just don pos and neg on relevant subsets
        
        pal.neg <- colorRampPalette(c(brewer.pal(n = 9, name = "Blues")[c(8:5,1)]))(256)
        pal.pos <- colorRampPalette(c(brewer.pal(n = 9, name = "Reds")[c(1,5:8)]))(256)
        
        breaks.neg <- seq(-4.5,0, 0.25)
        breaks.pos <- seq(0, 7, 0.25)
        
        int.pos.usa <- classIntervals(usa@data$plotValue, style = "fixed", fixedBreaks = breaks.pos)
        int.neg.usa <- classIntervals(usa@data$plotValue, style = "fixed", fixedBreaks = breaks.neg)
        
        int.pos.mex <- classIntervals(mex@data$plotValue, style = "fixed", fixedBreaks = breaks.pos)
        int.neg.mex <- classIntervals(mex@data$plotValue, style = "fixed", fixedBreaks = breaks.neg)
        
        usa@data$color[usa@data$plotValue > 0] <- findColours(int.pos.usa, pal.pos)[usa@data$plotValue > 0]
        usa@data$color[usa@data$plotValue < 0] <- findColours(int.neg.usa, pal.neg)[usa@data$plotValue < 0]
        usa@data$color[usa@data$plotValue < -4.5] <- brewer.pal(n = 9, name = "Blues")[7]
        usa@data$color[usa@data$plotValue > 7] <- brewer.pal(n = 9, name = "OrRd")[7]	
        
        mex@data$color[mex@data$plotValue > 0] <- findColours(int.pos.mex, pal.pos)[mex@data$plotValue > 0]
        mex@data$color[mex@data$plotValue < 0] <- findColours(int.neg.mex, pal.neg)[mex@data$plotValue < 0]	
        mex@data$color[mex@data$plotValue < -4.5] <-  brewer.pal(n = 9, name = "Blues")[7]
        mex@data$color[mex@data$plotValue > 7] <- brewer.pal(n = 9, name = "OrRd")[7]	
        
        
        usa_siggy <- usa[usa@data$siggy == T,]
        mex_siggy <- mex[mex@data$siggy == T,]	
        
        
        
        
        
        pdf("outputs/raw_figures/Figure3_c_map.pdf", width = 6, height = 6)
        
            plot(usa, lwd = 0.025, xlim = c(-125, -66), ylim = c(17, 47), col = usa@data$color, border = NA)		
            plot(mex, lwd = 0.025, add=T, col = mex@data$color, border = NA)		
            
            plot(usa_siggy, add=T, lwd = 1.5)
            plot(mex_siggy, add=T, lwd = 1.5)
            
  
        dev.off()
        
        
        
        
        
        
        
        pdf("outputs/raw_figures/Figure3_c_hists.pdf", width = 10, height = 8)
        
            par(mfrow = c(2,1))
            par(mar = c(3,3,3,3))
            
            #usa
            xx <- hist(usa@data$plotValue, breaks = seq(-4.5,7,.5), plot = F)$mids
            colhist1 <- rep(NA, length(xx))
            bn <- classIntervals(hist(xx, breaks = seq(-4.5,7,.5), plot = F)$mids,  style = "fixed", fixedBreaks = breaks.neg)
            bp <- classIntervals(hist(xx, breaks = seq(-4.5,7,.5), plot = F)$mids,  style = "fixed", fixedBreaks = breaks.pos)
            colhist1[xx<0]<-findColours(bn, pal.neg)[xx<0]
            colhist1[xx>0]<-findColours(bp, pal.pos)[xx>0]
            
            hist(usa@data$plotValue, breaks = seq(-4.5,7,.5), col = colhist1,axes=F,xlab = "",ylab = "",main = "", xlim = c(-10,10))
            axis(1,tick = F,line = -0.5, labels = seq(-3, 3, 1),at = seq(-3, 3, 1)-.25)
            
            
            #mexico
            xx <- hist(mex@data$plotValue, breaks = c(-15,seq(-4.5,7,.5),15), plot = F)$mids
            colhist2 <- rep(NA, length(xx))
            bn <- classIntervals(hist(xx, breaks = c(-15,seq(-4.5,7,.5),15), plot = F)$mids,  style = "fixed", fixedBreaks = breaks.neg)
            bp <- classIntervals(hist(xx, breaks = c(-15,seq(-4.5,7,.5),15), plot = F)$mids,  style = "fixed", fixedBreaks = breaks.pos)
            colhist2[xx<0]<-findColours(bn, pal.neg)[xx<0]
            colhist2[xx>0]<-findColours(bp, pal.pos)[xx>0]
            
            hist(mex@data$plotValue, breaks = c(-15,seq(-4.5,7,.5),15), col = colhist2,axes=F,xlab = "",ylab = "",main = "", xlim = c(-10,10))
            axis(1,tick = F,line = -0.5, labels = seq(-10,10,1),at =seq(-10,10,1)-.25)
            
        dev.off()
        
        
        
        
        
        
        
