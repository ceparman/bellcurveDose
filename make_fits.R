
library(tidyr)
library(dplyr)
library(minpack.lm)
library(nls2)




make_fits<- function(tdata){
  
  
  fit_function <- function(value,dip_i,plt1_i,plt2_i,ec1_i,ec2_i,nh1_i,nh2_i)
  {
    dip_i+(plt1_i - dip_i)/(1+10**((ec1_i-value)*nh1_i)) + (plt2_i - dip_i)/(1+10**(( value-ec2_i)*nh2_i))
    
    
  }
  
  
  #basic formula
  
  formula <- paste0("value ~ dip_i+",
                    "(plt1_i - dip_i)/(1+10**((ec1_i-logc)*nh1_i)) +", 
                    "(plt2_i - dip_i)/(1+10**(( logc-ec2_i)*nh2_i))")
  
  formula2 <- paste0("value ~ dip+",
                     "(plt1 - dip)/(1+10**((ec1-logc)*nh1)) +", 
                     "(plt2 - dip)/(1+10**(( logc-ec2)*nh2))")  
  
  
fit1_predict <- NULL  
fit2_predict <- NULL   
fit3_predict <- NULL  
new.data <- NULL
ifit <- NULL



ec50<- data.frame(fit=c("Stage 1 Initial Guess","Stage 2 Brute Force nls2 ","Stage 3 Full nlsLM fit"),
                  EC50 = rep(NA,3),GI50=rep(NA,3))

#### initial parameter guess

l<-nrow(tdata)

means<-tdata %>% group_by(logc) %>%  summarise (mean = mean(value))
sds <- tdata %>% group_by(logc) %>%  summarise (sd = sd(value))

means<-merge(sds,means)

dip_i<- max(means$mean)-min(means$mean)
plt1_i<-mean(means$mean[1])
plt2_i<-mean(means$mean[nrow(means)])

i<-2:nrow(means)
diff<- means$mean[i] - means$mean[i-1]
ec1_i <- means$logc[ which(diff==max(diff))]
ec2_i <-  means$logc[ which(diff==min(diff))]

#still need estimate for nh1 and nh2

nh1_i <- -5
nh2_i <- -5

new.data <- data.frame(logc = seq(min(tdata$logc),max(tdata$logc),len = 500)) 

fit1_predict<-fit_function(new.data$logc,dip_i,plt1_i,plt2_i,ec1_i,ec2_i,nh1_i,nh2_i)

##Stage 1 50s

ec50$EC50[1] = round(10**ec1_i,2)
ec50$GI50[1] = round(10**ec2_i,2)






fitParmOrder<- c("nh1","nh2","ec1","ec2","dip","plt1","plt2")
######################################################



form <- str_replace(formula,paste0(fitParmOrder[1],"_i"),fitParmOrder[1])
form <- str_replace(form,paste0(fitParmOrder[2],"_i"),fitParmOrder[2])

bf_nh_fit<-nls2(formula = form,data=tdata,start=list(nh1 = c(-100,0),nh2=c(-100,0)),
                nls.lm.control(maxiter = 100), algorithm = "brute-force")


nh1_i <- bf_nh_fit$m$getPars()[[1]]
nh2_i <- bf_nh_fit$m$getPars()[[2]]

################ Fit plt1 and plt2



form <- str_replace(formula,paste0(fitParmOrder[6],"_i"),fitParmOrder[6])
form <- str_replace(form,paste0(fitParmOrder[7],"_i"),fitParmOrder[7])

bf_plt_fit<-nls2(formula = form,data=tdata,start=list(plt1 = c(.1*plt1_i,5*plt1_i),plt2=c(.1*plt2_i,5*plt2_i)),
                nls.lm.control(maxiter = 100), algorithm = "brute-force")


plt1_i <- bf_plt_fit$m$getPars()[[1]]
plt2_i <- bf_plt_fit$m$getPars()[[2]]


###################################################
# 
# 
# ################ Fit ec1 and ec2
# 
# 
form <- str_replace(formula,paste0(fitParmOrder[3],"_i"),fitParmOrder[4])
form <- str_replace(form,paste0(fitParmOrder[3],"_i"),fitParmOrder[4])

bf_ec_fit<-nls2(formula = form,data=tdata,start=list(ec1 = c(.95*ec1_i,1.05*ec1_i),ec2=c(.99*ec2_i,1.05*ec2_i)),
                nls.lm.control(maxiter = 10), algorithm = "brute-force")


ec1_i <- bf_ec_fit$m$getPars()[[1]]
ec2_i <- bf_ec_fit$m$getPars()[[2]]

# 




##Stage 2 50s

ec50$EC50[2] = round(10**ec1_i,2)
ec50$GI50[2] = round(10**ec2_i,2)



fit2_predict<-fit_function(new.data$logc,dip_i,plt1_i,plt2_i,ec1_i,ec2_i,nh1_i,nh2_i)





fitStart<- list( dip = dip_i, plt1 = plt1_i, plt2 =plt2_i, ec1=ec1_i,ec2=ec2_i,
                 nh1 = nh1_i, nh2 = nh2_i)
seStart <- list( dip = NA, plt1 = NA, plt2 =NA, ec1=NA,ec2=NA,
                 nh1 = NA, nh2 = NA)

set.seed(100)

for(j in 1:100)
{
  
  fitParmOrder<-fitParmOrder[sample(1:7)]
  
    for (i in 1:length(fitParmOrder))  #cycle through trying to fit one parameter at a time
      { try(
      {
      #print(paste0("i is ",i,"fitting ",fitParmOrder[i]))
      
      
      # modify formula
      form <- str_replace(formula,paste0(fitParmOrder[i],"_i"),fitParmOrder[i])
      #create short start list  
      start<-fitStart[which(names(fitStart) == fitParmOrder[i])]
      #do fit
      tfit<-NULL
      suppressWarnings( tfit<- nlsLM(form,data=tdata,start=start,control = nls.lm.control(maxiter = 1000)))
      print(summary(tfit))
      #modify start value  
      if(!is.null(tfit)) {
        eval(parse(text= paste0("fitStart$",fitParmOrder[i]," <-",tfit$m$getPars()[[1]] )   ))
        
        eval(parse(text= paste0("seStart$",fitParmOrder[i]," <-",
                                summary(tfit)$parameters[,2])   ))
        
        
        ifit<-tfit
      }
        
     
      })  
    } #rinse and repeat 
    

  
  # break out if we can do a full fit
  
  
  lfit<-NULL
  ifit<-NULL
  tfit<-NULL
  
  try({
    lfit<-  nlsLM(formula2,data = tdata, start =fitStart, 
                  control = nls.lm.control(maxiter = 1000))
  })
  
  if(!is.null(lfit))  break  else lfit <- NULL
  
  
} #end j

##Stage 3 50s and prediction

if(!is.null(lfit)){ print("full fit")  #   full fit , use it
  fit3_predict <-  predict(object = lfit,newdata = new.data)
  ec50$EC50[3] = round(10**lfit$m$getPars()[[4]],2)
 ec50$GI50[3] = round(10**lfit$m$getPars()[[5]],2)

 
} 

if( is.null(lfit) &!is.null(ifit)){   print("partial fit") # no full fit or but partial
    
  fit3_predict<-fit_function(new.data$logc,fitStart$dip,fitStart$plt1,fitStart$plt2,
                             fitStart$ec1,fitStart$ec2,fitStart$nh1,fitStart$nh2)
  
  ec50$EC50[3] = round(10**fitStart$ec1,2)
  ec50$GI50[3] = round(10**fitStart$ec2,2)
  
  } else (print("no fit"))     #no fit

  
list(fit1=fit1_predict,fit2=fit2_predict,fit3=fit3_predict,fit_x=new.data$logc,means=means,ec50=ec50)

}




