
make_plots <- function(fit1_predict,fit2_predict,fit3_predict,fit_x,means)

{
dplot<-  ggplot() +
  
  geom_point(aes(x=means$logc, y=means$mean))+
  geom_errorbar(aes(x=means$logc,ymin=means$mean-means$sd, ymax=means$mean+means$sd))+
 
  xlab("Log (Concentration) nM")+
  ylab("Viable Cell %")

if(!is.null(fit3_predict)){
  dplot<- dplot + geom_line(aes(y = fit3_predict, x= fit_x ,color="Stage 3 Full nlsLM fit" ))

}    

dplot <- dplot +geom_line(aes(x=fit_x,y=fit2_predict,color="Stage 2 Brute Force nls2 "))
dplot <- dplot +geom_line(aes(x=fit_x,y=fit1_predict,color="Stage 1 Initial Guess"))
dplot <- dplot + scale_color_manual(values=c("red","blue","black"))+ theme(legend.title=element_blank())


dplot

}



