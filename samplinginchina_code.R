#this code is for RJ in china program,last modification 20190709
###################################### enviornment set
require(readxl)
require(stringr)
require(psych)
require(stargazer)
library(ggplot2)
library(gtools)
require(dplyr)
library(readstata13)
library(ordinal) 
library(foreign) 
library(TeachingSampling)
library(zoo)
library(pps)
library(gridExtra)
library(MASS)


#############################
#read data
#############################
setwd("D:/OneDrive/research/sampling in law/code/git")
simdata<-readRDS("rjchina_fin.rds")

###################################### 
#Descriptive analysis
######################################

# Descriptive analysis of the variables
describe(simdata$senlength)

table(simdata$sentype_n)
prop.table( table(simdata$sentype_n))

table(simdata$probation_bin)
prop.table( table(simdata$probation_bin))

table(simdata$reconcile)
prop.table( table(simdata$reconcile))

table(simdata$compensation)
prop.table( table(simdata$compensation))

# Histogram
png("hist.png",width=800, height=300)

simdata %>%
  ggplot(aes(x = senlength)) + 
  geom_histogram(bins = 156,
                 colour = "black") +
  scale_x_continuous(name = "Sentence length (month)",
                     breaks = seq(0, 180, 5),
                     limits=c(0, 180),
                     expand = c(0, 1)) +
  scale_y_continuous(name = "Frequency",
                     expand = c(0, 0)) +
  theme_classic() 
  
dev.off()

#regression 
reg1<-lm(senlength~reconcile+compensation,data=simdata)
summary(reg1)

reg2<-glm.nb(senlength~reconcile+compensation,data=simdata,link = log)
summary(reg2)

reg3<-glm(probation_bin~reconcile+compensation,data=simdata,family = binomial(link = logit))
summary(reg3)

###################################### 
#Define functions to run simulation
######################################

#main function to generate simulated results 
data_sim<-function(rep=1000,size_n=1000,samptype="srs",data=simdata) {
set.seed(123456)
srsdata<-data.frame(len=NA,p_youqi=NA,p_prob=NA,reconcile=NA,compensate=NA,
                    lm_coef_con=NA,lm_coef_b1=NA,lm_coef_b2=NA,
                    nb_coef_con=NA,nb_coef_b1=NA,nb_coef_b2=NA,
                    lg_coef_con=NA,lg_coef_b1=NA,lg_coef_b2=NA)
  for (i in 1:rep) {
    
    if (samptype=="srs")  {
      samp_data<-data[sample(nrow(data), size=size_n, replace =F),]      
    }
    if (samptype=="ss")  {
      #define function to obtain systematic sample
      obtain_sys = function(N,n){
        k = ceiling(N/n)
        r = sample(1:k, 1)
        seq(r, r + k*(n-1), k)
      }
      
      #obtain systematic sample
      samp_data = data[obtain_sys(nrow(data), size_n), ]
    }
    if (samptype=="str_prov") {
      samp_data<-data %>%
        group_by(prov) %>%
        sample_frac(size_n/nrow(data))
    }
    if (samptype=="pps_prov5200") {
      samp_data<-data %>%
        filter(prov %in% names(table(data$prov))[ppss(table(data$prov),5)] ) %>%
        group_by(prov) %>%
        sample_n(200)
    }
    if (samptype=="pps_prov10100") {
      samp_data<-data %>%
        filter(prov %in% names(table(data$prov))[ppss(table(data$prov),10)] ) %>%
        group_by(prov) %>%
        sample_n(100)      
    }
      
    
    srsdata[i,1]<-mean(samp_data$senlength)
    srsdata[i,2]<-mean(samp_data$sentype_n=="youqi")
    srsdata[i,3]<-mean(samp_data$probation_bin)
    srsdata[i,4]<-mean(samp_data$reconcile)
    srsdata[i,5]<-mean(samp_data$compensation)

    
    reg<-lm(senlength~reconcile+compensation,data=samp_data)
    srsdata[i,6]<-reg$coefficients[1]
    srsdata[i,7]<-reg$coefficients[2]
    srsdata[i,8]<-reg$coefficients[3]
    
    nb<-glm.nb(senlength~reconcile+compensation,data=samp_data,link = log)
    srsdata[i,9]<-nb$coefficients[1]
    srsdata[i,10]<-nb$coefficients[2]
    srsdata[i,11]<-nb$coefficients[3]
    
    lg<-glm(probation_bin~reconcile+compensation,data=samp_data,family = binomial(link = logit))
    srsdata[i,12]<-lg$coefficients[1]
    srsdata[i,13]<-lg$coefficients[2]
    srsdata[i,14]<-lg$coefficients[3]
    
  }
return(srsdata)
}

#conver the simulated result to table
makesta<-function(data=simdata) {
  out<-vector(length =  ncol(data))
  for(i in 1:ncol(data)) {
    out[i]<-paste0(round(mean(data[,i], na.rm = TRUE),2) ,"±",
                   round(sd  (data[,i], na.rm = TRUE),2))
  }
  return(out)
}

#plot the empirical sampling distribution of the sentence length 

plot_len<-function(procdata=srsdata,method="SRS",rep=1000,n=1000) {
  meanv<-mean(procdata$len,na.rm=T)
  sdv<-sd(procdata$len,na.rm=T)  
  plt<-procdata %>%
    ggplot(aes(x = len)) + 
    geom_histogram(aes(y = ..density..),
                   binwidth =sdv/5 ,
                   colour = "black") +
     stat_function(fun=dnorm,linetype = 1,colour = "black", args=
                  list(mean=mean(procdata$len, na.rm = TRUE), 
                           sd=sd(procdata$len, na.rm = TRUE))) +
    scale_x_continuous(name = "Sentence length (month)",
                       breaks = seq(15,20,0.5),
                       limits=c(15,20),
                       expand = c(0, 0)) +
    theme_classic() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    annotate(geom="text", x=19.00, y=0.3, 
             label=paste0("Mean=",round(meanv,3),"\n",
                          "SE=",round(sdv,3),"\n",
                          "Replicates=",rep,"\n",
                          "Method=",method,"\n",
                          "Sample size=",n
                          ))
return(plt)

}

#plot the empirical sampling distribution of the sentence length 

plot_youqi<-function(procdata=srsdata,method="SRS",rep=1000,n=1000) {

  meanv<-mean(procdata$p_youqi,na.rm=T)
  sdv<-sd(procdata$p_youqi,na.rm=T)
  plt<-procdata %>%
      ggplot(aes(x = p_youqi)) + 
      geom_histogram(aes(y = ..density..),
                     binwidth =sdv/5  ,
                     colour = "black") +
      stat_function(fun=dnorm,linetype = 1,colour = "black", args=
                      list(mean=mean(procdata$p_youqi, na.rm = TRUE), 
                           sd=sd(procdata$p_youqi, na.rm = TRUE))) +
      scale_x_continuous(name = "Proportion of fixed-term imprisonment",
                         breaks = seq(meanv-0.05,meanv+0.05,0.01),
                         limits=c(meanv-0.05,meanv+0.05),
                         labels=scales::percent,
                         expand = c(0, 0)) +
      theme_classic() +
      theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
      annotate(geom="text", x=0.89, y=30, 
               label=paste0("Mean=",round(meanv,3),"\n",
                           "SE=",round(sdv,3),"\n",
                           "Replicates=",rep,"\n",
                           "Method=",method,"\n",
                           "Sample size=",n
                           ))
  return(plt)
}


#plot the empirical sampling distribution of the proportion of probation

plot_prob<-function(procdata=srsdata,method="SRS",rep=1000,n=1000) {
  
  meanv<-mean(procdata$p_prob,na.rm=T)
  sdv<-sd(procdata$p_prob,na.rm=T)
  
  plt<-procdata %>%
    ggplot(aes(x = p_prob)) + 
    geom_histogram(aes(y = ..density..),
                   binwidth =sdv/5  ,
                   colour = "black") +
    stat_function(fun=dnorm,linetype = 1,colour = "black", args=
                    list(mean=mean(procdata$p_prob, na.rm = TRUE), 
                         sd=sd(procdata$p_prob, na.rm = TRUE))) +
    scale_x_continuous(name = "Proportion of probation",
                       breaks = seq(meanv-0.06,meanv+0.06,0.01),
                       limits=c(meanv-0.06,meanv+0.06),
                       labels=scales::percent,
                       expand = c(0, 0)) +
    theme_classic() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    annotate(geom="text", x=0.61, y=20, 
             label=paste0("Mean=",round(meanv,3),"\n",
                          "SE=",round(sdv,3),"\n",
                          "Replicates=",rep,"\n",
                          "Method=",method,"\n",
                          "Sample size=",n
                          ))
  return(plt)
}



###################################### 
#Conduct simulation
######################################


set.seed(123456)
#exp 1 rep
temp<-data_sim(rep=500,size_n=1000,samptype="srs")
result1<-makesta(temp)
temp<-data_sim(rep=1000,size_n=1000,samptype="srs")
result2<-makesta(temp)
temp<-data_sim(rep=2000,size_n=1000,samptype="srs")
result3<-makesta(temp)
temp<-data_sim(rep=5000,size_n=1000,samptype="srs")
result4<-makesta(temp)
finalout=cbind(result1,result2,result3,result4)
rm(result1,result2,result3,result4)
write.csv(finalout,"table1.csv")

#exp 2 method
temp<-data_sim(rep=2000,size_n=1000,samptype="srs")
result1<-makesta(temp)
temp<-data_sim(rep=2000,size_n=1000,samptype="ss")
result2<-makesta(temp)
temp<-data_sim(rep=2000,size_n=1000,samptype="str_prov")
result3<-makesta(temp)
temp<-data_sim(rep=2000,size_n=1000,samptype="pps_prov5200")
result4<-makesta(temp)
temp<-data_sim(rep=2000,size_n=1000,samptype="pps_prov10100")
result5<-makesta(temp)
finalout=cbind(result1,result2,result3,result4,result5)
rm(result1,result2,result3,result4,result5)
write.csv(finalout,"table2.csv")

#exp 3 sample size 
temp<-data_sim(rep=2000,size_n=500,samptype="srs")
result1<-makesta(temp)
temp<-data_sim(rep=2000,size_n=1000,samptype="srs")
result2<-makesta(temp)
temp<-data_sim(rep=2000,size_n=2000,samptype="srs")
result3<-makesta(temp)
temp<-data_sim(rep=2000,size_n=5000,samptype="srs")
result4<-makesta(temp)
finalout=cbind(result1,result2,result3,result4)
rm(result1,result2,result3,result4)
write.csv(finalout,"table3.csv")


###################################### 
#additional simulation for the change sample size to standard error 
######################################

#generate statistics
datasds<-vector(length = 15)

for (i in c(seq(100,5000,by=200)) ) {
  temp<-data_sim(rep=1000,size_n=i,samptype="srs")
  sds<-apply(temp,2, sd, na.rm = TRUE) 
  datasds<-rbind(datasds,c(sds,i))
}


datasds<-as.data.frame(datasds)
names(datasds)<-c("Length","Prop_fixed","Prop_probation","Reconciliation","Compensation",
                  "Model1_con","Model1_beta1","Model1_beta2",
                  "Model2_con","Model2_gamma1","Model2_gamma2",
                  "Model3_con","Model3_alpha1","Model3_alpha2","sample_size")

datasds<-datasds[datasds$sample_size!=0,]

#plot the results 
p1<-ggplot(datasds, aes(x=sample_size, y=Length)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p2<-ggplot(datasds, aes(x=sample_size, y=Prop_fixed)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p3<-ggplot(datasds, aes(x=sample_size, y=Prop_probation)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p4<-ggplot(datasds, aes(x=sample_size, y=Reconciliation)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p5<-ggplot(datasds, aes(x=sample_size, y=Compensation)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p6<-ggplot(datasds, aes(x=sample_size, y=Model1_con)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p7<-ggplot(datasds, aes(x=sample_size, y=Model1_beta1)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p8<-ggplot(datasds, aes(x=sample_size, y=Model1_beta2)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p9<-ggplot(datasds, aes(x=sample_size, y=Model2_con)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p10<-ggplot(datasds, aes(x=sample_size, y=Model2_gamma1)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p11<-ggplot(datasds, aes(x=sample_size, y=Model2_gamma2)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p12<-ggplot(datasds, aes(x=sample_size, y=Model3_con)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p13<-ggplot(datasds, aes(x=sample_size, y=Model3_alpha1)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')

p14<-ggplot(datasds, aes(x=sample_size, y=Model3_alpha2)) + 
  geom_point() +
  theme_classic() +
  scale_x_continuous(name = "Sample size")+
  geom_smooth(method = 'loess')


png("sim4.png",width=1600, height=800)
grid.arrange(p1,p2,p3,p4,p5,
             p6,p7,p8,p9,p10,
             p11,p12,p13,p14,
             ncol = 3, nrow = 5)
dev.off()



###################################### 
#plot the emprical standard error
######################################

#appendixa replicates
temp<-data_sim(rep=500,size_n=1000,samptype="srs")
sim_500_1000_srs_len  <-plot_len(temp,rep=500,n=1000)
sim_500_1000_srs_youqi<-plot_youqi(temp,rep=500,n=1000)
sim_500_1000_srs_prob <-plot_prob(temp,rep=500,n=1000)

temp<-data_sim(rep=1000,size_n=1000,samptype="srs")
sim_1000_1000_srs_len  <-plot_len(temp,rep=1000,n=1000)
sim_1000_1000_srs_youqi<-plot_youqi(temp,rep=1000,n=1000)
sim_1000_1000_srs_prob <-plot_prob(temp,rep=1000,n=1000)

temp<-data_sim(rep=2000,size_n=1000,samptype="srs")
sim_2000_1000_srs_len  <-plot_len(temp,rep=2000,n=1000)
sim_2000_1000_srs_youqi<-plot_youqi(temp,rep=2000,n=1000)
sim_2000_1000_srs_prob <-plot_prob(temp,rep=2000,n=1000)

temp<-data_sim(rep=5000,size_n=1000,samptype="srs")
sim_5000_1000_srs_len  <-plot_len(temp,rep=5000,n=1000)
sim_5000_1000_srs_youqi<-plot_youqi(temp,rep=5000,n=1000)
sim_5000_1000_srs_prob <-plot_prob(temp,rep=5000,n=1000)

png("appendixa.png",width = 1200,height  = 600)
grid.arrange(sim_500_1000_srs_len , sim_500_1000_srs_youqi,  sim_500_1000_srs_prob , 
             sim_1000_1000_srs_len, sim_1000_1000_srs_youqi, sim_1000_1000_srs_prob ,
             sim_2000_1000_srs_len, sim_2000_1000_srs_youqi, sim_2000_1000_srs_prob ,
             sim_5000_1000_srs_len, sim_5000_1000_srs_youqi, sim_5000_1000_srs_prob ,
             ncol = 3, nrow = 4)
dev.off()

#appendixb method
temp<-data_sim(rep=1000,size_n=1000,samptype="srs")
sim_1000_1000_srs_len  <-plot_len(temp,method="srs",rep=1000,n=1000)
sim_1000_1000_srs_youqi<-plot_youqi(temp,method="srs",rep=1000,n=1000)
sim_1000_1000_srs_prob <-plot_prob(temp,method="srs",rep=1000,n=1000)

temp<-data_sim(rep=1000,size_n=1000,samptype="ss")
sim_1000_1000_ss_len  <-plot_len(temp,method="ss",rep=1000,n=1000)
sim_1000_1000_ss_youqi<-plot_youqi(temp,method="ss",rep=1000,n=1000)
sim_1000_1000_ss_prob <-plot_prob(temp,method="ss",rep=1000,n=1000)

temp<-data_sim(rep=1000,size_n=1000,samptype="str_prov")
sim_1000_1000_str_len  <-plot_len(temp,method="stratified",rep=1000,n=1000)
sim_1000_1000_str_youqi<-plot_youqi(temp,method="stratified",rep=1000,n=1000)
sim_1000_1000_str_prob <-plot_prob(temp,method="stratified",rep=1000,n=1000)

temp<-data_sim(rep=1000,size_n=1000,samptype="pps_prov5200")
sim_1000_1000_pps1_len  <-plot_len(temp,method="pps1",rep=1000,n=1000)
sim_1000_1000_pps1_youqi<-plot_youqi(temp,method="pps1",rep=1000,n=1000)
sim_1000_1000_pps1_prob <-plot_prob(temp,method="pps1",rep=1000,n=1000)

temp<-data_sim(rep=1000,size_n=1000,samptype="pps_prov10100")
sim_1000_1000_pps2_len  <-plot_len(temp,method="pps2",rep=1000,n=1000)
sim_1000_1000_pps2_youqi<-plot_youqi(temp,method="pps2",rep=1000,n=1000)
sim_1000_1000_pps2_prob <-plot_prob(temp,method="pps2",rep=1000,n=1000)

png("appendixb.png",width=1200, height=750)
grid.arrange(sim_1000_1000_srs_len,  sim_1000_1000_srs_youqi,  sim_1000_1000_srs_prob ,
             sim_1000_1000_ss_len ,  sim_1000_1000_ss_youqi,   sim_1000_1000_ss_prob , 
             sim_1000_1000_str_len,  sim_1000_1000_str_youqi,  sim_1000_1000_str_prob ,
             sim_1000_1000_pps1_len, sim_1000_1000_pps1_youqi, sim_1000_1000_pps1_prob ,
             sim_1000_1000_pps2_len, sim_1000_1000_pps2_youqi, sim_1000_1000_pps2_prob ,
             ncol = 3, nrow = 5)
dev.off()

#appendixc sample size 
temp<-data_sim(rep=1000,size_n=500,samptype="srs")
sim_1000_500_srs_len  <-plot_len(temp,method="srs",rep=1000,n=500)
sim_1000_500_srs_youqi<-plot_youqi(temp,method="srs",rep=1000,n=500)
sim_1000_500_srs_prob<-plot_prob(temp,method="srs",rep=1000,n=500)

temp<-data_sim(rep=1000,size_n=1000,samptype="srs")
sim_1000_1000_srs_len  <-plot_len(temp,method="srs",rep=1000,n=1000)
sim_1000_1000_srs_youqi<-plot_youqi(temp,method="srs",rep=1000,n=1000)
sim_1000_1000_srs_prob<-plot_prob(temp,method="srs",rep=1000,n=1000)

temp<-data_sim(rep=1000,size_n=2000,samptype="srs")
sim_1000_2000_srs_len  <-plot_len(temp,method="srs",rep=1000,n=2000)
sim_1000_2000_srs_youqi<-plot_youqi(temp,method="srs",rep=1000,n=2000)
sim_1000_2000_srs_prob<-plot_prob(temp,method="srs",rep=1000,n=2000)

temp<-data_sim(rep=1000,size_n=5000,samptype="srs")
sim_1000_5000_srs_len  <-plot_len(temp,method="srs",rep=1000,n=5000)
sim_1000_5000_srs_youqi<-plot_youqi(temp,method="srs",rep=1000,n=5000)
sim_1000_5000_srs_prob <-plot_prob(temp,method="srs",rep=1000,n=5000)

png("appendixc.png",width=1200, height=600)
grid.arrange(sim_1000_500_srs_len,  sim_1000_500_srs_youqi,  sim_1000_500_srs_prob , 
             sim_1000_1000_srs_len, sim_1000_1000_srs_youqi, sim_1000_1000_srs_prob ,
             sim_1000_2000_srs_len, sim_1000_2000_srs_youqi, sim_1000_2000_srs_prob ,
             sim_1000_5000_srs_len, sim_1000_5000_srs_youqi, sim_1000_5000_srs_prob ,
             ncol = 3, nrow = 4)
dev.off()


