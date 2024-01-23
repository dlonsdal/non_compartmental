
################################
##       Simulate data        ##
##.   bioavailability study   ##
##         PK Year 1.         ##
################################
setwd("/Users/daganlonsdale/Documents/GitHub/non_compartmental/docs")
rm(list=ls()) # clear workspace 
library(tidyverse)
library(openxlsx)
library(kableExtra)

# Let's start by creating our sampling times

# Make a vector of sampling times
time <-  c(0, 0.1, 0.5, 1, 1.5, 2, 4, 6, 8, 12, 24, 36)  

# Now let's make a vector of the subject ID's. Remember, each ID will have 8 samples (one for each time point)

# We will need a row in our data frame for each sample

id <- sort(rep(LETTERS[1:6], 14)) # rep needs to be same length as number of sample times

time<- rep(c(0, 0.1, 0.5, 1, 1.25,1.5,1.75, 2, 4, 6, 8, 12, 24, 36)   , length(unique(id))) 


#OK, now we need to simulate some PK parameters
# use J Clin Pharmacol 2003;43:1370-1376
set.seed(1234)
v<- rep(rnorm(n=length(unique(id)),mean=35,sd=6),each=length(unique(time)))

# OK let us do the same for Clearance of 10 L/hr
cl<-rep(rnorm(n=length(unique(id)),mean=5,sd=0.5),each=length(unique(time)))

range(cl) # check no negative as I have been lazy and simulated with normal distribution

# OK let us do the same for absorption (ka) 
ka<-rep(2*exp(rnorm(length(unique(id)),sd=0.5)),each=length(unique(time)))
#had to do this tiwce as set.seed gives too narrow distribution randomly the first time

#sex variable
sex<-c("Male","Female")
sex<-rep(sample(sex,length(unique(id)),replace=TRUE),each=length(unique(time)))

#Now let's make a data.frame of all this data
ke<-cl/v
Ftab<- 0.60 #assume bioavailability of 60% for tablet

sim1<-data.frame(id,v,cl,ke,ka,time,sex, F=Ftab,study='ORAL')
sim2<-data.frame(id,v,cl,ke,ka,time,sex, F=Ftab,study='IV')


# we can look at this
#View(sim)

# Great, that is the easy bit, now we need to create the concentration column!
# Remember that C=(F*D*ka/(Vd*(ka-ke))*(exp(-ke*t)-exp(-ka*t)) 
# Let's take a dose of 1000 mg 
Dose<- 1000

sim1$conc<-sim1$F*(Dose*sim1$ka/(sim1$v*(sim1$ka-sim1$ke)))*(exp(-(sim1$cl/sim1$v)*sim1$time)-exp(-sim1$ka*sim1$time))

sim2$conc<-(Dose/sim2$v)*(exp(-(sim2$cl/sim2$v)*sim2$time))


sim<-rbind(sim1,sim2)

# OK, this is starting to look like data from an experiment/ PK study
# But in a PK study we would not have the v or cl columns! Let's delete them
sim$cl<-NULL
sim$v<-NULL
sim$F<-NULL
sim$ka<-NULL
sim$ke<-NULL

sim1$cl<-NULL
sim1$v<-NULL
sim1$F<-NULL
sim1$ka<-NULL
sim1$ke<-NULL

sim2$cl<-NULL
sim2$v<-NULL
sim2$F<-NULL
sim2$ka<-NULL
sim2$ke<-NULL

sim<-sim %>% mutate_at(vars(conc), round,1)
sim1<-sim1 %>% mutate_at(vars(conc), round,1)
sim2<-sim2 %>% mutate_at(vars(conc), round,1)


sim$id<-as.factor(sim$id)

ggplot(data=subset(sim,study=='ORAL'), aes(x=time, y=conc,group=id,col=id))+
  geom_line()+
  geom_point()+
  theme_classic()+
  xlab("time (hours)")+
  ylab("concentration of liquid drug (mg/L)")
ggplot(data=subset(sim,study=='IV'), aes(x=time, y=conc,group=id,col=id))+
  geom_line()+
  geom_point()+
  theme_classic()+
  xlab("time (hours)")+
  ylab("concentration of tablet drug (mg/L)")

ggplot(data=sim,aes(x=time, y=conc,group=interaction(study, id),col=id) )+
  geom_line()+
  geom_point()+
  theme_classic()+
  xlab("time (hours)")+
  ylab("concentration of tablet drug (mg/L)")
write.csv(sim1,"PK_oral.csv", row.names = F)
write.csv(sim2,"PK_IV.csv",row.names=F)
