library(data.table)
library(tidyverse)
library(survival)
library(survminer)


bikeData<-lapply(list.files("PublicTripData/Quarterly/",pattern = ".csv",full.names = TRUE),function(filepath){
    fread(filepath,data.table = FALSE)
  })%>%
  do.call('rbind',.)%>%
  as.tibble%>%
  filter(Distance_Miles<50)

#If a bike has not been used within 30 days, assume removal from the system
removaldate<-max(as.Date(bikeData$StartDate,format="%m/%d/%Y"))-30

bikeLevelData<-bikeData%>%
  filter(BikeName!="")%>%
  group_by(BikeName)%>%
  mutate(StartDate=as.Date(StartDate,format="%m/%d/%Y"))%>%
  summarise(
    usuallyCasual=sum(PaymentPlan=="Casual")>sum(PaymentPlan=="Subscriber"),
    totalMiles=sum(Distance_Miles,na.rm=TRUE),
    RemovedFromService=max(StartDate,na.rm = TRUE)<removaldate
    )%>%
  data.frame()


coxphModel<-coxph(Surv(totalMiles,event=RemovedFromService)~usuallyCasual, data=bikeLevelData)

ggsurvplot(survfit(formula(coxphModel),data=bikeLevelData),
           conf.int = TRUE,
           risk.table = TRUE,
           legend="none",
           pval=TRUE,
           title="Bike Survival by Ridership type",
           xlab="Miles"
          )
