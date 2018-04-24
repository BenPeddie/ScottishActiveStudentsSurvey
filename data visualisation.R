######Code to create Table 1#####
data.sum<-matrix(nrow=23,ncol=8)
# row names of table
row.names(data.sum)<-colnames(survey[,1:23])
# column names
colnames(data.sum)<-c("Exp/Resp","Data Type","Min","Median","Mean","Max","Variance","SD")
# first four rows explanatory, the remainder response
data.sum[,1]<-c(rep("Explanatory",4),rep("Response",19))
# data type
data.sum[,2]<-c("Binary", "Binary", "Binary",rep("Ordinal",18),
                "Latent Variable with an underlying continuous scale","Ordinal")
#Fill in the summary statistics of each of the variables
for (i in 1:22){
  data.sum[i,3]<-min(survey[,i])
  data.sum[i,4]<-median(survey[,i])
  data.sum[i,5]<-round(mean(survey[,i]),3)
  data.sum[i,6]<-max(survey[,i])
  data.sum[i,7]<-round (var(survey[,i]),3)
  data.sum[i,8]<-round(sd(survey[,i]),3)
}
#change to a data fram
data.sum<-as.data.frame(data.sum)
#round numbers to 3 digits
round(sd(survey$swemwbsrawscaled),3)
## ======================== ##


##### Code to create Figure 2 #########
# First Plot (Top Left) - By Gym Member
swe.gym<-data.frame(swemwbs=survey$swemwbs[survey$gym.member==1])
swe.no.gym<-data.frame(swemwbs=survey$swemwbs[survey$gym.member==0])

swe.gym$label<-'Gym Member'
swe.no.gym$label<-'Not Gym Member'
swe.by.gym<-rbind(swe.gym,swe.no.gym)

a<-ggplot(swe.by.gym,aes(swemwbs,fill=label))+geom_density(alpha=0.5)
a<-a+labs(title="Density of SWEMWBS Responses by Gym Member", x="SWEMWBS",y="Density")
a<-a+scale_fill_discrete(name="Gym Member?",labels=c("Yes","No"))

# Top Right - By Club Member
swe.club<-data.frame(swemwbs=survey$swemwbs[survey$club.member==1])
swe.no.club<-data.frame(swemwbs=survey$swemwbs[survey$club.member==0])

swe.club$label<-'Club Member'
swe.no.club$label<-'Not Club Member'
swe.by.club<-rbind(swe.club,swe.no.club)

b<-ggplot(swe.by.club,aes(swemwbs,fill=label))+geom_density(alpha=0.5)
b<-b+labs(title="Density of SWEMWBS Responses by Club Member", x="SWEMWBS",y="Density")
b<-b+scale_fill_discrete(name="Club Member?",labels=c("Yes","No"))

# Bottom Left - By Competitive
swe.comp<-data.frame(swemwbs=survey$swemwbs[survey$competitive==1])
swe.no.comp<-data.frame(swemwbs=survey$swemwbs[survey$competitive==0])

swe.comp$label<-'Plays Competitively'
swe.no.comp$label<-'Does Not Play Competitively'
swe.by.comp<-rbind(swe.comp,swe.no.comp)

c<-ggplot(swe.by.comp,aes(swemwbs,fill=label))+geom_density(alpha=0.5)
c<-c+labs(title="Density of SWEMWBS Responses by Competitive", x="SWEMWBS",y="Density")
c<-c+scale_fill_discrete(name="Play Competitively?",labels=c("Yes","No"))

# Bottom Right - By Activity Level
swe.act.1<-data.frame(swemwbs=survey$swemwbs[survey$activity.level==1])
swe.act.2<-data.frame(swemwbs=survey$swemwbs[survey$activity.level==2])
swe.act.3<-data.frame(swemwbs=survey$swemwbs[survey$activity.level==3])
swe.act.4<-data.frame(swemwbs=survey$swemwbs[survey$activity.level==4])


swe.act.1$label<-'Less than 30 Minutes'
swe.act.2$label<-'31-90 Minutes'
swe.act.3$label<-'91-150 Minutes'
swe.act.4$label<-'150+ Minutes'

swe.by.act<-rbind(swe.act.1,swe.act.2,swe.act.3,swe.act.4)

d<-ggplot(swe.by.act,aes(swemwbs,fill=label))+geom_density(alpha=0.3)
d<-d+labs(title="Density of SWEMWBS Responses by Activity Level", x="SWEMWBS",y="Density")
d<-d+scale_fill_discrete(name="Activity Level")

# All four plots in one diagram
ggarrange(a,b,c,d)
##-------------------------------##


##### Code to make Figure 3 #####
a.1<-data.frame(table(survey[,1]))
b.1<-data.frame(table(survey[,2]))
c.1<-data.frame(table(survey[,3]))
d.1<-data.frame(table(survey[,4]))
a.1$label<-"Gym Member?"
b.1$label<-"Club Member?"
c.1$label<-"Competitive?"
d.1$label<-"Activity Level"


bar.all<-rbind(a.1,b.1,c.1,d.1)
bar.all$Var1<-c("No","Yes","No","Yes","No","Yes","Under 30 Minutes","31-90 Minutes","91-150 Minutes","150+ Minutes")
bar.all$Var1<-factor(bar.all$Var1, levels=c("Yes","No","Under 30 Minutes","31-90 Minutes","91-150 Minutes","150+ Minutes"))

e<-ggplot(bar.all,aes(label,Freq))+
   geom_bar(aes(fill=Var1),position="dodge",stat="identity")+
  labs(title="Bar Chart of Responses for Response Variables", 
       x="Response Variables",y="Frequency")+
  scale_fill_discrete(name="Responses")

