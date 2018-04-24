#install.packages("Kendall")
#### Code to write tables 4 and 5 ####
library("Kendall")
taub<-matrix(nrow=18,ncol=4)
colnames(taub)<-colnames(survey[1:4])
rownames(taub)<-colnames(survey[5:22])

taupvalue<-matrix(nrow=18,ncol=4)
colnames(taupvalue)<-colnames(survey[1:4])
rownames(taupvalue)<-colnames(survey[5:22])

for (i in 1:4){
  for (j in 1:18){
    taub[j,i]<-Kendall(survey[,i],survey[,j+4])$tau
    taupvalue[j,i]<-Kendall(survey[,i],survey[,j+4])$sl
  }
}

taub
taupvalue
