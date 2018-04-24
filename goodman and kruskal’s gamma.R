#install.packages("DescTools")
library('DescTools')

#### Code to write Tables 2 and 3 ####
gamma<-matrix(nrow=18,ncol=4)
colnames(gamma)<-colnames(survey[1:4])
rownames(gamma)<-colnames(survey[5:22])

ASE<-matrix(nrow=18,ncol=4)
colnames(ASE)<-colnames(survey[1:4])
rownames(ASE)<-colnames(survey[5:22])

for (i in 1:4){
  for (j in 1:18){
    gamma[j,i]<-GKgamma(table(survey[,i],survey[,j+4]))$gamma
    ASE[j,i]<-GKgamma(table(survey[,i],survey[,j+4]))$sigma
  }
}

z<-gamma/ASE

round(gamma,3)
round(z,3)

