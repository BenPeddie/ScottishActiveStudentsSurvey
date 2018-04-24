library("MuMIn")

## Create generalised linear models from the data with all combinations of the different 
## explanatory variables, using the poission family
dredge.data<-survey[,c(1:4,24)]
globalmodel.pois<-glm(swemwbsrawscaled~.,data=,dredge.data,family = poisson(link="log"),na.action = "na.fail")
## Function to return the numeric term of the estimate from function dispersiontest 
## , function to be used by dredge formula as an extra to show if we have overdispersion
dispersion<-function(model){
  a<-dispersiontest(model)
  return(as.numeric(a$estimate))
}
## Use dredge formula with extra as disp.test to evaluate models with all combinations of
## explanatory variables, adding in a column for the disp.test as above. Output of Dredge 
## gives us the model selection Table 6
dredged.models.pois<-dredge(globalmodel.pois,extra=dispersion)

## Output shows dispersions of around 1.43/1.44
## Switch to Negative Binomial due to significant evidence of overdispersion

library(MASS) #MASS library has the function glm.nb for creating negative binomial GLMs

globalmodel.nb<-glm.nb(swemwbsrawscaled~.,data=,dredge.data,na.action = "na.fail")
## Evaluate all models in the candidate set - output of dredge gives us Table 7
dredged.models.nb<-dredge(globalmodel.nb)

# Extract all the models
all.models.nb <- get.models(dredged.models2,subset=TRUE)

# perform model averaging on the 
avgmod.nb <- model.avg(all.models.nb)
## Summary gives us a range of tables including Table 8 and Table 10
a<-summary(avgmod.nb)
## confint gives us the last two columns of Table 8
confint(avgmod.nb)
a$coefmat.subset[,3]




