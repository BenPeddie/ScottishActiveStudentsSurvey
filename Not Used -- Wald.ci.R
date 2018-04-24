### Not Used in the study ####

wald.ci<-function(Table, aff.response, alpha=.05){
  # Gives two-sided Wald CI's for odds ratio, difference in proportions and relative risk.
  # Table is a 2x2 table of counts with rows giving the treatment populations
  # aff.response is a string like "c(1,1)" giving the cell of the beneficial response and the
  # treatment category
  # alpha is significance level
  
  pow<-function(x, a=-1) x^a
  z.alpha<-qnorm(1-alpha/2)
  
  if(is.character(aff.response))
    where<-eval(parse(text=aff.response))
  else where<-aff.response
  
  Next<-as.numeric(where==1) + 1
  
  # OR
  odds.ratio<-
    Table[where[1],where[2]]*Table[Next[1],Next[2]]/(Table[where[1],Next[2]]*Table[Next[1],where[
      2]])
  se.OR<-sqrt(sum(pow(Table)))
  ci.OR<-exp(log(odds.ratio) + c(-1,1)*z.alpha*se.OR)
  
  # difference of proportions
  p1<-Table[where[1],where[2]]/(n1<-Table[where[1],Next[2]] + Table[where[1],where[2]])
  p2<-Table[Next[1],where[2]]/(n2<-Table[Next[1],where[2]]+Table[Next[1],Next[2]])
  
  se.diff<-sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
  ci.diff<-(p1-p2) + c(-1,1)*z.alpha*se.diff
  
  # relative risk
  RR<-p1/p2
  se.RR<-sqrt((1-p1)/(p1*n1) + (1-p2)/(p2*n2))
  ci.RR<-exp(log(RR) + c(-1,1)*z.alpha*se.RR)
  
  list(OR=list(odds.ratio=odds.ratio, CI=ci.OR), proportion.difference=list(diff=p1-p2,
                                                                            CI=ci.diff), relative.risk=list(relative.risk=RR,CI=ci.RR))
}