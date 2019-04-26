library(readstata13)
library(foreign)
library('multiwayvcov')
library(mlogit)
library(xtable)
library(stargazer)##
library(ggplot2)
library('car')##
library(mgcv)
library(reshape2)
library(nnet)
library('Matching')

?GenMatch

#Load in replication data from B&W
county.data <- read.dta13("BW JOP county replication data.dta")
indiv.data <- read.dta("BW JOP individual replication data.dta")
state.data <- read.dta("BW JOP state replication data.dta")



###############################################################################################  
#TABLE 4

state.data.t4 <- state.data[ which(state.data$presyear==0 
                                   & state.data$year >= '1980'
                                   & state.data$GubElection == 1), ]

#The regression they ran, without year fixed effects
table4.1 <- lm(vep ~ uerate + incparty + uerate*incparty + s_black + college + SenElection
               + factor(year)+factor(fips_state), data=state.data.t4)
summary(table4.1)


head(state.data.t4)

t41.vcovCL <- cluster.vcov(table4.1, state.data.t4$fips_state, df_correction = TRUE)
ses.t41 <- sqrt(diag(t41.vcovCL))
ses.t41

#--- matching----------------------------------------------



mydata = state.data.t4

q_75 = quantile(mydata$uerate)[4]
q_25 = quantile(mydata$uerate)[2]

quantile(mydata$uerate)

mydata$uerate[mydata$uerate >= q_75] <- 1
mydata$uerate[mydata$uerate <= q_25] <- 0
mydata$uerate[mydata$uerate< q_75 & mydata$uerate > q_25] <- NA

nrow(mydata)
mydata = na.omit(mydata)
mydata$incparty = as.numeric(mydata$incparty)


Tr=mydata$uerate
Y = mydata$vep




X = cbind( mydata$incparty ,  mydata$s_black , mydata$college , mydata$SenElection, factor(mydata$fips_state), factor(mydata$year))



genout <- GenMatch(Tr=mydata$uerate, X=X,
                   pop.size=200, max.generations=10, wait.generations=25, caliper = 0.9)

mout <- Match(Y=Y, Tr=Tr, X=X,  Weight.matrix=genout, caliper = 0.9)
summary(mout)

mb <- MatchBalance(Tr~mydata$incparty+mydata$s_black + mydata$college + mydata$SenElection+  factor(mydata$fips_state)+factor(mydata$year),
                   match.out=mout, nboots=1000)



###################################################################################################
#TABLE 5 - correct coefficients and standard errors (except intercept)

#Convert educ and income variables to numeric
indiv.data1 <- indiv.data
indiv.data1$educ <- as.numeric(indiv.data1$educ)
indiv.data1$income <- as.numeric(indiv.data1$income)

indiv.data.ml <- mlogit.data(indiv.data1, choice="partyvote", shape="wide")

table5 <- mlogit(partyvote ~ 0 | uerate + totalspend_voter_inf + democrat + republican + black + hisp +
                   other + female + married + age + educ + income + incomedk + unemployed + factor(fips_state),
                 data=indiv.data.ml, reflevel="Abstain")
summary(table5)
#----------------Matching-----------------

mydata5 = indiv.data1



# deleting NAs
mydata5 = na.omit(mydata5)


#-------------------- changing the treatment variable to binary using 25 and 75 quantiles
quantile_75 =  quantile(mydata5$uerate)[4]
quantile_25 =  quantile(mydata5$uerate)[2]

mydata5$uerate[mydata5$uerate >= quantile_75] <- 1
mydata5$uerate[mydata5$uerate <= quantile_25] <- 0
mydata5$uerate[mydata5$uerate < quantile_75 & mydata5$uerate > quantile_25] <- NA
mydata5 = na.omit(mydata5)
mydata5$uerate
nrow(mydata5)

#------------------- editing the outcome variable To estimate the treatment effect on Democrats:
Democrat_Data =mydata5 
Democrat_Data$partyvote
Democrat_Data$partyvote =  as.character(Democrat_Data$partyvote)


Democrat_Data$partyvote[Democrat_Data$partyvote != "Democrat"] <- 0
Democrat_Data$partyvote[Democrat_Data$partyvote == "Democrat"] <- 1
Democrat_Data$partyvote =  as.numeric(Democrat_Data$partyvote)
Democrat_Data$partyvote

#------------------Matching to find the treatment effect on Democrats-----------

#uerate + totalspend_voter_inf + democrat + republican + black + hisp +
 # other + female + married + age + educ + income + incomedk + unemployed + factor(fips_state)


Democrat_Tr = Democrat_Data$uerate

X = cbind(Democrat_Data$totalspend_voter_inf, Democrat_Data$democrat , Democrat_Data$republican , Democrat_Data$black , Democrat_Data$hisp ,
            Democrat_Data$other , Democrat_Data$female , Democrat_Data$married , Democrat_Data$age , Democrat_Data$educ, 
           Democrat_Data$income,  Democrat_Data$unemployed)

genout <- GenMatch(Tr=Democrat_Tr, X=X,
                   pop.size = 16,max.generations=10, wait.generations=1, caliper = .1)

Y = Democrat_Data$partyvote

mout <- Match(Y=Y, Tr=Democrat_Tr, X=X,  Weight.matrix=genout, caliper = .1)
summary(mout)

mb <- MatchBalance(Democrat_Tr~Democrat_Data$totalspend_voter_inf+Democrat_Data$democrat + Democrat_Data$republican+Democrat_Data$black + Democrat_Data$hisp +
                     Democrat_Data$other + Democrat_Data$female + Democrat_Data$married + Democrat_Data$age + Democrat_Data$educ+
                     Democrat_Data$income+Democrat_Data$unemployed 
                   ,match.out=mout, nboots=500)


#Democrat_Data$totalspend_voter_inf
summary(mout)


#sensitivity analysis:
library('rbounds')
psens(mout, GammaInc = 0.1)
################----------- Matching to find the treatment effect on the Republican-----

Republican_Data =mydata5 
Republican_Data$partyvote =  as.character(Republican_Data$partyvote)

#Changing the outcome variable to republican:
Republican_Data$partyvote[Republican_Data$partyvote != "Republican"] <- 0
Republican_Data$partyvote[Republican_Data$partyvote == "Republican"] <- 1
Republican_Data$partyvote =  as.numeric(Republican_Data$partyvote)
Republican_Data$partyvote


#matching:
Republican_Tr = Republican_Data$uerate
Y = Republican_Data$partyvote

X = cbind(Republican_Data$totalspend_voter_inf, Republican_Data$democrat , Republican_Data$republican , Republican_Data$black , Republican_Data$hisp ,
          Republican_Data$other , Republican_Data$female , Republican_Data$married , Republican_Data$age , Republican_Data$educ, 
          Republican_Data$income,  Republican_Data$unemployed)

genout <- GenMatch(Tr=Republican_Tr, X=X,
                   pop.size = 16,max.generations=10, wait.generations=1, caliper = .1)


mout <- Match(Y=Y, Tr=Republican_Tr, X=X,  Weight.matrix=genout, caliper = .1)
summary(mout)

mb <- MatchBalance(Republican_Tr~Republican_Data$totalspend_voter_inf+Republican_Data$democrat + Republican_Data$republican+Republican_Data$black + Republican_Data$hisp +
                     Republican_Data$other + Republican_Data$female + Republican_Data$married + Republican_Data$age + Republican_Data$educ+
                     Republican_Data$income+Republican_Data$unemployed 
                   ,match.out=mout, nboots=500)


#Democrat_Data$totalspend_voter_inf
summary(mout)


################----------- Matching to find the treatment effect on the Abstain-----

Abstain_Data =mydata5 
Abstain_Data$partyvote =  as.character(Abstain_Data$partyvote)

#Changing the outcome variable to republican:
Abstain_Data$partyvote[Abstain_Data$partyvote != "Abstain"] <- 0
Abstain_Data$partyvote[Abstain_Data$partyvote == "Abstain"] <- 1
Abstain_Data$partyvote =  as.numeric(Abstain_Data$partyvote)
Abstain_Data$partyvote


#matching:
Y = Abstain_Data$partyvote


X = cbind(Republican_Data$totalspend_voter_inf, Republican_Data$democrat , Republican_Data$republican , Republican_Data$black , Republican_Data$hisp ,
          Republican_Data$other , Republican_Data$female , Republican_Data$married , Republican_Data$age , Republican_Data$educ, 
          Republican_Data$income,  Republican_Data$unemployed)

genout <- GenMatch(Tr=Republican_Tr, X=X,
                   pop.size = 16,max.generations=10, wait.generations=1, caliper = .1)


mout <- Match(Y=Y, Tr=Republican_Tr, X=X,  Weight.matrix=genout, caliper = .1)
summary(mout)

mb <- MatchBalance(Republican_Tr~Republican_Data$totalspend_voter_inf+Republican_Data$democrat + Republican_Data$republican+Republican_Data$black + Republican_Data$hisp +
                     Republican_Data$other + Republican_Data$female + Republican_Data$married + Republican_Data$age + Republican_Data$educ+
                     Republican_Data$income+Republican_Data$unemployed 
                   ,match.out=mout, nboots=500)


#Democrat_Data$totalspend_voter_inf
summary(mout)




# Replication Data ------------------------------------------------------------------------------------------------
replication_data = read.csv("Replication_Paper_Data.csv")

replication_data
quantile_75 = quantile(replication_data$uerate)[4]
quantile_25 = quantile(replication_data$uerate)[2]






replication_data$uerate[replication_data$uerate >= quantile_75] <- 1
replication_data$uerate[replication_data$uerate <= quantile_25] <- 0
replication_data$uerate[replication_data$uerate < quantile_75 & replication_data$uerate > quantile_25] <- NA
replication_data = na.omit(replication_data)


Democrat_Data =replication_data 
Democrat_Data$partyvote
Democrat_Data$partyvote =  as.character(Democrat_Data$partyvote)

Democrat_Data$partyvote[Democrat_Data$partyvote != "Democrat"] <- 0
Democrat_Data$partyvote[Democrat_Data$partyvote == "Democrat"] <- 1
Democrat_Data$partyvote =  as.numeric(Democrat_Data$partyvote)
Democrat_Data$partyvote




Republican_Data =replication_data 
Republican_Data$partyvote =  as.character(Republican_Data$partyvote)

Republican_Data$partyvote[Republican_Data$partyvote != "Republican"] <- 0
Republican_Data$partyvote[Republican_Data$partyvote == "Republican"] <- 1
Republican_Data$partyvote =  as.numeric(Republican_Data$partyvote)
Republican_Data$partyvote




X = cbind(replication_data$totalspend_voter_inf , replication_data$democrat , replication_data$republican , replication_data$black , 
          replication_data$hisp ,replication_data$other , replication_data$female , replication_data$married , replication_data$age , 
          replication_data$educ, replication_data$income  ,replication_data$unemployed,factor(replication_data$year ))


Tr = replication_data$uerate


genout <- GenMatch(Tr=Tr, X=X,
                   pop.size = 20,max.generations=10, wait.generations=25, caliper=0.1)

Y = Republican_Data$partyvote

mout <- Match(Y=Y, Tr=Tr, X=X,  Weight.matrix=genout, caliper = 0.1)
summary(mout)

mb <- MatchBalance(Tr~ replication_data$totalspend_voter_inf +replication_data$democrat + replication_data$republican + 
                     replication_data$black + replication_data$hisp +replication_data$other + replication_data$female +
                     replication_data$married + replication_data$age + replication_data$educ + replication_data$income  +
                     replication_data$unemployed+factor(replication_data$year),
                   match.out=mout, nboots=500)





#-----------------------------
#SE

#Remove NA's from dataset for SE
indiv.data2 <- na.omit(indiv.data)
nrow(indiv.data2)

cl.mlogit   <- function(fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- length(coefficients(fm))
  dfc <- (M/(M-1))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat.=crossprod(uj)/N)
  ses <- sqrt(diag(vcovCL))
  coeftest <- coeftest(fm, vcovCL) 
  return(newList <- list("summary" = coeftest, "vcovCL" = vcovCL, "ses" = ses))
}

ses.t51 <- cl.mlogit(table5, indiv.data2$fips_state)
