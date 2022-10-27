#########Xialing Zhao
########PS4
########Oct 15 2022

################################################################################
#variables are defined and were treated as described in Nevo (2000), "A Practitioner's Guide to 
#Estimation of Random Coefficients Logit Models of Demand," Journal of Economics & Management 
#Strategy 9(4): 513-548. 
################################################################################


rm(list=ls())
library(dplyr)
library(ivreg)
library(pacman)
library(ggplot2)
library(readxl)
library(lfe)
library(stargazer)
library(ggplot2)
library(ePCR)
library(tidyr)
#p_load(broom, tidyverse, data.table, ggplot2, latex2exp, dplyr, readstata13, stringr, readr, sp, sf, lfe, stargazer, fixest, texreg, janitor, quantreg, foreign, ggpubr, qte, sandwich, multiwayvcov,mfx,sqldf)

setwd("/Users/zhaoxialing/Desktop/ps 4/data")

################################################
#cereal market
################################################
cereal<-read_excel("cereal_ps3.xls")
#id: bbbbccyyq brand(24)-city(47)-year(1988)-quarter(2)
#share:market share of brand j in market t.
#price: products price
#sugar: % of sugar
#mushy: mushy or not, dummy
#z1-z20: IV's for the price variable 

#Q:what happened to brand6
#94*24
cereal%>%group_by(brand)%>%count()%>%print(n=23)
################################################
#demographic
################################################
#demographic variables from the CPS for 20 individuals in each market
demog <- read_excel("demog_ps3.xls")
#94 observation markets (47 cities by 2 quarters) 
#80 variables (20 individuals X 4 variables).
#v1-v80: random draws.For each market 80 iid normal draws are provided.
#demean: income,income squared, age, child dummy
#Q: what's order?


################
# Question 1
################

t_share<-aggregate(share~city+quarter,data=cereal,sum)
names(t_share)[names(t_share)=='share']<-'t_share'
cereal<-merge(cereal, t_share, by = c("city","quarter"))
cereal$out_share<-1-cereal$t_share
cereal$y<-log(cereal$share)-log(cereal$out_share)

# OLS without brand fixed effects
ols<-glm(y~price, data=cereal)
summary(ols)

# OLS with brand fixed effects
ols_fe<-felm(y~price|brand, data=cereal)
summary(ols_fe)

# IV without brand fixed effects
iv_nf<-ivreg(y~price|.-price+z1+z2+z3+z4+z5+z6+z7+z8+z9+z10+z11+z12+z13+
               z14+z15+z16+z17+z18+z19+z20,data=cereal)
summary(iv_nf)

# IV with brand fixed effects
iv_fe<-felm(y~0|brand|(price~z1+z2+z3+z4+z5+z6+z7+z8+z9+z10+z11+z12+
              z13+z14+z15+z16+z17+z18+z19+z20),data=cereal)
summary(iv_fe)

#model results
stargazer(ols,ols_fe,iv_nf,iv_fe,type="text",omit.stat = c('aic','f'),out="model results.html",dep.var.labels.include=FALSE,
          dep.var.caption="",
          column.labels=c ("OLS","OLS-FE","IV","IV-FE"))
      
################
# question 2
################

#generate ownership matrix
firmbr<-as.character(unique(cereal$firmbr))
ownership<-as.data.frame(0) 
ownership[1:24,1:24]<-0
colnames(ownership)<-firmbr
rownames(ownership)<-firmbr
ownership[1:24,1:24]<-firmbr
ownership<-gather(ownership)
ownership$first<-as.numeric(substr(ownership$key, 1, 1))
ownership$second<-as.numeric(substr(ownership$value, 1, 1))
ownership$ownership<-0
ownership$ownership[ownership$first==ownership$second]<-1
ownership<-ownership[c('key','value','ownership')]
ownership_long<-ownership
colnames(ownership_long)<-c('j','k','ownership')
ownership<-spread(ownership,key,ownership)
rownames(ownership)<-firmbr
ownership<-ownership[2:25]

#
alpha<-iv_fe$coefficients







nevos_model <- as.formula("y ~ sugar + mushy |price|
   z1+z2+z3+z4+z5+z6+z7+z8+z9+z10+z11+z12+z13+z14+z15+z16+z17+z18+z19+z20")
Data_1<-cereal
Data_1$constant<-1
Data_1$constant[1:4, 1:5]

install.packages("BLPestimatoR")
library(BLPestimatoR)
attach(Data_1)
cereal_data <- BLP_data(
  model = nevos_model,
  productData = cereal,
  blp_inner_tol = 1e-6, blp_inner_maxit = 5000,
  integration_draws = originalDraws_cereal,
  integration_weights = rep(1 / 20, 20)
)

K<-2
data <- simulate_BLP_dataset(nmkt = 94, nbrn = 24,
                             Xlin = c("price", "sugar", "mushy"),
                             Xexo = c("sugar", "mushy"),
                             Xrandom = paste0("x",1:K),instruments = paste0("iv",1:20),
                             true.parameters = list(Xlin.true.except.price = rep(0.2,5),
                                                    Xlin.true.price = -0.2,
                                                    Xrandom.true = rep(2,K),
                                                    instrument.effects = rep(2,20),
                                                    instrument.Xexo.effects = rep(1,5)),
                             price.endogeneity = list( mean.xi = -2,
                                                       mean.eita = 0,
                                                       cov = cbind( c(1,0.7), c(0.7,1))),
                             printlevel = 0, seed = 234234 )
install.packages("Rblpapi")
library(Rblpapi)

# NOT RUN {
# Parameters
i<-1
K<-2
Xlin_example <-  c("price", "sugar", "mushy")
Xexo_example <- c("sugar", "mushy")
Xrandom_example <- paste0("x",1:K)
instruments_example <- paste0("iv",1:10)

# Data generation
BLP_data <- get.BLP.dataset(nmkt = 25, nbrn = 20,
                            Xlin = Xlin_example,
                            Xexo = Xexo_example,
                            Xrandom = Xrandom_example,
                            instruments = instruments_example,
                            true.parameters = list(Xlin.true.except.price = rep(0.2,5),
                                                   Xlin.true.price = -0.2, Xrandom.true = rep(2,K),
                                                   instrument.effects = rep(2,10),
                                                   instrument.Xexo.effects = rep(1,5)),
                            price.endogeneity = list( mean.xi = -2,
                                                      mean.eita = 0,
                                                      cov = cbind( c(1,0.7), c(0.7,1))),
                            printlevel = 0, seed = 5326 )

# Estimation
BLP_est<- estimateBLP(Xlin = Xlin_example,
                      Xrandom = Xrandom_example,
                      Xexo =  Xexo_example,
                      instruments = instruments_example,
                      shares = "shares",
                      cdid = "cdid",
                      productData = BLP_data,
                      starting.guesses.theta2 = rep(1,K),
                      solver.control = list(maxeval = 5000),
                      solver.method = "BFGS_matlab",
                      
                      starting.guesses.delta =  rep(1, length(BLP_data$cdid)),
                      blp.control = list(inner.tol = 1e-6,
                                         inner.maxit = 5000),
                      integration.control= list(  method="MLHS",
                                                  amountNodes= 100,
                                                  seed= 3   ),
                      postEstimation.control= list(standardError = "robust",
                                                   extremumCheck = TRUE,
                                                   elasticities = "price"),
                      printLevel = 2)

# Show results
summary(BLP_est)
# }


