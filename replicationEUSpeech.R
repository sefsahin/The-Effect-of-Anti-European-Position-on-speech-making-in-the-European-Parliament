

rm(list=ls(all=TRUE)) 
install.packages("mfx")

library(mfx)
library(Zelig)
library(xtable)
library(readr)
library(stargazer)
library(ggplot2)
library(MASS)
library(tidyr)
library(readr)








#### to find eu_positions of parties I use chaper hill expert survey for year 1999
ch <- read_csv("1999-2019_CHES_dataset_means(v1).csv")
ch1 <- ch[ which(ch$year==1999), ]
ch12 <- data.frame( party_id=ch1$party_id ,eu_position=ch1$eu_position,eu_salience=ch1$eu_salience,
                    eu_dissent=ch1$eu_dissent,lrgen= ch1$lrgen)

##### omittting parties that are not present in Chaper Hill Survey
newdata <- ep5data[!(ep5data$np=="1303" | ep5data$np=="1418"),]


#####matching parties in the Prockhs's data(Hix's party id) with Chaper Hill party id  
matchingmat1a <- c(1004,1005,1101,1103,1104,1107,1108,1110,1111,1201,1202,1204,1205,1207,1301,1311,1314,1403,1404,1405)
matchingmat1b <- c(1302,1301,105,109,104,106,102,112,103,301,308,304,306,302,502,504,501,619,609,603)
matchingmat2a <- c(1407,1408,1409,1410,1412,1414,1416,1504,1505,1506,1601,1604,1606,1607,1608,1611,1613,1614,1705,1708)
matchingmat2b <- c(612,614,601,615,602,613,605,404,402,401,805,818,823,819,815,811,802,803,216,201)
matchingmat3a <- c(1711,1803,1901,1902,1903,1906,1907,1909,2001,2002,2003,2007,2101,2105,2106,2201,2202,2302,2304,2306,2307,2401,2404,2405,2409)
matchingmat3b <- c(211,3801,1001,1004,1005,1002,1006,1003,1403,1402,1401,1408,1202,1205,1206,701,702,1604,1605,1602,1601,1101,1102,1104,1108)

#### gather matching vectors in one big vector
matchingmataa <- c(matchingmat1a,matchingmat2a,matchingmat3a)
matchingmatbb <- c(matchingmat1b,matchingmat2b,matchingmat3b)

##### data matching
matchingdata <- data.frame(np=matchingmataa, party_id=matchingmatbb)
data1 <- merge(x = newdata, y = matchingdata, by = c("np"), all.x = TRUE)
lastdata <- merge(x = data1, y = ch12, by = c("party_id"), all.x = TRUE)
lastdata <-data.frame(lastdata, eu_positionsq =lastdata$eu_position*lastdata$eu_position)

summary(lastdata)
###### for simulations 
###### omitting NA's(last 5 observations) for MPs that do not have score for EU position
lastdata1 <- lastdata[-c(554:558),c(1:25)]

#######################################


###party manifesto
MPDataset_MPDS2020b <- read_csv("MPDataset_MPDS2020b.csv")
load("EUPrepdata.Rdata")
df <- separate(MPDataset_MPDS2020b, edate, into = c("day","month","year"), sep = "/")
df$newdate <- paste(df$year)

partymanifesto <- data.frame(country= df$countryname,  party=df$party,date= df$newdate, positive = df$per108, negative=df$per110  )

levels(partymanifesto$country)

lastpartymanifesto <- partymanifesto[which(partymanifesto$country %in%  c( "Austria"  ,    "Belgium",        "Denmark",      "Spain" ,
                                                                           "Germany" ,   "France" ,      "Greece",         "Italy" ,      "Denmark",
                                                                           "Luxembourg",   "Netherlands",    "Finland",     "Portugal",
                                                                           "Ireland",      "Sweden",         "United Kingdom" )),]

pmanifest <- subset(lastpartymanifesto,date %in% c ("1996","1997","1998","1999"))

#### p for party manifesto c for chaperhill
matchxp <- c(42520,42320,21112,21520,21111,21425,21322,21914,21221,41521,41521,41113,41221,41320,33610,33611,33320,31625,31230)
matchxc <- c(1302,1301,105,109,104,106,102,112,103,301,308,304,306,302,502,504,501,609,603)
matchyp <- c(31220,31320,31624,31110,34210,34511,34313,32710,32520,32421,32610,32720,32220,32212,13320)
matchyc <- c(601,602,613,605,404,402,401,805,823,819,815,811,802,803,201)
matchzp <- c(13420,23520,22521,22330,22110,22320,22952,22420,14810,14620,14320,14110,34520,35311,35313,53620,53520,11420,11620,11320,11220,51620,51320,51421,51951)
matchzc <- c(211,3801,1001,1004,1005,1002,1006,1003,1403,1402,1401,1408,1202,1205,1206,701,702,1604,1605,1602,1601,1101,1102,1104,1108)


matchingmatp <- c(matchxp,matchyp,matchzp)
matchingmatc <- c(matchxc,matchyc,matchzc)
##### data matching


matchingdata <- data.frame(party=matchingmatp, party_id=matchingmatc)
data1 <- merge(x = lastdata, y = matchingdata, by = c("party_id"), all.x = TRUE)
lastdata <- merge(x = data1, y = pmanifest, by = c("party"), all.x = TRUE)
lastdata <-data.frame(lastdata, manifesteu =-lastdata$negative+lastdata$positive)


###### Scatterplot and Barplot for Preliminary analyses ######
### Figure 1.


### Round EU positions scores to nearest integer
speech <- cbind(lastdata1$epwebsitespeechcount,round(lastdata1$eu_position,0))

speechhist <- cbind(c(mean(speech[which(speech[,2] == 1 )]),mean(speech[which(speech[,2] == 2 )]),mean(speech[which(speech[,2] == 3 )]),
                      mean(speech[which(speech[,2] == 4 )]),mean(speech[which(speech[,2] == 5 )]),mean(speech[which(speech[,2] == 6 )]),
                      mean(speech[which(speech[,2] == 7 )])),seq(1,7))



pdf("prelim.pdf")
par(mfrow=c(1,2))
plot(speechhist[,2], speechhist[,1],ylim = c(40,200), type = "b", frame = FALSE, pch = 19, 
     col = "black", xlab = "EU Position (Nearest)", ylab = "Mean of Speech Count",main="Speech Counts 
     by EU Position of MPs ")

barplot(table(speech[,2]), main="EU Position of MPs ",
        xlab="EU Position (Nearest)",ylab="Number of MPs")
dev.off()


####################



########  Replicating  models from Slapin & Proksh 2010 #######
##
attach(ep5data)
##
model.a <-
  glm.nb(
    epwebsitespeechcount~pernpepg+ perepgnp+  tenure+absent+rapporteurshipcount+ partyleader 
    +epleader+ numcommem+ numcomleader+groupsize+ natpartyperc +candidateselection+pernpepg*candidateselection,
    data = ep5data, 
    control = glm.control(maxit = 100)
  )


model.b <-
  glm.nb(
    epwebsitespeechcount~pernpepg+ perepgnp+  tenure+absent+rapporteurshipcount+ partyleader 
    +epleader+ numcommem+ numcomleader+groupsize+ natpartyperc +candidateselection+pernpepg*candidateselection
    +factor(memberstate)+factor(epg),
    data = ep5data, 
    control = glm.control(maxit = 100)
  )

summary(model.a)
summary(model.b)

## 
detach(ep5data)
## 

########### My models with eu position ##########

m2 <-
  glm.nb(
    epwebsitespeechcount~eu_position+I(eu_position^2)+ pernpepg+ perepgnp 
    + tenure+absent+rapporteurshipcount+ partyleader +epleader+ numcommem+ numcomleader
    +groupsize+ natpartyperc +candidateselection+  pernpepg*candidateselection,
    data = lastdata1, 
    control = glm.control(maxit = 100)
  )

### alternative independent variable party manifest
m22 <-
  glm.nb(
    epwebsitespeechcount~manifesteu+I(manifesteu^2)+ pernpepg+ perepgnp 
    +  tenure+absent+rapporteurshipcount+ partyleader +epleader+ numcommem 
    + numcomleader+groupsize+ natpartyperc +candidateselection+  pernpepg*candidateselection,
    data = lastdata, 
    control = glm.control(maxit = 100)
  )

##### Main model with EP political group and County Dummies
m3 <-
  glm.nb(
    epwebsitespeechcount~eu_position+I(eu_position^2)+ pernpepg 
    + perepgnp+  tenure+absent+rapporteurshipcount+ partyleader +epleader+ numcommem
    + numcomleader+groupsize+ natpartyperc +candidateselection+  pernpepg*candidateselection
    +factor(memberstate)+factor(epg),
    data = lastdata1, 
    Hessian=TRUE,
    control = glm.control(maxit = 100)
  )


### Main model with alternative independent variable party manifest

m33 <-
  glm.nb(
    epwebsitespeechcount~manifesteu+I(manifesteu^2)+ pernpepg+ perepgnp 
    +  tenure+absent+rapporteurshipcount+ partyleader +epleader+ numcommem 
    + numcomleader+groupsize+ natpartyperc +candidateselection+  pernpepg*candidateselection
    +factor(memberstate)+factor(epg),
    data = lastdata, 
    Hessian=TRUE,
    control = glm.control(maxit = 100)
  )


summary(m2)
summary(m22)
summary(m3)
summary(m33)


############
###### Regression Results ########
## Table 1.

stargazer(
  list(model.a, model.b, m2,m3),
  out = "table_lab.tex",
  title = "Negative binomial regression: Explaining the allocation of speaking time",
  omit        = "factor",
  intercept.bottom = TRUE
)

############ 
## Robustness Tests ## 

mrobustse <-zelig(epwebsitespeechcount~eu_position+I(eu_position^2)+ pernpepg+ perepgnp 
                  +  tenure+absent+rapporteurshipcount+ partyleader +epleader+ numcommem 
                  + numcomleader+groupsize+ natpartyperc +candidateselection+  pernpepg*candidateselection
                  +factor(memberstate)+factor(epg),
                  model="negbin",data= lastdata1 ,robust=TRUE)


### Main model with alternative independent variable party manifest

m33 <-
  glm.nb(
    epwebsitespeechcount~manifesteu+I(manifesteu^2)+ pernpepg+ perepgnp 
    +  tenure+absent+rapporteurshipcount+ partyleader +epleader+ numcommem 
    + numcomleader+groupsize+ natpartyperc +candidateselection+  pernpepg*candidateselection
    +factor(memberstate)+factor(epg),
    data = lastdata, 
    Hessian=TRUE,
    control = glm.control(maxit = 100)
  )

summary(mrobustse)
summary(m33)

#####
## Cross Validation 


set.seed(17)
m <- 10
beta_res <- matrix(NA, nrow = m, ncol = 33)
se_res <- matrix(NA, nrow = m, ncol = 33)

for (i in 1:m) {
  
  
  sel <- sample(1:nrow(lastdata1), floor(2 / 3 * nrow(lastdata1)))
  
  
  m3rob <-
    glm.nb(
      epwebsitespeechcount~eu_position+I(eu_position^2)+ pernpepg 
      + perepgnp+  tenure+absent+rapporteurshipcount+ partyleader +epleader+ numcommem
      + numcomleader+groupsize+ natpartyperc +candidateselection+  pernpepg*candidateselection
      +factor(memberstate)+factor(epg),
      data = lastdata1[sel,], 
      Hessian=TRUE,
      control = glm.control(maxit = 100)
    )
  
  
  
  beta_res[i,] <- c(m3rob$coefficients[1:27],m3rob$coefficients[29:33],m3rob$coefficients[35])
  se_res[i, ] <- sqrt(diag(vcov(m3rob)))
}


q <- apply(beta_res, 2, mean, na.rm = T)
seq <- apply(se_res ^ 2, 2, mean, na.rm = T)
sq <-
  apply(sweep(beta_res, 2, 
              apply(beta_res, 2, mean, 
                    na.rm = T)) ^ 2 / (m - 1), 2, 
        sum, na.rm = T) * (1 + 1 / m)

q_se <- sqrt(seq + sq)

m3coef <- c(m3$coefficients[1:27],m3$coefficients[29:33],m3$coefficients[35])
m3se <- sqrt(diag(vcov(m3)))

results <- cbind(m3coef,m3se,q, q_se)

########################

#########################################
## Simulation 1

## Rebel defection  Range
## Candt select 0-1


nsim <- 1000

gamma_hat1 <- c(m3$coefficients[1:27],m3$coefficients[29:33],m3$coefficients[35])

V_hat1 <- vcov(m3)

S1 <- mvrnorm(nsim, gamma_hat1, V_hat1)

theta1 <- m3$theta

rebelDef <- seq(min(lastdata1$pernpepg),0.1, 0.001)

quantile(lastdata1$perepgnp)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

scenarioCS <- cbind(1,mean(lastdata1$eu_position),
                    I(mean(lastdata1$eu_position)^2),
                    rebelDef,
                    mean(lastdata1$perepgnp), 
                    mean(lastdata1$tenure),
                    mean(lastdata1$absent),
                    mean(lastdata1$rapporteurshipcount),
                    getmode(lastdata1$partyleader),
                    getmode(lastdata1$epleader),
                    getmode(lastdata1$numcommem),
                    getmode(lastdata1$numcomleader),
                    mean(lastdata1$groupsize),
                    mean(lastdata1$natpartyper),
                    1,
                    1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                    rebelDef * 1
)

scenarioNotCS <- cbind(1,mean(lastdata1$eu_position),
                       I(mean(lastdata1$eu_position)^2),
                       rebelDef,
                       mean(lastdata1$perepgnp), 
                       mean(lastdata1$tenure),
                       mean(lastdata1$absent),
                       mean(lastdata1$rapporteurshipcount),
                       getmode(lastdata1$partyleader),
                       getmode(lastdata1$epleader),
                       getmode(lastdata1$numcommem),
                       getmode(lastdata1$numcomleader),
                       mean(lastdata1$groupsize),
                       mean(lastdata1$natpartyper),
                       0,
                       1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                       rebelDef * 0
)

###


XbetaCS <- S1 %*% t(scenarioCS)
XbetaNotCS <- S1 %*% t(scenarioNotCS)

lambdaCS <- exp(XbetaCS)
lambdaNotCS <- exp(XbetaNotCS)


#Expected Values
exp_scenarioCS <-
  apply(lambdaCS,c(1,2), function(x)
    mean(rnbinom(1000, size = theta1, mu = x)))

exp_scenarioNotCS <-
  apply(lambdaNotCS,c(1,2), function(x)
    mean(rnbinom(1000, size = theta1, mu = x)))

## 

Mean_CS <- apply(exp_scenarioCS, 2, mean)
ci_CS <- t(apply(exp_scenarioCS, 2, quantile, probs = c(0.025, 0.975)))

Mean_NotCS <- apply(exp_scenarioNotCS, 2, mean)
ci_NotCS <- t(apply(exp_scenarioNotCS, 2, quantile, probs = c(0.025, 0.975)))




###########################3


FDCS <- exp_scenarioCS - exp_scenarioNotCS

Mean_FDCS <- apply(FDCS, 2, mean)
ci_FDCS <- t(apply(FDCS, 2, quantile, probs = c(0.025, 0.975)))

## Figure 3
pdf(file = "FDrebelDef.pdf")
plot(rebelDef, Mean_FDCS , type="n",
     ylim = c(-10,30),
     ylab = "Speech Count",
     xlab = "Rebel Defection Point",
     main = "Diffrence of Candidate Selection in a Range of Rebel Defection",
     bty = "n",
     las = 1)
## CS
polygon(c(rev(rebelDef), rebelDef), c(rev(ci_FDCS[,2]),ci_FDCS[,1]),
        col = "gray80",
        border = NA)
lines(rebelDef, Mean_FDCS , lwd = 2,)
lines(rebelDef, ci_FDCS[,1], lty = "dashed", col = "gray20")
lines(rebelDef, ci_FDCS[,2], lty = "dashed", col = "gray20")

axis(1, at = lastdata1$pernpepg,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
abline(h= 0, col="black", lty= 2)

dev.off()


#############
## Simulation 2

## EU Point Range
## Red lines on quantiles

nsim <- 1000

gamma_hat2 <- c(m3$coefficients[1:27],m3$coefficients[29:33],m3$coefficients[35])

V_hat2 <- vcov(m3)

S2 <- mvrnorm(nsim, gamma_hat2, V_hat2)

theta2 <- m3$theta

eupos <- seq(1,7, length.out = 100)


scenarioEUP <- cbind(1,eupos,
                     I(eupos^2), 
                     mean(lastdata1$pernpepg),
                     mean(lastdata1$perepgnp),
                     mean(lastdata1$tenure),
                     mean(lastdata1$absent),
                     mean(lastdata1$rapporteurshipcount),
                     getmode(lastdata1$partyleader),
                     getmode(lastdata1$epleader),
                     getmode(lastdata1$numcommem),
                     getmode(lastdata1$numcomleader),
                     mean(lastdata1$groupsize),
                     mean(lastdata1$natpartyper),
                     getmode(lastdata1$candidateselection),
                     1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                     mean(lastdata1$pernpepg) * getmode(lastdata1$candidateselection)
)

scenarioEUP1 <- cbind(1,2.5,
                      I(2.5^2), 
                      mean(lastdata1$pernpepg),
                      mean(lastdata1$perepgnp),
                      mean(lastdata1$tenure),
                      mean(lastdata1$absent),
                      mean(lastdata1$rapporteurshipcount),
                      getmode(lastdata1$partyleader),
                      getmode(lastdata1$epleader),
                      getmode(lastdata1$numcommem),
                      getmode(lastdata1$numcomleader),
                      mean(lastdata1$groupsize),
                      mean(lastdata1$natpartyper),
                      getmode(lastdata1$candidateselection),
                      1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                      mean(lastdata1$pernpepg) * getmode(lastdata1$candidateselection)
)

scenarioEUP2 <- cbind(1,5.5,
                      I(5.5^2), 
                      mean(lastdata1$pernpepg),
                      mean(lastdata1$perepgnp),
                      mean(lastdata1$tenure),
                      mean(lastdata1$absent),
                      mean(lastdata1$rapporteurshipcount),
                      getmode(lastdata1$partyleader),
                      getmode(lastdata1$epleader),
                      getmode(lastdata1$numcommem),
                      getmode(lastdata1$numcomleader),
                      mean(lastdata1$groupsize),
                      mean(lastdata1$natpartyper),
                      getmode(lastdata1$candidateselection),
                      1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                      mean(lastdata1$pernpepg) * getmode(lastdata1$candidateselection)
)


###


XbetaEUP <- S2 %*% t(scenarioEUP)
XbetaEUP1 <- S2 %*% t(scenarioEUP1)
XbetaEUP2 <- S2 %*% t(scenarioEUP2)

lambdaEUP <- exp(XbetaEUP)
lambdaEUP1 <- exp(XbetaEUP1)
lambdaEUP2 <- exp(XbetaEUP2)


#Expected Values
exp_scenarioEUP <-
  apply(lambdaEUP,c(1,2), function(x)
    mean(rnbinom(1000, size = theta2, mu = x)))

exp_scenarioEUP1 <-
  apply(lambdaEUP1,c(1,2), function(x)
    mean(rnbinom(1000, size = theta2, mu = x)))

exp_scenarioEUP2 <-
  apply(lambdaEUP2,c(1,2), function(x)
    mean(rnbinom(1000, size = theta2, mu = x)))

## 

Mean_EUP <- apply(exp_scenarioEUP, 2, mean)
ci_EUP <- t(apply(exp_scenarioEUP, 2, quantile, probs = c(0.025, 0.975)))

## Figure 2
pdf(file = "EUPosition.pdf")
plot(eupos, Mean_EUP , type="n",
     ylim = c(0,160),
     ylab = "Speech Count",
     xlab = "EU position",
     main = "Explaining the allocation of speaking time with EU Position",
     bty = "n",
     las = 1)
## CS
polygon(c(rev(eupos), eupos), c(rev(ci_EUP[,2]),ci_EUP[,1]),
        col = "gray80",
        border = NA)
lines(eupos, Mean_EUP , lwd = 2)
lines(eupos, ci_EUP[,1], lty = "dashed", col = "gray20")
lines(eupos, ci_EUP[,2], lty = "dashed", col = "gray20")


axis(1, at = lastdata1$eu_position,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
abline(v= 2.5, col="red", lty= 2)
abline(v= 5.5, col="red", lty= 2)

dev.off()




## First Difference of EU position quantiles

FD <- exp_scenarioEUP1 - exp_scenarioEUP2

Mean_FD <- apply(FD, 2, mean)
ci_FD <- t(apply(FD, 2, quantile, probs = c(0.025, 0.975)))




