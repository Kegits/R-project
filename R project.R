(rm(list=ls()))
library(CASdatasets)
library(MASS)
library(fitdistrplus)
library(fBasics)
library(ggplot2)
library(actuar)
#######################COMMON FREQUENCY DISTRIBUTIONS##############################
data("swautoins")
data("freaggnumber")
data("ausautoBI8999")
###################### CLAIM FREQUENCY ##########################
freq1<-swautoins[,6]
freq2<-freaggnumber[,5]
freq3<-ausautoBI8999[,13]
basicStats(freq1)
basicStats(freq2)
basicStats(freq3)
qplot(freq1, main = "CLAIM FREQUENCY HISTOGRAM")
qplot(freq2, main = "CLAIM FREQUENCY HISTOGRAM")
qplot(freq3, main = "CLAIM FREQUENCY HISTOGRAM")
plotdist(freq1)
plotdist(freq2)
plotdist(freq3)
fitpoisson1 <- fitdist(freq1, "pois" ,method = c("mle"))
fitpoisson1
lambda1<-fitpoisson1$estimate
ks.test(freq1, "ppois", lambda1)
summary(fitpoisson1)
cdfcomp(fitpoisson1, xlab = "frequency", main = "Empirical and theoretical CDFs of SWAUTOINS")
plot(fitpoisson1)
fitpoisson2 <- fitdist(freq2, "pois" ,method = c("mle"))
fitpoisson2
lambda2<-fitpoisson2$estimate
ks.test(freq2, "ppois", lambda2)
summary(fitpoisson2)
cdfcomp(fitpoisson2, xlab = "frequency", main = "Empirical and theoretical CDFs of FREAGGNUMBER")
plot(fitpoisson2)
fitpoisson3 <- fitdist(freq3, "pois" ,method = c("mle"))
fitpoisson3
lambda3<-fitpoisson3$estimate
ks.test(freq3, "ppois", lambda3)
summary(fitpoisson3)
cdfcomp(fitpoisson3, xlab = "frequency", main = "Empirical and theoretical CDFs of AUSAUTOBI8999")
plot(fitpoisson3)
fitnb1 <- fitdist(freq1, "nbinom" ,method = c("mle"))
fitnb1
ks.test(freq1, "pnbinom", size = 0.2546263, mu = 51.8465104)
summary(fitnb1)
cdfcomp(fitnb1, xlab = "frequency", main = "Empirical and theoretical CDFs of SWAUTOINS")
qqcomp(fitnb1, main = "Q-Q PLOT FOR THE SWAUTOINS DATA SET")
plot(fitnb1)
fitnb2 <- fitdist(freq2, "nbinom" ,method = c("mle"))
fitnb2
ks.test(freq2, "pnbinom", size = 0.6815353, mu = 9.3037410)
summary(fitnb2)
cdfcomp(fitnb2, xlab = "frequency", main = "Empirical and theoretical CDFs of FREAGGNUMBER")

qqcomp(fitnb1, main = "Q-Q PLOT FOR THE FREAGGNUMBER DATA SET")
plot(fitnb2)
fitnb3 <- fitdist(freq3, "nbinom" ,method = c("mle"))
fitnb3
ks.test(freq3, "pnbinom", size = 8.185401e+05, mu = 2.134484e+00)
summary(fitnb3)
cdfcomp(fitnb3, xlab = "frequency", main = "Empirical and theoretical CDFs of AUSAUTOBI8999")
qqcomp(fitnb1, main = "Q-Q PLOT FOR THE AUSAUTOBI8999 DATA SET")
plot(fitnb3)
fitgeom1 <- fitdist(freq1, "geom" ,method = c("mle"))
fitgeom1
ks.test(freq1, "pgeom", prob = 0.01891585)
summary(fitgeom1)
cdfcomp(fitgeom1, xlab = "frequency", main = "Empirical and theoretical CDFs of SWAUTOINS")
plot(fitgeom1)
fitgeom2 <- fitdist(freq2, "geom" ,method = c("mle"))
fitgeom2
ks.test(freq2, "pgeom", prob = 0.01891585)
summary(fitgeom2)
cdfcomp(fitgeom1, xlab = "frequency", main = "Empirical and theoretical CDFs of FREAGGNUMBER")
plot(fitgeom2)
fitgeom3 <- fitdist(freq3, "geom" ,method = c("mle"))
fitgeom3
ks.test(freq3, "pgeom", prob = 0.3190618)
summary(fitgeom3)
cdfcomp(fitgeom3, xlab = "frequency", main = "Empirical and theoretical CDFs of AUSAUTOBI8999")
plot(fitgeom3)
###################### CLAIM SEVERITY ##########################
sev1<-swautoins[,7]
sev2<-freaggnumber[,4]
sev3<-ausautoBI8999[,15]
qplot(sev1, main = "CLAIM AMOUNT HISTOGRAM")
qplot(sev2, main = "CLAIM AMOUNT HISTOGRAM")
qplot(sev3, main = "CLAIM AMOUNT HISTOGRAM")
plotdist(sev1)
plotdist(sev2)
plotdist(sev3)
fitexp1 <- fitdist(sev1, "exp" ,method = c("mme"))
fitexp1
ks.test(sev1, "pexp", rate = 3.890935e-06)
summary(fitexp1)
cdfcomp(fitexp1, xlab = "severity", main = "Empirical and theoretical CDFs of SWAUTOINS")
plot(fitexp1)
fitexp2 <- fitdist(sev2, "exp" ,method = c("mme"))
fitexp2
ks.test(sev2, "pexp", rate = 0.01048419)
cdfcomp(fitexp2, xlab = "severity", main = "Empirical and theoretical CDFs of FREAGGNUMBER")
summary(fitexp2)
fitexp3 <- fitdist(sev3, "exp" ,method = c("mme"))
fitexp3
ks.test(sev3, "pexp", rate = 2.606392e-05)
cdfcomp(fitexp3, xlab = "severity", main = "Empirical and theoretical CDFs of AUSAUTOBI8999")
plot(fitexp3)
fitgamma1 <- fitdist(sev1, "gamma" ,method = c("mme"))
fitgamma1
ks.test(sev1, "pgamma", shape = 6.385692e-02, rate = 2.484631e-07)
cdfcomp(fitgamma1, xlab = "severity", main = "Empirical and theoretical CDFs of SWAUTOINS")
summary(fitgamma1)
plot(fitgamma1)
fitgamma2 <- fitdist(sev2, "gamma" ,method = c("mme"))
fitgamma2
ks.test(sev2, "pgamma", shape = 0.253142349, rate = 0.002653993)

cdfcomp(fitgamma2, xlab = "severity", main = "Empirical and theoretical CDFs of FREAGGNUMBER")
summary(fitgamma2)
plot(fitgamma2)
fitgamma3 <- fitdist(sev3, "gamma" ,method = c("mme"))
fitgamma3
ks.test(sev3, "pgamma", shape = 1.778434e-01, rate = 4.635297e-06)
cdfcomp(fitgamma3, xlab = "severity", main = "Empirical and theoretical CDFs of AUSAUTOBI8999")
severity1<-swautoins[,7]
severity2<-freaggnumber[,4]
severity3<-ausautoBI8999[,15]
seve1 <- severity1[severity1>0]
seve2 <- severity2[severity2>0]
seve3 <- severity3[severity3>0]
fitweibull1 <- fitdist(seve1, "weibull" ,method = c("mle"))
fitweibull1
ks.test(seve1, "pweibull", shape = 4.984071e-01, scale = 1.225059e+05)
summary(fitweibull1)
cdfcomp(fitweibull1, xlab = "severity", main = "Empirical and theoretical CDFs of SWAUTOINS")
plot(fitweibull1)
fitweibull2 <- fitdist(seve2, "weibull" ,method = c("mle"))
fitweibull2
ks.test(seve2, "pweibull", shape = 0.6797365, scale = 68.4266618)
summary(fitweibull2)
cdfcomp(fitweibull2, xlab = "severity", main = "Empirical and theoretical CDFs of FREAGGNUMBER")
plot(fitweibull2)
fitweibull3 <- fitdist(seve3, "weibull" ,method = c("mle"))
fitweibull3
ks.test(seve3, "pweibull", shape = 0.70892, scale = 28763.05944)
summary(fitweibull3)
cdfcomp(fitweibull3, xlab = "severity", main = "Empirical and theoretical CDFs of AUSAUTOBI8999")
plot(fitweibull3)
fitpareto1 <- fitdist(sev1, "pareto" ,method = c("mle"))
fitpareto1
ks.test(sev1, "ppareto", shape = 0.3095329, scale = 935.6428520)
summary(fitpareto1)
cdfcomp(fitpareto1, xlab = "severity", main = "Empirical and theoretical CDFs of SWAUTOINS")
qqcomp(fitpareto1, main = "Q-Q PLOT FOR THE SWAUTOINS DATA SET")
plot(fitpareto1)
fitpareto2 <- fitdist(sev2, "pareto" ,method = c("mle"))
fitpareto2
ks.test(sev2, "ppareto", shape = 1.478428, scale = 55.610505)
summary(fitpareto2)
cdfcomp(fitpareto2, xlab = "severity", main = "Empirical and theoretical CDFs of FREAGGNUMBER")
qqcomp(fitpareto2, main = "Q-Q PLOT FOR THE FREAGGNUMBER DATA SET")
plot(fitpareto2)
fitpareto3 <- fitdist(sev3, "pareto" ,method = c("mle"))
fitpareto3
ks.test(sev3, "ppareto", shape = 1.809774, scale = 32132.220058)
summary(fitpareto3)
qqcomp(fitpareto3, main = "Q-Q PLOT FOR THE AUSAUTOBI8999 DATA SET")
cdfcomp(fitpareto3, xlab = "severity", main = "Empirical and theoretical CDFs of AUSAUTOBI8999")
plot(fitpareto3)
########## MIXTURE DISTRIBUTIONS#######################################
data("swautoins")
data("ausautoBI8999")
data("freaggnumber")
################Discrete Mixture Distributions ###################################
freq1<-swautoins[,6]
freq2<-freaggnumber[,5]
freq3<-ausautoBI8999[,13]
#####Poisson Gamma ############################
fitpoissongamma1 <- fitdist(freq1, "nbinom" ,method = c("mle"))

fitpoissongamma1
ks.test(freq1, "pnbinom", size = 0.2546263, mu = 51.8465104)
summary(fitpoissongamma1)
cdfcomp(fitpoissongamma1, xlab = "frequency", main = "Empirical and theoretical CDFs of SWAUTOINS")
qqcomp(fitpoissongamma1, main = "Q-Q PLOT FOR THE SWAUTOINS DATA SET")
plot(fitpoissongamma1)
fitpoissongamma2 <- fitdist(freq2, "nbinom" ,method = c("mle"))
fitpoissongamma2
ks.test(freq2, "pnbinom", size = 0.6815353, mu = 9.3037410)
summary(fitpoissongamma2)
cdfcomp(fitpoissongamma2, xlab = "frequency", main = "Empirical and theoretical CDFs of FREAGGNUMBER")
qqcomp(fitpoissongamma2, main = "Q-Q PLOT FOR THE FREAGGNUMBER DATA SET")
plot(fitpoissongamma2)
fitpoissongamma3 <- fitdist(freq3, "nbinom" ,method = c("mle"))
fitpoissongamma3
ks.test(freq3, "pnbinom", size = 8.185401e+05, mu = 2.134484e+00)
summary(fitpoissongamma3)
cdfcomp(fitpoissongamma3, xlab = "frequency", main = "Empirical and theoretical CDFs of AUSAUTOBI8999")
qqcomp(fitpoissongamma3, main = "Q-Q PLOT FOR THE AUSAUTOBI8999 DATA SET")
plot(fitpoissongamma3)
n1<-length(freq1)
n2<-length(freq2)
n3<-length(freq3)
####poisson erlang####special case of gamma with parameter integer###
####swaitoins
fitpoisson <- fitdist(freq1, "pois" ,method = c("mle"))
fitpoisson
x1 <- rpois(n1,51.86572 )
fitgamma1 <- fitdist(x1, "gamma" ,method = c("mme"))
fitgamma1
ks.test(freq1, "pgamma", shape = 52.052770, rate = 1.002915)
summary(fitgamma1)
qqcomp(fitgamma1, main = "Q-Q PLOT FOR THE SWAUTOINS DATA SET")
plot(fitgamma1)
cdfcomp(fitgamma1, xlab = "frequency", main = "Empirical and theoretical CDFs of SWAUTOINS")
####freaggnumber#####
fitpoisson2 <- fitdist(freq2, "pois" ,method = c("mle"))
fitpoisson2
x2 <- rpois(n2,9.3075)
fitgamma2 <- fitdist(x2, "gamma" ,method = c("mme"))
fitgamma2
ks.test(freq2, "pgamma", shape = 9.304759, rate = 1.002044)
summary(fitgamma2)
qqcomp(fitgamma2, main = "Q-Q PLOT FOR THE FREAGGNUMBER DATA SET")
plot(fitgamma2)
cdfcomp(fitgamma2, xlab="frequency",main = "Empirical and theoretical CDFs of Freaggnumber")
####ausautoBI8999#####
fitpoisson3 <- fitdist(freq3, "pois" ,method = c("mle"))
fitpoisson3
x3 <- rpois(n3,2.13419)
fitgamma3 <- fitdist(x3, "gamma" ,method = c("mme"))
fitgamma3
ks.test(freq3, "pgamma", shape = 2.199746, rate = 1.017605)
summary(fitgamma3)
qqcomp(fitgamma3, main = "Q-Q PLOT FOR THE AUSAUTOBI8999 DATA SET")
plot(fitgamma3)
cdfcomp(fitgamma3, xlab="frequency",main = "Empirical and theoretical CDFs of AusaotoBI8999")
######poisson exponential#######
####swaitoins#####
fitpoisson <- fitdist(freq1, "pois" ,method = c("mle"))
fitpoisson
x1 <- rpois(n1,51.86572 )

fitexp1 <- fitdist(x1, "exp" ,method = c("mme"))
fitexp1
ks.test(freq2, "pexp", rate = 0.01931196)
summary(fitexp1)
qqcomp(fitexp1, main = "Q-Q PLOT FOR THE SWAUTOINS DATA SET")
plot(fitexp1)
cdfcomp(fitexp1, xlab = "frequency", main = "Empirical and theoretical CDFs of SWAUTOINS")
####freaggnumber#####
fitpoisson2 <- fitdist(freq2, "pois" ,method = c("mle"))
fitpoisson2
x2 <- rpois(n2,9.30752)
fitexp2 <- fitdist(x2, "exp" ,method = c("mme"))
fitexp2
ks.test(freq2, "pexp", rate = 0.1079098)
summary(fitexp2)
qqcomp(fitexp2, main = "Q-Q PLOT FOR THE FREAGGNUMBER DATA SET")
plot(fitexp2)
cdfcomp(fitexp2, xlab="frequency",main = "Empirical and theoretical CDFs of Freaggnumber")
####ausautoBI8999#####
fitpoisson3 <- fitdist(freq3, "pois" ,method = c("mle"))
fitpoisson3
x3 <- rpois(n3,2.13419)
fitexp3 <- fitdist(x3, "exp" ,method = c("mme"))
fitexp3
ks.test(freq3, "pexp", rate = 0.4674884)
summary(fitexp3)
qqcomp(fitexp3, main = "Q-Q PLOT FOR THE AUSAUTOBI8999 DATA SET")
plot(fitexp3)
cdfcomp(fitexp3, xlab="frequency",main = "Empirical and theoretical CDFs of AusaotoBI8999")
################Continuous Mixture Distributions ###################################
sev1<-swautoins[,7]
sev2<-freaggnumber[,4];sev3<-ausautoBI8999[,15]
########Exponential Gamma #########
fitexpgamma1 <- fitdist(sev1, "pareto" ,method = c("mle"))fitexpgamma1
ks.test(sev1, "ppareto", shape = 0.3095329, scale = 935.6428520)
summary(fitexpgamma1)
cdfcomp(fitexpgamma1, xlab = "severity", main = "Empirical and theoretical CDFs of SWAUTOINS")
qqcomp(fitexpgamma1, main = "Q-Q PLOT FOR THE SWAUTOINS DATA SET")
plot(fitexpgamma1)
fitexpgamma2 <- fitdist(sev2, "pareto" ,method = c("mle"))
fitexpgamma2
ks.test(sev2, "ppareto", shape = 1.478428, scale = 55.610505)
summary(fitexpgamma2)
cdfcomp(fitexpgamma2, xlab = "severity", main = "Empirical and theoretical CDFs of FREAGGNUMBER")
qqcomp(fitexpgamma2, main = "Q-Q PLOT FOR THE FREAGGNUMBER DATA SET")
plot(fitexpgamma2)
fitexpgamma3 <- fitdist(sev3, "pareto" ,method = c("mle"))
fitexpgamma3
ks.test(sev3, "ppareto", shape = 1.809774, scale = 32132.220058)
summary(fitexpgamma3)
qqcomp(fitexpgamma3, main = "Q-Q PLOT FOR THE AUSAUTOBI8999 DATA SET")
cdfcomp(fitexpgamma3, xlab = "severity", main = "Empirical and theoretical CDFs of AUSAUTOBI8999")
plot(fitexpgamma3)
########Inverse Exponential Gamma #########
severity1<-swautoins[,7]
sev1 <- severity1[severity1>0]
fitinvexpgamma1 <- fitdist(sev1, "invpareto", start = list(shape = 1, scale = 500), method = c("mle"))
fitinvexpgamma1$estimate
ks.test(sev1, "pinvpareto", shape = 8.191853e-01, scale = 5.868999e+04)
summary(fitinvexpgamma1)
cdfcomp(fitinvexpgamma1, xlab = "severity", main = "EMPIRICAL AND THEORETICAL CDF FOR SWAUTOINS")
plot(fitinvexpgamma1)

severity2<-freaggnumber[,4]
sev2 <- severity2[severity2>0]
fitinvexpgamma2 <- fitdist(sev2, "invpareto", start = list(shape = 1, scale = 500), method = c("mle"))
fitinvexpgamma2$estimate
ks.test(sev2, "pinvpareto", shape = 1.58294, scale = 17.08338)
summary(fitinvexpgamma2)
cdfcomp(fitinvexpgamma2, xlab = "severity", main = "EMPIRICAL AND THEORETICAL CDF FOR FREAGGNUMBER")
plot(fitinvexpgamma2)
severity3<-ausautoBI8999[,15]
sev3 <- severity3[severity3>0]
fitinvexpgamma3 <- fitdist(sev3, "invpareto", start = list(shape = 1, scale = 500), method = c("mle"))
fitinvexpgamma3$estimate
ks.test(sev3, "pinvpareto", shape = 1.441671, scale = 8848.936723)
summary(fitinvexpgamma3)
cdfcomp(fitinvexpgamma3, xlab = "severity", main = "EMPIRICAL AND THEORETICAL CDF FOR AUSAUTOBI8999")
plot(fitinvexpgamma3)
#########Simulation ##################
#######Poisson erlang ###########
set.seed(2500)
n=2500
p=51.86
r=1/p
pois=rpois(n,p)
sfitgamma <- fitdist(pois, "gamma" ,method = c("mme"))
sfitgamma1
gofstat(fitgamma1)
###simulation of data using gamma inverse exponential/inverse pareto
ps=rexp(2500,r)
sfitinvexpgamma<-fitdist(ps,"invpareto",start = list(shape=1,scale=500),method=c("mle"))
sfitinvexpgamma
gofstat(sfitinvexpgamma)
