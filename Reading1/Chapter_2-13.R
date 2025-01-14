# This code is to accompany Maximum Likelihood Methods Strategies for Social Science,
# Michael D. Ward and John S. Ahlquist, 2018, Cambridge University Press,
# ISBN 9781107185821.  The code is presented as is, but is designed to replicate all of
# the material in the book, chapter by chapter.  Any comments, suggestions, or problems can
# be reported to the authors: michael.don.ward@gmail.com and John Ahlquist jahlquist@ucsd.edu.
# These programs were designed to run on a variety of platforms, but currently uses
# platform       x86_64-apple-darwin15.6.0   
# arch           x86_64                      
# os             darwin15.6.0                
# system         x86_64, darwin15.6.0        
# version.string R version 3.4.4 (2018-03-15)
# nickname       Someone to Lean On
# We used Rstudio, Version 1.1.447 
# This code requires installation of the loaded libraries.

rm(list=ls()); gc()
library(foreign); library(shape); library(nlme); library(MASS); library(WDI);library(ProfileLikelihood)
set.seed(123)
# Read the data and run the simple linear model  
#wdi<-na.omit(data.frame(read.csv("wdi2010.csv",header=T)))
#out<-lm(log(co2.kt) ~ log(gdp.pc.ppp), data=wdi)
wdi<-WDI(country = "all",
indicator = c("EN.POP.DNST", #popdensity
    "EN.ATM.CO2E.KT", #c02 emissions
     "NY.GDP.PCAP.PP.CD"), #GDPpcPPP
   start = 2012, end = 2012, extra = TRUE, cache = NULL)
wdi<-na.omit(subset(wdi, region !="Aggregates"))
names(wdi)[4:6]<-c("pop.den", "co2.kt","gdp.pc.ppp" )
attach(wdi)
# Profile likelihood
wdi$lgdppc<-log(wdi$gdp.pc.ppp)
xx <- profilelike.lm(formula = log(co2.kt)~1, data=wdi, profile.theta="lgdppc",
lo.theta=0.94, hi.theta=1.14, length=500)

xx$profile.lik<-(xx$profile.lik - min(xx$profile.lik))/( max(xx$profile.lik) - min(xx$profile.lik) )
summary(xx$profile.lik)

## Figure 2.1 #######################################
#pdf(file = "/Users/mdw/Git/HalfMoonBay/graphics/chapter1/ch1fig3.pdf",width = 6, height = 6, onefile = TRUE, family = "Times")
with(xx, 
  plot(theta,profile.lik,las=1,lty=1,lwd=4,
    type="l",pch=19,xlab="",axes=F,
    ylab="",yaxt="n",bty="l",main="Likelihood Diagnostics",
    xlim=c(0.85,1.15),bty="l")
  )
axis(2,at=c(0,.73,1.0),#at=c(.15,.24,.73,1.0),
	las=1, labels=c(NA,
	#expression(logL(hat(theta)[g])),
	expression(logL(theta[0])),
	expression(logL(hat(theta)))))
axis(1,at=c(.8,1,1.05899,1.2),las=1,labels=c(NA,expression(theta[0]),expression(hat(theta)),NA))
#abline(h=max(xx$profile.lik),col="slategray2")
segments(.85,1.0,1.05899,1.,lty="dashed",lwd=2)
#abline(h=summary(xx$profile.lik)[1])
#segments(.85,.15,.95,.15,lwd=2)
#segments(.85,.15,1.05899,.15,lwd=2,lty="dashed")
#set tic at 0.95
#segments(.85,.24,.9558,.24,lwd=2)
#segments(.85,.24,1.05899,.24,lwd=2,lty="dashed")
#set tic at 0.96
segments(1.058999,0,1.058999,1,lwd=2,lty="dashed")
# set tic at 1.06
segments(1.0,0,1.0,.73,lwd=2,lty="dashed")
segments(.85,.73,1.0,.73,lty="dashed",lwd=2)
segments(0.983,0.5935472,1.009141,0.829,lwd=2,col="blue")
arrows(1,0.15,1.05899,.15,code=3,length=.1,lwd=2)
#text(x = 0.85, y = .865, '}', srt = 0, cex = 8,family = 'Helvetica Neue UltraLight')
#text(x = 0.85, y = .195, '}', srt = 0, cex = 2, family = 'Helvetica Neue UltraLight')
text(x=.87,y=.88,'LR',cex=2)
#text(x=.87,y=.19,'W',cex=2)
text(x=1.03,y=.21,'W',cex=2)
arrows(0.85,0.73,0.85,1,code=3,length=.1,lwd=2)
text(x=.97,y=.88,'LM, slope of tangent',cex=1)
#text(1.1125, 0.42,'Likelihood Function',cex=1)
text(1.13, 0.39,'log Likelihood',cex=1)
arrows(0.9690819,0.8422534,0.9968465,0.7404011,col="blue",code=2,length=.1,lwd=2)
#dev.off()
###################################################### 