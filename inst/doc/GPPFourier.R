### R code from vignette source 'GPPFourier.Rnw'

###################################################
### code chunk number 1: GPPFourier.Rnw:20-21
###################################################
require(GPPFourier)


###################################################
### code chunk number 2: GPPFourierFig1Code
###################################################
par(mfrow=c(1,2))
DO <- Kruibeke[Kruibeke$time>="2010-06-03"&Kruibeke$time<="2010-06-13",]
plot(DO,ylab="O2",type="l")
plotF(detrend(DO$O2), 
		dt=as.numeric(difftime(DO$time[2], DO$time[1], units="days")), 
		xlim=c(0,3), 
		type="b")


###################################################
### code chunk number 3: GPPFourierFig1
###################################################
par(mfrow=c(1,2))
plot(DO,ylab="O2",type="l")
plotF(detrend(DO$O2), 
		dt=as.numeric(difftime(DO$time[2], DO$time[1], units="days")), 
		xlim=c(0,3), 
		type="b")


###################################################
### code chunk number 4: GPP1
###################################################
dt <- difftime(DO$time[2], DO$time[1], units="days")
GPP <- GPPFourier(DO$O2, 
	dt=dt, 
	Nfilt=1/as.numeric(dt, unit="days"), 
	fDL=fDLfun(DO$time[1], phi=51.176, lambda=4.326)
)
GPP # volume specific primary production, i.e. mmol O2/m3/d


###################################################
### code chunk number 5: GPPFourierFig2_code
###################################################
par(mfrow=c(2,1))
plot(Kruibeke, 
      pch=20,
      xlim=as.POSIXct(c("2010-01-01", "2010-12-31")))
GPPAll_4 <- WindowGPPFourier.gts(Kruibeke, 
                                  gapMaxN = 10, 
                                  Width = 10, 
                                  filtWidth=1*24, 
                                  phi=51.176,
                                  lambda=4.326, 
                                  Detrend=TRUE, 
                                  filter=TRUE, 
                                  filtcorrect=FALSE)
plot(GPPAll_4$time,
      GPPAll_4$GPP*9.3/1.3, 
      col="black", 
      pch=19, 
      type="b",
      xlab="time", 
      ylab="GPP", 
      xlim=as.POSIXct(c("2010-01-01", "2010-12-31")))



###################################################
### code chunk number 6: GPPFourierFig2
###################################################
par(mfrow=c(2,1), cex=2)
plot(Kruibeke, pch=20,xlim=as.POSIXct(c("2010-01-01", "2010-12-31")))
GPPAll_4 <- WindowGPPFourier.gts(Kruibeke, gapMaxN = 10, Width = 10, filtWidth=1*24, phi=51.176,lambda=4.326, Detrend=TRUE, filter=TRUE, filtcorrect=FALSE)
plot(GPPAll_4$time,GPPAll_4$GPP*9.3, 
                col="black", 
                pch=19, 
                type="b", 
                xlab="time", 
                ylab="GPP", 
                xlim=as.POSIXct(c("2010-01-01", "2010-12-31")))



###################################################
### code chunk number 7: GPPFourierFig4_code
###################################################

# Calculate GPP(t) by complex demodulation of simulated water column time series
 
dt <- as.numeric(difftime(watercolumn$time[2], watercolumn$time[1] , units="days"))
Nfilt <- 1/dt
GPPt <- GPPFourier_t(watercolumn[,c("time", "O2")], 
                     dt=dt, units="days", 
                    Detrend=TRUE, 
                    filter=TRUE, 
                    Nfilt=Nfilt,
                    NLowPass=Nfilt, 
                    fDL=NULL, 
                    circular=FALSE, 
                    sides=2, 
                    nf=2)

par(mfrow=c(2,1), cex=1.2)
plot(watercolumn[,c("time", "O2")], 
     type="l", 
     xlab="", 
     ylab=expression(paste(O[2], " [", mu, "M]")))
title(main="Water column")
plot(GPPt, type="l", lwd=3, ylim=c(0,30), xlab="", ylab="GPP")
lines(watercolumn[,c("time","GPP")], col="red")
legend("topleft", 
       lty=1, 
       col=c("red", "black"), 
       legend=c( "Simulated GPP", 
                 expression(paste("Complex demodulated ", O[2], " series"))), 
       bty="n")


###################################################
### code chunk number 8: GPPFourierFig4
###################################################

# Calculate GPP(t) by complex demodulation of simulated water column time series
 
dt <- as.numeric(difftime(watercolumn$time[2], watercolumn$time[1] , units="days"))
Nfilt <- 1/dt
GPPt <- GPPFourier_t(watercolumn[,c("time", "O2")], 
                     dt=dt, 
                     units="days", 
                     Detrend=TRUE, 
                     filter=TRUE, 
                     Nfilt=Nfilt, 
                     NLowPass=Nfilt, 
                     fDL=NULL, 
                     circular=FALSE, 
                     sides=2, 
                     nf=2)

par(mfrow=c(2,1), cex=1.2)
plot(watercolumn[,c("time", "O2")], 
     type="l", 
     xlab="", 
     ylab=expression(paste(O[2], " [", mu, "M]")))
title(main="Water column")
plot(GPPt, type="l", lwd=3, ylim=c(0,30), xlab="", ylab="GPP")
lines(watercolumn[,c("time","GPP")], col="red")
legend("topleft", 
       lty=1, 
       col=c("red", "black"), 
       legend=c( "Simulated GPP", expression(paste("Complex demodulated ", O[2], " series"))), 
       bty="n")


###################################################
### code chunk number 9: GPPFourierFig5_code
###################################################


# Calculate GPP(t) by complex demodulation of simulated estuary time series

dt <- as.numeric(difftime(estuary$time[2], estuary$time[1] , units="days"))
Nfilt <- 1/dt
GPPt <- GPPFourier_t(estuary[,c("time", "O2")], 
                     dt=dt, 
                     Detrend=FALSE, 
                     filter=TRUE, 
                     Nfilt=Nfilt, 
                     NLowPass=Nfilt, 
                     fDL=NULL, 
                     circular=FALSE, 
                     sides=2, 
                     nf=2)

par(mfrow=c(2,1), cex=1.2)
plot(estuary[,c("time", "O2")], 
     type="l", 
     xlab="", 
     ylab=expression(paste(O[2], " [", mu, "M]")))
title(main="Estuary")
plot(GPPt, 
     type="l", 
     lwd=3, 
     xlab="", 
     ylab="GPP", 
     ylim=c(0,30))
lines(estuary$time, 
      filter(filter(estuary$GPP, rep(1/Nfilt, Nfilt)),rep(1/Nfilt, Nfilt)), 
      type="l", 
      lwd=3, 
      ylim=c(0,30), 
      xlab="", 
      ylab="GPP", 
      col="red")
legend("topleft", 
       lty=1, 
       col=c("black", "red"), 
       legend=c(expression(paste("Demodulated ", O[2], " series")), "GPP"), 
       bty="n") 



###################################################
### code chunk number 10: GPPFourierFig5
###################################################


# Calculate GPP(t) by complex demodulation ofsimulated estuary time series

dt <- as.numeric(difftime(estuary$time[2], estuary$time[1] , units="days"))
Nfilt <- 1/dt
GPPt <- GPPFourier_t(estuary[,c("time", "O2")], 
                     dt=dt, 
                     Detrend=FALSE, 
                     filter=TRUE, 
                     Nfilt=Nfilt, 
                     NLowPass=Nfilt, 
                     fDL=NULL, 
                     circular=FALSE, 
                     sides=2, 
                     nf=2)

par(mfrow=c(2,1), cex=1.2)
plot(estuary[,c("time", "O2")], type="l", xlab="", ylab=expression(paste(O[2], " [", mu, "M]")))
title(main="Estuary")
plot(GPPt, type="l", lwd=3, xlab="", ylab="GPP", ylim=c(0,30))
lines(estuary$time, filter(filter(estuary$GPP, rep(1/Nfilt, Nfilt)),rep(1/Nfilt, Nfilt)), type="l", lwd=3, ylim=c(0,30), xlab="", ylab="GPP", col="red")
legend("topleft", lty=1, col=c("black", "red"), legend=c(expression(paste("Demodulated ", O[2], " series")), "GPP"), bty="n") 



###################################################
### code chunk number 11: GPPFourierFig3_code
###################################################

 
# Calculate GPP(t) by complex demodulation of Hoernum Tief O2 time series
dt <- as.numeric(diff(Hoernum$time)[1], units="days")
Nf <- 1/dt
phi <- 54.783
lambda <- 8.45
# Diurnal harmonic
T1 <- 1
f1 <- 1/T1
# Tidal harmonics
TO1 <- 25.81933871/24	# O1 period
fO1 <- 1/TO1
TQ1 <- 26.868350/24   # Large lunar elliptic diurnal
fQ1 <- 1/TQ1

GPPt <- GPPFourier_t(gapfill(Hoernum), 
                     Nf=Nf, 
                     nf=2, 
                     NLowPass=Nf, 
                     phi=phi, 
                     lambda=lambda)

GPPt15 <- GPPFourier_t(gapfill(Hoernum), 
                       Nf=Nf, 
                       nf=2, 
                       NLowPass=15/dt, 
                       phi=phi, 
                       lambda=lambda)


par(mfrow=c(2,1))
plot(GPPt, type="l", ylab="GPP(t)")
title(main="Hoernum Tief")
lines(GPPt15, lwd=3)
legend("topright", legend=c("1 Day filter", "15 Day filter"),lwd=c(1,3), bty="n") 
plotF((GPPt$GPPt-GPPt15$GPPt)[!is.na(GPPt15$GPPt)], dt=dt, xlim=c(0,0.5), type="b", pch=20)
abline(v=c(f1-fO1, f1-fQ1))
title(main="Spectrum of difference")



###################################################
### code chunk number 12: GPPFourierFig3
###################################################

 
# Calculate GPP(t) by complex demodulation of Hoernum Tief O2 time series
dt <- as.numeric(diff(Hoernum$time)[1], units="days")
Nf <- 1/dt
phi <- 54.783
lambda <- 8.45
# Diurnal harmonic
T1 <- 1
f1 <- 1/T1
# Tidal harmonics
TO1 <- 25.81933871/24	# O1 period
fO1 <- 1/TO1
TQ1 <- 26.868350/24   # Large lunar elliptic diurnal
fQ1 <- 1/TQ1

GPPt <- GPPFourier_t(gapfill(Hoernum), 
                     Nf=Nf, nf=2, 
                     NLowPass=Nf, 
                     phi=phi, 
                     lambda=lambda)
GPPt15 <- GPPFourier_t(gapfill(Hoernum), 
                       Nf=Nf, 
                       nf=2, 
                       NLowPass=15/dt, 
                       phi=phi, 
                       lambda=lambda)

par(mfrow=c(2,1))
plot(GPPt, type="l", ylab="GPP(t)")
title(main="Hoernum Tief")
lines(GPPt15, lwd=3)
legend("topright", legend=c("1 Day filter", "15 Day filter"),lwd=c(1,3), bty="n") 
plotF((GPPt$GPPt-GPPt15$GPPt)[!is.na(GPPt15$GPPt)], dt=dt, xlim=c(0,0.5), type="b", pch=20)
abline(v=c(f1-fO1, f1-fQ1))
title(main="Spectrum of difference")



