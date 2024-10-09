library(gnm)

# DEFINE SPLINES OF DAY OF THE YEAR
knitr::opts_chunk$set(echo = TRUE, message = FALSE)
source(knitr::purl("04_crossbasis_definition.rmd", quiet=TRUE))

# DEFINE THE STRATA
df$stratum = factor(df$S1)

modfull <- gnm(Y ~ basis_bc + basis_vaccine + factor(df$T1)  + offset(log(E)), 
               eliminate=stratum, data=df, family=quasipoisson)

coef = coef(modfull)
vcov = vcov(modfull)



indt <- grep("basis_bc", names(coef(modfull)))
predt_bc <- crosspred(basis_bc, coef = coef[indt], vcov=vcov[indt,indt],
                      model.link = "log", bylag = 0.5, cen=round(quantile(data_prep$pollution_bc_wk,0.05), 2))

pdf("figs/bc_scenario_time_series.pdf", width = 6, height = 6)
# get exposures values
vars <- predt_bc$predvar

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- predt_bc$matRRfit
rr.lci <- predt_bc$matRRlow
rr.uci <- predt_bc$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
r2 <- max(range(rr, rr.lci, rr.uci))

# get selected exposure variable positions
md <- which(round(vars, 2) == 0.4)
mx <- which(round(vars, 2) == 1.7)


# define colours
col1 <- brewer.pal(9, "Purples")[4]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(9, "Purples")[9]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))


# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.5)

# median pollution
plot(lagbylag, rr[md,], col = col1, type = "l", lwd = 1, 
     xlab = "Lag, weeks", ylab = "RR", main = "", 
     ylim = c(0.8,1.8), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = 0:nlag)
axis(2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[md,], rev(rr.uci[md,]))
polygon(xx, yy, col = tcol1, border = tcol1)


# maximal pollution
lines(lagbylag, rr[mx,], col = col2, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
polygon(xx, yy, col = tcol2, border = tcol2)
abline(h = 1, lty = 3)

#expression(paste("BC", " " ,"µg"/"m"^3))
legend("topleft",
       legend = c(expression(paste("BC = ","0.4"," ", "µg"/"m"^3)),
                  expression(paste("BC = ","1.7"," ", "µg"/"m"^3))      ),
       col = c(col1, col2), cex = 0.8,
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.2, horiz = F)

dev.off()

pdf("figs/bc_time_series.pdf", width = 8, height = 6)
plot(predt_bc, ptype="overall",  xlab = expression(paste("BC", " " ,"µg"/"m"^3)), ylab="RR", col=6, ylim=c(0,10),
     xlim = c(quantile(data_prep$pollution_bc_wk,0.01),quantile(data_prep$pollution_bc_wk,0.99)))
abline(v=0.27, col="slateblue4", lty = 3, lwd = 2)#20%
abline(v=0.44, col="slateblue4", lty = 3, lwd = 2)#50%
abline(v=1.2, col="slateblue4", lty = 3, lwd = 2)#95%
text(0.27, 50,col="slateblue4", "20%", cex = 1)
text(0.44, 50,col="slateblue4", "50%", cex = 1)
text(1.2, 50,col="slateblue4", "95%", cex = 1)
dev.off()

BC_univariate = crosspred(basis_bc, coef = coef[indt], vcov=vcov[indt,indt],at = c(0.27,0.44,1.2),
                          model.link = "log", bylag = 0.5, cen=round(quantile(data_prep$pollution_bc_wk,0.05),2))
BC_univariate = data.frame(pred = c(0.27,0.44,1.2), allRR = BC_univariate$allRRfit, allRRlow = BC_univariate$allRRlow, allRRhigh = BC_univariate$allRRhigh)




# OTHER POLLUTANTS
modfull <- gnm(Y ~ basis_ozone + basis_vaccine + factor(df$T1)  + offset(log(E)), 
               eliminate=stratum, data=df, family=quasipoisson)

coef = coef(modfull)
vcov = vcov(modfull)

indt <- grep("basis_ozone", names(coef(modfull)))
predt <- crosspred(basis_ozone, coef = coef[indt], vcov=vcov[indt,indt],
                      model.link = "log", bylag = 0.5, cen=round(quantile(data_prep$pollution_ozone_wk,0.05), 2))

pdf("figs/ozone_time_series.pdf", width = 8, height = 6)
plot(predt,col="grey", ptype="overall", xlab=expression(paste("Ozone", " " ,"µg"/"m"^3)), ylab="RR", main="", ylim = c(0,3),xlim = c(quantile(data_prep$pollution_ozone_wk,0.01),quantile(data_prep$pollution_ozone_wk,0.99))) 
abline(v=31.7, col="grey4", lty = 3, lwd = 2)#20%
abline(v=45.8, col="grey4", lty = 3, lwd = 2)#50%
abline(v=69.9, col="grey4", lty = 3, lwd = 2)#95%
text(31.7, 50,col="grey4", "20%", cex = 1)
text(45.8, 50,col="grey4", "50%", cex = 1)
text(69.9, 50,col="grey4", "95%", cex = 1)


ozone_univariate = crosspred(basis_ozone, coef = coef[indt], vcov=vcov[indt,indt],at = c(31.7,45.8,69.9),
                             model.link = "log", bylag = 0.5, cen=round(quantile(data_prep$pollution_ozone_wk,0.05),2))
ozone_univariate = data.frame(pred =c(31.7,45.8,69.9), allRR = ozone_univariate$allRRfit, allRRlow = ozone_univariate$allRRlow, allRRhigh = ozone_univariate$allRRhigh)


#NO2
modfull <- gnm(Y ~ basis_no2+ basis_vaccine + factor(df$T1)  + offset(log(E)), 
               eliminate=stratum, data=df, family=quasipoisson)

coef = coef(modfull)
vcov = vcov(modfull)

indt <- grep("basis_no2", names(coef(modfull)))
predt <- crosspred(basis_no2, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.5, cen=round(quantile(data_prep$pollution_no2_wk,0.05), 2))

pdf("figs/no2_time_series.pdf", width = 8, height = 6)
plot(predt,col="grey", ptype="overall", xlab=expression(paste("NO2", " " ,"µg"/"m"^3)), ylab="RR", main="", ylim = c(0,4), xlim = c(quantile(data_prep$pollution_no2_wk,0.01),quantile(data_prep$pollution_no2_wk,0.99))) 
abline(v=6.5, col="grey4", lty = 3, lwd = 2)#20%
abline(v=10.6, col="grey4", lty = 3, lwd = 2)#50%
abline(v=22.5, col="grey4", lty = 3, lwd = 2)#95%
text(6.5, 50,col="grey4", "20%", cex = 1)
text(10.6, 50,col="grey4", "50%", cex = 1)
text(22.5, 50,col="grey4", "95%", cex = 1)

dev.off()

no2_univariate = crosspred(basis_no2, coef = coef[indt], vcov=vcov[indt,indt],at = c(6.5,10.6,22.5),
                           model.link = "log", bylag = 0.5, cen=round(quantile(data_prep$pollution_no2_wk,0.05), 2))
no2_univariate = data.frame(pred =c(6.5,10.6,22.5), allRR = no2_univariate$allRRfit, allRRlow = no2_univariate$allRRlow, allRRhigh = no2_univariate$allRRhigh)



#PM10
modfull <- gnm(Y ~ basis_pm10+ basis_vaccine + factor(df$T1)  + offset(log(E)), 
               eliminate=stratum, data=df, family=quasipoisson)

coef = coef(modfull)
vcov = vcov(modfull)

indt <- grep("basis_pm10", names(coef(modfull)))
predt <- crosspred(basis_pm10, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.5, cen=round(quantile(data_prep$pollution_pm10_wk,0.05), 2))

pdf("figs/pm10_time_series.pdf", width = 8, height = 6)
plot(predt,col="grey", ptype="overall", xlab=expression(paste("PM10", " " ,"µg"/"m"^3)), ylab="RR", main="", ylim = c(0,2), xlim = c(quantile(data_prep$pollution_pm10_wk,0.01),quantile(data_prep$pollution_pm10_wk,0.99)))  
abline(v=9.1, col="grey4", lty = 3, lwd = 2)#20%
abline(v=15.0, col="grey4", lty = 3, lwd = 2)#50%
abline(v=29.3, col="grey4", lty = 3, lwd = 2)#95%
text(9.1, 50,col="grey4", "20%", cex = 1)
text(15.0, 50,col="grey4", "50%", cex = 1)
text(29.3, 50,col="grey4", "95%", cex = 1)
dev.off()

pm10_univariate = crosspred(basis_pm10, coef = coef[indt], vcov=vcov[indt,indt],at = c(9.1,15.0,29.3),
                            model.link = "log", bylag = 0.5, cen=round(quantile(data_prep$pollution_pm10_wk,0.05), 2))
pm10_univariate = data.frame(pred = c(9.1,15.0,29.3), allRR = pm10_univariate$allRRfit, allRRlow = pm10_univariate$allRRlow, allRRhigh = pm10_univariate$allRRhigh)


#PM25
modfull <- gnm(Y ~ basis_pm25+ basis_vaccine + factor(df$T1)  + offset(log(E)), 
               eliminate=stratum, data=df, family=quasipoisson)

coef = coef(modfull)
vcov = vcov(modfull)

indt <- grep("basis_pm25", names(coef(modfull)))
predt <- crosspred(basis_pm25, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.5, cen=round(quantile(data_prep$pollution_pm25_wk,0.05), 2))

pdf("figs/pm25_time_series.pdf", width = 8, height = 6)
plot(predt,col="grey", ptype="overall", xlab=expression(paste("PM25", " " ,"µg"/"m"^3)), ylab="RR", main="", ylim = c(0,3), xlim = c(quantile(data_prep$pollution_pm25_wk,0.01),quantile(data_prep$pollution_pm25_wk,0.99))) 
abline(v=4.7, col="grey4", lty = 3, lwd = 2)#20%
abline(v=8.5, col="grey4", lty = 3, lwd = 2)#50%
abline(v=18.4, col="grey4", lty = 3, lwd = 2)#95%
text(4.7, 50,col="grey4", "20%", cex = 1)
text(8.5, 50,col="grey4", "50%", cex = 1)
text(18.4, 50,col="grey4", "95%", cex = 1)
dev.off()

pm25_univariate = crosspred(basis_pm25, coef = coef[indt], vcov=vcov[indt,indt],at = c(4.7,8.5,18.4),
                            model.link = "log", bylag = 0.5, cen=round(quantile(data_sort$pollution_pm25_wk,0.05), 2))
pm25_univariate = data.frame(pred = c(4.7,8.5,18.4), allRR = pm25_univariate$allRRfit, allRRlow = pm25_univariate$allRRlow, allRRhigh = pm25_univariate$allRRhigh)

