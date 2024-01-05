###valley analysis for chaos

setwd("~/Dropbox/R_projects/chaos")

#make a log file

coef_log <- data.frame(site=character(),
                       ks_norm=double(),
                       ks_lnorm=double(),
                       ks_par=double(),
                       aic_norm=double(),
                       aic_lnorm=double(),
                       aic_par=double(),
                       bic_norm=double(),
                       bic_lnorm=double(),
                       bic_par=double(),
                       ks_min=character(),
                       aic_min=character(),
                       bic_min=character(),
                       stringsAsFactors=FALSE) 

#make site list

f <- list.files(path="raw_data",
                recursive=T,
                pattern="\\_val.csv$"
                ,full.names=T)

shortnames <- list.files(path="raw_data",
                              recursive=T,
                              pattern="\\_val.csv$"
                              ,full.names=F)

shortnames <- gsub('.{4}$', '', shortnames)

figpath <- paste0("figs/", shortnames)

counter <- 0

#debugging 

#i <- f[14]
##here's where the loop will go! 

for(i in f)
{
  

data <- read.csv(paste(i))

#effective diamter calc
data$eff_diam <- 2*sqrt(data$area/3.14)

### Distributions

library(fitdistrplus)
library(actuar)

f1 <- fitdist(data$eff_diam,"norm")
f2 <- fitdist(data$eff_diam,"lnorm")
f3 <- fitdist(data$eff_diam,"pareto", start = list(shape = 1, scale = 500))

kst <- gofstat(list(f1,f2,f3),fitnames = c("Normal","Log-Normal", "Pareto"))

#make a log file for this round

coef_log_this_site <- data.frame(site=character(),
                       ks_norm=double(),
                       ks_lnorm=double(),
                       ks_par=double(),
                       aic_norm=double(),
                       aic_lnorm=double(),
                       aic_par=double(),
                       bic_norm=double(),
                       bic_lnorm=double(),
                       bic_par=double(),
                       ks_min=character(),
                       aic_min=character(),
                       bic_min=character(),
                       stringsAsFactors=FALSE) 

coef_log_this_site <- data.frame(site=i, ks_norm=kst$ks[1], ks_lnorm=kst$ks[2], ks_par=kst$ks[3], aic_norm=kst$aic[1], aic_lnorm=kst$aic[2], aic_par=kst$aic[3],bic_norm=kst$bic[1], bic_lnorm=kst$bic[2], bic_par=kst$bic[3], ks_min=ifelse(kst$ks[1]<kst$ks[2] & kst$ks[1]<kst$ks[3],"Norm",ifelse(kst$ks[2]<kst$ks[1] & kst$ks[2]<kst$ks[3], "LNorm","Par")),aic_min=ifelse(kst$aic[1]<kst$aic[2] & kst$aic[1]<kst$aic[3],"Norm",ifelse(kst$aic[2]<kst$aic[1] & kst$aic[2]<kst$aic[3], "LNorm","Par")), bic_min=ifelse(kst$bic[1]<kst$bic[2] & kst$bic[1]<kst$bic[3],"Norm",ifelse(kst$bic[2]<kst$bic[1] & kst$bic[2]<kst$bic[3], "LNorm","Par")))

coef_log <- rbind(coef_log,coef_log_this_site)

quartz()

par(mfrow = c(2, 2), mar = c(4, 4, 6, 1))
plot.legend <- c("normal", "lognormal", "pareto")
denscomp(list(f1, f2, f3), legendtext = plot.legend)
mtext(paste(paste(gsub('.{4}$', '', i))), 1, line = -16)
qqcomp(list(f1, f2, f3), legendtext = plot.legend)
cdfcomp(list(f1, f2, f3), legendtext = plot.legend, xlogscale = TRUE)
##this can be done on a non-log-scale,too
ppcomp(list(f1, f2, f3), legendtext = plot.legend)

counter <- counter + 1

quartz.save(file=paste(figpath[counter],"_distrib_stats.png",sep = ""), type = "png", device = dev.cur(), dpi = 600)


}

write.csv(coef_log, "figs/site_stats.csv")
