###compares volumes of chaos units


setwd("~/Dropbox/R_projects/chaos")

#make site list

voids <- list.files(path="raw_data",
                recursive=T,
                pattern="\\voidvol.csv$"
                ,full.names=T)

valleys <- list.files(path="raw_data",
                   recursive=T,
                   pattern="\\valleyvol.csv$"
                   ,full.names=T)

totals <- list.files(path="raw_data",
                     recursive=T,
                     pattern="\\_totalvol.csv$"
                     ,full.names=T)

blocks <- list.files(path="raw_data",
                    recursive=T,
                    pattern="\\_blockvol.csv$"
                    ,full.names=T)

lenses <- list.files(path="raw_data",
                     recursive=T,
                     pattern="\\_lensvol.csv$"
                     ,full.names=T)

areas <- list.files(path="raw_data",
                     recursive=T,
                     pattern="\\_area.csv$"
                     ,full.names=T)

shortnames <- list.files(path="raw_data",
                         recursive=T,
                         pattern="\\_blockvol.csv$"
                         ,full.names=F)

shortnames <- substr(shortnames,1,nchar(shortnames)-13)

figpath <- paste0("figs/", shortnames)

nicenames <- c("Aram Chaos", "Aromatum Chaos", "Atlantis Chaos", "Aureum Chaos", "Eos Chaos", "Gorgonium Chaos", "Hydraotes Chaos", "Iamuna Chaos", "Orson Welles Crater", "OW1", "OW2", "OW3", "Oxia Chaos", "Pyrrhae Chaos", "R171 Crater", "R89 Crater", "South Ister Chaos", "Xanthe Chaos")

#initialize log 

volume_log <- data.frame(site=character(),
                       voidvolume=double(),
                       blockvolume=double(),
                       valleyvolume=double(),
                       totalvolume=double(),
                       lensvolume=double(),
                       area=double(),
                       stringsAsFactors=FALSE) 

for(i in 1:length(shortnames))
{
#bring in csvs

voidvol <- read.csv(voids[i])
blockvol <- read.csv(blocks[i])
valleyvol <- read.csv(valleys[i])
totalvol <- read.csv(totals[i])
lensvol <- read.csv(lenses[i])
area <- read.csv(areas[i])


# take just the positive values 

voidvol <- (voidvol[voidvol$VOLUME>0,])
blockvol <- blockvol[blockvol$VOLUME>0,]
valleyvol <- valleyvol[valleyvol$VOLUME>0,]
totalvol <- totalvol[totalvol$VOLUME>0,]
lensvol <- lensvol[lensvol$VOLUME>0,]

#sums

thisvoidvol <- sum(voidvol$VOLUME)
thisblockvol <- sum(blockvol$VOLUME)
thisvalleyvol <- sum(valleyvol$VOLUME)
thistotalvol <- sum(totalvol$VOLUME)
thislensvol <- sum(lensvol$VOLUME)
thisarea <- area$Area[1]

sumlog <- data.frame(site=shortnames[i],
                      voidvolume=thisvoidvol,
                      blockvolume=thisblockvol,
                      valleyvolume=thisvalleyvol,
                      totalvolume=thistotalvol,
                      lensevol=thislensvol,
                      area=thisarea,
                      stringsAsFactors=FALSE)

volume_log <- rbind(volume_log, sumlog)
}

#some computations on the log

volume_log$blocktovoidratio <- volume_log$blockvolume/volume_log$voidvolume

volume_log$measvalleytoblockratio <- volume_log$valleyvolume/volume_log$blockvolume

volume_log$calcvalleyvol <- volume_log$lensevol - volume_log$blockvolume 

volume_log$calcvalleytoblockratio <- volume_log$calcvalleyvol/volume_log$blockvolume

volume_log$effectivechaosdiameter <- 2*sqrt(volume_log$area/pi)

volume_log$expected_depth <- 0.286*((volume_log$effectivechaosdiameter/1000)^0.582)

volume_log$effective_depth <- (volume_log$voidvolume/volume_log$area)/1000

volume_log$depthratio <- volume_log$effective_depth/volume_log$expected_depth

##export figures


##block vol void vol 
tiff("figs/blockvol_void_vol.tif", width = 8, height = 5, units = 'in', res = 300)

plot(log10(volume_log$blockvolume)~log10(volume_log$voidvolume), pch = 20, xlab = expression("Void Volume ("*log[10]*" m)"), ylab = expression("Block Volume ("*log[10]*" m)"))
blockvoidlm <- lm(log10(volume_log$blockvolume)~log10(volume_log$voidvolume))
abline(blockvoidlm, lty = 3, col = "gray")
summary(blockvoidlm)

dev.off()

##valley to block ratio

tiff("figs/meas_valley_to_block_vol.tif", width = 8, height = 5, units = 'in', res = 300)

plot(volume_log$measvalleytoblockratio~log10(volume_log$blockvolume), pch = 20, xlab = expression("Block Volume ("*log[10]*" m)"), ylab = "Measured Valley to Block Volume Ratio")

measvalleyvoltoblockratiolm <- lm(volume_log$measvalleytoblockratio~log10(volume_log$blockvolume))
abline(measvalleyvoltoblockratiolm,lty = 3, col = "gray")
summary(measvalleyvoltoblockratiolm)

dev.off()

tiff("figs/calc_valley_to_block_vol.tif", width = 8, height = 5, units = 'in', res = 300)

plot(volume_log$calcvalleytoblockratio~log10(volume_log$blockvolume), pch = 20, xlab = expression("Block Volume ("*log[10]*" m)"), ylab = "Calculated Valley to Block Volume Ratio")
calcvalleyvoltoblockratiolm <- lm(volume_log$calcvalleytoblockratio~log10(volume_log$blockvolume))
abline(calcvalleyvoltoblockratiolm,lty = 3, col = "gray")
summary(calcvalleyvoltoblockratiolm)

dev.off()

### void volume to total volume

tiff("figs/void_vol_to_total_vol.tif", width = 8, height = 5, units = 'in', res = 300)

plot(log10(volume_log$voidvolume)~log10(volume_log$totalvolume), pch = 20, xlab = expression("Total Volume ("*log[10]*" m)"), ylab = expression("Void Volume ("*log[10]*" m)"))
voidtotlm <- lm(log10(volume_log$voidvolume)~log10(volume_log$totalvolume))
abline(voidtotlm, lty = 3, col = "gray")     
summary(voidtotlm)

dev.off()

meanblocktovoid <- mean(volume_log$blocktovoidratio)
sdblocktovoid <- sd(volume_log$blocktovoidratio)
