###valley analysis for chaos makes plots and determines if blocks get smaller towards the edges and if thickness and effective diameter are correlated

setwd("~/Dropbox/R_projects/chaos")

#make a log file

coef_log <- data.frame(site=character(),
                       thickdistslope=double(),
                       thickdistpval=double(),
                       areadistslope=double(),
                       areadistpval=double(),
                       effdiamthickslope=double(),
                       effdiamthickpval=double(),
                       interceptthickeffd = double(),
                       pval_thickdistintercept=double(),
                       number_of_blocks=double(),
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

shortnames <- gsub('.{8}$', '', shortnames)

figpath <- paste0("figs/", shortnames)

nicenames <- c("Aram Chaos", "Aromatum Chaos", "Atlantis Chaos", "Aureum Chaos", "Eos Chaos", "Gorgonium Chaos", "Hawaii Marine Landslide", "Hydraotes Chaos", "Iamuna Chaos", "Luzzi et al. L2", "Luzzi et al., L2b", "Luzzi et al., L2c", "Luzzi et al., L2d", "Orson Welles Crater", "OW1", "OW2", "OW3", "Oxia Chaos", "Pyrrhae Chaos", "R171 Crater", "R89 Crater", "Luzzi et al., S1", "South Ister Chaos", "Xanthe Chaos")

#for testing only

counter <- 1
#i <- "hawaii"

##here's where the loop will go! 

for(i in shortnames)
{
  

data <- read.csv(paste("raw_data/",i,"_val.csv", sep = ""))
center <- read.csv(paste("raw_data/",i,"_ctr.csv", sep = ""))

#effective diamter calc
data$eff_diam <- 2*sqrt(data$area/3.14)

#thickness calc
data$thickness <- data$val_max - data$val_min

##distance to center
data$dist2ctr <- (sqrt(((data$northing - center$northing)^2)+((data$easting - center$easting)^2)))/1000

##calculate number of polygons

numpolys <- nrow(data)

## data cleanup
data <- data[data$val_min>-9999,]
data <- data[data$thickness > 10,]


### Plots

## Block area histo

tiff(paste("figs/",i,"_valley_area.tif", sep = ""), width = 8, height = 5, units = 'in', res = 300)

par(mar= c(5,6,3,3))
hist(log10(data$area), main = paste(nicenames[counter],"Block Area", sep = " "), xlab = expression(Log10~Area~(m^2)), ylab = "Number of Blocks")

dev.off()

## Block thickness histo 

tiff(paste("figs/",i,"_block_thickness.tif", sep = ""), width = 8, height = 5, units = 'in', res = 300)

hist(log10(data$thickness), main = paste(nicenames[counter],"Block Thickness", sep = " "), xlab = "Log10 Thickness (m)", ylab = "Number of Blocks")

dev.off()

## Block thickness vs distance W/LINE

tiff(paste("figs/",i,"_thick_dist.tif", sep = ""), width = 8, height = 5, units = 'in', res = 300)

plot(log10(data$thickness)~data$dist2ctr, main = paste(nicenames[counter],"Block Thickness vs. Distance from Chaos Center", sep = " "), xlab = "Distance to Chaos Center (km)", ylab = "Log10 Thickness (m)", pch = 20, cex = 0.5)

thickdistlm <- lm(log10(data$thickness)~data$dist2ctr)
abline(thickdistlm, lty = 3, color = "grey81")

dev.off()

## Block area vs distance W/LINE


tiff(paste("figs/",i,"_area_dist.tif", sep = ""), width = 8, height = 5, units = 'in', res = 300)

par(mar=c(5,6,3,3))

plot(log10(data$area)~data$dist2ctr, main = paste(nicenames[counter],"Block Area vs. Distance from Chaos Center", sep = " "), xlab = "Distance to Chaos Center (km)", ylab = expression(Log10~Area~(m^2)), pch = 20, cex = 0.5)

areadistlm <- lm(log10(data$area)~data$dist2ctr)
abline(areadistlm, lty = 3, color = "grey81")

dev.off()

## Eff diam vs thickness W/LINE

tiff(paste("figs/",i,"_area_thick.tif", sep = ""), width = 8, height = 5, units = 'in', res = 300)

plot(log10(data$eff_diam)~log10(data$thickness), main = paste(nicenames[counter],"Block Effective Diameter vs. Thickness", sep = " ") , xlab = "Log10 Thickness (m)", ylab = "Log10 Effective Diameter (m)", pch = 20, cex = 0.5)

thick_effD_lm <- lm((log10(data$eff_diam)~log10(data$thickness)))
abline(thick_effD_lm, lty = 3, color = "grey81")

dev.off()

#make a log file for this round
slpareadist <- round(coef(areadistlm),3)[2]
slpthickdist <- round(coef(thickdistlm),3)[2]
slpthickeffd <- round(coef(thick_effD_lm),3)[2]

interceptthickeffd <- round(coef(thick_effD_lm),3)[1]

pval_thickeffD <- summary(thick_effD_lm)$coefficients[,4]
pval_thickeffD <- pval_thickeffD[2]

pval_areadist <- summary(areadistlm)$coefficients[,4]
pval_areadist <- pval_areadist[2]

pval_thickdist <- summary(thickdistlm)$coefficients[,4]
pval_thickdist <- pval_thickdist[2]

pval_thickdistintercept <- summary(thickdistlm)$coefficients[,4]
pval_thickdistintercept <- pval_thickdist[1]

this_chaos_coef_log <- data.frame(site=i,
                       thickdistslope=slpthickdist,
                       thickdistpval=pval_thickdist,
                       areadistslope=slpareadist,
                       areadistpval=pval_areadist,
                       effdiamthickslope=slpthickeffd,
                       effdiamthickpval=pval_thickeffD,
                       interceptthickeffd = interceptthickeffd,
                       pval_thickdistintercept=pval_thickdistintercept,
                       number_of_blocks=numpolys,
                       stringsAsFactors=FALSE) 

coef_log <- rbind(coef_log, this_chaos_coef_log)

#increment counter
counter <- counter+1

}

write.csv(coef_log, "~/Dropbox/R_projects/chaos/site_relationships.csv")

signifthickdist <- coef_log[coef_log$thickdistpval<0.05,c(1,2,3)]
signifareadist <- coef_log[coef_log$areadistpval<0.05,c(1,4,5)]
signifeffdiamthick <- coef_log[coef_log$effdiamthickpval<0.05,c(1,6,7)]
meaneffidamthickslope <- mean(signifeffdiamthick[1:18,2]) 

signifthickdistintercept <- coef_log[coef_log$pval_thickdistintercept<0.05,c(1,8,9)]

meanthickdistslope <- mean(signifeffdiamthick$effdiamthickslope)
stdevthickdistslope <- sd(signifeffdiamthick$effdiamthickslope)
meanthickdistintercept <- mean(signifthickdistintercept$interceptthickeffd)
stdevthickdistintercept <- sd(signifthickdistintercept$interceptthickeffd)

total_number_of_blocks <- sum(coef_log$number_of_blocks)
