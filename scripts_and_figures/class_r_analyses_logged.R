
## Ian McFadden
## imcfadden@ucla.edu

## UCLA Trait Transects 
## EEB122 Ecology Winter 2019

### Setup 

# Clear any remaining objects 
rm(list=ls())

# Set the working directory 
setwd("/Users/Many_Islands/Desktop/")

# Check the directory
getwd()

# Read in the data
dat <- read.csv("logged_data.csv")

# Look at the whole dataset 
dat

# Look at just the first 6 rows 
head(dat)


###### Analyses

# Correlation 
dat$log_leaf_length_cm     
dat$log_leaf_width_cm   

# Plot the data 
pdf("length_width.pdf", 4.5, 4.5)
plot(dat$log_leaf_length_cm, dat$log_leaf_width_cm, pch=16,
     xlab="Leaf length (log cm)",
     ylab="Leaf width (log cm)")
legend("topleft", "r=0.79, p<0.001", bty="n")
dev.off()

cor.test(dat$log_leaf_length_cm, dat$log_leaf_width_cm)

# ANOVA
pdf("thickness_site.pdf", 4, 5)
boxplot(dat$log_leaf_thickness_mm~dat$site, 
        ylab="Leaf thickness (log mm)", 
        xlab="Site")
dev.off()

mod <- aov(dat$log_leaf_thickness_mm~dat$site) 

summary(mod)

TukeyHSD(mod)

# Do a correlation test 
cor.test(dat$log_leaf_length_cm, dat$log_leaf_width_cm)

pdf("length_width.pdf", 4.5, 4.5)
plot(dat$log_leaf_length_cm, dat$log_leaf_width_cm, pch=16,
     xlab="Leaf length (log cm)",
     ylab="Leaf width (log cm)")

legend("topleft", "r=0.79, p < 0.001", bty="n")
dev.off()


# Plot plant height by site
pdf("height_site.pdf", 4, 5)
boxplot(dat$log_plant_height_cm~dat$site,
        ylab="Plant height (log cm)",
        xlab="Site")
dev.off()

# ANOVA
mod <- aov(dat$log_plant_height_cm~dat$site)

summary(mod)

TukeyHSD(mod)


# Create leaf volume variable 
dat$leaf_volume <- dat$log_leaf_length_cm * dat$log_leaf_width_cm * (dat$log_leaf_thickness_mm / 10)

head(dat)

# Plot volume by site
pdf("volume_by_site.pdf", 4.5, 4.5)
boxplot(dat$leaf_volume~dat$site)
dev.off()

# Boxplot and ANOVA 

mod <- aov(dat$leaf_volume~dat$site)

summary(mod)

TukeyHSD(mod)



plot(dat$log_leaf_thickness_mm~dat$site)



mod <- aov(dat$log_leaf_thickness_mm~dat$site)

summary(mod)

TukeyHSD(mod)


pdf("ucla_tt_anova_l_thick.pdf", 4.5, 4.5)
plot(dat$log_leaf_thickness_mm~dat$site, col="grey",
     xlab="Site", ylab="Leaf thickness (log mm)")
text(c(1,1,1), c("A", "B", "B"))
legend("topright", "F = 47.16***", bty="n")
dev.off()


# t-test 
thickness_desert <- dat$log_leaf_thickness_mm[which(dat$site=="Desert")]
thickness_stream <- dat$log_leaf_thickness_mm[which(dat$site=="Stream")]

t.test(thickness_desert, thickness_stream)

boxplot(thickness_desert, thickness_stream)

