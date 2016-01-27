##First 
library(ggplot2)
getwd()
setwd("~/My R documents")
setwd("D:/R documents")
data<-read.csv("tILSunSh_IAA_123015.csv")
summary(data)
data<-droplevels(data[data$genotype!="",])
summary(data)

data$genotype<-relevel(data$genotype,ref="M82")
data$light_trt<-relevel(data$light_trt,ref="sun")
data$hormone_trt<-relevel(data$hormone_trt,ref="mock")
summary(data)

data$flat<-as.factor(data$flat)
data$shelf<-as.factor(data$shelf)
summary(data)

mod4<-lm(total_height_mm~light_trt*hormone_trt,data=data[data$genotype=="M82",])
summary(mod4)
summary(mod3)

####I try to find out the difference of each step
p <- ggplot(data, aes(x = hormone_trt, y = total_height_mm, fill = light_trt)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(dodge.width = 0.7)) +
  facet_grid(.~genotype) +
  scale_fill_manual(values = c("gray90", "gray")) +
  theme_bw() +
  ylab("Total Height (mm)") + xlab("") + labs(fill = "Light Treatment")###this is Susan's total cript
p

### remove the last four steps and make the difference between color and fill
p <- ggplot(data, aes(x = hormone_trt, y = total_height_mm, color = light_trt)) +
  geom_boxplot() +
  geom_jitter() 
p

### figure out the function of facet_grid in ggplot2
p <- ggplot(data, aes(x = hormone_trt, y = total_height_mm, color = light_trt)) +
  geom_boxplot() +
  geom_jitter() +
  facet_grid(.~genotype) 
p

p <- ggplot(data, aes(x = hormone_trt, y = total_height_mm, color = light_trt)) +
  geom_boxplot() +
  geom_jitter() +facet
  facet_grid(genotype~.) 
p

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
# With one variable 
#p + facet_grid(. ~ cyl) 
#p + facet_grid(cyl ~ .)

# With two variables
#p + facet_grid(vs ~ am)
#p + facet_grid(am ~ vs)
p + facet_grid(vs ~ am, margins=TRUE)

set.seed(6809)
diamonds <- diamonds[sample(nrow(diamonds), 1000), ]
diamonds$cut <- factor(diamonds$cut,
                       levels = c("Ideal", "Very Good", "Fair", "Good", "Premium"))

# Repeat first example with new order
p <- ggplot(diamonds, aes(carat, ..density..)) +
  geom_histogram(binwidth = 1)
p + facet_grid(. ~ cut)

### figure out the function of scale_fill_manual(which belongs to scale_manual in ggplot2)
p <- ggplot(data, aes(x = hormone_trt, y = total_height_mm, fill = light_trt)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(dodge.width = 0.7)) +
  facet_grid(.~genotype) +
  scale_fill_manual(values = c("gray90", "gray")) 
p

p <- ggplot(data, aes(x = hormone_trt, y = total_height_mm, color = light_trt)) +
  geom_boxplot() +
  geom_jitter() +
  facet_grid(.~genotype) +
  scale_colour_manual(values = c("red", "blue")) 
p

###figure out the function of cripts after "theme_bw"
p <- ggplot(data, aes(x = hormone_trt, y = total_height_mm, fill = light_trt)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(dodge.width = 0.7)) +
  facet_grid(.~genotype) +
  scale_fill_manual(values = c("gray90", "gray")) +
  theme_bw()#+
  #ylab("Total Height (mm)") + xlab("") + labs(fill = "Light Treatment")
p

p <- ggplot(data, aes(x = hormone_trt, y = total_height_mm, fill = light_trt)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(dodge.width = 0.7)) +
  facet_grid(.~genotype) +
  scale_fill_manual(values = c("gray90", "gray")) +
  #theme_bw() +
  ylab("Total Height (mm)") + xlab("") + labs(fill = "Light Treatment")
p

###figure out the function of "pdf"
pdf("ILSunSh_boxplots_123115.pdf")
p <- ggplot(data, aes(x = hormone_trt, y = total_height_mm, fill = light_trt))
p <- p + geom_boxplot()
p <- p + geom_point(position = position_jitterdodge(dodge.width = 0.7))
p <- p + facet_grid(.~genotype) 
p <- p + scale_fill_manual(values = c("gray90", "gray"))
p <- p + theme_bw()
p <- p + ylab("Total Height (mm)") + xlab("") + labs(fill = "Light Treatment")
p
dev.off()


#########How to make the histogram ###get the Estimated, Std. Error, P-value
mod <- lm(total_height_mm ~ genotype*light_trt*hormone_trt, data = data)
mod1 <- lm(epicotyl_mm ~ genotype*light_trt*hormone_trt, data = data)
mod2 <- lm(int1_mm ~ genotype*light_trt*hormone_trt, data = data)
mod3 <- lm(pet1_mm ~ genotype*light_trt*hormone_trt, data = data)

#grab the sem from the model
sem <- summary(mod)$coefficients[,"Std. Error"]
sem
names(sem)
sem1 <- summary(mod1)$coefficients[,"Std. Error"]
sem2 <- summary(mod2)$coefficients[,"Std. Error"]
sem3 <- summary(mod3)$coefficients[,"Std. Error"]

#create a data frame that has "gt" and "light" columns correpsponding to the names in the sem vector
pred.df <- data.frame(
  genotype=ifelse(grepl("genotype",names(sem)),                    #if there is a "gt" in the name,
                  sub("genotype(.+?)(:|$).*","\\1",names(sem)), #then extract the gt
                  levels(data$genotype)[1]),                #else put in the reference genotype
  
  light_trt=ifelse(grepl("light",names(sem)),              #if there is a "light" in the name,
                   sub(".*light_trt(.+?)(:|$).*","\\1",names(sem)),   #then extract the light
                   levels(data$light_trt)[1]),              #else 
  
  hormone_trt = ifelse(grepl("hormone", names(sem)), 
                       sub(".*hormone_trt(.+?)", "\\1", names(sem)), 
                       levels(data$hormone_trt)[1])
)

#get the predicted means from the model
pred.df$totalH <- predict(mod,pred.df,re.form=NA)
pred.df
pred.df$epi <- predict(mod1, pred.df, re.form = NA)
pred.df$int1 <- predict(mod2, pred.df, re.form = NA)
pred.df$pet1 <- predict(mod3, pred.df, re.form = NA)
pred.df

# add the p-values
pred.df$Pheight <- summary(mod)$coefficients[, 4]
pred.df
pred.df$Pepi <- summary(mod1)$coefficients[, 4]
pred.df$Pint1 <- summary(mod2)$coefficients[, 4]
pred.df$Ppet1 <- summary(mod3)$coefficients[, 4]
pred.df

#add the error
pred.df$errorH <- sem
pred.df$errorepi <- sem1
pred.df$errorint1 <- sem2
pred.df$errorpet1 <- sem3
pred.df
head(pred.df)

###set the reference
pred.df$genotype <- relevel(pred.df$genotype, ref = "M82")
pred.df$light_trt <- relevel(pred.df$light_trt, ref = "sun")
pred.df$hormone_trt <- relevel(pred.df$hormone_trt, ref = "mock")
head(pred.df)

#plot it by geom_bar, respectively!
library(ggplot2)
##Total_H_mm
pl <- ggplot(pred.df, aes(x=hormone_trt, y=totalH, ymin=totalH-errorH, ymax=totalH+errorH, fill=light_trt))
pl <- pl + geom_bar(position="dodge",stat="identity")
pl <- pl + facet_wrap(~ genotype,nrow=1)
pl <- pl + geom_errorbar(position=position_dodge(width=.9),width=.5)
pl <- pl + ylab("Total Height (mm)") + xlab("") + labs(fill = "Light Treatment")
pl <- pl + scale_fill_manual(values = c("gray90", "gray")) + theme_bw()
pl

##Epi_mm
ql <- ggplot(pred.df, aes(x=hormone_trt, y=epi, ymin=epi-errorepi, ymax=epi+errorepi, fill=light_trt))
ql <- ql + geom_bar(position="dodge",stat="identity")
ql <- ql + facet_wrap(~ genotype,nrow=1)
ql <- ql + geom_errorbar(position=position_dodge(width=.9),width=.5)
ql <- ql + ylab("Epicotyl Length (mm)") + xlab("") + labs(fill = "Light Treatment")
ql <- ql + scale_fill_manual(values = c("gray90", "gray")) + theme_bw()
ql

##Int1_mm
rl <- ggplot(pred.df, aes(x=hormone_trt, y=int1, ymin=int1-errorint1, ymax=int1+errorint1, fill=light_trt))
rl <- rl + geom_bar(position="dodge",stat="identity")
rl <- rl + facet_wrap(~ genotype,nrow=1)
rl <- rl + geom_errorbar(position=position_dodge(width=.9),width=.5)
rl <- rl + ylab("Internode 1 Length (mm)") + xlab("") + labs(fill = "Light Treatment")
rl <- rl + scale_fill_manual(values = c("gray90", "gray")) + theme_bw()
rl

##Pet1_mm
sl <- ggplot(pred.df, aes(x=hormone_trt, y=pet1, ymin=pet1-errorpet1, ymax=pet1+errorpet1, fill=light_trt))
sl <- sl + geom_bar(position="dodge",stat="identity")
sl <- sl + facet_wrap(~ genotype,nrow=1)
sl <- sl + geom_errorbar(position=position_dodge(width=.9),width=.5)
sl <- sl + ylab("Petiole 1 Length (mm)") + xlab("") + labs(fill = "Light Treatment")
sl <- sl + scale_fill_manual(values = c("gray90", "gray")) + theme_bw()
sl


###try to print all of the histogram in one page###wonderful
names(pred.df)
totalH <- pred.df[, c(1, 2, 3, 4, 8, 13)]
epi <- pred.df[, c(1, 2, 3, 5, 9, 14)]
int1 <- pred.df[, c(1, 2, 3, 6, 10, 15)]
pet1 <- pred.df[, c(1, 2, 3, 7, 11, 12)]
names(pred.df)

head(epi)
names(epi)[4:6] <- c("value", "pval", "SE")
names(totalH)[4:6] <- c("value", "pval", "SE")
names(int1)[4:6] <- c("value", "pval", "SE")
names(pet1)[4:6] <- c("value", "pval", "SE")
epi$organ <- "Epicotyl"
totalH$organ <- "Total Height"
int1$organ <- "Internode 1"
pet1$organ <- "Petiole 1"
names(pred.df)

dm <- rbind(totalH, epi, int1, pet1)
head(dm)

tl <- ggplot(dm, aes(x = hormone_trt, y = value, ymin = value-SE, ymax = value+SE, fill = light_trt))
tl <- tl + geom_bar(position="dodge",stat="identity")
tl <- tl + facet_wrap(organ ~ genotype, scales = "free")
tl <- tl + geom_errorbar(position=position_dodge(width=.9),width=.5)
tl <- tl + ylab("Length (mm)") + xlab("") + labs(fill = "Light Treatment")
tl <- tl + scale_fill_manual(values = c("gray90", "lightblue")) + theme_bw()
tl

ggsave("ILSunSh_IAA_ALLHistograms_123115.pdf")##first way to save
pdf("ILSunSh_IAA_All_Histograms_123115.pdf")##second way to save
tl
dev.off()
