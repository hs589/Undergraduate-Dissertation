####import data and packages####
nodupes <- read.csv("R sheet minus dupes.csv",header = TRUE) # data with outliers and duplicate data removed 
true <- read.csv("NOOUTLIERSTRUE.CSV",header = TRUE) # data with outliers removed. IQR was calculted in R, then a new csv file created with otuliers removed.
# NOTE: Mysticeti is spelt wrong in file (Mysteceti)
install.packages("tidyverse")
install.packages("Hmisc")
library(tidyverse)
library(Hmisc)


######calculate IQR using Hmisc#####

p <-  smedian.hilow(nodupes$Width[nodupes$Parvorder=="Mysteceti"], conf.int=.5)
q <-  smedian.hilow(nodupes$Width[nodupes$Parvorder=="Odontoceti"], conf.int=.5)
r <-  smedian.hilow(nodupes$Width[nodupes$Parvorder=="Archaeoceti"], conf.int=.5)
r
r[2]
IQRr <- r[3] - r[2]
IQRp <- p[3] - p[2]
IQRq <- q[3] - q[2]
IQRr
p[3]
p[3] + IQRp
p[2] - IQRp

q[3] + IQRq
q[2] - IQRq

r[3] + IQRr
r[2] - IQRr

######################boxplots excluding Archaoeceti######################
par(mfrow = c(1,2))
#WITH OTULIERS
noarch <- nodupes %>%  filter(Parvorder!='Archaeoceti')
boxplot(noarch$Width ~ noarch$Parvorder ,
        xlab = "",
        ylab = expression("Bizygomatic Width mm" ^-1),
        main = "Outliers",
        col = c("lightgreen", "lightblue"))
#OUTLIERS REMOVED
truearch <- true %>%  filter(Parvorder!='Archaeoceti')
boxplot(truearch$Width ~ truearch$Parvorder,
        xlab = "",
        ylab = expression("Bizygomatic Width mm" ^-1),
        main = "Outliers Removed",
        col = c("lightgreen", "lightblue"))


######individual species points over time WITHOUT outliers#######
ggplot(true, aes(x= Epoch.Time,
                 y = Width, 
                 color = Parvorder,
                 pch = Parvorder)) +
  geom_jitter() +
  geom_smooth(data = true, 
              aes(x = as.numeric(Epoch.Time), y = as.numeric(Width)),
              method = "lm",  se = TRUE) +
  geom_vline(xintercept = 38,linetype="solid")+
  geom_vline(xintercept = 33.9,linetype= "dotted") +
  geom_vline(xintercept = 15,linetype = "dotdash") +
  geom_vline(xintercept = 3,linetype= 5) +
  xlab("Millions of Years Ago") +
  ylab(expression("Bizygomatic Width mm" ^-1)) +
  scale_x_reverse()


######individual species points over time WITH outliers#######
ggplot(nodupes, aes(x= Epoch.Time,
                    y = Width, 
                    color = Parvorder,
                    pch = Parvorder)) +
  geom_jitter() +
  geom_smooth(data = nodupes, 
              aes(x = as.numeric(Epoch.Time), y = as.numeric(Width)),
              method = "lm",  se = TRUE) +
  geom_vline(xintercept = 38,linetype="solid")+
  geom_vline(xintercept = 33.9,linetype= "dotted") +
  geom_vline(xintercept = 15,linetype = "dotdash") +
  geom_vline(xintercept = 3,linetype= 5) +
  xlab("Millions of Years Ago") +
  ylab(expression("Bizygomatic Width mm" ^-1)) +
  scale_x_reverse()

?geom_smooth()

##the same minus archaeoceti
ggplot(truearch, aes(x= Epoch.Time,
                     y = Width, 
                     color = (Parvorder),
                     pch = (Parvorder))) +
  geom_jitter() +
  geom_smooth(data = truearch, aes(x = as.numeric(Epoch.Time), y = as.numeric(Width)),
              method = "lm",  se = TRUE, span = 0.99) +
  geom_vline(xintercept = 38,linetype="dashed")+
  geom_vline(xintercept = 27,linetype="dotted") +
  geom_vline(xintercept = 35,linetype="dotdash") +
  scale_x_reverse()

#######STATISTICAL ANALYSES#####
summary(true$Width[true$Parvorder == "Mysteceti"])
summary(true$Width[true$Parvorder == "Odontoceti"])
summary(nodupes$Width[nodupes$Parvorder == "Mysteceti"])
summary(nodupes$Width[nodupes$Parvorder == "Odontoceti"])
#WITH OUTLIERS
hist(rank(noarch$Width))
shapiro.test((noarch$Width[noarch$Parvorder == "Odontoceti"]))
shapiro.test(true$Width)
#non-parametric therefore:
#kruskal walis
modout <- lm(noarch$Width ~ noarch$Parvorder * noarch$Epoch)
anova(modout)

kruskal.test(noarch$Width ~ noarch$Period)
kruskal.test(noarch$Width ~ noarch$Parvorder)

#wilcoxon rank sum
wilcox.test(noarch$Width~noarch$Parvorder,
            method = "bonferroni")
?pairwise.wilcox.test
#dunn test
dunnTest(noarch$Width ~ noarch$Parvorder,
         method = "bonferroni")


par(mfrow = c(1,1))
boxplot(truearch$Width ~  truearch$Parvorder ,
        col = c("lightgreen", "lightblue"),
        xlab = "",
        ylab = "Bizygomatic Width / mm")
boxplot(noarch$Width ~ noarch$Parvorder)
#NORMALITY
hist(log(true$Width[true$Parvorder == "Odontoceti"]))
shapiro.test(log(true$Width[true$Parvorder == "Mysteceti"]))
#NORMAL AFTER LOG TRANSFORM
#HOMOGENOUS VARIENCE
var.test(log(true$Width[true$Parvorder == "Mysteceti"]),log(true$Width[true$Parvorder == "Archaeoceti"]))
bartlett.test(log(true$Width) ~ (true$Parvorder))
bartlett.test(rank(OUT$Width) ~ OUT$Epoch + OUT$Parvorder)
hist(rank(OUT$Width[OUT$Parvorder=="Odontoceti"]))
#correlation coefficient
cor.test(log(truearch$Width[truearch$Parvorder=='Odontoceti']), -truearch$Epoch.Time[truearch$Parvorder=='Odontoceti'])

cor.test(log(truearch$Width[truearch$Parvorder=='Mysteceti']), -truearch$Epoch.Time[truearch$Parvorder=='Mysteceti'])

#t test
t.test(log(true$Width[true$Parvorder=='Odontoceti']), log(true$Width[true$Parvorder=='Archaeoceti']))
t.test(log(true$Width[true$Parvorder=='Mysteceti']), log(true$Width[true$Parvorder=='Archaeoceti']))
#####HELP#####
version$version.string
citation(r)
?jitte
?legend()
?geom_smooth
?kruskal.test