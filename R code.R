########install pacakges and data##########
data <- read.csv("R sheet.csv", header = TRUE)
nodupes <- read.csv("R sheet minus dupes.csv",header = TRUE)
OUT <- read.csv("NOOUTLIERS.CSV",header = TRUE)
true <- read.csv("NOOUTLIERSTRUE.CSV",header = TRUE)
 
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("FSA")
install.packages("car")
library(tidyverse)
library(Hmisc)
library(FSA)
library(car)

citation()
toBibtex(citation(package = "Hmisc"))

##calculate IQR using Hmisc

r <-  smedian.hilow(nodupes$Width[nodupes$Parvorder=="Archaeoceti"], conf.int=.5)
r
IQRr <- r[3] - r[2]
IQRr
p[2] - IQR  
outlierR2 <- r[2] - IQRr
outlierR2
p
q
r
#########
mean(data$Width[data$Parvorder=="Archaeoceti"])


DATA########
mean(data$Width[data$Parvorder=="Archaeoceti"])
hist(data$EpochTime)
shapiro.test(sqrt(data$EpochTime))

boxplot(data$Width[data$Parvorder=="Mysteceti"]~
          data$EpochTime[data$Parvorder=="Mysteceti"],
        labels() = data$Epoch)

plot(data$Epoch,data$Width)

data$Epoch <- as.factor(data$Epoch)



t.test(data$Width[data$Parvorder=="Mysteceti"],data$Width[data$Parvorder=="Odontoceti"])

modparv <- lm(data$Width ~ data$Parvorder * data$EpochTime)
plot(modparv)
anova(modparv)

kruskal.test(data$Width ~ data$Parvorder * data$EpochTime)


NODUPES###############
noarch <- nodupes %>%  filter(Parvorder!='Archaeoceti')
arch <- true %>%  filter(Parvorder=='Archaeoceti')
mys <- true %>%  filter(Parvorder== 'Mysteceti')
odo  <- true %>%  filter(Parvorder== 'Odontoceti')

OUT <- nodupes %>% filter(Width < outlierO )

mean(nodupes$Width[nodupes$Parvorder=="Mysteceti"])
outlierO
nodupes$Parvorder <- as.factor(nodupes$Parvorder)
outlier
###################################      REMOVE OUTLIERS     ################################
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
x[!x %in% boxplot.stats(x)$out]


####BASIC PLOTS####
#WIDTH BY ORDER WITH OUTLIERS
boxplot(noarch$Width ~
          noarch$Parvorder,
        col = c("lightgreen","lightblue"),
        ylab = "Bizygomatic Width / mm",
        xlab = "Parvorder of Cetacea")
#WIDTH BY ORDER WITHout OUTLIERS
boxplot(nodupes$Width ~
          nodupes$Parvorder,
        col = c("hotpink","lightblue","purple"),
        ylab = "Bizygomatic Width / mm",
        xlab = "Parvorder of Cetacea")

#WIDTH BY EPOCH
boxplot(nodupes$Width ~
          nodupes$Epoch)


############

hist(nodupes$Epoch.Time)
hist(nodupes$Width)

shapiro.test(sqrt(nodupes$Epoch.Time))
shapiro.test(piss)

hist(sqrt(nodupes$Epoch.Time))
piss <- rank(nodupes$Width)


boxplot(nodupes$Epoch.Range ~ nodupes$Width)
#non parametric it is then######################
wilcox.test(nodupes$Width[nodupes$Parvorder != "Archaeoceti"] ~ 
              nodupes$Parvorder[nodupes$Parvorder != "Archaeoceti"])
?wilcox.test



##########################PLOTS##########################
outlier
#####3 boxplots showing width over time grouped by parvorder WITHOUT outliers####

ggplot(true, aes(x = reorder(Epoch, -Epoch.Time),
                    y = Width, colour = reorder(Period, -Epoch.Time))) +
  geom_boxplot() +
  facet_wrap(~Parvorder, ncol = 2) +
  geom_smooth(method = "lm", se=FALSE, formula = y~x, color = "black", aes(group=1))+
  xlab("Epoch") +
  ylab("Bizygomatic Width / mm") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(colour = "Geological Period") +
  scale_y_continuous(limits = c(0,p[3])) +
  geom_vline(xintercept = 2.446,linetype="dashed")

 #####3 boxplots showing width over time grouped by parvorder WITH outliers####

ggplot(nodupes, aes(x = reorder(Epoch, -Epoch.Time),
                    y = Width, colour = reorder(Period, -Epoch.Time))) +
  geom_boxplot() +
  facet_wrap(~Parvorder, ncol = 3) +
  geom_smooth(method = "lm", se=FALSE, formula = y~x, color="black", aes(group=1))+
  xlab("Epoch") +
  ylab("Bizygomatic Width / mm") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(colour = "Geological Period") +
  geom_vline(xintercept = 2.446,linetype="dashed")

3.3/7.4
#####individual species points over time WITHOUT outliers####
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

?linetype
#+
#geom_smooth(data = subset(mys, Width < outlier), 
            #aes(x = as.numeric(Epoch.Time), y = as.numeric(Width)),
            #method = "lm",  se = TRUE) +
  #geom_smooth(data = subset(arch, Width < outlierR & Width > outlierR2), 
              #aes(x = as.numeric(Epoch.Time), y = as.numeric(Width)),
             # method = "lm",  se = TRUE) 
######individual species points over time WITH outliers####
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

#geom_smooth(method = "lm", se=FALSE, formula = nodupes$Width[nodupes$Parvorder=="Mysteceti"]~
#nodupes$Epoch.Time[nodupes$Parvorder=="Mysteceti"], 
#color="black"
?geom_vline
geom_smooth(data = nodupes, aes(x = Epoch.Time[Parvorder=="Mysteceti"], y = Epoch.Time[Parvorder=="Mysteceti"]))
  
##calculate IQR using Hmisc

r <-  smedian.hilow(nodupes$Width[nodupes$Parvorder=="Archaeoceti"], conf.int=.5)
r
q
p
IQRr <- r[3] - r[2]
IQRr
r[2] - IQRr  
outlierR2 <- r[2] - IQRr
outlierR2
p#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
##########################STATISTICAL ANALYSES ########################################
#WITH OUTLIERS
hist(rank(noarch$Width))
shapiro.test((noarch$Width[noarch$Parvorder == "Odontoceti"]))

shapiro.test(rank(OUT$Width[OUT$Parvorder == "Archaeoceti"]))
shapiro.test(nodupes$Width)
shapiro.test(lognoarch$Width)
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
max(noarch$Width[noarch$Parvorder == "Mysteceti"])
max(noarch$Width[noarch$Parvorder == "Odontoceti"])

par(mfrow = c(1,1))
boxplot(truearch$Width ~  truearch$Parvorder ,
        col = c("lightgreen", "lightblue"),
        xlab = "",
        ylab = "Bizygomatic Width / mm")
boxplot(noarch$Width ~ noarch$Parvorder)
t.test(truearch$Width[truearch$Parvorder == "Mysteceti"], 
       truearch$Width[truearch$Parvorder == "Odontoceti"])

#############WITHOUT OUTLIERS###############
#NORMALITY
hist(log(true$Width[true$Parvorder == "Odontoceti"]))
shapiro.test(log(true$Width[true$Parvorder == "Mysteceti"]))
#NORMAL AFTER LOG TRANSFORM
#HOMOGENOUS VARIENCE
var.test(log(true$Width[true$Parvorder == "Mysteceti"]),log(true$Width[true$Parvorder == "Archaeoceti"]))
bartlett.test(log(true$Width) ~ (true$Parvorder))
bartlett.test(rank(OUT$Width) ~ OUT$Epoch + OUT$Parvorder)
hist(rank(OUT$Width[OUT$Parvorder=="Odontoceti"]))

mod <- lm(rank(OUT$Width[OUT$Parvorder == "Mysteceti"]) ~  rank(OUT$Epoch.Time[OUT$Parvorder == "Mysteceti"]))
cor.test(rank(OUT$Width[OUT$Parvorder == "Mysteceti"]), rank(OUT$Epoch[OUT$Parvorder == "Mysteceti"]))
mod1 <- lm(OUT$Width ~ OUT$Parvorder)
mod1
summary(mod)
anova(mod)
TukeyHSD(aov(mod),
         method = "bonferroni")

hist(rank(truearch$Epoch.Time))
shapiro.test(rank(truearch$Epoch.Time))



###remove archaeoceti####
truearch <- true %>%  filter(Parvorder!='Archaeoceti')
hist(log(truearch$Width[truearch$Parvorder=='Mysteceti']))
shapiro.test(log(truearch$Width[truearch$Parvorder=='Mysteceti']))
shapiro.test(log(truearch$Epoch.Time))
par(mfrow = c(2,2))
cor.test(log(truearch$Width[truearch$Parvorder=='Odontoceti']), -truearch$Epoch.Time[truearch$Parvorder=='Odontoceti'])
cor.test(log(truearch$Width), truearch$Epoch.Time)


summary(truearch$Width[truearch$Parvorder == "Odontoceti"])
summary(true$Width[true$Parvorder == "Archaeoceti"])
boxplot(truearch$Width ~ truearch$Parvorder)
#t test
t.test(log(true$Width[true$Parvorder=='Odontoceti']), log(true$Width[true$Parvorder=='Archaeoceti']))


t.test(log(true$Width[true$Parvorder=='Odontoceti' & true$Period == "Paleogene" ]), log(true$Width[true$Parvorder=='Mysteceti'& true$Epoch.Time > 27 ]))
#anova & post-hoc
mod <- lm(log(truearch$Width) ~ truearch$Parvorder * truearch$Period)
anova(mod)
TukeyHSD(aov(mod),
         method = "bonferroni")
######################another boxplot######################
par(mfrow = c(1,2))
boxplot(noarch$Width ~ noarch$Parvorder ,
        xlab = "",
        ylab = expression("Bizygomatic Width mm" ^-1),
        main = "Outliers",
        col = c("lightgreen", "lightblue"))
boxplot(truearch$Width ~ truearch$Parvorder,
        xlab = "",
        ylab = expression("Bizygomatic Width mm" ^-1),
        main = "Outliers Removed",
        col = c("lightgreen", "lightblue"))

summary(truearch$Width[truearch$Parvorder == "Odontoceti" & truearch$Period == "Quaternary"])

########hypohtesis 3######
mean(true$Width[true$Parvorder=='Mysteceti' & true$Epoch.Time < 27])
mean(true$Width[true$Parvorder=='Mysteceti' & true$Epoch.Time > 27])
? 
#####HELP#####
version$version.string
citation(r)
?jitte
?legend()
?geom_smooth
?kruskal.test
