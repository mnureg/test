####--------------------------------------######
#     Pes participants                  ########
###---------------------------------------######

library(readr)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
#pardata <-read_csv("C:/Users/Mauricio/Google Drive/Manuscripts/Argentine PES participants/PES-Chaco-R-Mau Sep21-2017.csv") #for windows
pardata <- read_csv("~/Google Drive/Manuscripts/Argentine PES participants/PES-Chaco-R-Mau Sep21-2017.csv") #for Mac

# do some data cleaning and manipulation
#remove NA's
pardata<-pardata[complete.cases(pardata$ActividadMau2),]
#get rid of proyectos de formulacion because they last one year and bias the analyses
pardata<-subset(pardata, Modalidade!=c("Formulacion")) #exclude jatropha from analyses
pardata$ProvinceDifferent<-as.factor(pardata$ProvinceDifferent)
#add proportion of enrolled land
pardata$propland<-pardata$SUP_PLAN_H/pardata$SUP_PREDIO
pardata<-pardata[complete.cases(pardata$propland),]
pardata$logSuperficie<-log(pardata$SUP_PREDIO) #did log of land because some land is just huge
plot(pardata$propland,pardata$logSuperficie)
#add and define adverse selection
summary(pardata$DURACION) #we will define temporal adverse selection (TAS) as points that fall < mean
summary(pardata$Ind_Prod90) #we will define spatial adverse selection (SAS) as points that fall < mean. This layer 
#has ag suitabilities standarized for each province (i.e., a 50 in Salta may not be the same than in Chaco). Higher ag in > #
summary(pardata$NewAgSuit) #this layer has standarized values for all the region. Higher ag is in lower numbers (best ag is in 1)
pardata$NewAgSuit<-((pardata$NewAgSuit)*-1)+9 #invert values so that higher values = higher ag

pardata$AS<-NA#Adverse selection
pardata$TAS<-NA #Temporal adverse selection
pardata$SAS<-NA #Spatial Adverse Selection

#create AS variable in Time (TAS)
for (i in 1:length(pardata$DURACION)) {
  if (pardata$DURACION[i]<median(pardata$DURACION)) { #chosing median because it splits distribution in half
    pardata$TAS[i]=1 #it is 1 when we have adverse selection in Time
  } else {
    pardata$TAS[i]=0
  }
}
#create AS variable in Space (SAS)
for (i in 1:length(pardata$NewAgSuit)) {
  if (pardata$NewAgSuit[i]<median(pardata$NewAgSuit)) { #chosing median because it splits distribution in half
    pardata$SAS[i]=1 #it is 1 when we have adverse selection in Time
  } else {
    pardata$SAS[i]=0
  }
}


pardata$AS[pardata$TAS>pardata$SAS]=c("TAS") 
pardata$AS[pardata$TAS<pardata$SAS]=c("SAS") 
pardata$AS[((pardata$TAS)+(pardata$SAS))==2]=c("WORST") 
pardata$AS[((pardata$TAS)+(pardata$SAS))==0]=c("BEST") 
#set factors as factors
pardata$AS<-as.factor(pardata$AS)
pardata$ActividadMau2<-as.factor(pardata$ActividadMau2)
pardata$Modalidade<-as.factor(pardata$Modalidade)
#fix indigenous levels (we have Indigenous and indigenous)
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="Indigenous"] <- c("indigenous")
#Fix type of conservation projects
summary(pardata$Modalidade)
levels(pardata$Modalidade)[levels(pardata$Modalidade)=="NTFP"] <- c("NTFP-Silviculture")
levels(pardata$Modalidade)[levels(pardata$Modalidade)=="Silviculture"] <- c("NTFP-Silviculture")
levels(pardata$Modalidade)[levels(pardata$Modalidade)=="SilvoPastoril"] <- c("Silviculture-SilvoPastoril")
#Fix stakeholder levels 

levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="campesinos"] <- c("Indigenous-Campesinos")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="indigenous"] <- c("Indigenous-Campesinos")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="government"] <- c("Gov-Univ-Ngo")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="university"] <- c("Gov-Univ-Ngo")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="ngo"] <- c("Gov-Univ-Ngo")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="legal"] <- c("Legal-RealSt")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="real_estate"] <- c("Legal-RealSt")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="agriculture"] <- c("Agriculture")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="business, non-agro"] <- c("Business, non-agro")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="cattle"] <- c("Cattle")
levels(pardata$ActividadMau2)[levels(pardata$ActividadMau2)=="silviculture"] <- c("Silviculture")

#try Lyn's suggestion to split stakeholders in public vs private
pardata$ActividadMau3<-pardata$ActividadMau2
levels(pardata$ActividadMau3)[levels(pardata$ActividadMau3)=="agriculture"] <- c("private")
levels(pardata$ActividadMau3)[levels(pardata$ActividadMau3)=="business, non-agro"] <- c("private")
levels(pardata$ActividadMau3)[levels(pardata$ActividadMau3)=="indigenous-campesinos"] <- c("private")
levels(pardata$ActividadMau3)[levels(pardata$ActividadMau3)=="cattle"] <- c("private")
levels(pardata$ActividadMau3)[levels(pardata$ActividadMau3)=="gov-univ-ngo"] <- c("public")
levels(pardata$ActividadMau3)[levels(pardata$ActividadMau3)=="legal-realSt"] <- c("private")
levels(pardata$ActividadMau3)[levels(pardata$ActividadMau3)=="silviculture"] <- c("private")

#change 0 and 1 to words in Provincedifferent (absentees)
levels(pardata$ProvinceDifferent)[levels(pardata$ProvinceDifferent)=="0"] <- c("Local")
levels(pardata$ProvinceDifferent)[levels(pardata$ProvinceDifferent)=="1"] <- c("Absentee")

#how much money is allocated by each stakeholder
money.actors<-(aggregate(MontoTotal ~ActividadMau2, pardata, sum))
money.actors$prop<-NA

for (i in 1:length(money.actors$MontoTotal)) {
  money.actors$prop[i]<-(money.actors$MontoTotal[i]*100)/(sum(money.actors$MontoTotal))
}

# sort stakeholders decreasingly by money

money.actors
attach((money.actors))
money.actors <- money.actors[order(-MontoTotal),] 

ggplot(money.actors, aes(x = reorder(ActividadMau2, -MontoTotal), y = prop)) + 
  geom_bar(stat = "identity") +theme_bw()+ 
  xlab("Stakeholder categories")+ ylab("Proportion of money allocated") +
  theme(axis.text.x=element_text(angle=30, hjust=1))
#labs(x = "Stakeholder categories", y = "Proportion of money allocated")
#xlab("Stakeholder categories")+ ylab("Proportion of money allocated")

#how many plans have adverse selection
table(pardata$AS, pardata$Provincia)
#shade = data.frame(x1=c(0,32), x2=c(0,95), y1=c(0,21), y2=c(0,5))
pardata$Provincia<-as.factor(pardata$Provincia) #make sure province is a factor
ggplot(pardata, aes(NewAgSuit, DURACION))+ theme_bw()+
  geom_point(data=pardata, mapping=aes(x=NewAgSuit, y=(DURACION), 
                                       size=SUP_PLAN_H, shape=Provincia, colour=Provincia, alpha=1), position=position_jitter(h=.7))+
  #  geom_point(pardata, aes(y = DURACION), shape = factor(Provincia), alpha=.5, position=position_jitter(h=.5))+
  geom_hline(yintercept = median(pardata$DURACION))+
  geom_vline(xintercept = median(pardata$NewAgSuit))+
  xlab("Agriculture potential")+ ylab("Contract length")

#for absentee landowners
pardata$ProvinceDifferent<-as.factor(pardata$ProvinceDifferent) #make sure province is a factor
ggplot(pardata, aes(NewAgSuit, DURACION))+ theme_bw()+
  geom_point(data=pardata, mapping=aes(x=NewAgSuit, y=(DURACION), 
                                       size=1, shape=ProvinceDifferent, colour=ProvinceDifferent, alpha=1), position=position_jitter(h=.7))+
  geom_hline(yintercept = median(pardata$DURACION))+
  geom_vline(xintercept = median(pardata$NewAgSuit))+
  xlab("Agriculture potential")+ ylab("Contract length")


table(pardata$AS,pardata$Provincia)
#barplot of provinces and AS
data3<-data.frame(table(pardata$Provincia, pardata$AS))
names(data3)
library(plyr)
data3<-rename(data3, c("Var1"="Province", "Var2"="A.S." ))
#horizontal barplot using proportion of projects
ggplot(data3, aes(fill=A.S., y=Freq, x=(Province))) + #note that Freq is not frequency but number of cases
  geom_bar( stat="identity", position="fill")+
  scale_fill_grey()+theme_bw()+
  coord_flip() + theme(legend.position = "top") + xlab("Province")+ ylab("Proportion of projects")

#vertical barplot using total # of projects
ggplot(data3, aes(fill=A.S., y=Freq, x=(Province))) + #note that Freq is not frequency but number of cases
  geom_bar( stat="identity")+ #, position="fill"
  scale_fill_grey()+theme_bw()+
  theme(legend.position = "top") + xlab("Province")+ ylab("Number of projects")

#Now with actors instead of province
ggplot(pardata, aes(NewAgSuit, DURACION))+ theme_bw()+
  geom_point(data=pardata, mapping=aes(x=NewAgSuit, y=(DURACION), 
                                       size=SUP_PLAN_H, shape=ActividadMau2, colour=ActividadMau2, alpha=1), position=position_jitter(h=.7))+
  #  geom_point(pardata, aes(y = DURACION), shape = factor(Provincia), alpha=.5, position=position_jitter(h=.5))+
  geom_hline(yintercept = median(pardata$DURACION))+
  geom_vline(xintercept = median(pardata$NewAgSuit))+
  
  xlab("Agriculture potential")+ ylab("Contract length")

#How many landholding are big and have AS?
#create AS variable in Space (SAS)
pardata$big<-NA
for (i in 1:length(pardata$SUP_PLAN_H)) {
  if (pardata$SUP_PLAN_H[i]<median(pardata$SUP_PLAN_H)) { #chosing median because it splits distribution in half
    pardata$big[i]=1 #it is 1 when we have adverse selection in Time
  } else {
    pardata$big[i]=0
  }
}
table(pardata$AS,pardata$big) 

#Commence analyses!
#Check multicolineariy between predictor variables (chi square test of independence)
#size and socio-econ
boxplot(log(pardata$SUP_PLAN_H)~pardata$ActividadMau2)
rel.lm<-lm(SUP_PLAN_H~ActividadMau2, data=pardata) #remember that Multiple R-squared is the variance explained in our model
sqrt(summary(rel.lm)$r.squared) #correlations between observed size of land and predicted sizes of lands based in our model
#size and LU-plan type
boxplot(log(pardata$SUP_PLAN_H)~pardata$Modalidade)
rel.lm<-lm(SUP_PLAN_H~Modalidade, data=pardata) #remember that Multiple R-squared is the variance explained in our model
sqrt(summary(rel.lm)$r.squared) #correlations between observed size of land and predicted sizes of lands based in our model
#size and Absentee
boxplot(log(pardata$SUP_PLAN_H)~pardata$ProvinceDifferent)
rel.lm<-lm(SUP_PLAN_H~ProvinceDifferent, data=pardata) #remember that Multiple R-squared is the variance explained in our model
sqrt(summary(rel.lm)$r.squared) #correlations between observed size of land and predicted sizes of lands based in our model
#Chi square for potential categorical predictors
tbl2 = table(pardata$ActividadMau2, pardata$ProvinceDifferent) #correlated
tbl3 = table(pardata$ActividadMau2, pardata$Modalidade) #correlated
tbl4 = table(pardata$ProvinceDifferent, pardata$Modalidade) #not correlated

chisq2<-chisq.test(tbl2)
chisq3<-chisq.test(tbl3)
chisq4<-chisq.test(tbl4) #absentee landowners are not correlated with modalidad

library(corrplot)
corrplot(chisq2$residuals, is.cor = FALSE)
corrplot(chisq3$residuals, is.cor = FALSE)
corrplot(chisq4$residuals, is.cor = FALSE)

######   explore these relationships  ###########
#absentee and Socio-Econ activity
library(ggplot2)
runningcounts.df <- as.data.frame(table(pardata$ProvinceDifferent, pardata$ActividadMau2))
runningcounts.df<-rename(runningcounts.df, c("Var1"="Absentee_Landowner", "Var2"="Actors", "Freq"="Numb.Plans" ))
ggplot(runningcounts.df, aes(Absentee_Landowner, Actors)) +
  geom_tile(aes(fill = Numb.Plans), colour = "black") +
  scale_fill_gradient(low = "white", high = "steelblue")

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(panel.grid = element_blank())   
}
ggplot(runningcounts.df, aes(Absentee_Landowner, Actors)) +
  geom_point(aes(size = Numb.Plans, color = Numb.Plans, stat = "identity", position = "identity"), shape = 15) +
  scale_size_continuous(range = c(3,15)) + 
  scale_color_gradient(low = "white", high = "black") +
  theme_nogrid()
#horizontal barplot using proportion of projects
ggplot(runningcounts.df, aes(fill=Actors, y=Numb.Plans, x=(Absentee_Landowner))) + #note that Freq is not frequency but number of cases
  geom_bar( stat="identity", position="fill")+
  scale_fill_grey()+theme_bw()+
  theme(legend.position = "top") + xlab("Absentee_Landowner")+ ylab("Proportion of projects")


#Socio-Economic activity and type of plan
library(ggplot2)
runningcounts.df <- as.data.frame(table(pardata$ActividadMau2, pardata$Modalidade))
runningcounts.df<-rename(runningcounts.df, c("Var1"="Actors", "Var2"="L.U.Plan", "Freq"="Numb.Plans" ))
ggplot(runningcounts.df, aes(Actors, L.U.Plan)) +
  geom_tile(aes(fill = Numb.Plans), colour = "black") +
  scale_fill_gradient(low = "white", high = "steelblue")

ggplot(runningcounts.df, aes(Actors, L.U.Plan)) +
  geom_point(aes(size = Numb.Plans, color = Numb.Plans, stat = "identity", position = "identity"), shape = 15) +
  scale_size_continuous(range = c(3,15)) + 
  scale_color_gradient(low = "white", high = "black") +
  theme_nogrid()+theme(axis.text.x = element_text(angle = 15, hjust = 1))

#horizontal barplot using proportion of projects
ggplot(runningcounts.df, aes(fill=L.U.Plan, y=Numb.Plans, x=(Actors))) + #note that Freq is not frequency but number of cases
  geom_bar( stat="identity", position="fill")+
  scale_fill_grey()+theme_bw()+
  theme(legend.position = "top") + xlab("Actors")+ ylab("Proportion of projects")+theme_nogrid()+theme(axis.text.x = element_text(angle = 15, hjust = 1))

#remove NA's
pardata<-pardata[complete.cases(pardata$ProvinceDifferent),]

###--------------------    All provinces together -----------------------------#####

library(nnet) #Multinomial logistic regression
pardata$AS <- relevel(pardata$AS, ref = "BEST") #Set baseline comparison to BEST
#null and full
null <- multinom(AS ~ 1, data = pardata)
project.absent.size <- multinom(AS ~ -1+scale(SUP_PLAN_H)+Modalidade+ProvinceDifferent, data = pardata)
stake.size <- multinom(AS ~ ActividadMau2+scale(SUP_PLAN_H), data = pardata)
#full2<-  multinom(AS ~ ActividadMau3+scale(SUP_PLAN_H)+Modalidade, data = pardata) #using Ly's sugestions of splitting data into priv vs pub
#single variable
absent<- multinom(AS ~ -1+ProvinceDifferent, data = pardata)
stake<- multinom(AS ~ ActividadMau2, data = pardata)
#stake2<- multinom(AS ~ ActividadMau3, data = pardata) #using Ly's sugestions of splitting data into priv vs pub
size<-multinom(AS ~ scale(SUP_PLAN_H), data = pardata)
project <- multinom(AS ~ Modalidade, data = pardata)
#meaningfull comparisons 2 variables
project.size <- multinom(AS ~ Modalidade+scale(SUP_PLAN_H), data = pardata)
project.absent <- multinom(AS ~ Modalidade+ProvinceDifferent, data = pardata)
size.absent <- multinom(AS ~ scale(SUP_PLAN_H)+ProvinceDifferent, data = pardata)
#model selection
library(MuMIn)
model.sel(null, project.absent.size, stake.size, absent, stake, project, size, project.size, project.absent, size.absent) 
#lets look at project
summary(project.absent.size)
confint(project.absent.size, method="Wald")
library(stargazer)
stargazer(project.absent.size, type="html", out="multi1.htm") #exports table as a html called multi1.html
stargazer(project.absent.size, type="text")
multi1.rrr = exp(coef(project.absent.size)) #for full model
stargazer(project.absent.size,type="html", coef=list(multi1.rrr), p.auto=FALSE,  out="multi1.htm")
multi1.rrr = exp(coef(absent)) #For absentee model
stargazer(absent, type="html", coef=list(multi1.rrr), p.auto=FALSE,  out="multi1.htm")

confint(absent, method="Wald")
#relevel full model
pardata$AS2 <- relevel(pardata$AS, ref = "TAS")
project.absent.size2 <- multinom(AS ~ -1+scale(SUP_PLAN_H)+Modalidade+ProvinceDifferent, data = pardata)
confint(project.absent.size2, method="Wald")

#look at data results
#library(effects)
#fit.eff <- Effect("Modalidade", "project.absent")
#data.frame(fit.eff$model.matrix, fit.eff$prob, fit.eff$lower.prob, 
#          fit.eff$upper.prob)
#plot(fit.eff)
#head(pp <- fitted(project.size)) #predicted probabilities from model results

#plot ggplot
###                               Project + size                                        ####
#changes in predicted probability associated with one of our two variables
dses <- data.frame(Modalidade = c("Conservation", "NTFP-Silviculture", "Restoration", "Silviculture-SilvoPastoril"), 
                   SUP_PLAN_H = mean(pardata$SUP_PLAN_H))
predict(project.size, newdata = dses, "probs")
#use the predicted probabilities is to look at the averaged predicted probabilities for different
#values of the continuous predictor variable write within each level of ses.
dwrite <- data.frame(Modalidade = rep(c("Conservation", "NTFP-Silviculture", "Restoration", "Silviculture-SilvoPastoril"),
                                      each = 100), SUP_PLAN_H =seq(min(pardata$SUP_PLAN_H),max(pardata$SUP_PLAN_H),
                                                                   length.out=100) )
## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(project.size, newdata = dwrite, type = "probs", se = TRUE))
lpp <- melt(pp.write, id.vars = c("Modalidade", "SUP_PLAN_H"), value.name = "probability")
head(lpp) 

plot1<-ggplot(lpp, aes(x = SUP_PLAN_H, y = probability, linetype = Modalidade, colour = Modalidade, size=Modalidade)) + 
  geom_line() + 
  facet_grid(variable ~., scales = "free")+ theme_bw()+ theme(legend.justification = "top")+
  scale_linetype_manual(values=c("dotdash", "dotted", "solid", "longdash"))+
  scale_size_manual(values=c(0.8, 0.8,0.8,0.8))+ 
  #scale_color_manual(values=c("#999999","#56B4E9", "#F0E442" , "#D55E00"))+
  xlab("Project size (Ha)")+ ylab("Probability")
#theme(legend.position = c(0.77, 1.02))

###                               Absent + size                                        ####
#changes in predicted probability associated with one of our two variables
dses <- data.frame(ProvinceDifferent =c("Local", "Absentee"), 
                   SUP_PLAN_H = mean(pardata$SUP_PLAN_H))
predict(size.absent, newdata = dses, "probs")
#use the predicted probabilities is to look at the averaged predicted probabilities for different
#values of the continuous predictor variable write within each level of ses.
dwrite <- data.frame(ProvinceDifferent = (rep(c("Local", "Absentee"),
                                              each = 100)),
                     SUP_PLAN_H =seq(min(pardata$SUP_PLAN_H),max(pardata$SUP_PLAN_H),
                                     length.out=100))
## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(size.absent, newdata = dwrite, type = "probs", se = TRUE))
lpp <- melt(pp.write, id.vars = c("ProvinceDifferent", "SUP_PLAN_H"), value.name = "probability")
head(lpp) 

plot2<-ggplot(lpp, aes(x = SUP_PLAN_H, y = probability, linetype = ProvinceDifferent, colour = ProvinceDifferent, size=ProvinceDifferent)) + 
  geom_line() + 
  facet_grid(variable ~., scales = "free")+ theme_bw()+ theme(legend.justification = "top")+
  scale_linetype_manual(values=c("dotdash", "dotted", "solid", "longdash"))+
  scale_size_manual(values=c(0.8, 0.8,0.8,0.8))+
  #scale_color_manual(values=c("#999999","#56B4E9", "#F0E442" , "#D55E00"))+
  xlab("Project size (Ha)")+ ylab("Probability")
#theme(legend.position = c(0.77, 1.02))

require(gridExtra)
grid.arrange(plot1, plot2, ncol=1)

#Pearson's chi square test to see if AS varies by province 
chisq.test(pardata$AS,pardata$Provincia)

#How many stakeholders have adverse selection
data2<-data.frame(table(pardata$ActividadMau2,pardata$AS))
names(data2)
library(plyr)
data2<-rename(data2, c("Var1"="Actors", "Var2"="A.S." ))

ggplot(data2, aes(fill=A.S., y=Freq, x=(Actors))) + #note that Freq is not frequency but number of cases
  geom_bar( stat="identity", position="fill")+
  scale_fill_grey()+theme_bw()+
  coord_flip() + theme(legend.position = "top") + xlab("Actors")+ ylab("Proportion of projects")

# Mosaic Plot Example
library(vcd)
mosaic(~ ProvinceDifferent + Modalidade + AS, data = pardata,
       main = "Survival on the Titanic", shade = TRUE, legend = TRUE)


###--------------------    Salta                   -----------------------------#####
salta.data<-subset(pardata, Provincia=="Salta")
s.null <- multinom(AS ~ 1, data = salta.data)
s.full <- multinom(AS ~ ActividadMau2+propland+ProvinceDifferent+Modalidade, data = salta.data)
#single variable
s.stake<- multinom(AS ~ ActividadMau2, data = salta.data)
s.prop<-multinom(AS ~ propland, data = salta.data)
s.absent<- multinom(AS ~ ProvinceDifferent, data = salta.data)
s.project <- multinom(AS ~ Modalidade, data = salta.data)
#meaningfull comparisons 2 variables
s.project.prop <- multinom(AS ~ Modalidade+propland, data = salta.data)
s.project.absent <- multinom(AS ~ Modalidade+ProvinceDifferent, data = salta.data)
s.project.stake <- multinom(AS ~ Modalidade+ActividadMau2, data = salta.data)
s.prop.absent <- multinom(AS ~ propland+ProvinceDifferent, data = salta.data)
s.prop.stake <- multinom(AS ~ propland+ActividadMau2, data = salta.data)
s.absent.stake <- multinom(AS ~ ProvinceDifferent+ActividadMau2, data = salta.data)
#meaningfull comparisons 3 variables
s.notProject <- multinom(AS ~ ActividadMau2+propland+ProvinceDifferent, data = salta.data)
s.notStake <- multinom(AS ~ propland+ProvinceDifferent+Modalidade, data = salta.data)
s.notAbscent <- multinom(AS ~ ActividadMau2+propland+Modalidade, data = salta.data)
s.notProp <- multinom(AS ~ ActividadMau2+ProvinceDifferent+Modalidade, data = salta.data)
#model selection
library(MuMIn)
model.sel(s.null, s.full, s.stake, s.prop,absent, s.project, s.project.prop,
          s.project.absent, s.project.stake, s.prop.absent,
          s.prop.stake, s.absent.stake, s.notProject, s.notStake, s.notAbscent, s.notProp)
###--------------------    Formosa                   -----------------------------#####

###--------------------    Chaco                   -----------------------------#####

###--------------------    Stgo del Estero           -----------------------------#####



#Exploratory plots and analyses I did before   -------------------------------------------########

# order stakeholders decreasing
pardata <- within(pardata, 
                  ActividadMau2 <- factor(ActividadMau2, 
                                          levels=names(sort(table(ActividadMau2), 
                                                            decreasing=TRUE))))


library(ggplot2)
library(gridExtra)
#subset data per province
chaco.data<-subset(pardata, Provincia=="Chaco")
salta.data<-subset(pardata, Provincia=="Salta")
formosa.data<-subset(pardata, Provincia=="Formosa")
stgo.data<-subset(pardata, Provincia=="Santiago del Estero")


#Gender & Number of proyects
gender.all<-ggplot(pardata, aes(Gender))+geom_bar()
gender.chaco<-ggplot(chaco.data, aes(Gender))+geom_bar()
gender.salta<-ggplot(salta.data, aes(Gender))+geom_bar()
gender.formosa<-ggplot(formosa.data, aes(Gender))+geom_bar()
gender.stgo<-ggplot(stgo.data, aes(Gender))+geom_bar()
grid.arrange(gender.all,gender.chaco, gender.salta,  gender.formosa, gender.stgo,  
             ncol=3, nrow=2)
#Stakeholders & Number of proyects
stake.all<-ggplot(pardata, aes(ActividadMau2))+geom_bar()+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.chaco<-ggplot(chaco.data, aes(ActividadMau2))+geom_bar()+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.salta<-ggplot(salta.data, aes(ActividadMau2))+geom_bar()+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.formosa<-ggplot(formosa.data, aes(ActividadMau2))+geom_bar()+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.stgo<-ggplot(stgo.data, aes(ActividadMau2))+geom_bar()+theme(axis.text.x=element_text(angle=45, hjust=1))
grid.arrange(stake.all,stake.chaco, stake.salta,  stake.formosa, stake.stgo,  
             ncol=3, nrow=2)

#Proportion of land enrolled and gender
gender.land.all<-ggplot(pardata, aes(x=Gender, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.land.chaco<-ggplot(chaco.data, aes(Gender, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.land.salta<-ggplot(salta.data, aes(Gender, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.land.formosa<-ggplot(formosa.data, aes(Gender, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.land.stgo<-ggplot(stgo.data, aes(Gender, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
grid.arrange(gender.land.all,gender.land.chaco, gender.land.salta,  
             gender.land.formosa, gender.land.stgo,  
             ncol=3, nrow=2)

#Proportion of land enrolled and stakeholders
stake.land.all<-ggplot(pardata, aes(x=ActividadMau2, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.land.chaco<-ggplot(chaco.data, aes(ActividadMau2, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.land.salta<-ggplot(salta.data, aes(ActividadMau2, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.land.formosa<-ggplot(formosa.data, aes(ActividadMau2, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.land.stgo<-ggplot(stgo.data, aes(ActividadMau2, y=propland))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
grid.arrange(stake.land.all,stake.land.chaco, stake.land.salta,  
             stake.land.formosa, stake.land.stgo,  
             ncol=3, nrow=2)

#Enrollment length and gender
gender.length.all<-ggplot(pardata, aes(x=Gender, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.length.chaco<-ggplot(chaco.data, aes(Gender, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.length.salta<-ggplot(salta.data, aes(Gender, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.length.formosa<-ggplot(formosa.data, aes(Gender, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.length.stgo<-ggplot(stgo.data, aes(Gender, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
grid.arrange(gender.length.all,gender.length.chaco, gender.length.salta,  
             gender.length.formosa, gender.length.stgo,  
             ncol=3, nrow=2)

#Enrollment length and stakeholders
stake.length.all<-ggplot(pardata, aes(x=ActividadMau2, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.length.chaco<-ggplot(chaco.data, aes(ActividadMau2, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.length.salta<-ggplot(salta.data, aes(ActividadMau2, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.length.formosa<-ggplot(formosa.data, aes(ActividadMau2, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.length.stgo<-ggplot(stgo.data, aes(ActividadMau2, y=DURACION))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
grid.arrange(stake.length.all,stake.length.chaco, stake.length.salta,  
             stake.length.formosa, stake.length.stgo,  
             ncol=3, nrow=2)

#ag suitability and gender
gender.ag.all<-ggplot(pardata, aes(x=Gender, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.ag.chaco<-ggplot(chaco.data, aes(Gender, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.ag.salta<-ggplot(salta.data, aes(Gender, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.ag.formosa<-ggplot(formosa.data, aes(Gender, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
gender.ag.stgo<-ggplot(stgo.data, aes(Gender, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)
grid.arrange(gender.ag.all,gender.ag.chaco, gender.ag.salta,  
             gender.ag.formosa, gender.ag.stgo,  
             ncol=3, nrow=2)

#Ag Suitability and stakeholders
stake.ag.all<-ggplot(pardata, aes(x=ActividadMau2, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.ag.chaco<-ggplot(chaco.data, aes(ActividadMau2, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.ag.salta<-ggplot(salta.data, aes(ActividadMau2, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.ag.formosa<-ggplot(formosa.data, aes(ActividadMau2, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
stake.ag.stgo<-ggplot(stgo.data, aes(ActividadMau2, y=Ind_Prod90))+geom_violin()+ geom_jitter(height = 0, width = 0.1)+theme(axis.text.x=element_text(angle=45, hjust=1))
grid.arrange(stake.ag.all,stake.ag.chaco, stake.ag.salta,  
             stake.ag.formosa, stake.ag.stgo,  
             ncol=3, nrow=2)

#Ag Suitability and proportion of land
ag.prop<-ggplot(pardata, aes(x=propland, y=logSuperficie)) +geom_point(aes(colour = factor(Provincia)))

#Ag Suitability and lenght of enrollment,
prop.time<-ggplot(pardata, aes(x=propland, y=ActividadMau2)) +geom_point(aes(colour = factor(Provincia)))

#Where do people live? WordCloud
library(tm)
library(SnowballC)
library(wordcloud)

StgoCorpus <- Corpus(VectorSource(stgo.data$Prov2))
SaltaCorpus <- Corpus(VectorSource(salta.data$Prov2))
FormosaCorpus <- Corpus(VectorSource(formosa.data$Prov2))
ChacoCorpus <- Corpus(VectorSource(chaco.data$Prov2))

wordcloud(StgoCorpus, max.words = 100, random.order = FALSE)
wordcloud(SaltaCorpus, max.words = 100, random.order = FALSE)
wordcloud(FormosaCorpus, max.words = 100, random.order = FALSE)
wordcloud(ChacoCorpus, max.words = 100, random.order = FALSE)

StgoCorpus <- Corpus(VectorSource(stgo.data$Modalidade))
SaltaCorpus <- Corpus(VectorSource(salta.data$Modalidade))
FormosaCorpus <- Corpus(VectorSource(formosa.data$Modalidade))
ChacoCorpus <- Corpus(VectorSource(chaco.data$Modalidade))

wordcloud(StgoCorpus, max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(SaltaCorpus, max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(FormosaCorpus, max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(ChacoCorpus, max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
