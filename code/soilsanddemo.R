#DOES SOIL INTERACT WITH ENDOPHYTE STATUS TO INFLUENCE DEMOGRAPHY?
library(tidyverse)
library(lme4)
library(scales)

soils <- read.csv ("data/reproduction_and_biomass - endo&soil.csv")

str(soils)

#coericing response variables into numeric form
soils$abg_mass_tot <- as.numeric(soils$abg_mass_tot, na.rm=T)

hist(resid(soils))

#coericing explanatory variables into factors
soils$Site <- as.factor(soils$Site)

#biomass m1
biomass <- lm(abg_mass_tot~Site, data=soils)
summary(abg_mass_tot)
anova(biomass)
plot(biomass)
boxplot(abg_mass_tot~Site, data=soils)
coef(biomass)
points(1:7,c(coef(biomass)[1],
             coef(biomass)[1]+coef(biomass)[2],
             coef(biomass)[1]+coef(biomass)[3],
             coef(biomass)[1]+coef(biomass)[4],
             coef(biomass)[1]+coef(biomass)[5],
             coef(biomass)[1]+coef(biomass)[6],
             coef(biomass)[1]+coef(biomass)[7]),col="deeppink4",pch=16,cex=2)

#biomass m2
biomass_2 <- lm(abg_mass_tot~Endo*Site, data=soils)
summary(biomass_2)
anova(biomass_2)
plot(biomass_2)
boxplot(abg_mass_tot~Endo*Site, data=soils)
coef(biomass_2)
points(1:14,c(coef(biomass_2)[1],
             coef(biomass_2)[1]+coef(biomass_2)[2],
             coef(biomass_2)[1]+coef(biomass_2)[3],
             coef(biomass_2)[1]+coef(biomass_2)[3]+coef(biomass_2)[2]+coef(biomass_2)[9],
             coef(biomass_2)[1]+coef(biomass_2)[4],
             coef(biomass_2)[1]+coef(biomass_2)[4]+coef(biomass_2)[2]+coef(biomass_2)[10],
             coef(biomass_2)[1]+coef(biomass_2)[5],
             coef(biomass_2)[1]+coef(biomass_2)[5]+coef(biomass_2)[2]+coef(biomass_2)[11],
             coef(biomass_2)[1]+coef(biomass_2)[6],
             coef(biomass_2)[1]+coef(biomass_2)[6]+coef(biomass_2)[2]+coef(biomass_2)[12],
             coef(biomass_2)[1]+coef(biomass_2)[7],
             coef(biomass_2)[1]+coef(biomass_2)[7]+coef(biomass_2)[2]+coef(biomass_2)[13],
             coef(biomass_2)[1]+coef(biomass_2)[8],
             coef(biomass_2)[1]+coef(biomass_2)[8]+coef(biomass_2)[2]+coef(biomass_2)[14]),col="deeppink4",pch=16,cex=2)

?boxplot

soils %>% group_by(Species) %>% filter(Endo="E-")

soils %>% filter(Species=="ELVI") %>% count(Endo == "E-")
soils %>% filter(Endo=="E-") %>% group_by(Species) %>% summarise(count_E_minus=n())

soils %>% filter(Species=="ELVI" & Endo=="E-" & abg_mass_tot > 0)

#biomass_3
biomass_3 <- lm(abg_mass_tot~Endo*Site*Species, data=soils)
summary(biomass_3)
anova(biomass_3)
#plot(biomass_3)
endo_col <- c("tomato","cornflowerblue")
endo_est_col <- c("firebrick4","royalblue4")
boxplot(abg_mass_tot~Endo*Site, data=soils, subset = Species =="AGHY", main="AGHY", col=endo_col)

coef(biomass_3)
points(1:14,c(coef(biomass_3)[1],
              coef(biomass_3)[1]+coef(biomass_3)[2],
              coef(biomass_3)[1]+coef(biomass_3)[3],
              coef(biomass_3)[1]+coef(biomass_3)[3]+coef(biomass_3)[2]+coef(biomass_3)[10],
              coef(biomass_3)[1]+coef(biomass_3)[4],
              coef(biomass_3)[1]+coef(biomass_3)[4]+coef(biomass_3)[2]+coef(biomass_3)[11],
              coef(biomass_3)[1]+coef(biomass_3)[5],
              coef(biomass_3)[1]+coef(biomass_3)[5]+coef(biomass_3)[2]+coef(biomass_3)[12],
              coef(biomass_3)[1]+coef(biomass_3)[6],
              coef(biomass_3)[1]+coef(biomass_3)[6]+coef(biomass_3)[2]+coef(biomass_3)[13],
              coef(biomass_3)[1]+coef(biomass_3)[7],
              coef(biomass_3)[1]+coef(biomass_3)[7]+coef(biomass_3)[2]+coef(biomass_3)[14],
              coef(biomass_3)[1]+coef(biomass_3)[8],
              coef(biomass_3)[1]+coef(biomass_3)[8]+coef(biomass_3)[2]+coef(biomass_3)[15]
              ),col="midnightblue",bg=endo_est_col,pch=21,cex=2)

boxplot(abg_mass_tot~Endo*Site, data=soils, subset = Species =="ELVI", main="EVLI", col=endo_col)
points(1:14,c(coef(biomass_3)[1]+coef(biomass_3)[9],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[2]+coef(biomass_3)[16],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[17]+coef(biomass_3)[3],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[17]+coef(biomass_3)[3]+coef(biomass_3)[2]+coef(biomass_3)[10]+coef(biomass_3)[23],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[18]+coef(biomass_3)[4],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[18]+coef(biomass_3)[4]+coef(biomass_3)[2]+coef(biomass_3)[11]+coef(biomass_3)[24],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[19]+coef(biomass_3)[5],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[19]+coef(biomass_3)[5]+coef(biomass_3)[2]+coef(biomass_3)[12]+coef(biomass_3)[25],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[20]+coef(biomass_3)[6],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[20]+coef(biomass_3)[6]+coef(biomass_3)[2]+coef(biomass_3)[13]+coef(biomass_3)[26],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[21]+coef(biomass_3)[7],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[21]+coef(biomass_3)[7]+coef(biomass_3)[2]+coef(biomass_3)[14]+coef(biomass_3)[27],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[22]+coef(biomass_3)[8],
              coef(biomass_3)[1]+coef(biomass_3)[9]+coef(biomass_3)[22]+coef(biomass_3)[8]+coef(biomass_3)[2]+coef(biomass_3)[15]+coef(biomass_3)[28]
              ),col="midnightblue",bg=endo_est_col,pch=21,cex=2)


# dropping 0s and recoding site factor to be dry to wet, not alphabetic
#biomass_4
biomass_4 <- lmer(abg_mass_tot~Endo*Site*Species+(1|Pop), data=soils)
summary(biomass_4)
anova(biomass_4)
boxplot(abg_mass_tot~Endo*Site, data=soils, subset = Species =="AGHY", main="AGHY", col=endo_col)
coef(biomass_4)
points(1:14,c(coef(biomass_4)[1],
              coef(biomass_4)[1]+coef(biomass_4)[2],
              coef(biomass_4)[1]+coef(biomass_4)[3],
              coef(biomass_4)[1]+coef(biomass_4)[3]+coef(biomass_4)[2]+coef(biomass_4)[10],
              coef(biomass_4)[1]+coef(biomass_4)[4],
              coef(biomass_4)[1]+coef(biomass_4)[4]+coef(biomass_4)[2]+coef(biomass_4)[11],
              coef(biomass_4)[1]+coef(biomass_4)[5],
              coef(biomass_4)[1]+coef(biomass_4)[5]+coef(biomass_4)[2]+coef(biomass_4)[12],
              coef(biomass_4)[1]+coef(biomass_4)[6],
              coef(biomass_4)[1]+coef(biomass_4)[6]+coef(biomass_4)[2]+coef(biomass_4)[13],
              coef(biomass_4)[1]+coef(biomass_4)[7],
              coef(biomass_4)[1]+coef(biomass_4)[7]+coef(biomass_4)[2]+coef(biomass_4)[14],
              coef(biomass_4)[1]+coef(biomass_4)[8],
              coef(biomass_4)[1]+coef(biomass_4)[8]+coef(biomass_4)[2]+coef(biomass_4)[15]
),col="midnightblue",bg=endo_est_col,pch=21,cex=2)





#inflorescence count
inflocount <- glmer(n_Inflo~Endo*Site*Species+(1|Pop),family="poisson", data=soils)
summary(inflocount)
anova(inflocount)





spikelets <- glmer(avg_spikelet~Endo*Species*Pop*Site*,family="poisson", data=soils)

##QUESTIONS

#Would an AIC be appropriate?

#Keep it simple
#maybe a log transformation is necessary but we could use a linear model, look at residuals see if reasonable

#Site is most rep of soil

#Which variable best represents soil? Pop or Site or both?

#What kind of figures are we imagining? Box plots?
#How do we account for the different birthdates? Make a variable for age and then what? Center age??

table(soils$Species)

boxplot(data=soils, abg_mass_sans_inflo ~ Endo*Site, col= c("magenta","aquamarine"))

?boxplot


#Could put in same model or two models for different species

#pop as random effect, which would be a lmm