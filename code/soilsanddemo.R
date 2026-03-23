#DOES SOIL INTERACT WITH ENDOPHYTE STATUS TO INFLUENCE DEMOGRAPHY?
library(tidyverse)
library(lme4)
library(scales)
library(lmerTest)
library(DHARMa)
library(emmeans)

#SETUP#_________________________________
#reading in soil data
soils <- read.csv ("data/reproduction_and_biomass - endo&soil.csv")

#coericing response variables into numeric form
soils$abg_mass_tot <- as.numeric(soils$abg_mass_tot, na.rm=T)

#coericing explanatory variables into factors
soils$Site <- as.factor(soils$Site)
soils$Site <- factor(soils$Site, c("SON","KER","BFL","BAS","COL","HUN","LAF"))

#creating colors for E+ & E-
endo_col <- c("tomato","cornflowerblue")
endo_est_col <- c("firebrick4","royalblue4")

#PRELIMINARY MODELS FOR BIOMASS#_________________________________
#biomass m1
biomass <- lm(abg_mass_tot~Site, data=soils)
summary(biomass)
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



soils %>% filter(Species=="ELVI") %>% count(Endo == "E-")
soils %>% filter(Endo=="E-") %>% group_by(Species) %>% summarise(count_E_minus=n())
soils %>% filter(Endo=="E+") %>% group_by(Site) %>% summarise(count_E_plus=n())

#biomass_3
biomass_3 <- lm(abg_mass_tot~Endo*Site*Species, data=soils)
summary(biomass_3)
anova(biomass_3)
#plot(biomass_3)
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


#biomass_4
biomass_4 <- lmer(abg_mass_tot~Endo*Site*Species+(1|Pop), data=soils)
summary(biomass_4)
anova(biomass_4)
#Strong evidence of effects of: Site and Site:Species interaction,
#Some evidence of effects of Species, Endo:Site interaction and Endo:Site:Species interaction
fixef(biomass_4)

#Ploting for AGHY
boxplot(abg_mass_tot~Endo*Site, data=soils, subset = Species =="AGHY", main="AGHY", col=endo_col)
points(1:14,c(fixef(biomass_4)[1],
              fixef(biomass_4)[1]+fixef(biomass_4)[2],
              fixef(biomass_4)[1]+fixef(biomass_4)[3],
              fixef(biomass_4)[1]+fixef(biomass_4)[3]+fixef(biomass_4)[2]+fixef(biomass_4)[10],
              fixef(biomass_4)[1]+fixef(biomass_4)[4],
              fixef(biomass_4)[1]+fixef(biomass_4)[4]+fixef(biomass_4)[2]+fixef(biomass_4)[11],
              fixef(biomass_4)[1]+fixef(biomass_4)[5],
              fixef(biomass_4)[1]+fixef(biomass_4)[5]+fixef(biomass_4)[2]+fixef(biomass_4)[12],
              fixef(biomass_4)[1]+fixef(biomass_4)[6],
              fixef(biomass_4)[1]+fixef(biomass_4)[6]+fixef(biomass_4)[2]+fixef(biomass_4)[13],
              fixef(biomass_4)[1]+fixef(biomass_4)[7],
              fixef(biomass_4)[1]+fixef(biomass_4)[7]+fixef(biomass_4)[2]+fixef(biomass_4)[14],
              fixef(biomass_4)[1]+fixef(biomass_4)[8],
              fixef(biomass_4)[1]+fixef(biomass_4)[8]+fixef(biomass_4)[2]+fixef(biomass_4)[15]
),col="midnightblue",bg=endo_est_col,pch=21,cex=2)

#Plotting for ELVI
boxplot(abg_mass_tot~Endo*Site, data=soils, subset = Species =="ELVI", main="EVLI", col=endo_col)
points(1:14,c(fixef(biomass_4)[1]+fixef(biomass_4)[9],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[2]+fixef(biomass_4)[16],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[17]+fixef(biomass_4)[3],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[17]+fixef(biomass_4)[3]+fixef(biomass_4)[2]+fixef(biomass_4)[10]+fixef(biomass_4)[23],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[18]+fixef(biomass_4)[4],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[18]+fixef(biomass_4)[4]+fixef(biomass_4)[2]+fixef(biomass_4)[11]+fixef(biomass_4)[24],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[19]+fixef(biomass_4)[5],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[19]+fixef(biomass_4)[5]+fixef(biomass_4)[2]+fixef(biomass_4)[12]+fixef(biomass_4)[25],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[20]+fixef(biomass_4)[6],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[20]+fixef(biomass_4)[6]+fixef(biomass_4)[2]+fixef(biomass_4)[13]+fixef(biomass_4)[26],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[21]+fixef(biomass_4)[7],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[21]+fixef(biomass_4)[7]+fixef(biomass_4)[2]+fixef(biomass_4)[14]+fixef(biomass_4)[27],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[22]+fixef(biomass_4)[8],
              fixef(biomass_4)[1]+fixef(biomass_4)[9]+fixef(biomass_4)[22]+fixef(biomass_4)[8]+fixef(biomass_4)[2]+fixef(biomass_4)[15]+fixef(biomass_4)[28]
),col="midnightblue",bg=endo_est_col,pch=21,cex=2)


#FINAL MODEL FOR BIOMASS#_________________________________
#biomass m2
#filtering for aghy
aghysoils<-soils %>% filter(Species=="AGHY")

biomass_2 <- lm(abg_mass_tot~Endo*Site, data=aghysoils)
summary(biomass_2)
anova(biomass_2)
# Post Hoc Comparisons
(hoc_site_mass <- emmeans(biomass_2, pairwise~Site))
(hoc_ensi_mass <- emmeans(biomass_2, pairwise~Endo | Site))

#plot(biomass_2)
boxplot(abg_mass_tot~Endo*Site, data=aghysoils,
        main="Effects of Endophyte Status and Soil on AGHY Biomass", 
        cex.main=1.05, col=endo_col)
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
              coef(biomass_2)[1]+coef(biomass_2)[8]+coef(biomass_2)[2]+coef(biomass_2)[14]
              ),col="midnightblue",bg=endo_est_col,pch=21,cex=2)
legend("topleft",
       legend=c("E-","E+"),
       pch=15,
       pt.cex=2,
       col=c("tomato", "cornflowerblue"),
       bty="n")


?legend

#MODEL FOR INFLO COUNT#_________________________________
#inflorescence count
aghysoils<-soils %>% filter(Species=="AGHY")
inflocount <- glm(n_Inflo~Endo*Site,family="poisson", data=aghysoils)
siminflo<-simulateResiduals(inflocount)
#plot(siminflo)
summary(inflocount)
anova(inflocount)

# Post Hoc Comparisons
(hoc_site_inflo <- emmeans(inflocount, pairwise~Site))
(hoc_ensi_inflo <- emmeans(inflocount, pairwise~Endo | Site))

coef(inflocount)

boxplot(n_Inflo~Endo*Site, data=aghysoils,
        main="Effects of Endophyte Status and Soil on AGHY Inflo Count", 
        cex.main=1.05, col=endo_col)
points(1:14, c(exp(coef(inflocount)[1]),
               exp(coef(inflocount)[1]+coef(inflocount)[2]),
               exp(coef(inflocount)[1]+coef(inflocount)[3]),
               exp(coef(inflocount)[1]+coef(inflocount)[3]+coef(inflocount)[2]+coef(inflocount)[9]),
               exp(coef(inflocount)[1]+coef(inflocount)[4]),
               exp(coef(inflocount)[1]+coef(inflocount)[4]+coef(inflocount)[2]+coef(inflocount)[10]),
               exp(coef(inflocount)[1]+coef(inflocount)[5]),
               exp(coef(inflocount)[1]+coef(inflocount)[5]+coef(inflocount)[2]+coef(inflocount)[11]),
               exp(coef(inflocount)[1]+coef(inflocount)[6]),
               exp(coef(inflocount)[1]+coef(inflocount)[6]+coef(inflocount)[2]+coef(inflocount)[12]),
               exp(coef(inflocount)[1]+coef(inflocount)[7]),
               exp(coef(inflocount)[1]+coef(inflocount)[7]+coef(inflocount)[2]+coef(inflocount)[13]),
               exp(coef(inflocount)[1]+coef(inflocount)[8]),
               exp(coef(inflocount)[1]+coef(inflocount)[8]+coef(inflocount)[2]+coef(inflocount)[14])),
       col="midnightblue",bg=endo_est_col,pch=21,cex=2)
legend("topleft",
       legend=c("E-","E+"),
       pch=15,
       pt.cex=2,
       col=c("tomato", "cornflowerblue"),
       bty="n")

#MODEL FOR TOTAL SPIKELET COUNT#_________________________________
#spikelet
tot_spikelets <- glm(tot_spikelet~Endo*Site,family="poisson", data=aghysoils)
simtot<-simulateResiduals(tot_spikelets)
#plot(siminflo)
summary(tot_spikelets)
anova(tot_spikelets)

# Post Hoc Comparisons
(hoc_site_spik <- emmeans(tot_spikelets, pairwise~Site))
(hoc_ensi_spik <- emmeans(tot_spikelets, pairwise~Endo | Site))

coef(tot_spikelets)

boxplot(tot_spikelet~Endo*Site, data=aghysoils,
        main="Effects of Endophyte Status and Soil on AGHY Total Spikelet Count", 
        cex.main=1.05, col=endo_col)
points(1:14, c(exp(coef(tot_spikelets)[1]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[2]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[3]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[3]+coef(tot_spikelets)[2]+coef(tot_spikelets)[9]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[4]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[4]+coef(tot_spikelets)[2]+coef(tot_spikelets)[10]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[5]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[5]+coef(tot_spikelets)[2]+coef(tot_spikelets)[11]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[6]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[6]+coef(tot_spikelets)[2]+coef(tot_spikelets)[12]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[7]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[7]+coef(tot_spikelets)[2]+coef(tot_spikelets)[13]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[8]),
               exp(coef(tot_spikelets)[1]+coef(tot_spikelets)[8]+coef(tot_spikelets)[2]+coef(tot_spikelets)[14])),
       col="midnightblue",bg=endo_est_col,pch=21,cex=2)
legend("topleft",
       legend=c("E-","E+"),
       pch=15,
       pt.cex=2,
       col=c("tomato", "cornflowerblue"),
       bty="n")

#MODEL FOR AVERAGE SPIKELET COUNT#_________________________________
#spikelet
avg_spikelets <- glm(avg_spikelet~Endo*Site,family="poisson", data=aghysoils)
simavg<-simulateResiduals(avg_spikelets)
#plot(siminflo)
summary(avg_spikelets)
anova(avg_spikelets)

# Post Hoc Comparisons
(hoc_site_spik <- emmeans(avg_spikelets, pairwise~Site))
(hoc_ensi_spik <- emmeans(avg_spikelets, pairwise~Endo | Site))

coef(avg_spikelets)

boxplot(avg_spikelet~Endo*Site, data=aghysoils,
        main="Effects of Endophyte Status and Soil on AGHY Average Spikelet Count", 
        cex.main=1.05, col=endo_col)
points(1:14, c(exp(coef(avg_spikelets)[1]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[2]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[3]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[3]+coef(avg_spikelets)[2]+coef(avg_spikelets)[9]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[4]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[4]+coef(avg_spikelets)[2]+coef(avg_spikelets)[10]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[5]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[5]+coef(avg_spikelets)[2]+coef(avg_spikelets)[11]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[6]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[6]+coef(avg_spikelets)[2]+coef(avg_spikelets)[12]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[7]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[7]+coef(avg_spikelets)[2]+coef(avg_spikelets)[13]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[8]),
               exp(coef(avg_spikelets)[1]+coef(avg_spikelets)[8]+coef(avg_spikelets)[2]+coef(avg_spikelets)[14])),
       col="midnightblue",bg=endo_est_col,pch=21,cex=2)
legend("topleft",
       legend=c("E-","E+"),
       pch=15,
       pt.cex=2,
       col=c("tomato", "cornflowerblue"),
       bty="n")



##QUESTIONS

#Would an AIC be appropriate?

#How do we account for the different birth dates? 
#Make a variable for age and then what? Center age??


