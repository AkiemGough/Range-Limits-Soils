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

#filtering for AGHY
#because ELVI doesn't have a useful sample size for E+ & E- comparison
aghysoils<-soils %>% filter(Species=="AGHY")


#MODEL FOR ABOVEGROUND BIOMASS#_________________________________
#writing the model
biomass_2 <- lm(abg_mass_tot~Endo*Site, data=aghysoils)

#checking residuals
simmass<-simulateResiduals(biomass_2)
plot(simmass)

#summary stats and analytic results
summary(biomass_2)
anova(biomass_2)
##SIGNIFICANT EFFECT OF SITE BUT NOT OF ENDO NOR ENDO:SITE INTERACTION

# Post Hoc Comparisons
(hoc_site_mass <- emmeans(biomass_2, pairwise~Site))
##SIGNIFICANT DIFFERENCES BETWEEN ALL SITES EXCEPT:
# SON:KER, SON:COL, KER:COL, BFL:BAS, BFL:LAF & HUN:LAF
(hoc_ensi_mass <- emmeans(biomass_2, pairwise~Endo | Site))
##NO SITE HAS SIGNIFICANT DIFFERENCES BETWEEN E+ & E-

#generating figure
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

#MODEL FOR INFLO COUNT#_________________________________
#writing model
inflocount <- glm(total_inflo~Endo*Site,family="poisson", data=aghysoils)

#checking residuals
siminflo<-simulateResiduals(inflocount)
plot(siminflo)

#summary stats and analytic results
summary(inflocount)
anova(inflocount)
##MARGINALLY SIGNIFICANT EFFECT OF SITE BUT NOT OF ENDO NOR ENDO:SITE INTERACTION

# Post Hoc Comparisons
(hoc_site_inflo <- emmeans(inflocount, pairwise~Site))
##ALL P-VALUES >0.1, NO CONVINCING DIFFERENCES
#KER HAS TOO MANY PLANTS THAT DID NOT FLOWER, MAYBE SHOULD REMOVE FROM REPRODUCTION ANALYSES
(hoc_ensi_inflo <- emmeans(inflocount, pairwise~Endo | Site))
##NO SITE HAS SIGNIFICANT DIFFERENCES BETWEEN E+ & E-


#generating figure
boxplot(total_inflo~Endo*Site, data=aghysoils,
        main="Effects of Endophyte Status and Soil on AGHY Inflo Count", 
        cex.main=1.05, col=endo_col)
coef(inflocount)
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
#writing model
tot_spikelets <- glm(tot_spikelet~Endo*Site,family="poisson", data=aghysoils)

#checking residuals
simspiktot<-simulateResiduals(tot_spikelets)
plot(simspiktot)

#summary stats and analytic results
summary(tot_spikelets)
anova(tot_spikelets)
##SIGNIFICANT EFFECTS OF ENDO, SITE & ENDO:SITE INTERACTION

# Post Hoc Comparisons
(hoc_site_spik <- emmeans(tot_spikelets, pairwise~Site))
##SIGNIFICANT DIFFERENCES BETWEEN ALL SITES EXCEPT FOR COMPARISONS WITH KER
#KER HAS TOO MANY PLANTS THAT DID NOT FLOWER, MAYBE SHOULD REMOVE FROM REPRODUCTION ANALYSES
(hoc_ensi_spik <- emmeans(tot_spikelets, pairwise~Endo | Site))
##ALL SITES HAVE SIGNIFICANT DIFFERENCES BETWEEN E+ & E- EXCEPT FOR COL

#generating figure
boxplot(tot_spikelet~Endo*Site, data=aghysoils,
        main="Effects of Endophyte Status and Soil on AGHY Total Spikelet Count", 
        cex.main=1.05, col=endo_col)
coef(tot_spikelets)
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
#writing model
avg_spikelets <- glm(avg_spikelet~Endo*Site,family="poisson", data=aghysoils)

#checking residuals
simspikavg<-simulateResiduals(avg_spikelets)
plot(simspikavg)

#summary stats and analytic results
summary(avg_spikelets)
anova(avg_spikelets)
##SIGNIFICANT EFFECTS OF ENDO, SITE & ENDO:SITE INTERACTION

# Post Hoc Comparisons
(hoc_site_spik <- emmeans(avg_spikelets, pairwise~Site))
##SIGNIFICANT DIFFERENCES BETWEEN ALL SITES EXCEPT FOR COMPARISONS WITH KER AND:
#SON:BFL, SON:HUN, SON:LAF, BFL:HUN, BFL:LAF & BAS:COL
#KER HAS TOO MANY PLANTS THAT DID NOT FLOWER, MAYBE SHOULD REMOVE FROM REPRODUCTION ANALYSES
(hoc_ensi_spik <- emmeans(avg_spikelets, pairwise~Endo | Site))
##ALL SITES HAVE SIGNIFICANT DIFFERENCES BETWEEN E+ & E- EXCEPT FOR KER & LAF

#generating figures
boxplot(avg_spikelet~Endo*Site, data=aghysoils,
        main="Effects of Endophyte Status and Soil on AGHY Average Spikelet Count", 
        cex.main=1.05, col=endo_col)
coef(avg_spikelets)
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