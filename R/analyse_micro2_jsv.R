# Tentative d'analyses des premiers résultats, manip Jessica Save Garrido , microO2
# 8/11/2017


##########Fonction preparée############


microO2_import2  <- function(file){
  X <- read.table(file = file, skip = 13, sep = '\t', header = TRUE)
  X <- X[ , c(1:3,5, 13, 15)]
  X <- tidyr::unite(X, Date, c(1,2), sep = " ")
  X$Date <- as.POSIXct(X$Date, format = "%d.%m.%Y %H:%M:%S")
  X <- dplyr::rename( X, temps = Time..s., O2 = Ch1, P = "X.mbar.", T = "X.C...C.")
}
########### traitement des premières données############
########### lumière ON/OFF############
#package employé durant l'analyse

library(readr)
library(tidyverse)

#importation
t43 <-microO2_import2(file = "data/T43/T430311.txt")
#creation d'une nouvelle variable en character
t43$time <-as.character(t43$Date)

#importation et transfo de la date 
T43_light <- read_csv("data/T43/T430311_light.csv")
T43_light%>%rename(Date = time)->T43_light
T43_light$time <- as.character(T43_light$Date)


#objectif faire coller le document sur le temps et le document sur l'éclairage
t43_comb <- left_join(t43, T43_light, by = "time")
t43_comb %>% fill(light , .direction = "down")->t43_comb

#probleme spécifique à ce jeu de données
#petite correction supplémentaires car jessica a d'abord lancé le programme de lumière avant l'enregistrement de la sond
t43_comb$light[is.na(t43_comb$light)] <- 1
t43_comb$light <- as.factor(t43_comb$light)

# petit graphique des données avec la lumière 
ggplot(data = t43_comb, mapping = aes(x = temps, y = O2, color = light))+
  geom_line(aes(group = 1))


# extraction du tableau complet le temps en seconde entre l'allumage et l'éclairage 
t43_comb%>%filter(!is.na(Date.y)) -> test
x <- test$temps


## Tentons de calculer des régressions linéaires 


library("dplyr")
summary(lm1 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[1]+2) & temps < (x[1]+22))))
lm1 %>% (function(lm, model = lm[["model"]], vars = names(model))
  ggplot(model, aes_string(x = vars[2], y = vars[1])) +
    geom_point() + stat_smooth(method = "lm", formula = y ~ x))

#analyses des résidus
#plot(lm., which = 1)
lm1 %>% qplot(.fitted, .resid, data = .) +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted")

#plot(lm., which = 2)
lm1 %>% qplot(sample = .stdresid, data = .) +
  geom_abline(intercept = 0, slope = 1, colour = "darkgray") +
  xlab("Theoretical quantiles") +
  ylab("Standardized residuals") +
  ggtitle("Normal Q-Q")

#plot(lm., which = 3)
lm1 %>% qplot(.fitted, sqrt(abs(.stdresid)), data = .) +
  geom_smooth(se = FALSE) +
  xlab("Fitted values") +
  ylab(expression(bold(sqrt(abs("Standardized residuals"))))) +
  ggtitle("Scale-Location")

#calcul des 9 regressions

summary(lm1 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[1]+2) & temps < (x[1]+22))))
lm1$coefficients[[2]] -> a1
summary(lm2 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[2]+2) & temps < (x[2]+22))))
lm2$coefficients[[2]] -> a2
summary(lm3 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[3]+2) & temps < (x[3]+22))))
lm3$coefficients[[2]] -> a3
summary(lm4 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[4]+2) & temps < (x[4]+22))))
lm4$coefficients[[2]] -> a4
summary(lm5 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[5]+2) & temps < (x[5]+22))))
lm5$coefficients[[2]] -> a5
summary(lm6 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[6]+2) & temps < (x[6]+22))))
lm6$coefficients[[2]] -> a6
summary(lm7 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[7]+2) & temps < (x[7]+22))))
lm7$coefficients[[2]] -> a7
summary(lm8 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[8]+2) & temps < (x[8]+22))))
lm8$coefficients[[2]] -> a8
summary(lm9 <- lm(O2 ~ temps, data = filter(t43_comb, temps > (x[9]+2) & temps < (x[9]+22))))
lm9$coefficients[[2]] -> a9
#tableau des 9 régresssions uniquement coefficient 
test <-data_frame(model = c("lm1" , "lm2" , "lm3", "lm4", "lm5","lm6" , "lm7", "lm8", "lm9") , coef = c(a1, a2, a3, a4, a5, a6, a7, a8, a9 ))
mean(test)
#visualisation graphique
p6 <-ggplot(data = t43_comb, mapping = aes(x = temps, y = O2, color = light))+
  geom_line(aes(group = 1)) +
  geom_smooth(data = filter(t43_comb, temps > (x[1]+2) & temps < (x[1]+12)), method = "lm" ,formula = y ~ x, color = 2) +
  geom_smooth(data = filter(t43_comb, temps > (x[2]+2) & temps < (x[2]+12)), method = "lm" ,formula = y ~ x, color = 3) +
  geom_smooth(data = filter(t43_comb, temps > (x[3]+2) & temps < (x[3]+12)), method = "lm" ,formula = y ~ x, color = 2) +
  geom_smooth(data = filter(t43_comb, temps > (x[4]+2) & temps < (x[4]+12)), method = "lm" ,formula = y ~ x, color = 3) +
  geom_smooth(data = filter(t43_comb, temps > (x[5]+2) & temps < (x[5]+12)), method = "lm" ,formula = y ~ x, color = 2) +
  geom_smooth(data = filter(t43_comb, temps > (x[6]+2) & temps < (x[6]+12)), method = "lm" ,formula = y ~ x, color = 3) +
  geom_smooth(data = filter(t43_comb, temps > (x[7]+2) & temps < (x[7]+12)), method = "lm" ,formula = y ~ x, color = 2) +
  geom_smooth(data = filter(t43_comb, temps > (x[8]+2) & temps < (x[8]+12)), method = "lm" ,formula = y ~ x, color = 3) +
  geom_smooth(data = filter(t43_comb, temps > (x[9]+2) & temps < (x[9]+12)), method = "lm" ,formula = y ~ x, color = 2)
p6

# test sur t34 

t34 <-microO2_import2(file = "data/T34/T342710.txt")
summary(lm. <- lm(O2 ~ temps, data = filter(t34, temps >(120) & temps < (120+20))))
summary(lm. <- lm(O2 ~ temps, data = filter(t34, temps >(700) & temps < (700+20))))

0.0007348/-0.0519244

p2 <-ggplot(data = t34, mapping = aes(x = temps, y = O2))+
  geom_line()+
  geom_smooth(data=filter(t34, temps >(120) & temps < (120+20)), method = "lm" ,formula = y ~ x, color = 2)+
  geom_smooth(data=filter(t34, temps >(700) & temps < (700+20)), method = "lm" ,formula = y ~ x, color = 2)

p2
#on a un coefficient de pente de -0.05 pour les deux 

#test  t35
t35 <-microO2_import2(file = "data/T35/T352710.txt")

p3 <-ggplot(data = t35, mapping = aes(x = temps, y = O2))+
  geom_line() +
  geom_smooth(data=filter(t35, temps >(90) & temps < (90+20)), method = "lm" ,formula = y ~ x, color = 2)+
  geom_smooth(data=filter(t35, temps >(625) & temps < (625+20)), method = "lm" ,formula = y ~ x, color = 2)

p3

summary(lm. <- lm(O2 ~ temps, data = filter(t35, temps >(90) & temps < (90+20))))
summary(lm. <- lm(O2 ~ temps, data = filter(t35, temps >(625) & temps < (625+20))))


# on a un coefficient de pente de - 0.026 et 0.027 ce qui reste assez similaire


#test  t37
t37 <-microO2_import2(file = "data/T37/T372710.txt")

p4 <-ggplot(data = t37, mapping = aes(x = temps, y = O2))+
  geom_line()  +
  geom_smooth(data=filter(t37, temps >(110) & temps < (110+20)), method = "lm" ,formula = y ~ x, color = 2)+
  geom_smooth(data=filter(t37, temps >(605) & temps < (605+20)), method = "lm" ,formula = y ~ x, color = 2)
p4

summary(lm. <- lm(O2 ~ temps, data = filter(t37, temps >(110) & temps < (110+20))))
summary(lm. <- lm(O2 ~ temps, data = filter(t37, temps >(605) & temps < (605+20))))


#test T38
t38 <-microO2_import2(file = "data/T38/T382710.txt")

p5 <-ggplot(data = t38, mapping = aes(x = temps, y = O2))+
  geom_line() +
  geom_smooth(data=filter(t38, temps >(105) & temps < (105+20)), method = "lm" ,formula = y ~ x, color = 2)+
  geom_smooth(data=filter(t38, temps >(605) & temps < (605+20)), method = "lm" ,formula = y ~ x, color = 2)
p5

summary(lm. <- lm(O2 ~ temps, data = filter(t38, temps >(105) & temps < (105+20))))
summary(lm. <- lm(O2 ~ temps, data = filter(t38, temps >(605) & temps < (605+20))))


#test T31
t31 <-microO2_import2(file = "data/T31/T312710.txt")

p1 <- ggplot(data = t31, mapping = aes(x = temps, y = O2))+
  geom_line()+
  geom_smooth(data=filter(t31, temps >(120) & temps < (120+20)), method = "lm" ,formula = y ~ x, color = 2)+
  geom_smooth(data=filter(t31, temps >(790) & temps < (790+20)), method = "lm" ,formula = y ~ x, color = 2)

p1 

summary(lm. <- lm(O2 ~ temps, data = filter(t31, temps >(120) & temps < (120+20))))
summary(lm. <- lm(O2 ~ temps, data = filter(t31, temps >(790) & temps < (790+20))))


library(ggpubr)

lumiere <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2,labels = "AUTO")
lumiere

# Voir remarque dans carnet de reflexions


##########################################################################################################

######Placement de la sonde#############

library(tidyverse)
library(ggpubr)

#T40

#recule la sonde progressivement 

t40 <-microO2_import2(file = "data/T40/T40.txt")


vec <-data.frame(temps = c(0, 380), dist=c("0.00mm", "0.253mm"))
p5 <- ggplot(t40)+
  geom_line(mapping = aes(x = temps, y = O2))+
  geom_vline(xintercept = vec$temps, linetype = "dotted")+
  annotate(geom = "text", x = (vec$temps+50), y =11.5, label = vec$dist )

#t17

t17 <-microO2_import2(file = "data/T17/T172010.txt")

vec <-c(120, 200, 300, 375)
p1 <- ggplot(t17)+
  geom_line(mapping = aes(x = temps, y = O2)) +
  geom_vline(xintercept = c(120, 200, 300, 375), linetype="dotted") +
  annotate(geom = "text", x=(vec+30), y = 7.6,label = c("0mm","0mm","0mm","0mm" ))

#t19

t19 <-microO2_import2(file = "data/T19/T192010.txt")


vec <-data.frame(temps = c(115, 200, 350), dist=c("0mm", "4mm", "0mm"))
p2 <-ggplot(t19)+
  geom_line(mapping = aes(x = temps, y = O2))+
  geom_vline(xintercept = vec$temps, linetype = "dotted")+
  annotate(geom = "text", x = c(150, 275, 475), y =9.2, label = vec$dist ) +
  labs(x = "Oxygène dissous (mg/L)", y = "Temps (s)")
t20 <-microO2_import2(file = "data/T20/T202010.txt")


vec <-c(126, 350)
p3 <- ggplot(t20)+
  geom_line(mapping = aes(x = temps, y = O2))+
  geom_vline(xintercept = vec, linetype = "dotted")+
  annotate(geom = "text", x=(vec+30), y = 7.9,label = c("0mm","0mm"))


t21 <-microO2_import2(file = "data/T21/T212010.txt")

vec <-c(150, 700)
p4<-ggplot(t21)+
  geom_line(mapping = aes(x = temps, y = O2))+
  geom_vline(xintercept = vec, linetype = "dotted")+
  annotate(geom = "text", x=(vec+30), y = 9.6,label = c("0mm","0mm"))
  


position <- ggarrange(p1, p2, p3, p4, p5, ncol = 3, nrow = 2,labels = "AUTO")

position

lumiere
