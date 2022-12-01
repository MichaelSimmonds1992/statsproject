rm(list = ls())


## set working directory and import data
setwd("~/lund/stats and modelling/stats/croc_data")
read.csv("crocodile_data.csv", sep = "comma" head = T)

## logit functions
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))

## subset data and rename colmuns 
crocs <-  subset(crocodile_data, select=c('species','T','F','M'))
colnames(crocs)[4] = "Male"
colnames(crocs)[3] = "Female"
colnames(crocs)[2] = "Temperature"

## calculate total clutch size
crocs$total_clutchsize <- crocs$Male + crocs$Female 
## calculate the proportion of males and females in each clutch
crocs$proportion_males <- crocs$Male/crocs$total_clutchsize
crocs$proportion_female <- crocs$Female/crocs$total_clutchsize
## remove NAs
crocs <- na.exclude(crocs)
##subset data for each species
ca <- crocs[crocs$species == "Crocodylus acutus",] 
cm <- crocs[crocs$species == "Crocodylus moreletii",]
am <- crocs[crocs$species == "Alligator mississippiensis",]
cn <- crocs[crocs$species == "Crocodylus niloticus",]
cc <- crocs[crocs$species == "Caiman crocodilus",]
cj <- crocs[crocs$species == "Crocodylus johnsoni",]
cp <- crocs[crocs$species == "Crocodylus porosus",]
c_p <- crocs[crocs$species == "Crocodylus palustris",]
cl <- crocs[crocs$species == "Caiman latirostris",]
## plot data for all species
ggplot(crocs, aes(x=crocs$Temperature, y=crocs$proportion_males))+
  geom_point()
  
## fit a model for the species seperatly
m_ca <-  glm(ca$proportion_males ~ ca$Temperature, family=quasibinomial(link="logit"), weights=ca$total_clutchsize)
m_am <-  glm(am$proportion_males ~ am$Temperature, family=quasibinomial(link="logit"), weights=am$total_clutchsize)
m_cj <-  glm(cj$proportion_males ~ cj$Temperature, family=quasibinomial(link="logit"), weights=cj$total_clutchsize)
m_cm <-  glm(cm$proportion_males ~ cm$Temperature, family=quasibinomial(link="logit"), weights=cm$total_clutchsize)
m_cp <-  glm(cp$proportion_males ~ cp$Temperature, family=quasibinomial(link="logit"), weights=cp$total_clutchsize)
summary(m_ca)
summary(m_am)
summary(m_cj)
summary(m_cm)
summary(m_cp)

##combine to coefficents of each model
coefs <- rbind(summary(m_ca)$coef,  summary(m_cj)$coef, summary(m_cm)$coef, summary(m_cp)$coef, summary(m_am)$coef)

## produce the regression line for each species
x_pred_ca <-  seq(from=min(ca$Temperature), to=max(ca$Temperature), by=0.1)
y_hat_ca <-  coefs[1,1] + coefs[2,1]*x_pred_ca
p_hat_ca <-  invlogit(y_hat_ca)

x_pred_cj <-  seq(from=min(cj$Temperature), to=max(cj$Temperature), by=0.1)
y_hat_cj <-  coefs[3,1] + coefs[4,1]*x_pred_cj
p_hat_cj <-  invlogit(y_hat_cj)

x_pred_cm <-  seq(from=min(cm$Temperature), to=max(cm$Temperature), by=0.1)
y_hat_cm <-  coefs[5,1] + coefs[6,1]*x_pred_cm
p_hat_cm <-  invlogit(y_hat_cm)

x_pred_cp <-  seq(from=min(cp$Temperature), to=max(cp$Temperature), by=0.1)
y_hat_cp <-  coefs[7,1] + coefs[8,1]*x_pred_cp
p_hat_cp <-  invlogit(y_hat_cp)

x_pred_am <-  seq(from=min(am$Temperature), to=max(am$Temperature), by=0.1)
y_hat_am <-  coefs[9,1] + coefs[10,1]*x_pred_am
p_hat_am <-  invlogit(y_hat_am)

## calculate the temperature at which 50% of clutch is predicted to be male
b0overbx_ca <- -coefs[1,1]/coefs[2,1] 
b0overbx_cj <- -coefs[3,1]/coefs[4,1] 
b0overbx_cm <- -coefs[5,1]/coefs[6,1] 
b0overbx_cp <- -coefs[7,1]/coefs[8,1] 
b0overbx_am <- -coefs[9,1]/coefs[10,1] 

##pseudoRsquared

predict(m_am, type="response")
amRsquared <- cor(predict(m_am, type="response"), am$proportion_males)^2

predict(m_ca, type="response")
caRsquared <- cor(predict(m_ca, type="response"), ca$proportion_males)^2

predict(m_cj, type="response")
cjRsquared <- cor(predict(m_cj, type="response"), cj$proportion_males)^2

predict(m_cp, type="response")
cpRsquared <- cor(predict(m_cp, type="response"), cp$proportion_males)^2

predict(m_cm, type="response")
cmRsquared <- cor(predict(m_cm, type="response"), cm$proportion_males)^2

##bind used dataset back together to make a plot
final_crocs <- rbind(ca,cj,cm,cp,am)


##final plot
plot(final_crocs$Temperature, final_crocs$proportion_males, col="grey", pch=20,
     xlab = "Incubation temperature (°C)", ylab = "Proportion of males per clutch")
lines(x_pred_am, p_hat_am, col='red', lwd=2)
lines(x_pred_ca, p_hat_ca, col='blue', lwd=2)
lines(x_pred_cj, p_hat_cj, col='dark green',lwd=2)
lines(x_pred_cm, p_hat_cm, col='purple',lwd=2)
lines(x_pred_cp, p_hat_cp, col='black',lwd=2)
legend("topleft", c("Alligator mississippiensis", "Crocodylus acutus", "Crocodylus johnsoni", "Crocodylus moreletii", "Crocodylus porosus"),
                     col = c("red", "blue", "dark green","purple", "black"), pch=20)



