# -------------------------------------------------------------------------------------
# Scriptet til denne uge er ganske langt. Nederst er der nogle plots
# som er ret avancerede. Hop dem over, hvis du stadig famler med R, 
# det er ikke vigtigt at du kan lave dem paa nuvaerende tidspunkt

# Jeg har forsoegt at forsimple koden lidt i forhold til Lenes slides. 
# Naar du ser forelaesningsvideoerne (som jo er baseret paa SAS), 
# kan du have scriptet her aabent ved siden af og proeve at koere 
# kommandoerne og genfinde tallene. Hav evt samtidigt Lenes slides 
# med R-kodning aabent. I store traek ligner min kode Lenes - jeg 
# giver lidt forklaringer her og som sagt har jeg proevet
# at forsimple lidt / genbruge noget fra sidste uge. 

# e-bogen er opdateret til at omfatte kommandoerne brugt i dette
# script, saa tag ogsaa gerne et hurtigt kig paa den - det er meningen at
# du skal kunne bruge den som et opslagsvaerk. Ind i mellem er der ogsaa
# lidt ekstra forklaringer i e-bogen til hvordan du bruger output.

# Saa ja, mange filer i luften paa samme tid, men forhaabentligt faar I 
# paa et tidspunkt overblikket over hvordan de bruges bedst / i samspil

# Jeg ville gerne have tilpasset Lenes slides til min R-kode, jeg
# naar det naeppe til mandag ...




# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

# ---------------- Video 1: Two-sample t-test -----------------------------------------


vit <- read.csv("http://publicifsv.sund.ku.dk/~sr/BasicStatistics/datasets/vitamin.csv")
vit$country <- factor( vit$country, levels=c(1,2,4,6),
                       labels=c("DK", "SF", "EI", "PL"))
vit$sunexp  <- factor( vit$sun, levels=1:3, 
                       labels=c("Avoid sun", "Sometimes in sun", "Prefer sun"))


# --
# -- LTS slide 3 - subset bestaaende kun af DK og EI
vitd2 <- subset(vit, country %in% c("DK","EI") )
# her husker R de oevrige labels for SF og PL ...
table( vitd2$country )
# dem fjerner vi med droplevels()
vitd2$country <- droplevels( vitd2$country )
table( vitd2$country )

boxplot( vitd ~ country, data=vitd2)
# --



# --
# LTS slide 4 - summary statistics for Vitamin D (kode kopieret 
# fra uge 1)

sumStat <- function(x){
  mean=mean(x)
  median=median(x)
  Q1=quantile(x, probs=.25)
  Q3=quantile(x, probs=.75)
  SD=sd(x)
  min=min(x)
  max=max(x)
  samlet <- data.frame(mean,median,SD, Q1,Q3,min,max )
  row.names(samlet) <- NULL
  return( samlet ) 
}
tx  <- tapply( vitd2$vitd, vitd2$country, sumStat)
tx
do.call('rbind',tx)
# --


# --
# LTS slide 7

t.test( vitd ~ country, data=vitd2 )
# --


# --
# LTS slide 8 

var.test( vitd ~ country, data=vitd2 )
t.test( vitd ~ country, data=vitd2, var.equal=T )
# --





# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

# ---- Video 2: Signifikanstest, type I og type II fejl, styrke -----------------------


# --
# LTS slide 23

install.packages("TeachingDemos")
library(TeachingDemos)

vitDK <- subset(vitd2, country=='DK')
vitEI <- subset(vitd2, country=='EI')


# CI for spredning for DK-kvinder: 
sigma.test( vitDK$vitd )
# Fra output skal vi kun bruge CI 
# (vi er ikke interesseret i test af H_0: sigma^2=1, som er default)
sigma.test( vitDK$vitd )$conf.int
# og vi skal saa bruge kvadratrod for at faa spredning isf varians
sqrt( sigma.test( vitDK$vitd )$conf.int )


# CI for spredning for EI-kvinder: 
sqrt( sigma.test( vitEI$vitd )$conf.int )
# --


# --
# LTS slide 24
power.t.test(n = NULL, delta = 5, sd = 20, power = 0.8)
# --




# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

# -------------- Video 3: Ikke-parametriske tests -------------------------------------

# KOMMANDOER ER HER IKKE OPDATERET ENDNU men det bliver noget a la

wilcox.test( vitd ~ country, data=vitd2)

install.packages("exactRankTests")
library(exactRankTests)
wilcox.exact( vitd ~ country, exact=T, data=vitd2)

kruskal.test( vitd ~ country, data=vit )

# Median test 
install.packages('agricolae') # koer kun Ã©n gang
library(agricolae)
Median.test( vit$vitd,vit$country )

# Welch one-way
install.packages("onewaytests")
library(onewaytests)
welch.test( vitd ~ country, data=vit)



# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

# -------------- Video 4: One-way ANOVA -----------------------------------------------

# --
# LTS slide 35
tx  <- tapply( vit$vitd, vit$country, sumStat )
do.call('rbind',tx)
# --


# --
# LTS slide 37

# DK er referenceniveau (foedt med kode 1, og vi kan se DK staar foerst i tabel)
table(vit$country)
# Vi aendrer referencenivau til SF med relevel()
vit$country <- relevel( vit$country, ref='SF')
# Nu staar SF foerst
table(vit$country)

model.oneway <- lm( vitd ~ country, data=vit )


# Bartlett's test for ens varianser
bartlett.test( vitd ~ country, data=vit )

# Levene's test for ens varianser
install.packages("lawstat")
library(lawstat)
levene.test( vit$vitd, vit$country)
# --


# -- 
# LTS slide 39
anova( model.oneway )
# --

# -- 
# LTS slide 40
summary( model.oneway )
# --


# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

# -------------- Video 5: Modelkontrol ------------------------------------------------


# --
# LTS slide 45
# Der findes forskellige varianter, benyt ekstra argument location='mean' 
# bemaerk at det ikke er helt det samme test som i SAS ...
levene.test( vit$vitd, vit$country, location='mean')
# --


# --
# LTS slide 47 

# vi kan plotte model-objekt
par(mfrow=c(2,2))
plot( model.oneway )
par(mfrow=c(1,1))

# vi kan udvaelge de plots vi er mest interesserede i med which (som
# refererer til plot nr 1-4 som vist ovenfor)
plot( model.oneway, which=1 )
plot( model.oneway, which=2 )
# etc.
# --


# --
# LTS slide 56-57
aov.oneway  <- aov( model.oneway )
TukeyHSD( aov.oneway )
# --

# --
# LTS slide 61
kruskal.test( vitd ~ country, data= vit )
# --

# --
# LTS slide 62 (bemaerk at jeg her bruger subset() i dataargumentet -
vit3 <- subset(vit, country %in% c('SF','PL'))
vit3$country <- droplevels( vit3$country )
t.test( vitd ~  country, data=vit3 )
# --




# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

# -------------- Video 6: Two-way anova -----------------------------------------------

# --
# LTS slide 63
# 0/1 - kode 1 angiver somtimes/prefer sun
vit3$sol <- ifelse( vit3$sunexp=='Avoid sun', 0, 1)
table( vit3$sunexp )
table( vit3$sol )
# laver sol til en faktor
vit3$sol <- factor( vit3$sol, levels=0:1, labels=c('Nej','Ja'))
table( vit3$sol )


tab = table( vit3$country, vit3$sol)
tab
prop.table( tab, 1 )
# --


# --
# LTS slide 66
boxplot( vitd ~ sol*country, data=vit3, col=c("red","blue") )
# --


# --
# LTS slide 69
model.twoway = lm( vitd ~ country + sol, data=vit3 )
anova( model.twoway )
# --


# --
# LTS slide 70
summary( model.twoway )
confint( model.twoway )
# --


# --
# LTS slide 73
plot( model.twoway, which=1 )
plot( model.twoway, which=2 )
# --


# --
# LTS slide 74

# NB: Dette er rimeligt svaert ... og kraever at du foerst har set
# min video om grafik i R-intro kap 7.2 
# http://r.sund.ku.dk/graphics.html#customizegraph

# Figuren laves i to trin - foerst praedikterer jeg
# dernaest plottes data og praediktionerne tegnes ind

# -- Praediktion 
# Vi skal specificere de kombinationer af de forklarende variable
# vi oensker at praediktere for i et datasaet
ny <- data.frame( country=c('SF','SF','PL','PL'), 
                  sol=c('Ja','Nej','Ja','Nej'))
ny

# praedikterer nu fra model.twoway paa disse kovariatvaerdier
pred2 = predict( model.twoway, ny)
pred2

# Hvis vi vil se kovariatvaerdier og praediktioner sammen:
cbind( ny, pred2 )
# Dvs for de finske kvinder, som er glade for solen, estimerer vi
# at de har et gennemsnitligt vit D paa 50.02

# -- Plot
# vi vil (scatter)plotte vitd for hvert niveau af land
# men benytter vi vitd ~ country faar vi boxplot fordi
# country er en faktor. Vi kan tvinge den til at vaere numerisk
# med as.numeric() 
plot( vitd ~ as.numeric( country ), data=vit3, 
      col=sol, xlab='country' )
# Hvilket land har faaet kode 1 i plot? Det ser vi ved:
table( vit3$country )
table( as.numeric(vit3$country) )
# dvs SF har faaet kode 1, PL kode 2. 
# Tilsvarende kan vi finde at sol='Nej' har faaet kode 1 (sort),
# sol='Ja' kode 2 (roed)

# Lad os skrive det paa akserne i stedet:
plot( vitd ~ as.numeric( country ), data=vit3, 
      col=sol, xlab='country', axes=F )
axis(1, at=c(1,2), labels=c('SF', 'PL'))
axis(2)
box()
legend(1.5,95,c('Nej','Ja'),pch=1, col=1:2, title='Sol')
lines( c(1,2), pred2[c(1,3)], col='red', type='b', pch=19, cex=1.5)
lines( c(1,2), pred2[c(2,4)], type='b', pch=19, cex=1.5)
# pyh? ...



# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

# -------------------- Video 7: Interaktion -------------------------------------------

# --
# LTS slide 76
vitSF <- subset( vit3, country=='SF')
vitPL <- subset( vit3, country=='PL')

# Bemaerk omvendt fortegn ifht Lenes slides 
# (bare gang CI med -1, og byt graenserne)
t.test( vitd ~ sol, data=vitSF, var.equal=T )
t.test( vitd ~ sol, data=vitPL, var.equal=T )

# Bemaerk ogsaa at t.test() ikke giver os forskellen i gennemsnit
# saa det maa vi selv bestemme, feks for SF ved
names( t.test( vitd ~ sol, data=vitSF, var.equal=T ) ) # hvad er gemt naar vi laver t-test?
gns <- t.test( vitd ~ sol, data=vitSF, var.equal=T )$estimate
gns
gns[2]-gns[1]
# R beregner CI for gns[1]-gns[2], saa derfor skal vi altsaa vende det

# Tilsvarende for PL
gns <- t.test( vitd ~ sol, data=vitPL, var.equal=T )$estimate
gns[2]-gns[1]
# --


# --
# LTS slide 77
# Samme plot som, nu tilfoejer vi bare linjer svarende til 
# gennemsnittene for hver kombination af sol og land
plot( vitd ~ as.numeric( country ), data=vit3, 
      col=sol, xlab='country', axes=F )
axis(1, at=c(1,2), labels=c('SF', 'PL'))
axis(2)
box()
legend(1.5,95,c('Nej','Ja'),pch=1, col=1:2, title='Sol')
lines( c(1,2), 
       c( mean(vitSF$vitd[ vitSF$sol=='Ja' ]), mean(vitPL$vitd[ vitPL$sol=='Ja' ])), 
       col='red', type='b', pch=19, cex=1.5)
lines( c(1,2), 
       c( mean(vitSF$vitd[ vitSF$sol=='Nej' ]), mean(vitPL$vitd[ vitPL$sol=='Nej' ])), 
       col='black', type='b', pch=19, cex=1.5)
# --



# --
# LTS slide 82
model.int <- lm( vitd ~ country*sol, data=vit3 )
anova( model.int )
# --


# --
# LTS slide 83
summary( model.int )
confint( model.int )
# --


# --
# LTS slide 87
model.int.land <- lm( vitd ~ sol + country:sol, data=vit3)
summary( model.int.land )
confint( model.int.land )
# --


# --
# LTS slide 89 - diverse t-test

# 1. linje i tabel:
t.test( vitd ~ sol, data=vit3, var.equal=T )
# 2. linje i tabel (model.twoway):
coef( model.twoway ) # viser kun estimater
confint( model.twoway )
# 3.+4. linje i tabel (T-test som slide 76)
t.test( vitd ~ sol, data=vitSF, var.equal=T )
t.test( vitd ~ sol, data=vitPL, var.equal=T )
# --



# --
# LTS slide 90
# definerer sol-variabel
vit4 <- subset(vit, country %in% c('DK','EI'))
vit4$country <- droplevels( vit4$country )
vit4$sol <- ifelse( vit4$sunexp=='Avoid sun', 0, 1)
vit4$sol <- factor( vit4$sol, levels=0:1, labels=c('Nej','Ja'))

vit4$sol.country <- interaction( vit4$sol, vit4$country )
table( vit4$sol.country )

boxplot( vitd ~ sol.country,  data=vit4,
         col=c('red','blue','red','blue'))
# --


# --
# LTS slide 91 - samme kode som slide 77, nu bare paa vit4 data isf vit3
vitDK <- subset(vit4, country=='DK')
vitEI <- subset(vit4, country=='EI')

plot( vitd ~ as.numeric( country ), data=vit4, 
      col=sol, xlab='country', axes=F )
axis(1, at=c(1,2), labels=c('DK', 'EI'))
axis(2)
box()
legend(1.5,95,c('Nej','Ja'),pch=1, col=1:2, title='Sol')
lines( c(1,2), 
       c( mean(vitDK$vitd[ vitDK$sol=='Ja' ]), mean(vitEI$vitd[ vitEI$sol=='Ja' ])), 
       col='red', type='b', pch=19, cex=1.5)
lines( c(1,2), 
       c( mean(vitDK$vitd[ vitDK$sol=='Nej' ]), mean(vitEI$vitd[ vitEI$sol=='Nej' ])), 
       col='black', type='b', pch=19, cex=1.5)



# --
# LTS slide 92
vit4$country <- relevel( vit4$country, ref='EI') # Lene har EI som ref i sine outputs
model.int2 <- lm( vitd ~ country*sol, data=vit4) 
summary( model.int2 )
confint( model.int2 )
# --


# --
# LTS slide 94
model.int.sol <- lm( vitd ~ country + country:sol, data=vit4 )
anova( model.int.sol )
# --

# --
# LTS slide 95
summary( model.int.sol )
# --

# --
# LTS slide 96
vit$country <- relevel( vit$country, ref='PL') # PL som referenceland
# er noedt til lige at definere sol i vit-data med alle lande
vit$sol <- ifelse( vit$sunexp=='Avoid sun', 0, 1)
vit$sol <- factor( vit$sol, levels=0:1, labels=c('Nej','Ja'))

model.int.alle <- lm( vitd ~ country*sol, data=vit) 
anova( model.int.alle)
# --


# --
# LTS slide 97
model.int.alle.sol <- lm( vitd ~ country+country:sol, data=vit) 
summary( model.int.alle.sol )
# --