##############################################################################################
#
#                        R-Code: Zensus-Analyse Register
#
# Dieses Programm entstand in Zusammenarbeit mit SPIEGEL ONLINE. Esanalysiert die Ergebnisse 
# des Zensus 2011 im Vergleich zu den Bevölkerungszahlen der kommunalen Melderegister in 
# einzelnen Bundesländern.
# Im Fokus stehen die Abweichungen des Zensus von den Einwohnerzahlen im Register der Gemeinden 
# und ob die Tatsache, dass eine Gemeinde eine Stichprobe hatte (große Gemeinden mit > 10.000 
# Einw.), zu größeren und anderen Abweichungen führte.
#
# Download-URL dieses Programms: https://github.com/DerSpiegel/datenlese/blob/master/Zensus_2011/Zensus_Ungleichheits-Analyse_Programm.R
# 
# Das Progarmm nutzt die freie Statistik-Software R. Download: www.r-project.org
#
# Empfohlene Nutzung zusammen mit RStudio. Download: www.rstudio.com
#
# Autor: Björn Schwentker, www.demografie-blog.de
#
# Datenquellen: Statistisches Bundesamt und Statistische Landesämter und 
#               Zentrales Melderegister Rheinland-Pfalz der KommWis
#               Download: https://raw.github.com/DerSpiegel/datenlese/master/Zensus_2011/Zensus_GESAMT_latin1.csv
#
################################################################################################

#            PROGRAMM STARTEN: 
#
# Entweder alle Zeilen des Codes auswählen und "Run" wählen (im "Code"-Menü: "Run Line(s)")
# oder Befehl "Source" wählen (im "Code"-Menü)


# WICHTIG: Pfad für Input-Datei setzen!!
# Per Default wird die Input-Datei mit den Zensustdaten, 'Zensus_GESAMT_latin1.csv',
# automatisch von GitHub heruntergeladen. In der Variable daten.pfad
# lässt sich aber auch ein Verzeichnispfad auf der eigenen Festplatte angeben
daten.pfad <- 'http://data.demografie-blog.de/zensus-unzensiert/Zensus_GESAMT_latin1.csv'


# WICHTIG: Pfad für Output-Dateien setzen
# Alle Output-Dateien landen im ARBEITSVERZEICHNIS von R. Man setzt es über die
# Programmzeile:
# setwd("Verzeichnispfad")   # Pfadname eintragen und "#"-Zeichen ganz am Anfang dieser Zeile entfernen


##################################################################################################
#                                                       
#                            Datei-Output dieses Codes                   
# 
#
# Die Datei "Register_Infos_latin1.txt" enthält in "prosa" die Ergebnisse einer einer
# Regressions-Analyse, die berechnet, welchen Effekt die Stichproben-Methode auf die pro-
# zentuale Abweichung des Zensusergebnisses vom kommunalen Melderegister hat. Als alternative
# Einflussgröße wird die Einwohnerzahl der Gemeinden untersucht. Für die Effekte wedren deren 
# Stärke und statistische Aussagekraft beschrieben.
#
# Außerdem enstetehn fürjedes Bundesland zwei Dateien:
#
# 1. eine PDF-Grafik, die die Abweichung für jede einzelne Gemeinde darstellt. (ohne Stichprobe: 
#    grün; mit Stichprobe:rot) Der Dateiname ist in der in der Variable filename.XX.pdf angegeben 
#    (XX ist dabei das Kürzel des Bundeslandes, z.B. NI oder RP)
#    
# 2. eine CSV-Tabelle, die die durchschnittlichen prozentualen Abweichung des Zensusergebnisses
#    vom Register für verschiedene Gemeindegrößenklassen angibt. Alle Klassen mit >= 10.000 
#    Einwohner hatten eine Stichprobe, alle < 10.000 Einwohner nicht. Es gibt einzelne Gemeinden mit
#    weniger als 10.000 Einwohnern, die trotzdem eine Stichprobe hatten. Diese werden der kleinsten 
#    Klasse mit mehr als 10.000 Einwohnern zugeschlagen. Der Dateiname is in der Variable 
#    filename.XX.tabelle angegeben (XX ist dabei das Kürzel des Bundeslandes, z.B. NI oder RP).
#    Der Text der Datei ist latin-1-kodiert.
#
###################################################################################################


# Optionen & Funktionen:
options(scipen=100, OutDec=",")                   # Wissenschaftliche Notation für Zahlen ausschalten, Komma bei Kommazahlen ist "," (statt default ".")
round0 <- function (x) format(round(x, digits=0), nsmall=0, big.mark=".")      # Funktion: Zahlen für schöne Ausgabe ohne Nachkommaziffern
round1 <- function (x) format(round(x, digits=1), nsmall=1, big.mark=".")      # Funktion: Zahlen für schöne Ausgabe mit einer Nachkommaziffer
roundx <- function (x, d) format(round(x, digits=d), nsmall=d, big.mark=".")   # Funktion: Zahlen für schöne Ausgabe mit d Nachkommaziffern
signifikanz <- function(p) {                                    # Funktion: Ausgabe des Signifikanzniveaus der linearen Regression
  text_a <- ifelse(p<= 0.001, 'höchst sigifikant', ifelse(p <= 0.01, 'sehr signifikant', ifelse(p <= 0.05, 'signifikant', 'nicht signifikant')))
  text_b <- ifelse(p < 0.001, ' (p<0,001)', paste(' (p=', roundx(p,3) ,')', sep=''))
  return (paste(text_a, text_b, sep='')) 
}


##################################################################################################
#                                                                                                #
#                                      Daten importieren                                         #
#                                                                                                #
##################################################################################################

# Zensus-Datei mit Angaben für alle deutschen Gemeinden in Data.frame "gesamt" einlesen:
gesamt <- read.csv2(daten.pfad, header=TRUE, 
                    colClasses=c(rep("character", 4), rep("numeric", 8)),
                    na.strings="-", encoding="latin1", skip=25)

####  Aufbau der Datei 'Zensus_GESAMT_latin1.csv' bzw. des Data.frames 'gesamt':  ####
#
# Text-Codierung:  Latin-1 (ISO 8859-1)
#
# SPALTEN:
#
# 1. RS_12: 		     eindeutiger 12-stelliger Regionalschlüssel der Gemeinde (siehe https://de.wikipedia.org/wiki/Amtlicher_Gemeindeschl%C3%BCssel#Regionalschl.C3.BCssel)
# 2. Gemeinde: 		   Name der Gemeinde
# 3. Land:			     Bundesland, in dem die Gemeinde liegt
# 4. Kreis:			     Kreis, in dem die Gemeinde liegt
# 5. Zensus:		     Einwohnerzahl der Gemeinde laut Zensusergebnis zum Stichtag 09.05.2011
# 6. Fortschreibung: Amtliche Einwohnerzahl der Gemeinde am 30.04.2011 laut Fortschreibung
# 7. Register:		   Einwohnerzahl der Gemeinde laut kommunalem Melderegister für einzelne Bundesländer:
#					             - Niedersachsen: zum Stichtag 09.05.2011 nach Zensuserhebung des Statistischen Landesamtes Niedersachsen("MR1") 
#					             - Rheinland-Pfalz: zum Stichtag 30.04.2011 aus dem zentralen Melderegister von Rheinland-Pfalz (KommWis) 
# 8. FS_diff:		     Differenz zwischen Zensusergebnis und Fortschschreibung (Spalte 5 - Spalte 6)
# 9. FS_diff_proz:	 Selbe Differenz in Prozent ((Spalte 5 - Spalte 6)/Spalte 6 * 100)
# 10. R_diff:		     Differenz zwischen Zensusergebnis und lokalem Melderegister (Spalte 5 - Spalte 7)
# 11. R_diff_proz:	 Selbe Differenz in Prozent ((Spalte 5 - Spalte 7)/Spalte 7 * 100)
# 12. ST_Fehler:	   Stichprobenfehler ("einfacher relativer Standardfehler") der Gemeinde in Prozent


# Ändern sie die folgenden Variablen, um die Größe der Gemeinden einzuschränken,
# die in das statistische Modell einbezogen werden.
limit.unten <- 5000           # Kleinste Gemeindegröße für Regressionsanalyse (muss < 10.000 sein)
limit.oben <- 20000           # Größte Gemeindegröße für Regressionsanalyse (muss > 10.000 sein)


##################################################################################################
#                                                                                                #
#                         Analyse für alle Bundesländer zusammen                                 #             
#                                                                                                #
# Dieser Teil analysiert die Zensusergebnisse für Niedersachsen (NI) und Rheinland-Pfalz (RP)    #
# zusammen. Im Idealfall würde man ganz Deutschland, also alle Bundesländer zusammen analysie-   #
# ren. Uns liegen die nötigen Daten aber nur für NI und RLP vor. Gemeinsam umfasts dieser        #
# 29,4% aller deutschen Gemeinden und 14,6% aller Bürger (laut Zensus)                           #
#                                                                                                #
##################################################################################################

# Alle Gemeinden von NI und RP auswählen anhand der ersten zwei Stellen 
# im Regionalschlüssel "RS_12" (="03" für NI und ="07" für RP):
ALLE <- subset(gesamt, substr(RS_12, 1,2)=="03" | substr(RS_12, 1,2)=="07")

# Gemeinden mit (gross) und ohne Stichprobe (klein) separieren:
ALLE.klein <- ALLE[which(is.na(ALLE$ST_Fehler)),]       # Alle Gemeinden OHNE Stichprobe ("klein")
ALLE.gross <- ALLE[which(!is.na(ALLE$ST_Fehler)),]      # Alle Gemeinden MIT Stichprobe ("gross")

# Welchen Anteil von Deutschland haben wir in dieser Analyse?
ALLE.Menschen.Anteil <- sum(ALLE$Zensus)/sum(gesamt$Zensus)           # Anteil an allen dt. Bürgern (Zensus-Daten, wei register nicht für ganz Dt.)
ALLE.Gemeinden.Anteil <- length(ALLE$Zensus)/length(gesamt$Zensus)    # Anteil an allen dt. Gemeinden

# Lineare Regression: Beeinflusst der Methodenwechsel die prozentuale
# Registerabweichung r_diff_proz? Wenn ja, wie stark?

reg_ALLE <- ALLE                                              # Daten für Regression vorbereiten in data.frame reg_ALLE
reg_ALLE$Methode <- ifelse(is.na(reg_ALLE$ST_Fehler), 0, 1)   # neue Spalte einführen mit Dummy-Variable "Methode":
                                                              # Methode=1, wenn die Gemeinde eine Stichprobe hatte, sonst 0
N.gross.reg <- length(subset(ALLE.gross, Register <= limit.oben)$Register)      # Gem. MIT Stichpr. in Regression
N.klein.reg <- length(subset(ALLE.klein, Register >= limit.unten)$Register)     # Gem. OHNE Stichpr. in Regression
N.reg <- N.gross.reg + N.klein.reg                                              # Alle Gemeinden in Regression

reg_ALLE <- subset(reg_ALLE, Register >= limit.unten & Register <=limit.oben)     # Gemeinden der entsprechenden Größe auswählen 
ergebnis_reg_ALLE <- lm(R_diff_proz ~ Register + Methode, data=reg_ALLE)            # Lineare Regression berechnen. Ergebnise landen in ergebnis_reg_NI

# Poisson Regression
ergebnis_poi_ALLE <- glm(Zensus ~ I(log(Register)) + Methode,family=quasipoisson,data=reg_ALLE) 


# Basiszahlen berechnen:
ALLE.N <- length(ALLE$Gemeinde)       # Anzahl Gemeinden in ALLE insgesamt
ALLE.Bev <- sum(ALLE$Register)        # Bevölkerung in ALLE insgesamt laut Register
ALLE.N.klein <- length(ALLE.klein$Gemeinde)    # Anzahl Gemeinden OHNE Stichprobe
ALLE.Bev.klein <- sum(ALLE.klein$Register)     # Bevölkerung in Gemeinden OHNE Stichprobe laut Register
ALLE.N.gross <- length(ALLE.gross$Gemeinde)    # Anzahl Gemeinden MIT Stichprobe
ALLE.Bev.gross <- sum(ALLE.gross$Register)     # Bevölkerung in Gemeinden MIT Stichprobe laut Register



# Wie viele Gemeinden haben jeweils positive oder negative Abweichung?
ALLE.N.R_diff_plus.klein <- length(ALLE.klein$Gemeinde[ALLE.klein$R_diff>0])     # Gem. OHNE Stichpr. mit POSITIVER Abweichung (Zensus > Register)
ALLE.N.R_diff_plus.klein.proz <- ALLE.N.R_diff_plus.klein/ALLE.N.klein*100       # ... in Prozent
ALLE.N.R_diff_gleich.klein <- length(ALLE.klein$Gemeinde[ALLE.klein$R_diff==0])  # Gem. OHNE Stichpr. OHNE Abweichung (Zensus = Register)
ALLE.N.R_diff_gleich.klein.proz <- ALLE.N.R_diff_gleich.klein/ALLE.N.klein*100   # ... in Prozent
ALLE.N.R_diff_minus.klein <- length(ALLE.klein$Gemeinde[ALLE.klein$R_diff<0])    # Gem. OHNE Stichpr. mit NEGATIVER Abweichung (Zensus < Register)
ALLE.N.R_diff_minus.klein.proz <- ALLE.N.R_diff_minus.klein/ALLE.N.klein*100     # ... in Prozent
ALLE.N.R_diff_plus.gross <- length(ALLE.gross$Gemeinde[ALLE.gross$R_diff>0])     # Gem. MIT Stichpr. mit POSITIVER Abweichung (Zensus > Register)
ALLE.N.R_diff_plus.gross.proz <- ALLE.N.R_diff_plus.gross/ALLE.N.gross*100       # ... in Prozent
ALLE.N.R_diff_gleich.gross <- length(ALLE.gross$Gemeinde[ALLE.gross$R_diff==0])  # Gem. MIT Stichpr. OHNE Abweichung (Zensus = Register)
ALLE.N.R_diff_gleich.gross.proz <- ALLE.N.R_diff_gleich.gross/ALLE.N.gross*100   # ... in Prozent
ALLE.N.R_diff_minus.gross <- length(ALLE.gross$Gemeinde[ALLE.gross$R_diff<0])    # Gem. MIT Stichpr. mit NEGATIVER Abweichung (Zensus < Register)
ALLE.N.R_diff_minus.gross.proz <- ALLE.N.R_diff_minus.gross/ALLE.N.gross*100     # ... in Prozent


# Prosa-Informationen ausgeben
prosa <- "*** ANALYSE ALLE BUNDESLÄNDER (NI+RP) ***\n\n"
prosa <- paste(prosa, "\n++ I. Einfluss der Stichprobe auf das Zensusergebnis ++\n\n", sep="")
prosa <- paste(prosa, "Lineare Regression für Gemeinden mit ", round0(limit.unten), " bis ", round0(limit.oben), " Einwohnern\n", sep="")
prosa <- paste(prosa, "(", round0(N.reg), " Gemeinden, ", round0(N.gross.reg), " mit Stichprobe und ", round0(N.klein.reg), " ohne)\n\n", sep="")
prosa <- paste(prosa, "Multivariate Analyse von zwei Einflussgrößen: \na) Stichprobe/keine Stichprobe \nb) Größe der Gemeinde (Einwohner laut Melderegister)\n\n", sep="")
prosa <- paste(prosa, "a) Eine Stichprobe ändert die %-Abweichung des Zensusergebnisses vom Register um\n ", 
               roundx(summary(ergebnis_reg_ALLE)$coefficients[3,1], 2), " Prozentpunkte (Beta-Wert) gegenüber einer Gemeinde ohne Stichprobe.
Dieser Zusammenhang ist statistisch ", signifikanz(summary(ergebnis_reg_ALLE)$coefficients[3,4]), ".
Konfidenzintervall (95%): [", confint(ergebnis_reg_ALLE, 'Methode', level=0.95)[1] , " bis ", confint(ergebnis_reg_ALLE, 'Methode', level=0.95)[2], "]\n\n", sep="")
prosa <- paste(prosa, "b) Die Größe der Gemeinde (1.000 Einwohner mehr) ändert die %-Abweichung des Zensusergebnisses vom Register um\n",
               roundx(summary(ergebnis_reg_ALLE)$coefficients[2,1]*1000,2), " Prozentpunkte (Beta-Wert*1.000).
Dieser Zusammenhang ist statistisch ", signifikanz(summary(ergebnis_reg_ALLE)$coefficients[2,4]), ".
Konfidenzintervall (95%): [", confint(ergebnis_reg_ALLE, 'Register', level=0.95)[1]*1000 , " bis ", confint(ergebnis_reg_ALLE, 'Register', level=0.95)[2]*1000, "]\n\n", sep="")
prosa <- paste(prosa, "Angaben zur Güte der Regression: \n", sep="")
prosa <- paste(prosa, "Bestimmtheitsmaß R²: ", roundx(summary(ergebnis_reg_ALLE)$r.squared,2), "\n\n", sep="")

prosa <- paste(prosa, "\n++ II. Alternativer Test mit Poisson-Regression  ++\n\n", sep="")
prosa <- paste(prosa, "Eine Stichprobe ändert die %-Abweichung des Zensusergebnisses vom Register um\n ", 
               roundx((exp(ergebnis_poi_ALLE$coefficients[3])-1)*100, 2), " Prozentpunkte (Beta-Wert) gegenüber einer Gemeinde ohne Stichprobe.
Dieser Zusammenhang ist statistisch ", signifikanz(summary(ergebnis_poi_ALLE)$coefficients[3,4]), ".
Konfidenzintervall (95%): [", (exp(confint(ergebnis_poi_ALLE, 'Methode', level=0.95)[1])-1)*100 , " bis ", (exp(confint(ergebnis_poi_ALLE, 'Methode', level=0.95)[2])-1)*100, "]\n\n\n", sep="")


prosa <- paste(prosa, "++ III. Basisangaben zu den Gemeinden ++\n\n", sep="")
prosa <- paste(prosa, round0(ALLE.Bev), " Einwohner in insgesamt", round0(ALLE.N), "Gemeinden.\n...darunter:\n")
prosa <- paste(prosa, "   Gemeinden OHNE Stichprobe: ", round0(ALLE.N.klein), " (", round1(ALLE.N.klein/ALLE.N*100), "%)\n", sep="")
prosa <- paste(prosa, "   Bevölkerung dort:          ", round0(ALLE.Bev.klein), " (", round1(ALLE.Bev.klein/ALLE.Bev*100), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden MIT Stichprobe:  ", round0(ALLE.N.gross), " (", round1(ALLE.N.gross/ALLE.N*100), "%)\n", sep="")
prosa <- paste(prosa, "   Bevölkerung dort:          ", round0(ALLE.Bev.gross), " (", round1(ALLE.Bev.gross/ALLE.Bev*100), "%)\n", sep="")

prosa <- paste(prosa, "\nUnter den ", round0(ALLE.N.klein), " Gemeinden OHNE Stichprobe gabe es...\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit POSITIVER Abweichung (Zensus > Register): ", round0(ALLE.N.R_diff_plus.klein), " (", round1(ALLE.N.R_diff_plus.klein.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden OHNE Abweichung (Zensus = Register): ", round0(ALLE.N.R_diff_gleich.klein), " (", round1(ALLE.N.R_diff_gleich.klein.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit NEGATIVER Abweichung (Zensus < Register): ", round0(ALLE.N.R_diff_minus.klein), " (", round1(ALLE.N.R_diff_minus.klein.proz), "%)\n", sep="")
prosa <- paste(prosa, "\nUnter den ", round0(ALLE.N.gross), " Gemeinden MIT Stichprobe gabe es...\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit POSITIVER Abweichung (Zensus > Register): ", round0(ALLE.N.R_diff_plus.gross), " (", round1(ALLE.N.R_diff_plus.gross.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden OHNE Abweichung (Zensus = Register): ", round0(ALLE.N.R_diff_gleich.gross), " (", round1(ALLE.N.R_diff_gleich.gross.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit NEGATIVER Abweichung (Zensus < Register): ", round0(ALLE.N.R_diff_minus.gross), " (", round1(ALLE.N.R_diff_minus.gross.proz), "%)\n\n\n\n", sep="")


##################################################################################################
#                                                                                                #
#                              Analyse für einzelne Bundesländer                                 #             
#                                                                                                #
# Die Programmcodes sind für jedes Bundesland im Prinzip identisch. Es müsen nur die Namen der   #
# Variablen angepasst werden. Nimmt man z.B.den Code für Niedersachsen (NI), muss man, um den    #
# für Rheinland-Pfalz zu erstellen, überall "NI" durch "RP" ersetzen. Und da, wo Text ausge-     #
# geben wird, "Niedersachsen" durch "Rheinland-Pfalz". Außerdem müssen die beiden Dateinamen für #
# Grafik (PDF) und die Tabelle (CSV) angepasst werden.                                           #                                                                                                                    #
#                                                                                                #
##################################################################################################


##########################################################
#                                                        #
#            Analyse für Niedersachsen (NI)              #
#                                                        #
##########################################################

# Dateinamen für Grafik (PDF) und Tabelle (CSV)
filename.NI.pdf <- 'Abweichung_Zensus-Register_NI.pdf'
filename.NI.tabelle <- 'Abweichung_Zensus-Register_NI_Groessenklassen_latin1.csv'

# Alle Gemeinden von NI auswählen anhand der ersten zwei Stellen 
# im Regionalschlüssel "RS_12" (="03" für NI):
NI <- subset(gesamt, substr(RS_12, 1,2)=="03")

# Gemeinden mit (gross) und ohne Stichprobe (klein) separieren:
NI.klein <- NI[which(is.na(NI$ST_Fehler)),]       # Alle Gemeinden OHNE Stichprobe ("klein")
NI.gross <- NI[which(!is.na(NI$ST_Fehler)),]      # Alle Gemeinden MIT Stichprobe ("gross")

# Verschiedene Größenklassen von Gemeinden separieren
NI.gross.ab_30_Tsd <- subset(NI.gross, Register >= 30000)                    # Gem. MIT Stichpr. ab 30.000 Einwohner
NI.gross.20_30_Tsd <- subset(NI.gross, Register >= 20000 & Register < 30000)   # Gem. MIT Stichpr. ab 20.000 bis unter 30.000 Einw.
NI.gross.15_20_Tsd <- subset(NI.gross, Register >= 15000 & Register < 20000)   # Gem. MIT Stichpr. ab 15.000 bis unter 20.000 Einw.
NI.gross.10_20_Tsd <- subset(NI.gross, Register < 20000)                     # Gem. MIT Stichpr. bis unter 20.000 Einw. (ab etwa 10.000)
NI.gross.10_15_Tsd <- subset(NI.gross, Register < 15000)                     # Gem. MIT Stichpr. bis unter 15.000 Einw. (ab etwa 10.000)
NI.gross.10_11_Tsd <- subset(NI.gross, Register < 11000)                     # Gem. MIT Stichpr. bis unter 11.000 Einw. (ab etwa 10.000)
NI.klein.9_10_Tsd <- subset(NI.klein, Register >= 9000)                      # Gem. OHNE Stichpr. ab 9.000 Einw. (bis etwa 10.000)
NI.klein.5_10_Tsd <- subset(NI.klein, Register >= 5000)                      # Gem. OHNE Stichpr. ab 5.000 Einw. (bis etwa 10.000)
NI.klein.2.5_5_Tsd <- subset(NI.klein, Register >= 2500 & Register < 5000)     # Gem. OHNE Stichpr. ab 2.500 bis unter 5.000 Einw.
NI.klein.1_2.5_Tsd <- subset(NI.klein, Register >= 1000 & Register < 2500)     # Gem. OHNE Stichpr. ab 1.000 bis unter 2.500 Einw.
NI.klein.u1_Tsd <- subset(NI.klein, Register < 1000)                         # Gem. OHNE Stichpr. unter 1.000 Einw.

# Basiszahlen berechnen:
NI.N <- length(NI$Gemeinde)       # Anzahl Gemeinden in NI insgesamt
NI.Bev <- sum(NI$Register)        # Bevölkerung in NI insgesamt laut Register
NI.N.klein <- length(NI.klein$Gemeinde)    # Anzahl Gemeinden OHNE Stichprobe
NI.Bev.klein <- sum(NI.klein$Register)     # Bevölkerung in Gemeinden OHNE Stichprobe laut Register
NI.N.klein.9_10_Tsd <- length(NI.klein.9_10_Tsd$Gemeinde)     # Anzahl Gem. OHNE Stichpr. ab 9.000 Einw. (bis etwa 10.000)
NI.N.klein.5_10_Tsd <- length(NI.klein.5_10_Tsd$Gemeinde)     # Anzahl Gem. OHNE Stichpr. ab 5.000 Einw. (bis etwa 10.000)
NI.N.klein.2.5_5_Tsd <- length(NI.klein.2.5_5_Tsd$Gemeinde)   # Anzahl Gem. OHNE Stichpr. ab 2.500 bis unter 5.000 Einw.
NI.N.klein.1_2.5_Tsd <- length(NI.klein.1_2.5_Tsd$Gemeinde)   # Anzahl Gem. OHNE Stichpr. ab 1.000 bis unter 2.500 Einw.
NI.N.klein.u1_Tsd <- length(NI.klein.u1_Tsd$Gemeinde)         # Anzahl Gem. OHNE Stichpr. unter 1.000 Einw.
NI.N.gross <- length(NI.gross$Gemeinde)    # Anzahl Gemeinden MIT Stichprobe
NI.Bev.gross <- sum(NI.gross$Register)     # Bevölkerung in Gemeinden MIT Stichprobe laut Register
NI.N.gross.ab_30_Tsd <- length(NI.gross.ab_30_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. ab 30.000 Einw.
NI.N.gross.20_30_Tsd <- length(NI.gross.20_30_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. ab 20.000 bis unter 30.000 Einw.
NI.N.gross.15_20_Tsd <- length(NI.gross.15_20_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. ab 15.000 bis unter 20.000 Einw.
NI.N.gross.10_20_Tsd <- length(NI.gross.10_20_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. bis unter 20.000 Einw. (ab etwa 10.000)
NI.N.gross.10_15_Tsd <- length(NI.gross.10_15_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. bis unter 15.000 Einw. (ab etwa 10.000)
NI.N.gross.10_11_Tsd <- length(NI.gross.10_11_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. bis unter 11.000 Einw. (ab etwa 10.000)


# Analyse Methoden-Sprung bei 10.000 Einwohnern:
NI.R_diff_proz <- mean(NI$R_diff_proz)                            # ALLE Gemeinden: Arith. Mittel der prozentualen Register-Abweichungen
NI.R_diff_proz.klein <- mean(NI.klein$R_diff_proz)                # Alle Gem. OHNE Stichpr.: Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.abs.klein <- mean(abs(NI.klein$R_diff_proz))       # Alle Gem. OHNE Stichpr.: Arith. Mittel der absoluten proz. Abw.
NI.R_diff_proz.9_10_Tsd <- mean(NI.klein.9_10_Tsd$R_diff_proz)    # Gem. OHNE Stichpr. ab 9.000 Einw. (bis etwa 10.000): Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.5_10_Tsd <- mean(NI.klein.5_10_Tsd$R_diff_proz)    # Gem. OHNE Stichpr. ab 5.000 Einw. (bis etwa 10.000): Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.2.5_5_Tsd <- mean(NI.klein.2.5_5_Tsd$R_diff_proz)  # Gem. OHNE Stichpr. ab 2.500 bis unter 5.000 Einw.: Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.1_2.5_Tsd <- mean(NI.klein.1_2.5_Tsd$R_diff_proz)  # Gem. OHNE Stichpr. ab 1.000 bis unter 2.500 Einw.: Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.u1_Tsd <- mean(NI.klein.u1_Tsd$R_diff_proz)        # Gem. OHNE Stichpr. unter 1.000 Einw.: Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.gross <- mean(NI.gross$R_diff_proz)                # Alle Gem. MIT Stichpr.: Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.abs.gross <- mean(abs(NI.gross$R_diff_proz))       # Alle Gem. MIT Stichpr.: Arith. Mittel der absoluten proz. Abw.
NI.R_diff_proz.ab_30_Tsd <- mean(NI.gross.ab_30_Tsd$R_diff_proz)  # Gem. MIT Stichpr. ab 30.000 Einw.: Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.20_30_Tsd <- mean(NI.gross.20_30_Tsd$R_diff_proz)  # Gem. MIT Stichpr. ab 20.000 bis unter 30.000 Einw.: Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.15_20_Tsd <- mean(NI.gross.15_20_Tsd$R_diff_proz)  # Gem. MIT Stichpr. ab 15.000 bis unter 20.000 Einw.: Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.10_20_Tsd <- mean(NI.gross.10_20_Tsd$R_diff_proz)  # Gem. MIT Stichpr. bis unter 20.000 Einw. (ab etwa 10.000): Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.10_15_Tsd <- mean(NI.gross.10_15_Tsd$R_diff_proz)  # Gem. MIT Stichpr. bis unter 15.000 Einw. (ab etwa 10.000): Arith. Mittel der prozentualen Abweichungen
NI.R_diff_proz.10_11_Tsd <- mean(NI.gross.10_11_Tsd$R_diff_proz)  # Gem. MIT Stichpr. bis unter 11.000 Einw. (ab etwa 10.000): Arith. Mittel der prozentualen Abweichungen


# Wie viele Gemeinden haben jeweils positive oder negative Abweichung?
NI.N.R_diff_plus.klein <- length(NI.klein$Gemeinde[NI.klein$R_diff>0])     # Gem. OHNE Stichpr. mit POSITIVER Abweichung (Zensus > Register)
NI.N.R_diff_plus.klein.proz <- NI.N.R_diff_plus.klein/NI.N.klein*100       # ... in Prozent
NI.N.R_diff_gleich.klein <- length(NI.klein$Gemeinde[NI.klein$R_diff==0])  # Gem. OHNE Stichpr. OHNE Abweichung (Zensus = Register)
NI.N.R_diff_gleich.klein.proz <- NI.N.R_diff_gleich.klein/NI.N.klein*100   # ... in Prozent
NI.N.R_diff_minus.klein <- length(NI.klein$Gemeinde[NI.klein$R_diff<0])    # Gem. OHNE Stichpr. mit NEGATIVER Abweichung (Zensus < Register)
NI.N.R_diff_minus.klein.proz <- NI.N.R_diff_minus.klein/NI.N.klein*100     # ... in Prozent
NI.N.R_diff_plus.gross <- length(NI.gross$Gemeinde[NI.gross$R_diff>0])     # Gem. MIT Stichpr. mit POSITIVER Abweichung (Zensus > Register)
NI.N.R_diff_plus.gross.proz <- NI.N.R_diff_plus.gross/NI.N.gross*100       # ... in Prozent
NI.N.R_diff_gleich.gross <- length(NI.gross$Gemeinde[NI.gross$R_diff==0])  # Gem. MIT Stichpr. OHNE Abweichung (Zensus = Register)
NI.N.R_diff_gleich.gross.proz <- NI.N.R_diff_gleich.gross/NI.N.gross*100   # ... in Prozent
NI.N.R_diff_minus.gross <- length(NI.gross$Gemeinde[NI.gross$R_diff<0])    # Gem. MIT Stichpr. mit NEGATIVER Abweichung (Zensus < Register)
NI.N.R_diff_minus.gross.proz <- NI.N.R_diff_minus.gross/NI.N.gross*100     # ... in Prozent

# Abweichungen nach Größenklases in Tabelle anordnen und speichern

# Zuerst Spalten erschaffen:
NI.spalte.info <- c("< 1.000", "1.000 bis < 2.500", "2.500 bis < 5.000", "5.000 bis < 10.000", "9.000 bis < 10.000",
                    "10.000 bis < 11.000", "10.000 bis < 15.000", "10.000 bis < 20.000", "15.000 bis < 20.000", "20.000 bis < 30.000", "30.000 und mehr",
                    "ohne Stichprobe", "mit Stichprobe", "gesamt")

NI.spalte.N <- c(NI.N.klein.u1_Tsd, NI.N.klein.1_2.5_Tsd, NI.N.klein.2.5_5_Tsd, NI.N.klein.5_10_Tsd, NI.N.klein.9_10_Tsd,
                 NI.N.gross.10_11_Tsd, NI.N.gross.10_15_Tsd, NI.N.gross.10_20_Tsd, NI.N.gross.15_20_Tsd, NI.N.gross.20_30_Tsd, 
                 NI.N.gross.ab_30_Tsd, NI.N.klein, NI.N.gross, NI.N)
  
NI.spalte.proz <- c(NI.R_diff_proz.u1_Tsd, NI.R_diff_proz.1_2.5_Tsd, NI.R_diff_proz.2.5_5_Tsd, NI.R_diff_proz.5_10_Tsd, NI.R_diff_proz.9_10_Tsd,
                    NI.R_diff_proz.10_11_Tsd, NI.R_diff_proz.10_15_Tsd, NI.R_diff_proz.10_20_Tsd, NI.R_diff_proz.15_20_Tsd, NI.R_diff_proz.20_30_Tsd, 
                    NI.R_diff_proz.ab_30_Tsd, NI.R_diff_proz.klein, NI.R_diff_proz.gross, NI.R_diff_proz)
  
NI.tabelle <- data.frame(cbind(NI.spalte.info, NI.spalte.N, NI.spalte.proz))                         # Spalten zu Tabelle zusammenbinden
colnames(NI.tabelle) <- (c("Größenklasse (Einwohner)", "Anzahl Gemeinden","Registerabw. in %"))      # Spaltennamen hinzufügen
write.csv2(NI.tabelle, filename.NI.tabelle, quote=FALSE, row.names=FALSE, fileEncoding = "latin1")   # Tabelle als CSV-Datei speichern  


# Lineare Regression: Beeinflusst der Methodenwechsel die prozentuale
# Registerabweichung r_diff_proz? Wenn ja, wie stark?
reg_NI <- NI                                               # NI-Daten für Regression vorbereiten in data.frame reg_NI
reg_NI$Methode <- ifelse(is.na(reg_NI$ST_Fehler), 0, 1)    # neue Spalte einführen mit Dummy-Variable "Methode":
                                                           # Methode=1, wenn die Gemeinde eine Stichprobe hatte, sonst 0
N.gross.reg <- length(subset(NI.gross, Register <= limit.oben)$Register)      # Gem. MIT Stichpr. in Regression
N.klein.reg <- length(subset(NI.klein, Register >= limit.unten)$Register)     # Gem. OHNE Stichpr. in Regression
N.reg <- N.gross.reg + N.klein.reg                                            # Alle Gemeinden in Regression

reg_NI <- subset(reg_NI, Register >= limit.unten & Register <=limit.oben)     # Gemeinden der entsprechenden Größe auswählen 
ergebnis_reg_NI <- lm(R_diff_proz ~ Register + Methode, data=reg_NI)         # Lineare Regression berechnen. Ergebnise landen in ergebnis_reg_NI

# Poisson Regression
ergebnis_poi_NI <- glm(Zensus ~ I(log(Register)) + Methode,family=quasipoisson,data=reg_NI) 

# Prosa-Informationen ausgeben
prosa <- paste(prosa, "*** ANALYSE NIEDERSACHSEN ***\n\n", sep="")
prosa <- paste(prosa, "\n++ I. Einfluss der Stichprobe auf das Zensusergebnis ++\n\n", sep="")
prosa <- paste(prosa, "Lineare Regression für Gemeinden mit ", round0(limit.unten), " bis ", round0(limit.oben), " Einwohnern\n", sep="")
prosa <- paste(prosa, "(", round0(N.reg), " Gemeinden, ", round0(N.gross.reg), " mit Stichprobe und ", round0(N.klein.reg), " ohne)\n\n", sep="")
prosa <- paste(prosa, "Multivariate Analyse von zwei Einflussgrößen: \na) Stichprobe/keine Stichprobe \nb) Größe der Gemeinde (Einwohner laut Melderegister)\n\n", sep="")
prosa <- paste(prosa, "a) Eine Stichprobe ändert die %-Abweichung des Zensusergebnisses vom Register um\n ", 
roundx(summary(ergebnis_reg_NI)$coefficients[3,1], 2), " Prozentpunkte (Beta-Wert) gegenüber einer Gemeinde ohne Stichprobe.
Dieser Zusammenhang ist statistisch ", signifikanz(summary(ergebnis_reg_NI)$coefficients[3,4]), ".
Konfidenzintervall (95%): [", confint(ergebnis_reg_NI, 'Methode', level=0.95)[1] , " bis ", confint(ergebnis_reg_NI, 'Methode', level=0.95)[2], "]\n\n", sep="")
prosa <- paste(prosa, "b) Die Größe der Gemeinde (1.000 Einwohner mehr) ändert die %-Abweichung des Zensusergebnisses vom Register um\n",
roundx(summary(ergebnis_reg_NI)$coefficients[2,1]*1000,2), " Prozentpunkte (Beta-Wert*1.000).
Dieser Zusammenhang ist statistisch ", signifikanz(summary(ergebnis_reg_NI)$coefficients[2,4]), ".
Konfidenzintervall (95%): [", confint(ergebnis_reg_NI, 'Register', level=0.95)[1]*1000 , " bis ", confint(ergebnis_reg_NI, 'Register', level=0.95)[2]*1000, "]\n\n", sep="")
prosa <- paste(prosa, "Angaben zur Güte der Regression: \n", sep="")
prosa <- paste(prosa, "Bestimmtheitsmaß R²: ", roundx(summary(ergebnis_reg_NI)$r.squared,2), "\n\n", sep="")

prosa <- paste(prosa, "\n++ II. Alternativer Test mit Poisson-Regression  ++\n\n", sep="")
prosa <- paste(prosa, "Eine Stichprobe ändert die %-Abweichung des Zensusergebnisses vom Register um\n ", 
roundx((exp(ergebnis_poi_NI$coefficients[3])-1)*100, 2), " Prozentpunkte (Beta-Wert) gegenüber einer Gemeinde ohne Stichprobe.
Dieser Zusammenhang ist statistisch ", signifikanz(summary(ergebnis_poi_NI)$coefficients[3,4]), ".
Konfidenzintervall (95%): [", (exp(confint(ergebnis_poi_NI, 'Methode', level=0.95)[1])-1)*100 , " bis ", (exp(confint(ergebnis_poi_NI, 'Methode', level=0.95)[2])-1)*100, "]\n\n\n", sep="")

prosa <- paste(prosa, "++ III. Basisangaben zu den Gemeinden ++\n\n", sep="")
prosa <- paste(prosa, round0(NI.Bev), " Einwohner in insgesamt", round0(NI.N), "Gemeinden.\n...darunter:\n")
prosa <- paste(prosa, "   Gemeinden OHNE Stichprobe: ", round0(NI.N.klein), " (", round1(NI.N.klein/NI.N*100), "%)\n", sep="")
prosa <- paste(prosa, "   Bevölkerung dort:          ", round0(NI.Bev.klein), " (", round1(NI.Bev.klein/NI.Bev*100), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden MIT Stichprobe:  ", round0(NI.N.gross), " (", round1(NI.N.gross/NI.N*100), "%)\n", sep="")
prosa <- paste(prosa, "   Bevölkerung dort:          ", round0(NI.Bev.gross), " (", round1(NI.Bev.gross/NI.Bev*100), "%)\n", sep="")

prosa <- paste(prosa, "\nUnter den ", round0(NI.N.klein), " Gemeinden OHNE Stichprobe gabe es...\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit POSITIVER Abweichung (Zensus > Register): ", round0(NI.N.R_diff_plus.klein), " (", round1(NI.N.R_diff_plus.klein.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden OHNE Abweichung (Zensus = Register): ", round0(NI.N.R_diff_gleich.klein), " (", round1(NI.N.R_diff_gleich.klein.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit NEGATIVER Abweichung (Zensus < Register): ", round0(NI.N.R_diff_minus.klein), " (", round1(NI.N.R_diff_minus.klein.proz), "%)\n", sep="")
prosa <- paste(prosa, "\nUnter den ", round0(NI.N.gross), " Gemeinden MIT Stichprobe gabe es...\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit POSITIVER Abweichung (Zensus > Register): ", round0(NI.N.R_diff_plus.gross), " (", round1(NI.N.R_diff_plus.gross.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden OHNE Abweichung (Zensus = Register): ", round0(NI.N.R_diff_gleich.gross), " (", round1(NI.N.R_diff_gleich.gross.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit NEGATIVER Abweichung (Zensus < Register): ", round0(NI.N.R_diff_minus.gross), " (", round1(NI.N.R_diff_minus.gross.proz), "%)\n", sep="")


# Abweichung Zensus-Register plotten (logarithmische x-Achse)
# Achsen per Hand optimieren 

pdf(filename.NI.pdf, colormodel="cmyk", width=7, height=5, pointsize = 12)  # Plot als PDF speichern
par(xpd=NA,    # Grafikparameter: erlauben, außerhalb der Plot-Fläche in Grafiken zu zeichnen und zu schreiben
    las=1)     # Achsenbeschriftung horizontal

plot(NI$Register, NI$R_diff_proz, log="x", type="h", xaxt="n",lwd=1,
     main="Niedersachsen: Abw. Zensus-Register in %", xlab="Einwohnerzahl laut Register (logarithm. Skala)",
     ylab="Abweichung in %", ylim=c(-10,10), col="#aaaaaa", bty='n')    # Plot mit grauen Linien für proz. Gemeindeabweichungen
axis(side=1, labels=c(200,500, 1000, "3 Tsd.", "10 Tsd.", "30 Tsd.", "100 Tsd.", "500 Tsd."),
     at=c(200, 500, 1000, 3000, 10000, 30000, 100000, 500000))       # x-Achse
lines(c(200,500000), c(0,0), col="black")    # Null-Linie
points(NI.klein$Register, NI.klein$R_diff_proz, pch=19, cex=0.2, col="#00aa00")   # Grüne Punkte: Gemeinden ohne Stichprobe
points(NI.gross$Register, NI.gross$R_diff_proz, pch=19, cex=0.2, col="red")       # Rote Punkte: Gemeinden mit Stichprobe
lines(c(200,10000), c(NI.R_diff_proz.klein, NI.R_diff_proz.klein), col="darkgreen", lwd=2)   # Grüne Linie: Arithm. Mittel der proz. Abweichungen für Gemeinden ohne Stichprobe
lines(c(10000,500000), c(NI.R_diff_proz.gross, NI.R_diff_proz.gross), col="darkred", lwd=2)  # Rote Linie: Arithm. Mittel der proz. Abweichungen für Gemeinden mit Stichprobe

dev.off()  # PDF-Device für Plot schließen (und damit PDF speichern)


##########################################################
#                                                        #
#            Analyse für Rheinland-Pfalz (RP)            #
#                                                        #
##########################################################

# Dateinamen für Grafik (PDF) und Tabelle (CSV)
filename.RP.pdf <- 'Abweichung_Zensus-Register_RP.pdf'
filename.RP.tabelle <- 'Abweichung_Zensus-Register_RP_Groessenklassen_latin1.csv'

# Alle Gemeinden von RP auswählen anhand der ersten zwei Stellen 
# im Regionalschlüssel "RS_12" (="07" für RP):
RP <- subset(gesamt, substr(RS_12, 1,2)=="07")

# Gemeinden mit (gross) und ohne Stichprobe (klein) separieren:
RP.klein <- RP[which(is.na(RP$ST_Fehler)),]       # Alle Gemeinden OHNE Stichprobe ("klein")
RP.gross <- RP[which(!is.na(RP$ST_Fehler)),]      # Alle Gemeinden MIT Stichprobe ("gross")

# Verschiedene Größenklassen von Gemeidnen separieren
RP.gross.ab_30_Tsd <- subset(RP.gross, Register >= 30000)                    # Gem. MIT Stichpr. ab 30.000 Einwohner
RP.gross.20_30_Tsd <- subset(RP.gross, Register >= 20000 & Register < 30000)   # Gem. MIT Stichpr. ab 20.000 bis unter 30.000 Einw.
RP.gross.15_20_Tsd <- subset(RP.gross, Register >= 15000 & Register < 20000)   # Gem. MIT Stichpr. ab 15.000 bis unter 20.000 Einw.
RP.gross.10_20_Tsd <- subset(RP.gross, Register < 20000)                     # Gem. MIT Stichpr. bis unter 20.000 Einw. (ab etwa 10.000)
RP.gross.10_15_Tsd <- subset(RP.gross, Register < 15000)                     # Gem. MIT Stichpr. bis unter 15.000 Einw. (ab etwa 10.000)
RP.gross.10_11_Tsd <- subset(RP.gross, Register < 11000)                     # Gem. MIT Stichpr. bis unter 11.000 Einw. (ab etwa 10.000)
RP.klein.9_10_Tsd <- subset(RP.klein, Register >= 9000)                      # Gem. OHNE Stichpr. ab 9.000 Einw. (bis etwa 10.000)
RP.klein.5_10_Tsd <- subset(RP.klein, Register >= 5000)                      # Gem. OHNE Stichpr. ab 5.000 Einw. (bis etwa 10.000)
RP.klein.2.5_5_Tsd <- subset(RP.klein, Register >= 2500 & Register < 5000)     # Gem. OHNE Stichpr. ab 2.500 bis unter 5.000 Einw.
RP.klein.1_2.5_Tsd <- subset(RP.klein, Register >= 1000 & Register < 2500)     # Gem. OHNE Stichpr. ab 1.000 bis unter 2.500 Einw.
RP.klein.u1_Tsd <- subset(RP.klein, Register < 1000)                         # Gem. OHNE Stichpr. unter 1.000 Einw.

# Basiszahlen berechnen:
RP.N <- length(RP$Gemeinde)       # Anzahl Gemeinden in RP insgesamt
RP.Bev <- sum(RP$Register)        # Bevölkerung in RP insgesamt laut Register
RP.N.klein <- length(RP.klein$Gemeinde)  # Anzahl Gemeinden OHNE Stichprobe
RP.Bev.klein <- sum(RP.klein$Register)     # Bevölkerung in Gemeinden OHNE Stichprobe laut Register
RP.N.klein.9_10_Tsd <- length(RP.klein.9_10_Tsd$Gemeinde)     # Anzahl Gem. OHNE Stichpr. ab 9.000 Einw. (bis etwa 10.000)
RP.N.klein.5_10_Tsd <- length(RP.klein.5_10_Tsd$Gemeinde)     # Anzahl Gem. OHNE Stichpr. ab 5.000 Einw. (bis etwa 10.000)
RP.N.klein.2.5_5_Tsd <- length(RP.klein.2.5_5_Tsd$Gemeinde)   # Anzahl Gem. OHNE Stichpr. ab 2.500 bis unter 5.000 Einw.
RP.N.klein.1_2.5_Tsd <- length(RP.klein.1_2.5_Tsd$Gemeinde)   # Anzahl Gem. OHNE Stichpr. ab 1.000 bis unter 2.500 Einw.
RP.N.klein.u1_Tsd <- length(RP.klein.u1_Tsd$Gemeinde)         # Anzahl Gem. OHNE Stichpr. unter 1.000 Einw.
RP.N.gross <- length(RP.gross$Gemeinde)    # Anzahl Gemeinden MIT Stichprobe
RP.Bev.gross <- sum(RP.gross$Register)     # Bevölkerung in Gemeinden MIT Stichprobe laut Register
RP.N.gross.ab_30_Tsd <- length(RP.gross.ab_30_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. ab 30.000 Einw.
RP.N.gross.20_30_Tsd <- length(RP.gross.20_30_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. ab 20.000 bis unter 30.000 Einw.
RP.N.gross.15_20_Tsd <- length(RP.gross.15_20_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. ab 15.000 bis unter 20.000 Einw.
RP.N.gross.10_20_Tsd <- length(RP.gross.10_20_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. bis unter 20.000 Einw.
RP.N.gross.10_15_Tsd <- length(RP.gross.10_15_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. bis unter 15.000 Einw. (ab etwa 10.000)
RP.N.gross.10_11_Tsd <- length(RP.gross.10_11_Tsd$Gemeinde)     # Anzahl Gem. MIT Stichpr. bis unter 11.000 Einw. (ab etwa 10.000)


# Analyse Methoden-Sprung bei 10.000 Einwohnern:
RP.R_diff_proz <- mean(RP$R_diff_proz)                            # ALLE Gemeinden: Arith. Mittel der prozentualen Register-Abweichungen
RP.R_diff_proz.klein <- mean(RP.klein$R_diff_proz)                # Alle Gem. OHNE Stichpr.: Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.abs.klein <- mean(abs(RP.klein$R_diff_proz))       # Alle Gem. OHNE Stichpr.: Arith. Mittel der absoluten proz. Abw.
RP.R_diff_proz.9_10_Tsd <- mean(RP.klein.9_10_Tsd$R_diff_proz)    # Gem. OHNE Stichpr. ab 9.000 Einw. (bis etwa 10.000): Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.5_10_Tsd <- mean(RP.klein.5_10_Tsd$R_diff_proz)    # Gem. OHNE Stichpr. ab 5.000 Einw. (bis etwa 10.000): Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.2.5_5_Tsd <- mean(RP.klein.2.5_5_Tsd$R_diff_proz)  # Gem. OHNE Stichpr. ab 2.500 bis unter 5.000 Einw.: Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.1_2.5_Tsd <- mean(RP.klein.1_2.5_Tsd$R_diff_proz)  # Gem. OHNE Stichpr. ab 1.000 bis unter 2.500 Einw.: Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.u1_Tsd <- mean(RP.klein.u1_Tsd$R_diff_proz)        # Gem. OHNE Stichpr. unter 1.000 Einw.: Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.gross <- mean(RP.gross$R_diff_proz)                # Alle Gem. MIT Stichpr.: Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.abs.gross <- mean(abs(RP.gross$R_diff_proz))       # Alle Gem. MIT Stichpr.: Arith. Mittel der absoluten proz. Abw.
RP.R_diff_proz.ab_30_Tsd <- mean(RP.gross.ab_30_Tsd$R_diff_proz)  # Gem. MIT Stichpr. ab 30.000 Einw.: Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.20_30_Tsd <- mean(RP.gross.20_30_Tsd$R_diff_proz)  # Gem. MIT Stichpr. ab 20.000 bis unter 30.000 Einw.: Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.15_20_Tsd <- mean(RP.gross.15_20_Tsd$R_diff_proz)  # Gem. MIT Stichpr. ab 15.000 bis unter 20.000 Einw.: Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.10_20_Tsd <- mean(RP.gross.10_20_Tsd$R_diff_proz)  # Gem. MIT Stichpr. bis unter 20.000 Einw. (ab etwa 10.000): Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.10_15_Tsd <- mean(RP.gross.10_15_Tsd$R_diff_proz)  # Gem. MIT Stichpr. bis unter 15.000 Einw. (ab etwa 10.000): Arith. Mittel der prozentualen Abweichungen
RP.R_diff_proz.10_11_Tsd <- mean(RP.gross.10_11_Tsd$R_diff_proz)  # Gem. MIT Stichpr. bis unter 11.000 Einw. (ab etwa 10.000): Arith. Mittel der prozentualen Abweichungen


# Wie viele Gemeinden haben jeweils positive oder negative Abweichung?
RP.N.R_diff_plus.klein <- length(RP.klein$Gemeinde[RP.klein$R_diff>0])     # Gem. OHNE Stichpr. mit POSITIVER Abweichung (Zensus > Register)
RP.N.R_diff_plus.klein.proz <- RP.N.R_diff_plus.klein/RP.N.klein*100       # ... in Prozent
RP.N.R_diff_gleich.klein <- length(RP.klein$Gemeinde[RP.klein$R_diff==0])  # Gem. OHNE Stichpr. OHNE Abweichung (Zensus = Register)
RP.N.R_diff_gleich.klein.proz <- RP.N.R_diff_gleich.klein/RP.N.klein*100   # ... in Prozent
RP.N.R_diff_minus.klein <- length(RP.klein$Gemeinde[RP.klein$R_diff<0])    # Gem. OHNE Stichpr. mit NEGATIVER Abweichung (Zensus < Register)
RP.N.R_diff_minus.klein.proz <- RP.N.R_diff_minus.klein/RP.N.klein*100     # ... in Prozent
RP.N.R_diff_plus.gross <- length(RP.gross$Gemeinde[RP.gross$R_diff>0])     # Gem. MIT Stichpr. mit POSITIVER Abweichung (Zensus > Register)
RP.N.R_diff_plus.gross.proz <- RP.N.R_diff_plus.gross/RP.N.gross*100       # ... in Prozent
RP.N.R_diff_gleich.gross <- length(RP.gross$Gemeinde[RP.gross$R_diff==0])  # Gem. MIT Stichpr. OHNE Abweichung (Zensus = Register)
RP.N.R_diff_gleich.gross.proz <- RP.N.R_diff_gleich.gross/RP.N.gross*100   # ... in Prozent
RP.N.R_diff_minus.gross <- length(RP.gross$Gemeinde[RP.gross$R_diff<0])    # Gem. MIT Stichpr. mit NEGATIVER Abweichung (Zensus < Register)
RP.N.R_diff_minus.gross.proz <- RP.N.R_diff_minus.gross/RP.N.gross*100     # ... in Prozent

# Abweichungen nach Größenklases in Tabelle anordnen und speichern

# Zuerst Spalten erschaffen:
RP.spalte.info <- c("< 1.000", "1.000 bis < 2.500", "2.500 bis < 5.000", "5.000 bis < 10.000", "9.000 bis < 10.000",
                    "10.000 bis < 11.000", "10.000 bis < 15.000", "10.000 bis < 20.000", "15.000 bis < 20.000", 
                    "20.000 bis < 30.000", "30.000 und mehr", "ohne Stichprobe", "mit Stichprobe", "gesamt")

RP.spalte.N <- c(RP.N.klein.u1_Tsd, RP.N.klein.1_2.5_Tsd, RP.N.klein.2.5_5_Tsd, RP.N.klein.5_10_Tsd, RP.N.klein.9_10_Tsd,
                 RP.N.gross.10_11_Tsd, RP.N.gross.10_15_Tsd, RP.N.gross.10_20_Tsd, RP.N.gross.15_20_Tsd, RP.N.gross.20_30_Tsd, 
                 RP.N.gross.ab_30_Tsd, RP.N.klein, RP.N.gross, RP.N)

RP.spalte.proz <- c(RP.R_diff_proz.u1_Tsd, RP.R_diff_proz.1_2.5_Tsd, RP.R_diff_proz.2.5_5_Tsd, RP.R_diff_proz.5_10_Tsd, RP.R_diff_proz.9_10_Tsd,
                    RP.R_diff_proz.10_11_Tsd, RP.R_diff_proz.10_15_Tsd, RP.R_diff_proz.10_20_Tsd, RP.R_diff_proz.15_20_Tsd, RP.R_diff_proz.20_30_Tsd, 
                    RP.R_diff_proz.ab_30_Tsd, RP.R_diff_proz.klein, RP.R_diff_proz.gross, RP.R_diff_proz)

RP.tabelle <- data.frame(cbind(RP.spalte.info, RP.spalte.N, RP.spalte.proz))                         # Spalten zu Tabelle zusammenbinden
colnames(RP.tabelle) <- (c("Größenklasse (Einwohner)", "Anzahl Gemeinden","Registerabw. in %"))      # Spaltennamen hinzufügen
write.csv2(RP.tabelle, filename.RP.tabelle,  quote=FALSE, row.names=FALSE, fileEncoding = "latin1")  # Tabelle als CSV-Datei speichern  


# Lineare Regression: Beeinflusst der Methodenwechsel die prozentuale
# Registerabweichung r_diff_proz? Wenn ja, wie stark?
reg_RP <- RP                                               # RP-Daten für Regression vorbereiten in data.frame reg_RP
reg_RP$Methode <- ifelse(is.na(reg_RP$ST_Fehler), 0, 1)    # neue Spalte einführen mit Dummy-Variable "Methode":
                                                           # Methode=1, wenn die Gemeinde eine Stichprobe hatte, sonst 0
N.gross.reg <- length(subset(RP.gross, Register <= limit.oben)$Register)      # Gem. MIT Stichpr. in Regression
N.klein.reg <- length(subset(RP.klein, Register >= limit.unten)$Register)     # Gem. OHNE Stichpr. in Regression
N.reg <- N.gross.reg + N.klein.reg                                        # Alle Gemeinden in Regression

reg_RP <- subset(reg_RP, Register >= limit.unten & Register <=limit.oben) # Gemeinden der entsprechenden Größe auswählen 
ergebnis_reg_RP <- lm(R_diff_proz ~ Register + Methode, data=reg_RP)    # Lineare Regression berechnen. Ergebnise landen in ergebnis_reg_RP

# Poisson Regression
ergebnis_poi_RP <- glm(Zensus ~ I(log(Register)) + Methode,family=quasipoisson,data=reg_RP) 


# Prosa-Informationen ausgeben
prosa <- paste(prosa, "\n\n*** ANALYSE RHEINLAND-PFALZ ***\n\n", sep="")
prosa <- paste(prosa, "\n++ I. Einfluss der Stichprobe auf das Zensusergebnis ++\n\n", sep="")
prosa <- paste(prosa, "Lineare Regression für alle Gemeinden mit ", round0(limit.unten), " bis ", round0(limit.oben), " Einwohnern\n", sep="")
prosa <- paste(prosa, "(", round0(N.reg), " Gemeinden, ", round0(N.gross.reg), " mit Stichprobe und ", round0(N.klein.reg), " ohne)\n\n", sep="")
prosa <- paste(prosa, "Multivariate Analyse von zwei Einflussgrößen: \na) Stichprobe/keine Stichprobe \nb) Größe der Gemeinde (Einwohner laut Melderegister)\n\n", sep="")
prosa <- paste(prosa, "a) Eine Stichprobe ändert die %-Abweichung des Zensusergebnisses vom Register um\n ", 
roundx(summary(ergebnis_reg_RP)$coefficients[3,1], 2), " Prozentpunkte (Beta-Wert) gegenüber einer Gemeinde ohne Stichprobe.
Dieser Zusammenhang ist statistisch ", signifikanz(summary(ergebnis_reg_RP)$coefficients[3,4]), ".
Konfidenzintervall (95%): [", confint(ergebnis_reg_RP, 'Methode', level=0.95)[1] , " bis ", confint(ergebnis_reg_RP, 'Methode', level=0.95)[2], "]\n\n", sep="")
prosa <- paste(prosa, "b) Die Größe der Gemeinde (1.000 Einwohner mehr) ändert die %-Abweichung des Zensusergebnisses vom Register um\n",
roundx(summary(ergebnis_reg_RP)$coefficients[2,1]*1000,2), " Prozentpunkte (Beta-Wert*1.000).
Dieser Zusammenhang ist statistisch ", signifikanz(summary(ergebnis_reg_RP)$coefficients[2,4]), ".
Konfidenzintervall (95%): [", confint(ergebnis_reg_RP, 'Register', level=0.95)[1]*1000 , " bis ", confint(ergebnis_reg_RP, 'Register', level=0.95)[2]*1000, "]\n\n", sep="")
prosa <- paste(prosa, "Angaben zur Güte der Regression: \n", sep="")
prosa <- paste(prosa, "Bestimmtheitsmaß R²: ", roundx(summary(ergebnis_reg_RP)$r.squared,2), "\n\n", sep="")

prosa <- paste(prosa, "\n++ II. Alternativer Test mit Poisson-Regression  ++\n\n", sep="")
prosa <- paste(prosa, "Eine Stichprobe ändert die %-Abweichung des Zensusergebnisses vom Register um\n ", 
roundx((exp(ergebnis_poi_RP$coefficients[3])-1)*100, 2), " Prozentpunkte (Beta-Wert) gegenüber einer Gemeinde ohne Stichprobe.
Dieser Zusammenhang ist statistisch ", signifikanz(summary(ergebnis_poi_RP)$coefficients[3,4]), ".
Konfidenzintervall (95%): [", (exp(confint(ergebnis_poi_RP, 'Methode', level=0.95)[1])-1)*100 , " bis ", (exp(confint(ergebnis_poi_RP, 'Methode', level=0.95)[2])-1)*100, "]\n\n\n", sep="")

prosa <- paste(prosa, "++ III. Basisangaben zu den Gemeinden ++\n\n", sep="")
prosa <- paste(prosa, round0(RP.Bev), " Einwohner in insgesamt", round0(RP.N), "Gemeinden.\n...darunter:\n")
prosa <- paste(prosa, "   Gemeinden OHNE Stichprobe: ", round0(RP.N.klein), " (", round1(RP.N.klein/RP.N*100), "%)\n", sep="")
prosa <- paste(prosa, "   Bevölkerung dort:          ", round0(RP.Bev.klein), " (", round1(RP.Bev.klein/RP.Bev*100), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden MIT Stichprobe:  ", round0(RP.N.gross), " (", round1(RP.N.gross/RP.N*100), "%)\n", sep="")
prosa <- paste(prosa, "   Bevölkerung dort:          ", round0(RP.Bev.gross), " (", round1(RP.Bev.gross/RP.Bev*100), "%)\n", sep="")

prosa <- paste(prosa, "\nUnter den ", round0(RP.N.klein), " Gemeinden OHNE Stichprobe gabe es...\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit POSITIVER Abweichung (Zensus > Register): ", round0(RP.N.R_diff_plus.klein), " (", round1(RP.N.R_diff_plus.klein.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden OHNE Abweichung (Zensus = Register): ", round0(RP.N.R_diff_gleich.klein), " (", round1(RP.N.R_diff_gleich.klein.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit NEGATIVER Abweichung (Zensus < Register): ", round0(RP.N.R_diff_minus.klein), " (", round1(RP.N.R_diff_minus.klein.proz), "%)\n", sep="")
prosa <- paste(prosa, "\nUnter den ", round0(RP.N.gross), " Gemeinden MIT Stichprobe gabe es...\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit POSITIVER Abweichung (Zensus > Register): ", round0(RP.N.R_diff_plus.gross), " (", round1(RP.N.R_diff_plus.gross.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden OHNE Abweichung (Zensus = Register): ", round0(RP.N.R_diff_gleich.gross), " (", round1(RP.N.R_diff_gleich.gross.proz), "%)\n", sep="")
prosa <- paste(prosa, "   Gemeinden mit NEGATIVER Abweichung (Zensus < Register): ", round0(RP.N.R_diff_minus.gross), " (", round1(RP.N.R_diff_minus.gross.proz), "%)\n", sep="")


# Prosa-Information ausgeben und als Datei speichern:
cat(prosa)                                                       # Auf der Konsole ausgeben
cat(prosa, file = (con <- file("Register_Infos_latin1.txt", "w", encoding="latin1"))); close(con)    # In Datei speichern


# Abweichung Zensus-Register plotten (logarithmische x-Achse)
# Achsen per Hand optimieren! 
pdf(filename.RP.pdf, colormodel="cmyk", width=7, height=5, pointsize = 12)  # Plot als PDF speichern
par(xpd=NA,    # Grafikparameter: erlauben, außerhalb der Plot-Fläche in Grafiken zu zeichnen und zu schreiben
    las=1)     # Achsenbeschriftung horizontal

plot(RP$Register, RP$R_diff_proz, log="x", type="h", xaxt="n",
     main="Rheinl.-Pfalz: Abw. Zensus-Register in %", xlab="Einwohnerzahl laut Register (logarithm. Skala)",
     ylab="Abweichung in %", xlim=c(10, 250000), ylim=c(-10,10), 
     col="#aaaaaa", bty='n')    # Plot mit grauen Linien für proz. Gemeindeabweichungen
axis(side=1, labels=c(10,100, 1000, "10 Tsd.", "100 Tsd."),
     at=c(10,100, 1000, 10000, 100000))       # x-Achse
lines(c(10,250000), c(0,0), col="black")    # Null-Linie
points(RP.klein$Register, RP.klein$R_diff_proz, pch=19, cex=0.2, col="#00aa00")   # Grüne Punkte: Gemeinden ohne Stichprobe
points(RP.gross$Register, RP.gross$R_diff_proz, pch=19, cex=0.2, col="red")       # Rote Punkte: Gemeinden mit Stichprobe
lines(c(10,10000), c(RP.R_diff_proz.klein, RP.R_diff_proz.klein), col="darkgreen", lwd=2)    # Grüne Linie: Arithm. Mittel der proz. Abweichungen für Gemeinden ohne Stichprobe
lines(c(10000,250000), c(RP.R_diff_proz.gross, RP.R_diff_proz.gross), col="darkred", lwd=2)  # Rote Linie: Arithm. Mittel der proz. Abweichungen für Gemeinden mit Stichprobe

dev.off()  # PDF-Device für Plot schließen (und damit PDF speichern)

