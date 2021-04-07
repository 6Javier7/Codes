library(tidyverse)
library(vegan)
library(data.table)
library(lme4)
setwd("/home/javier/Descargas/Jhon")
abun <- read.csv("abundancia")
abun <- data.table(abun)
abun$Estacion <- factor(abun$Estacion, levels = c("P01","P02","P03","P04","P05","P06","P07","P08","P09","P010","P011","P012","P013","P014","P015","P016","P017","P018","P019","P020","P021","P022","P023","P024","P025","P026"))

abun1 <- abun %>% group_by(Estacion) %>% summarise(Abudefduf_saxatilis = sum(Abudefduf_saxatilis),
                                                            Acanthostracion_polygonia = sum(Acanthostracion_polygonia),
                                                            Acanthostracion_quadricornis = sum(Acanthostracion_quadricornis),
                                                            Acanthurus_bahianus = sum(Acanthurus_bahianus),
                                                            Acanthurus_chirurgus = sum(Acanthurus_chirurgus),
                                                            Acanthurus_coeruleus = sum(Acanthurus_coeruleus),
                                                            Aetobatus_narinari = sum(Aetobatus_narinari),
                                                            Aluterus_schoepfii = sum(Aluterus_schoepfii),
                                                            Aluterus_scriptus = sum(Aluterus_scriptus),
                                                            Anisotremus_virginicus = sum(Anisotremus_virginicus),
                                                            Apogon_maculatus = sum(Apogon_maculatus),
                                                            Aulostomus_maculatus = sum(Aulostomus_maculatus),
                                                            Balistes_capriscus  = sum(Balistes_capriscus ),
                                                            Balistes_vetula = sum(Balistes_vetula),
                                                            Bodianus_rufus = sum(Bodianus_rufus),
                                                            Bothus_lunatus = sum(Bothus_lunatus),
                                                            Calamus_bajonado = sum(Calamus_bajonado),
                                                            Calamus_penna = sum(Calamus_penna),
                                                            Cantherhines_macrocerus = sum(Cantherhines_macrocerus),
                                                            Cantherhines_pullus = sum(Cantherhines_pullus),
                                                            Canthidermis_sufflamen = sum(Canthidermis_sufflamen),
                                                            Canthigaster_rostrata = sum(Canthigaster_rostrata),
                                                            Carangoides_bartholomaei = sum(Carangoides_bartholomaei),
                                                            Carangoides_ruber = sum(Carangoides_ruber),
                                                            Caranx_crysos = sum(Caranx_crysos),
                                                            Caranx_hippos = sum(Caranx_hippos),
                                                            Caranx_latus = sum(Caranx_latus),
                                                            Centropomus_undecimalis = sum(Centropomus_undecimalis),
                                                            Centropyge_argi = sum(Centropyge_argi),
                                                            Cephalopholis_cruentatus = sum(Cephalopholis_cruentatus),
                                                            Cephalopholis_fulva = sum(Cephalopholis_fulva),
                                                            Chaetodon_capistratus = sum(Chaetodon_capistratus),
                                                            Chaetodon_ocellatus = sum(Chaetodon_ocellatus),
                                                            Chaetodon_striatus = sum(Chaetodon_striatus),
                                                            Chromis_cyanea = sum(Chromis_cyanea),
                                                            Chromis_multilineata = sum(Chromis_multilineata),
                                                            Clepticus_parrae = sum(Clepticus_parrae),
                                                            Coryphopterus_glaucofraenum = sum(Coryphopterus_glaucofraenum),
                                                            Cryptotomus_roseus = sum(Cryptotomus_roseus),
                                                            Dasyatis_americana = sum(Dasyatis_americana),
                                                            Diodon_holocanthus = sum(Diodon_holocanthus),
                                                            Diodon_hystrix = sum(Diodon_hystrix),
                                                            Elacatinus_evelynae = sum(Elacatinus_evelynae),
                                                            Epinephelus_adscensionis = sum(Epinephelus_adscensionis),
                                                            Epinephelus_guttatus = sum(Epinephelus_guttatus),
                                                            Epinephelus_striatus = sum(Epinephelus_striatus),
                                                            Equetus_lanceolatus = sum(Equetus_lanceolatus),
                                                            Equetus_punctatus = sum(Equetus_punctatus),
                                                            Fistularia_tabacaria = sum(Fistularia_tabacaria),
                                                            Gerres_cinereus = sum(Gerres_cinereus),
                                                            Ginglymostoma_cirratum = sum(Ginglymostoma_cirratum),
                                                            Gramma_loreto = sum(Gramma_loreto),
                                                            Gymnothorax_moringa = sum(Gymnothorax_moringa),
                                                            Haemulon_album = sum(Haemulon_album),
                                                            Haemulon_aurolineatum = sum(Haemulon_aurolineatum),
                                                            Haemulon_bonaeriensi = sum(Haemulon_bonaeriensi),
                                                            Haemulon_carbonarium = sum(Haemulon_carbonarium),
                                                            Haemulon_chrysargyreum = sum(Haemulon_chrysargyreum),
                                                            Haemulon_flavolineatum = sum(Haemulon_flavolineatum),
                                                            Haemulon_macrostomum = sum(Haemulon_macrostomum),
                                                            Haemulon_melanurum = sum(Haemulon_melanurum),
                                                            Haemulon_parra = sum(Haemulon_parra),
                                                            Haemulon_plumierii = sum(Haemulon_plumierii),
                                                            Haemulon_sciurus = sum(Haemulon_sciurus),
                                                            Haemulon_striatum = sum(Haemulon_striatum),
                                                            Halichoeres_bivittatus = sum(Halichoeres_bivittatus),
                                                            Halichoeres_cyanocephalus = sum(Halichoeres_cyanocephalus),
                                                            Halichoeres_garnoti = sum(Halichoeres_garnoti),
                                                            Halichoeres_maculipinna = sum(Halichoeres_maculipinna),
                                                            Halichoeres_pictus = sum(Halichoeres_pictus),
                                                            Halichoeres_poeyi = sum(Halichoeres_poeyi),
                                                            Halichoeres_radiatus = sum(Halichoeres_radiatus),
                                                            Holacanthus_ciliaris = sum(Holacanthus_ciliaris),
                                                            Holacanthus_tricolor = sum(Holacanthus_tricolor),
                                                            Holocentrus_adscensionis = sum(Holocentrus_adscensionis),
                                                            Holocentrus_rufus = sum(Holocentrus_rufus),
                                                            Hypoplectrus_guttavarius = sum(Hypoplectrus_guttavarius),
                                                            Hypoplectrus_indigo = sum(Hypoplectrus_indigo),
                                                            Hypoplectrus_nigricans = sum(Hypoplectrus_nigricans),
                                                            Hypoplectrus_puella = sum(Hypoplectrus_puella),
                                                            Hypoplectrus_unicolor = sum(Hypoplectrus_unicolor),
                                                            Inermia_vittata = sum(Inermia_vittata),
                                                            Kiphosus_sectator = sum(Kiphosus_sectator),
                                                            Lachnolaimus_maximus = sum(Lachnolaimus_maximus),
                                                            Lactophrys_bicaudalis = sum(Lactophrys_bicaudalis),
                                                            Lactophrys_trigonus = sum(Lactophrys_trigonus),
                                                            Lactophrys_triqueter = sum(Lactophrys_triqueter),
                                                            Lutjanus_analis = sum(Lutjanus_analis),
                                                            Lutjanus_apodus = sum(Lutjanus_apodus),
                                                            Lutjanus_cyanopterus = sum(Lutjanus_cyanopterus),
                                                            Lutjanus_griseus = sum(Lutjanus_griseus),
                                                            Lutjanus_jocu = sum(Lutjanus_jocu),
                                                            Lutjanus_mahogoni = sum(Lutjanus_mahogoni),
                                                            Lutjanus_synagris = sum(Lutjanus_synagris),
                                                            Malacanthus_plumieri = sum(Malacanthus_plumieri),
                                                            Malacoctenus_triangulatus = sum(Malacoctenus_triangulatus),
                                                            Melichthys_niger = sum(Melichthys_niger),
                                                            Microspathodon_chrysurus = sum(Microspathodon_chrysurus),
                                                            Mulloidichthys_martinicus = sum(Mulloidichthys_martinicus),
                                                            Mycteroperca_bonaci = sum(Mycteroperca_bonaci),
                                                            Mycteroperca_tigris = sum(Mycteroperca_tigris),
                                                            Mycteroperca_venenosa = sum(Mycteroperca_venenosa),
                                                            Myripristis_jacobus = sum(Myripristis_jacobus),
                                                            Ocyurus_chrysurus = sum(Ocyurus_chrysurus),
                                                            Ophioblennius_atlanticus_macclurei = sum(Ophioblennius_atlanticus_macclurei),
                                                            Pempheris_schomburgkii = sum(Pempheris_schomburgkii),
                                                            Pomacanthus_arcuatus = sum(Pomacanthus_arcuatus),
                                                            Pomacanthus_paru = sum(Pomacanthus_paru),
                                                            Pseudopeneus_maculatus = sum(Pseudopeneus_maculatus),
                                                            Rypticus_saponaceus = sum(Rypticus_saponaceus),
                                                            Scarus_coelestinus = sum(Scarus_coelestinus),
                                                            Scarus_coeruleus = sum(Scarus_coeruleus),
                                                            Scarus_guacamaia = sum(Scarus_guacamaia),
                                                            Scarus_iserti = sum(Scarus_iserti),
                                                            Scarus_taeniopterus = sum(Scarus_taeniopterus),
                                                            Scarus_vetula = sum(Scarus_vetula),
                                                            Scomberomorus_cavalla = sum(Scomberomorus_cavalla),
                                                            Scomberomorus_regalis = sum(Scomberomorus_regalis),
                                                            Serranus_tabacarius = sum(Serranus_tabacarius),
                                                            Serranus_tigrinus = sum(Serranus_tigrinus),
                                                            Sparisoma_atromarium = sum(Sparisoma_atromarium),
                                                            Sparisoma_aurofrenatum = sum(Sparisoma_aurofrenatum),
                                                            Sparisoma_chrysopterum = sum(Sparisoma_chrysopterum),
                                                            Sparisoma_radians = sum(Sparisoma_radians),
                                                            Sparisoma_rubripinne = sum(Sparisoma_rubripinne),
                                                            Sparisoma_viride = sum(Sparisoma_viride),
                                                            Sphyraena_barracuda = sum(Sphyraena_barracuda),
                                                            Stegastes_adustus = sum(Stegastes_adustus),
                                                            Stegastes_diencaeus = sum(Stegastes_diencaeus),
                                                            Stegastes_leucostictus = sum(Stegastes_leucostictus),
                                                            Stegastes_partitus = sum(Stegastes_partitus),
                                                            Stegastes_planifrons = sum(Stegastes_planifrons),
                                                            Stegastes_variabilis_Stegastes_Xanthurus = sum(Stegastes_variabilis_Stegastes_Xanthurus),
                                                            Thalassoma_bifasciatum = sum(Thalassoma_bifasciatum),
                                                            Urobatis_jamaicensis = sum(Urobatis_jamaicensis),
                                                            Xanthichthys_ringens = sum(Xanthichthys_ringens),
                                                            Xyrichtys_martinicensis = sum(Xyrichtys_martinicensis),
                                                            Xyrichtys_splendens = sum(Xyrichtys_splendens),
                                                            Seriola_rivoliana = sum(Seriola_rivoliana),
                                                            Synodus_intermedius = sum(Synodus_intermedius),
                                                            Caranx_lugubris = sum(Caranx_lugubris),
                                                            Gymnothorax_miliaris = sum(Gymnothorax_miliaris),
                                                            Gymnothorax_funebris = sum(Gymnothorax_funebris),
                                                            Heteropriacanthus_cruentatus = sum(Heteropriacanthus_cruentatus),
                                                            Alectis_ciliaris = sum(Alectis_ciliaris),
                                                            Scorpaena_plumieri = sum(Scorpaena_plumieri),
                                                            Carcharhinus_perezi = sum(Carcharhinus_perezi),
                                                            Hypoplectrus_aberrans = sum(Hypoplectrus_aberrans),
                                                            Calamus_calamus = sum(Calamus_calamus),
                                                            Hypoplectrus_providencianus = sum(Hypoplectrus_providencianus),
                                                            Pterois_volitans = sum(Pterois_volitans)) 

write.csv( abun1[,-1], "abun1")
abun2 <- abun1[,-1]
riq <- specnumber(abun2)
sac <- specaccum(abun2) # Curva de acumulacion
plot(sac, ci.type="polygon", ci.col="blue")
H <- diversity(abun2)# Shannon
Simp <- diversity(abun2, "simpson") # Simpson
InvSimp <- diversity(abun2, "inv") # Reciproco de Simpson
J <- H/log(specnumber(abun2)) # indice de equitatividad de J pielou
q <- quantile(rowSums(abun2)) #Numero minimo de individuos que escogi por especie
Rarefaccion <- rarefy(abun2, min(q)) #Rarefaccion
k <- sample(nrow(abun2), 1)
fish <- fisherfit(abun2[k,])
rad <- radfit(abun2[k,]) # Rangos de la distribucion de abundancias
pool1 <- poolaccum(abun2, permutations = 10000, minsize = 3)
specpool(abun2)
plot(pool1)

prov <- read.csv("providencia")
prov <- data.table(prov)
prov$Estacion <- factor(prov$Estacion, levels = c("P01","P02","P03","P04","P05","P06","P07","P08","P09","P010","P011","P012","P013","P014","P015","P016","P017","P018","P019","P020","P021","P022","P023","P024","P025","P026"))


m1c <- lm(Abundancia ~ Complejidad + Estacion/Transecto, prov)
shapiro.test(resid(m1c))

m1e <- lmer(Abundancia ~ Estacion/Transecto + (1|Complejidad), REML = FALSE,  prov) #Modelo anidado
install.packages("lsmeans")
library(lsmeans)
plot(m1c)
con <- emmeans(m1e, ~ Estacion, method = "tukey")
contrast(con)
pairs(con) #Pairwise comparisons
pwpm(con)
plot(con, comparisons = TRUE) #Graphical comparisons


prov1 <- prov %>% group_by(Estacion) %>% summarise(Complejidadm = mean(Complejidad, na.rm = T), Complejidadsd = sd(Abundancia))

cor.test(prov$Abundancia, prov$Complejidad, method = "spearman")


#Spearman's rank correlation rho

#data:  prov$Abundancia and prov$Complejidad
#S = 113543, p-value = 0.03299
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#-0.2357567 

prov2 <- na.omit(prov)
comp <- prov2[ , lapply(.SD, mean),  .SDcols = "Complejidad", by = "Estacion"]
criq <- cbind(comp[,2], riq)
plot(criq)
criq <- criq[order(Complejidad),]
cor.test(criq$Complejidad, criq$riq, method = "spearman")
args(cor.test)

#Spearman's rank correlation rho

#data:  criq$Complejidad and criq$riq
#S = 3934, p-value = 0.08436
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#-0.3449694 
