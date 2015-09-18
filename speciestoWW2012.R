# Change names in Lizzie's version to be consistent with Weber & Whittmann 2012
# By Amy Iler 

getwd() # should be in main gothic folder
data <- read.csv("gothicrecomp/species/gothicclean.csv", header=TRUE)

levels(data$species)[levels(data$species)=="Stipa lettermanii"] <- "Achnatherum lettermanii"
levels(data$species)[levels(data$species)=="Linum lewisii"] <- "Adenolinum lewisii"
levels(data$species)[levels(data$species)=="Amelanchier pumila"] <- "Amelanchier alnifolia"
levels(data$species)[levels(data$species)=="Arenaria congesta"] <- "Eremogone congesta"
levels(data$species)[levels(data$species)=="Artemisia dracunculus"] <- "Oligosporus dracunculus"
levels(data$species)[levels(data$species)=="Polygonum vivipara"] <- "Bistorta vivipara"
levels(data$species)[levels(data$species)=="Castilleja miniata"] <- "Castilleja linariifolia"
levels(data$species)[levels(data$species)=="Dasiphora fruticosa"] <- "Pentaphylloides floribunda"
levels(data$species)[levels(data$species)=="Deschampsia caespitosa"] <- "Deschampsia cespitosa"  # just a typo fix
levels(data$species)[levels(data$species)=="Descurainea incana"] <- "Descurainia incana"  # just a typo fix
levels(data$species)[levels(data$species)=="Lonicera involucrata"] <- "Distegia involucrata"
levels(data$species)[levels(data$species)=="Elymus trachycaulis"] <- "Elymus trachycaulus"  # just a typo fix
levels(data$species)[levels(data$species)=="Geum triflorum"] <- "Erythrocoma triflorum"
levels(data$species)[levels(data$species)=="Galium boreale"] <- "Galium septentrionale"
levels(data$species)[levels(data$species)=="Gentiana parryi"] <- "Pneumonanthe parryi"
levels(data$species)[levels(data$species)=="Gentianella amarella"] <- "Gentianella acuta"
levels(data$species)[levels(data$species)=="Heracleum maximum"] <- "Heracleum sphondylium"
levels(data$species)[levels(data$species)=="Lathyrus lanszwertii"] <- "Lathyrus leucanthus"
levels(data$species)[levels(data$species)=="Lupinus prunophilus"] <- "Lupinus polyphyllus" # This is a temporary fix to get rid of the old name; the Lupin spp. issue still needs to be addressed (15 Sept 2015)
levels(data$species)[levels(data$species)=="Scenecio bigelovii"] <- "Ligularia bigelovii"  #typo in original spelling (Scenecio vs. Senecio)
levels(data$species)[levels(data$species)=="Senecio bigelovii"] <- "Ligularia bigelovii" 
levels(data$species)[levels(data$species)=="Noccaea montana"] <- "Noccaea fendleri"
levels(data$species)[levels(data$species)=="Oreochrysum parryi"] <- "Solidago multiradiata"
levels(data$species)[levels(data$species)=="Orobanche fasciculata"] <- "Aphyllon fasciculatum"
levels(data$species)[levels(data$species)=="Poa sp."] <- "Poa pratensis"
levels(data$species)[levels(data$species)=="Rhodiola integrifolia"] <- "Tolmachevia integrifolium"
levels(data$species)[levels(data$species)=="Silene drummondii"] <- "Silene antirrhina"
levels(data$species)[levels(data$species)=="Valeriana capitata"] <- "Valeriana occidentalis"

sort(unique(data$species)) # quick check

write.csv(data, "gothicWW.csv", row.names=FALSE)
