### Started 14 March 2011 ###
### By Lizzie and Master Regetz ###

### Cleaning up Gothic, Act III ###
### This follows gothiccleanplots -- which just does plots ###
### This integrates plot changes and clean spp. then integrates cleaned names ####

## Updated 17 September 2015 to remove blank rows of final file ##
## Also changed file written to be based on gothicfixed (not gothic2 as before) ##

options(stringsAsFactors = FALSE)

setwd("/Users/Lizzie/Documents/Subversion/phenology/gothic/species")

# define helper function to replace all occurrences of
# consecutive spaces with a single space, and remove trailing spaces
fix.spaces <- function(x) {
    x <- gsub(" +", " ", x)
    sub(" +$", "", x)
}

# Jim, where is the test-clean on svn? #
## get the gothic data ##
gothic <- read.csv("/Users/Lizzie/Documents/Professional/NCEAS/Research/Phenology/Data/Gothic/Compiling data/October2010Progress/gothic-testclean/gothic-cleaned.csv", header=TRUE)
# put the new plot info in, will need it later 
gothic$filelow <- tolower(gothic$file)
lookupcorr <- read.csv("/Users/Lizzie/Documents/Subversion/phenology/gothic/plots/lookupplotcorr.csv", header=TRUE)
gothic <- merge(gothic, lookupcorr, by.x="filelow", by.y="sites", all.x=TRUE)
gothic$plotNums <- paste(gothic$plots, gothic$plotnums, sep="-")

## Cleaning rounds ##
# first round of cleaning by G. Aldridge #
cleanga <- read.csv("GAcleaned/cleansppsbyhandga.csv",header=TRUE)
# second round of cleaning by G. Aldridge #
cleanga2 <- read.csv("GAcleaned/cleansppsr2rga.csv",header=TRUE)
# third round of cleaning by G. Aldridge #
cleanga3 <- read.csv("GAcleaned/cleansppsr3rga.csv",header=TRUE)
# fourth round of cleaning by G. Aldridge #
cleanga4 <- read.csv("GAcleaned/cleansppsr4rga.csv",header=TRUE)
# last round of cleaning by G. Aldridge #
# appears as 6 since round 5 was plot-based #
cleanga6p1 <- read.csv("GAcleaned/cleansppsr6rga.csv",header=TRUE)
cleanga6p2 <- read.csv("GAcleaned/cleanednamesga.csv",header=TRUE)
cleanga6p2use <- subset(cleanga6p2, needs.change=="yes")
cleanga6 <- rbind(cleanga6p1, cleanga6p2use)
# cleaning by David Inouye (DI - file used here was created by Lizzie using
# cleanednamesDI.csv, 9 June 2011 version) #
# note that I changed this file some after comments from DI rec'd 3 and 6.Feb.2013 in cleaned_DIchanges3.csv and GothicInouye_phenspecies2.csv (see notes column)
cleandi <- read.csv("DIcleaned/cleaned_DIchanges.csv",header=TRUE)

## Create one look-up table for cleaned names ##
names(cleanga2)[names(cleanga2)=="need.change"] <- "needs.change"
names(cleanga2)[names(cleanga2)=="new.genus"] <- "lookup.gen"
names(cleanga2)[names(cleanga2)=="new.epithet"] <- "lookup.sp"
names(cleanga2)[names(cleanga2)=="species"] <- "givenname"
names(cleanga3)[names(cleanga3)=="new.genus"] <- "lookup.gen"
names(cleanga3)[names(cleanga3)=="new.epithet"] <- "lookup.sp"
names(cleanga3)[names(cleanga3)=="species"] <- "givenname"
names(cleanga6)[names(cleanga6)=="lookup.genus"] <- "lookup.gen"
names(cleanga6)[names(cleanga6)=="lookup.epithet"] <- "lookup.sp"
names(cleandi)[names(cleandi)=="lookup.genus"] <- "lookup.gen"
names(cleandi)[names(cleandi)=="lookup.epithet"] <- "lookup.sp"

cleanga3$needs.change <- "yes"
cleanga3$notes <- NA
cleanga6$notes <- NA

cleangarbind <- subset(cleanga, needs.change=="no" | lookup.gen!="") 
cleanga2rbind <- subset(cleanga2, needs.change=="no" |lookup.gen!="") 
cleanga3rbind <- subset(cleanga3, lookup.gen!="")
cleanga4rbind <- subset(cleanga4, needs.change=="no" |lookup.gen!="")
cleanga6rbind <- subset(cleanga6, needs.change=="no" |lookup.gen!="")

cleangarbind <- subset(cleangarbind, select=c("givenname", "needs.change", 
    "lookup.gen", "lookup.sp", "notes"))
cleanga2rbind <- subset(cleanga2rbind, select=c("givenname", "needs.change",
   "lookup.gen", "lookup.sp", "notes"))
cleanga3rbind <- subset(cleanga3rbind, select=c("givenname", "needs.change",
   "lookup.gen", "lookup.sp", "notes"))
cleanga4rbind <- subset(cleanga4rbind, select=c("givenname", "needs.change",
   "lookup.gen", "lookup.sp", "notes"))

cleanedbyGA <- rbind(cleangarbind, cleanga2rbind, cleanga3rbind,
    cleanga4rbind, cleanga6rbind)
# cleanedbyGA <- subset(cleanedbyGA, lookup.gen!="")
## fix some issues
# initially I tried to update each specifically so that spp that did not
# need changes were noted as such, then I just made the corrections
# Thus some species were changed even when they were right to start with, which seems fine.
cleanedbyGA$lookup.sp[which(cleanedbyGA$lookup.sp=="columbinaum")] <- "columbianum"
cleanedbyGA$needs.change[which(cleanedbyGA$givenname=="Aquilegia coerulea")] <- "no"
cleanedbyGA$lookup.gen[which(cleanedbyGA$givenname=="Aquilegia coerulea")] <- ""
cleanedbyGA$lookup.sp[which(cleanedbyGA$givenname=="Aquilegia coerulea")] <- ""
cleanedbyGA$lookup.sp[which(cleanedbyGA$lookup.sp=="engelmanii")] <- "engelmannii"
cleanedbyGA$needs.change[which(cleanedbyGA$givenname=="Castilleja linariaefolia")] <- "no"
cleanedbyGA$lookup.gen[which(cleanedbyGA$givenname=="Castilleja linariaefolia")] <- ""
cleanedbyGA$lookup.sp[which(cleanedbyGA$givenname=="Castilleja linariaefolia")] <- ""
cleanedbyGA$lookup.sp[which(cleanedbyGA$lookup.sp=="linariifolia")] <- "linariaefolia"
cleanedbyGA$lookup.sp[which(cleanedbyGA$lookup.sp=="nuttallianium")] <- "nuttallianum"
cleanedbyGA$lookup.gen[which(cleanedbyGA$lookup.gen=="Descurainia")] <- "Descurainea"
cleanedbyGA$lookup.sp[which(cleanedbyGA$lookup.sp=="trachycaulus")] <- "trachycaulis"
cleanedbyGA$lookup.gen[which(cleanedbyGA$lookup.gen=="Koeleria")] <- "Koelaria"
cleanedbyGA$lookup.sp[which(cleanedbyGA$lookup.sp=="stellata")] <- "stellatum"
cleanedbyGA$lookup.sp[which(cleanedbyGA$lookup.sp=="integerimus")] <- "integerrimus"
cleanedbyGA$needs.change[which(cleanedbyGA$givenname=="Koeleria macrantha")] <- "yes"
cleanedbyGA$lookup.gen[which(cleanedbyGA$givenname=="Koeleria macrantha")] <- "Koelaria"
cleanedbyGA$lookup.sp[which(cleanedbyGA$givenname=="Koeleria macrantha")] <- "macrantha"

subset(cleanedbyGA, duplicated(cleanedbyGA$givenname) & needs.change=="yes")
cleanedbyGA$givenname <- fix.spaces(cleanedbyGA$givenname)
# cleanedbyGA <- cleanedbyGA[!duplicated(cleanedbyGA$givenname),]
# most are duplicate nos, but not all, see above.
# for these we preferntially keep those with new names over needs.change=='no'
cleanedbyGA <- cleanedbyGA[order(cleanedbyGA$needs.change, decreasing=TRUE, na.last=TRUE) , ]
cleanedbyGA <- cleanedbyGA[!duplicated(cleanedbyGA$givenname),]
cleanedspp <- cleanedbyGA
 
write.csv(cleanedspp, "cleanedspp.csv", row.names=FALSE)

## Now start integrating cleaned names ##
# integrate cleaned names that must be plot-based
# aka the grasses
cleangrassga <- read.csv("GAcleaned/gothicunkngrassga.csv",header=TRUE)
cleangrass <- subset(cleangrassga, new.genus!="" & new.epithet!="",
    select=c("species", "new.genus", "new.epithet", "plotNums"))
cleangrass <- unique(cleangrass)
if (any(duplicated(cleangrass[c("species", "plotNums")]))) {
    stop("duplicated merge columns in cleangrass")
}
gothic1 <- merge(gothic, cleangrass, by=c("species", "plotNums"),
    all.x=TRUE, sort=FALSE)

# replaces species with new names, get which rows changed
gothic1$species <- ifelse(is.na(gothic1$new.genus)==FALSE,
    paste(gothic1$new.genus, gothic1$new.epithet), gothic1$species)
grassfill <- which(is.na(gothic1$new.genus)==FALSE) 

# get rid of unwanted columns 
gothic1 <- subset(gothic1, select=-c(new.genus, new.epithet))
gothic1$species <- fix.spaces(gothic1$species)

#
# integrate other cleaned names back into gothic data
#

# merged gothic and cleanedspp based on the original name
gothic2 <- merge(gothic1, cleanedspp, by.x="species", by.y="givenname",
    all.x=TRUE, sort=FALSE)

# transform needs.change into was.changed, replacing NAs with "tbd"
names(gothic2)[names(gothic2)=="needs.change"] <- "was.changed"

# for those that were cleaned, replace original names with cleaned names
gothic2$was.changed[is.na(gothic2$was.changed)] <- "tbd"
gothic2$species <- ifelse(gothic2$was.changed=="yes",
    paste(gothic2$lookup.gen, gothic2$lookup.sp), gothic2$species)
gothic2$species <- fix.spaces(gothic2$species)

# incorporate additional lookup.gen/lookup.sp corrections from DI
# ...first clean white space in DI lookup.gen column
cleandi$lookup.gen <- fix.spaces(cleandi$lookup.gen)
# ...identify which gothic2 rows correspond to DI corrections
needsCleanDI <- gothic2$species %in% cleandi$givenname
# ...update the lookup.gen and lookup.sp columns accordingly
gothic2$lookup.gen[needsCleanDI] <-
    cleandi$lookup.gen[match(gothic2$species[needsCleanDI],
    cleandi$givenname)]
gothic2$lookup.sp[needsCleanDI] <-
    cleandi$lookup.sp[match(gothic2$species[needsCleanDI],
    cleandi$givenname)]
# ...flag these records as having been changed
gothic2$was.changed[needsCleanDI] <- "yes"
# ...and generate updated binomials
gothic2$species[needsCleanDI] <- paste(gothic2$lookup.gen[needsCleanDI],
    gothic2$lookup.sp[needsCleanDI])

# add in plot-based cleaning above
# note unkn grasses
# change the bromes first, in case the word 'grass' ever co-occurs with brome/bromus
# okay, so I admit at this point we end up with none of this doing anything
# cleanedspp seems to cleans these all out, but I am leaving them for safe-keeping
unknbrome1 <- which(grepl("brome", gothic2$species)==TRUE)
unknbrome2 <- which(grepl("Brome", gothic2$species)==TRUE)
unknbrome3 <- which(grepl("Bromus", gothic2$species)==TRUE)
gothic2$species[unknbrome1] <- "Bromus sp."
gothic2$species[unknbrome2] <- "Bromus sp."
gothic2$species[unknbrome3] <- "Bromus sp."

unkngrasses <- which(grepl("grass", gothic2$species)==TRUE)
unknGrasses <- which(grepl("Grass", gothic2$species)==TRUE)
gothic2$species[unkngrasses] <- "unkngrass"
gothic2$species[unknGrasses] <- "unkngrass"

# note changes in was.changed
gothic2$was.changed[grassfill] <- "yes"
gothic2$was.changed[unkngrasses] <- "yes"
gothic2$was.changed[unknGrasses] <- "yes"
gothic2$was.changed[unknbrome1] <- "yes"
gothic2$was.changed[unknbrome2] <- "yes"
gothic2$was.changed[unknbrome3] <- "yes"

# Note that Stipa lettermanii (one n) and Muhlenbergia montana
# (the only two never noted as not needing cleaning and not cleaned)
# are, according to a 13.Apr.2011 email from G. Aldridge a-okay #
# So, as of August 2013, updating them from was.changed "tbd" to "no"
gothic2$was.changed[gothic2$species=="Stipa lettermanii"] <- "no"
gothic2$was.changed[gothic2$species=="Muhlenbergia montana"] <- "no"

# get rid of unwanted columns, don't leave leave 'notes'
# we can provide cleanedspp with them in it, for those who need it) #
gothic2 <- subset(gothic2, select=-c(lookup.gen, lookup.sp))

# what's left?
stillunkn <- subset(gothic2, was.changed=="tbd") # nothing!

# delete the species named Delete and Total Flowers
# okayed by Amy Iler in email 20.Sep.2012
gothicfixed <- subset(gothic2, species!="Delete")
gothicfixed <- subset(gothicfixed, species!="Total Flowers")
gothicnozeros <- subset(gothicfixed, capitula>0 | clusters>0
    | flowers>0 | inflorescences>0 | other>0 | plants>0
    | unknown1>0 | unknown2>0 | double.count>0)

knowns <- as.data.frame(sort(unique(gothicfixed$species)))
# allspp <- as.data.frame(sort(unique(gothic2$species)))

write.csv(knowns, "cleanednames.csv", row.names=FALSE)
write.csv(gothicnozeros, "gothicclean.csv", row.names=FALSE)


###
### See gothicclean_choppedCode.R for code that used to be here 
### (as of 7.Mar.12, EMW cut it)
###

