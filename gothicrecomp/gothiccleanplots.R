### Started 19 November 2010 ###
### By Lizzie ###

### Cleaning up Gothic, Act II (Lizzie, thanks to Regetz for Act I) ###
### This file cleans up the plot names (extracted from xls files in JR's code) ###
### First, it extracts as much as possible from the file ##

### Final corrections made in lookupplotcorr.csv -- incl. changes outside of R ###

## Updated 2 Sept 2015 to change paths to plots folder ##
## Updated 9 Sept 2015 to fix problem that labelled willow-meadow interface plots as wm ##

options(stringsAsFactors = FALSE)

gothic <- read.csv("/Users/Lizzie/Documents/Professional/NCEAS/Research/Phenology/Data/Gothic/Compiling data/October2010Progress/gothic-testclean/gothic-cleaned.csv", header=TRUE)

gothic$filelow <- tolower(gothic$file)
sites <- unique(gothic$filelow)

## Clean up plots ##

# interface plots #
interface <- which(grepl("int", sites)==TRUE)
#sites[interface]
# rocky meadow plots (incl. aspen forest) #
rmplots <- which(grepl("rm", sites)==TRUE)
# sites[rmplots]
rmplots1 <- which(grepl("rocky meadow", sites)==TRUE)
rmplots2 <- which(grepl("aspen forest", sites)==TRUE)
# wet meadow plots #
wmplots <- which(grepl("wm", sites)==TRUE)
# sites[wmplots]
wmplots1 <- which(grepl("wet meadow", sites)==TRUE)
wmplots2 <- which(grepl("w-m\\d", sites)==TRUE) # key fix to not label willow-meadow as wm -- need to add digit to not select all the willow-meadow plots!
# erythronium meadow plots #
emplots <- which(grepl("em", sites)==TRUE)
# sites[emplots]
emplots <- emplots[1:40]
emplots1 <- which(grepl("erythronium meadow", sites)==TRUE)
# greenhouse plots #
gpplots <- which(grepl("gp", sites)==TRUE)
# sites[gpplots]
gpplots1 <- which(grepl("greenhouse", sites)==TRUE)
# veratrum removal plots #
vrplots <- which(grepl("vr", sites)==TRUE)
# sites[vrplots]
vrplots1 <- which(grepl("veratrum removal", sites)==TRUE)

plots <- unique(gothic$filelow)
plots[interface] <- "int"  
plots[rmplots] <- "rm"  
plots[rmplots1] <- "rm"
plots[rmplots2] <- "rm"
plots[wmplots] <- "wm"  
plots[wmplots1] <- "wm"
plots[wmplots2] <- "wm"
plots[emplots] <- "em"  
plots[emplots1] <- "em" 
plots[gpplots] <- "gp"  
plots[gpplots1] <- "gp"
plots[vrplots] <- "vr"  
plots[vrplots1] <- "vr"

numsigns <- c()
for (i in 1:length(sites)){
numsigns[i] <- substr((strsplit(sites[i], "#")[[1]][2]),1,1) 
}

plotnums <- unique(gothic$filelow)
plotnums[(which(is.na(numsigns)==FALSE))] <-
    numsigns[(which(is.na(numsigns)==FALSE))]
inty <- interface[1:75]
plotnums[inty] <- substr(sites[inty], 4,4)
plotnums[rmplots] <- substr(sites[rmplots], 3,3)
plotnums[wmplots] <- substr(sites[wmplots], 3,3)
plotnums[emplots] <- substr(sites[emplots], 3,3)
plotnums[gpplots] <- substr(sites[gpplots], 3,3)
plotnums[vrplots] <- substr(sites[vrplots], 3,3)
wmplotsy <- wmplots2[1:30]
plotnums[wmplotsy] <- substr(sites[wmplotsy], 4,4)

newplotnames <- as.data.frame(cbind(sites, plots, plotnums))

# write these extractions out #
write.csv(newplotnames, "/Users/Lizzie/Documents/Subversion/phenology/gothic/plots/lookupplot.csv", row.names=FALSE)

# new file is adjusted version of the above, see Gothic_PlotNotes.txt for info #
lookupcorr <- read.csv("/Users/Lizzie/Documents/Subversion/phenology/gothic/plots/lookupplotcorr.csv", header=TRUE)

# final unique plots #
uniqueplots <-  sort(unique(paste(lookupcorr$plots, lookupcorr$plotnums, sep=" ")))
write.csv(uniqueplots, "/Users/Lizzie/Documents/Subversion/phenology/gothic/plots/uniqueplots.csv", row.names=FALSE)

