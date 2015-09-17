#
# This is sensitive to external perl dependencies being in the right
# place, and uses a perl script that has hard-coded paths that will
# likely only work on an Ubuntu box with R installed in a standard way
# from the R linux repo, and with the gdata package installed at the
# system level using install.packages("gdata").
#

options(stringsAsFactors=FALSE)

library(reshape)
library(zoo)

# uses xls2csv script from gdata, but hacked by regetz to prevent blank
# lines of data from being dropped
xls2csv <- function (xls, sheet = 1, verbose = FALSE, perl = "perl") {
    quotify <- function(x) paste("\"", x, "\"", sep = "")
    if (file.access(xls, 4) != 0) 
        stop("Unable to read xls file '", xls, "'.")
    script <- "xls2csvJR.pl"
    targetFile <- paste(tempfile(), "csv", sep = ".")
    cmd <- paste(quotify(perl), quotify(script), quotify(xls),
        quotify(targetFile), sheet, sep = " ")
    if (verbose) {
        cat("\n")
        cat("Converting xls file\n")
        cat("   ", quotify(xls), "\n")
        cat("to csv file \n")
        cat("   ", quotify(targetFile), "\n")
        cat("... \n\n")
    }
    if (verbose) 
        cat("Executing '", cmd, "'... \n\n")
    results <- system(cmd, intern = !verbose)
    if (verbose) 
        cat(results, "\n\n")
    if (verbose) 
        cat("Done.\n\n")
    if (file.access(targetFile, 4) != 0) 
        stop("Unable to read translated csv file '", 
            targetFile, "'.")
    if (verbose) 
        cat("Done.\n")
    file(targetFile)
}

reformat <- function(filename, top=NULL, date.format="%Y-%m-%d", year=NULL,
    verbose=FALSE) {

    # read in rows of data as unparsed text strings
    #r <- readLines(filename)

    base <- basename(filename)
    cat("-- processing", base, "--\n")

    if (base %in% c("rocky meadow #6 - 2004.xls")) {
        sheet <- 2
    } else {
        sheet <- 1
    }
    base <- basename(filename)

    con <- xls2csv(filename, sheet=sheet, verbose=verbose)
    r <- readLines(con)
    file.remove(summary(con)$description)
    close(con)

    # replace backslash-dquote with double dquote
    r <- gsub('\\\\\"', '""', r)

    # identify top row (header) of real data, if not explicitly set
    if (is.null(top)) {
        h <- grep('^"?date', iconv(r), ignore.case=TRUE)
        if (length(h)==0) {
            warning("!!SKIPPING!! [No 'Date' (header) row found]",
                immediate.=TRUE)
            return(NULL)
        } else {
            top <- h[1]
        }
        #cat("inferred header row:", top, "\n")
    } else {
        #cat("user-specified header row:", top, "\n")
    }

    # identify bottom row of real data
    tf <- grep("Total ?#? Flower", iconv(r), ignore.case=TRUE)
    if (length(tf)==0) {
        # line after the last non-blank value in the first column
        if (base %in% c("Meadow (Enders) plot #11 - 2007.xls",
            "Meadow plot #11 - 2008.xls", "Meadow plot #11 - 2009.xls")) {
            bottom <- tail(grep("^[^,]", iconv(r)), 1)+1
        } else {
            warning("!!SKIPPING!! [Can't determine bottom row]",
                immediate.=TRUE)
            return(NULL)
        }
    } else {
        bottom <- tf[1]-1
    }

    r2 <- r[seq(from=top, to=bottom)]

    # extract and clean header
    trim <- function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x)
    hdr <- trim(strsplit(r2[[1]], ",")[[1]])

    # now parse list of rows into a data frame
    conn <- textConnection(r2)
    dat <- read.csv(conn, stringsAsFactors=FALSE, check.names=FALSE,
        colClasses="character", na.strings=c("", " "))
    close(conn)

    # *assume* first column always contains species!
    names(dat)[1] <- "species"

    # figure out which columns to keep
    # keep first column, plus all those with header names that start
    # with a digit (in the hopes that this filters out all non-dates,
    # and keeps all dates...)
    cols.to.use <- c(1, which(nzchar(hdr) & grepl('^"?[[:digit:]]', hdr)))

    # figure out which rows to keep
    # first do some specific custom cleanup
    if (base=="rm8-1985.xls") {
        dat$species[grep("species b", dat$species)] <- NA
    } else if (base=="rm81987.xls") {
        dat$species[grep("species [ab]", dat$species)] <- NA
    } else if (base=="rm31992.xls") {
        dat$species[grep("sticky, ", dat$species)] <- NA
    } else if (base=="willow-meadow interface #4.xls") {
        dat$species[grep("\\(nodding ", dat$species)] <- NA
    } else if (base=="wm11996.xls") {
        dat$species[grep("nodding ", dat$species)] <- NA
    } else if (base=="wm21996.xls") {
        dat$species[grep("long-awns, ", dat$species)] <- NA
        dat$species[grep("nodding ", dat$species)] <- NA
    }
    # for starters, identify rows with a non-blank value in first column
    dat$species[grep("^[[:blank:]]*$", dat$species)] <- NA
    # deal with cases of species qualifier text appearing in second row
    common <- grep('^[[:blank:]]+', dat$species)
    if (length(common)>0) {
        if (any(is.na(dat$species[common-1]))) {
            warning("!!SKIPPING!! [confusing use of common name]",
                immediate.=TRUE)
            return(NULL)
        }
        cat("  Deleting what looks like common name:\n\t",
            paste(dQuote(paste(dat$species[common-1],
            dat$species[common], sep=": ")), collapse="\n\t"),
            "\n")
        dat$species[common] <- NA
    }
    labeled.rows <- which(!is.na(dat$species))
    # now drop species that have >2 rows before the next species
    # NOTE: we simply assume that the last species in the dataset has
    # two rows, because there is no easy way to test it
    rows.per.species <- diff(labeled.rows)
    if (any(rows.per.species>2)) {
        species.to.drop <- labeled.rows[rows.per.species>2]
        cat("  Dropping species that have >2 rows:\n\t",
            paste(dQuote(dat$species[species.to.drop]), collapse=", "),
            "\n")
        labeled.rows <- setdiff(labeled.rows, species.to.drop)
    }
    # make sure no species only have a single row
    if (any(rows.per.species==1)) {
        warning("!!SKIPPING!! [encountered species with 1 row of data]",
            immediate.=TRUE)
        return(NULL)
    }
    # and ultimately only keep each labeled row plus the one below it
    rows.to.use <- sort(c(labeled.rows, labeled.rows+1))

    # clean up the values used to indicate what was counted
    candidates <- which(sapply(dat,function(x) any(grepl("^ *(plants|flowers) *$", x))))
    what.was.counted <- dat[rows.to.use, tail(candidates[!grepl("[-[:digit:]/]+$",
        names(candidates))], 1)]
    # make sure we were able to figure out what was counted
    if (length(what.was.counted)==0) {
        cat("  No info on what plant part was counted\n")
        what.was.counted <- rep(NA, length(rows.to.use))
    }
    # clean it up
    what.was.counted[grepl("^[?[:space:]]*$", what.was.counted)] <- NA
    what.was.counted <- sub("[?[:space:]]+$", "", what.was.counted)
    what.was.counted <- sub("capitula.*", "capitula", what.was.counted)
    what.was.counted <- sub("^genets.*", "genets", what.was.counted)
    what.was.counted <- sub("clumps.*", "capitula", what.was.counted) # ???
    what.was.counted[what.was.counted %in% c(
        "calumps(genets)", "clusters of capitula", "clusters of flowers",
        "clusters of flowering", "clusters of flowes",
        "clusters of heads", "bunches",
        "cluster of flowering plants (genet?)")
    ] <- "clusters"
    what.was.counted[what.was.counted %in% c(
        "capituls", "cpitula", "captula", "capiula")
    ] <- "capitula"
    what.was.counted[what.was.counted %in% c(
        "flowersx", "flowwers", "flowwers", "flowerrs",
        "flwoers", "flowes", "individual flowers",
        "flowers on 1 stalk")
    ] <- "flowers"
    what.was.counted[what.was.counted %in% c(
        "inflorescences (stalks)", "infloresccences", "inflorescesnces",
        "infloerscences", "inflorescecnes", "inflorescenecs",
        "inflorecences", "inflorescenes", "inflorescencs",
        "inflorescnces", "infloresences", "inflorscences",
        "inflrescences", "inforescences", "inlorescences",
        "infloresceces", "inf;orescences", "inflorescences(?)",
        "inflorescence", "inflorescences'")
    ] <- "inflorescences"
    what.was.counted[what.was.counted %in% c(
        "plans", "plant")
    ] <- "plants"
    # collapse minor categories into 'other'
    what.was.counted[what.was.counted %in% c(
        "branches", "catkins", "genet", "genets", "gents", "ramets",
        "spikes", "stalks", "stallks", "stalks (genets?)", "satlks",
        "stems", "heads", "heads, not stems as in other years",
        "patches", "presence of flowers", "shrubs",
        "secondary inflorescences",
        "inflorescences (genets)",
        "some confusion with P. aviculare",
        "some confusion with P. douglasii",
        "branches with flowers",
        "Maximum number of branches, not of plants as in previous years.",
        "Maximum number of branches with flowers, not plants as in past years",
        "Maximum number of branches with flowers, not plants",
        "maximum number of branches, not plants as in other years",
        "Maximum number of branches with flowers; not plants as in previous years.",
        "maximum number of branches, not plants as in previous years.",
        "Number of branches with flowers, not of plants as in the past.",
        "Number of branches with flowers, not plants",
        "number of pairs of flowers", "umbels on E plant",
        "umbels on W plant", "umbels (inflorescences)",
        "flowers were counted on one day",
        "female", "male")
    ] <- "other"

    # drop the unwanted rows and columns
    dat <- dat[rows.to.use, cols.to.use]

    # fill in implicit species names, and make them unique in case there
    # are two *sets* of rows for same species (this does happen...)
    dup.species <- na.omit(dat$species[duplicated(dat$species)])
    if (length(dup.species)>0) {
        cat("  Duplicated species: ", paste(dQuote(dup.species),
            collapse=", "), "\n")
    }
    dat$species[!is.na(dat$species)] <-
        make.unique(dat$species[!is.na(dat$species)])
    dat$species <- na.locf(dat$species)

    # add column indicating what was counted
    dat$part <- what.was.counted

    # convert NA to unknown1 (if in row 1) or unknown2 (if in row 2)
    dat$part[intersect(seq(1, nrow(dat), by=2),
        which(is.na(dat$part)))] <- "unknown1"
    dat$part[intersect(seq(2, nrow(dat), by=2),
        which(is.na(dat$part)))] <- "unknown2"

    is.counted.twice <- duplicated(dat[c("species", "part")])
    if (any(is.counted.twice)) {
        dat$part[is.counted.twice] <- "double.count"
    }

    # reshape into a table with columns for each counted plant part
    dat.melted <- melt(dat, id=c("species", "part"), variable_name="date")
    result <- cast(dat.melted, species + date ~ part)

    # try hard to clean up our date column
    # first remove any trailing .1 (etc), resulting from duplicate dates
    result$date <- sub("[.].*", "", result$date)
    # remove some other trailing junk
    result$date <- sub(" +dwi", "", result$date)
    # now coerce to Date class if one of these known formats matches
    match1 <- grepl('^[[:digit:]]{1,2}/[[:digit:]]{1,2}$', result$date)
    match2 <- grepl('^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{2}$', result$date)
    match3 <- grepl('^[[:digit:]]{1,2}-[[:alpha:]]{3}$', result$date)
    match4 <- grepl('^[[:digit:]]{2}-[[:alpha:]]{3}-[[:digit:]]{2}$', result$date)
    match5 <- grepl('^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$', result$date)
    match6 <- grepl('^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}$', result$date)

    newdates <- as.Date(rep(NA, length(result$dates)))
    # CASE 1: 01/31 (no year specified)
    newdates[match1] <- as.Date(paste(result$date[match1], year,
        sep="/"), format="%m/%d/%Y")
    # CASE 2: 01/31/99  (this won't work properly on any years prior to 1970)
    newdates[match2] <- as.Date(result$date[match2], format="%m/%d/%y")
    # CASE 3: 01-Jan (no year specified)
    newdates[match3] <- as.Date(paste(result$date[match3], year,
        sep="-"), format="%d-%b-%Y")
    # CASE 4: 01-Jan-99  (this won't work properly on any years prior to 1970)
    newdates[match4] <- as.Date(result$date[match4], format="%d-%b-%y")
    # CASE 5: 1999-01-31
    newdates[match5] <- as.Date(result$date[match5], format="%Y-%m-%d")
    # CASE 6: 01/31/1999
    newdates[match6] <- as.Date(result$date[match6], format="%m/%d/%Y")
    # Make sure all dates were accounted for...
    if (any(is.na(newdates) & !is.na(result$date))) {
        warning("!!SKIPPING!! [unknown date format]",
            immediate.=TRUE)
        return(NULL)
    }
    result$date <- newdates

    result$file <- sub("\\.xls$", "", base)
    cols.first <- c("file", "species", "date")
    cols.other <- setdiff(names(result), cols.first)
    result <- result[, c(cols.first, cols.other)]
    result[] <- result[order(result$species), ]

    return(result)
}

getTextValues <- function(dat) {
    test <- dat[3:4]
    test[] <- lapply(test, as.character)
    test[is.na(test)] <- -999
    suppressWarnings(lapply(test, function(x) x[is.na(as.numeric(x))]))
    #lapply(test, function(x) which(is.na(as.numeric(x))))
}

forceNumeric <- function(dat, quiet=TRUE) {
   if (quiet) {
       opts <- options(warn=-1)
       on.exit(options(opts))
   }
   dat[3:4] <- lapply(dat[3:4], function(x) as.numeric(as.character(x)))
   return(dat)
}

cleanall <- function() {
    path <- "./data"
    years <- list.files(path, pattern="^\\d*$")
    dat.struct <- lapply(years,  function(year) {
        cat("\n******* ", year, " *******\n")
        if (year <= 2005) {
            path <- file.path(path, year, "GAedited")
        } else {
            path <- file.path(path, year)
        }
        if (year < 1997) {
            top <- NULL
        } else {
            top <- 2
        }
        files <- list.files(path, pattern="\\.xls$", full.names=TRUE)
        names(files) <- basename(files)
        # manually exclude certain files
        files.to.exclude <- c(
            "vr11984.xls", # duplicate of vr1-1984.xls
            "sumary88.xls", # data summarization
            "delphinium barbeyi 2008.xls" # data summarization
        )
        files <- files[setdiff(names(files), files.to.exclude)]
        # process files
        lapply(files, function(fn) reformat(fn, top=top, year=year))
    })
    datlist <- unlist(dat.struct, recursive=FALSE)
    all.colnames <- unique(unlist(sapply(datlist, names)))
    datlist <- lapply(datlist, function(x) {
        x[setdiff(all.colnames, names(x))] <- NA_character_
        x
        })
    dat <- do.call("rbind", datlist)
    return(dat)
}
