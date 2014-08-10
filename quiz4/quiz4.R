question1 <- function() {
    data_name <- "American Community Survey - 2006 Housing for Idaho"
    
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    
    cat("Downloading data file for", data_name, "...")
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", "data/get-data-ss06hid.csv", method = "curl")
    cat("Downloading data dictionary for", data_name, "...")
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf", "data/getdata-data-PUMSDataDict06.pdf", method = "curl")
    
    data <- read.csv("data/get-data-ss06hid.csv")
    
    split_names = strsplit(names(data), "wgtp")
    split_names[123]
}

create_gdp <- function() {
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    
    cat("Downloading Gross Domestic Product data...\n")
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", "data/getdata-data-GDP.csv", method = "curl")

    gdp <- read.csv("data/getdata-data-GDP.csv", stringsAsFactors = FALSE)
    
    cat("Cleaning GDP data...\n")
    gdp <- gdp[which(gdp$X != "" & gdp$Gross.domestic.product.2012 != "" & gdp$X.3 != ""), ]
    gdp$Gross.domestic.product.2012 <- as.integer(gdp$Gross.domestic.product.2012)
    gdp$X.3 <- as.numeric(gsub("(\\d),(\\d)", "\\1\\2", gdp$X.3))

    gdp
}

question2 <- function() {
    gdp <- create_gdp()$X.3
    
    cat("Average GDP: ", mean(gdp), "\n")
}

question3 <- function() {
    gdp <- create_gdp()

    countryNames <- gdp$X.2
    countries_starting_with_united <- grep("^United", countryNames)
    cat('Number of countries starting with "United": ', length(countryNames[countries_starting_with_united]), "\n")
}

create_gdp_reversed_sorted <- function() {
    library(plyr)
    
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    
    cat("Downloading Gross Domestic Product data...\n")
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", "data/getdata-data-GDP.csv", method = "curl")
    
    cat("Downlading educational data...\n")
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", "data/getdata-data-EDSTATS_Country.csv", method = "curl")
    
    gdp <- read.csv("data/getdata-data-GDP.csv", stringsAsFactors = FALSE)
    ed_status <- read.csv("data/getdata-data-EDSTATS_Country.csv", stringsAsFactors = FALSE)
    
    cat("Cleaning GDP data...\n")
    gdp <- gdp[which(gdp$X != "" & gdp$Gross.domestic.product.2012 != "" & gdp$X.3 != ""), ]
    gdp$Gross.domestic.product.2012 <- as.integer(gdp$Gross.domestic.product.2012)
    gdp$X.3 <- as.numeric(gsub("(\\d),(\\d)", "\\1\\2", gdp$X.3))
    
    merged_data <- merge(x = gdp, y = ed_status, by.x = "X", by.y = "CountryCode")
    sorted_descending_by_GDP <<- arrange(merged_data, desc(merged_data$Gross.domestic.product.2012))
    
    sorted_descending_by_GDP
}

question4 <- function() {
    gdp <- create_gdp_reversed_sorted()
    
    fiscal_year_ending_in_june <- gdp[grep("^Fiscal year end: June", gdp$Special.Notes), "Special.Notes"]
    
    cat("Number of countries with fiscal year ending in June: ", length(fiscal_year_ending_in_june), "\n")
}

question5 <- function() {
    installed_packages <- installed.packages()[, 1]
    if (!is.element("quantmod", installed_packages)) {
        cat("Installing quantmod package...\n")
        install.packages("quantmod")
    }
    library(quantmod)
    
    amzn = getSymbols("AMZN", auto.assign=FALSE)
    sampleTimes = index(amzn)
    
    sample_dates <- as.Date(sampleTimes)
    years <- format(sample_dates, "%Y")
    collected_in_2012 <- years[years == "2012"]
    cat("Collected in 2012: ", length(collected_in_2012), "\n")
    
    days <- weekdays(sample_dates)
    days <- days[which(days == "Monday" & years == "2012")]
    cat("Collected on in 2012: ", length(days), "\n")
}