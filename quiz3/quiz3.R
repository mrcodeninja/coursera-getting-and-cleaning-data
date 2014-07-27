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
    
    cat("Finding households on greater than 10 acres who sold more than $10,000 worth of agriculture products...\n")
    agricultureLogical <- (data$ACR == 3 & data$AGS == 6)
    agricultureLogical
    
    allMatchingHouseholds <- which(agricultureLogical)
    
    cat("First 3 households: ")
    head(allMatchingHouseholds, n = 3)
}

question2 <- function() {
    installed_packages <- installed.packages()[, 1]
    if (!is.element("jpeg", installed_packages)) {
        cat("Installing jpeg package...\n")
        install.packages("jpeg")
    }
    library(jpeg)
    
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    
    cat("Downloading image file...\n")
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", "data/getdata-jeff.jpg", method = "curl")
    
    picture <- jpeg::readJPEG(source = "data/getdata-jeff.jpg", native = TRUE)
    
    cat("Quantiles\n")
    quantile(picture, c(0.3, 0.8))
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

question3 <- function() {
    sorted_descending_by_GDP <- create_gdp_reversed_sorted()
    
    cat("Matching IDs: ", nrow(sorted_descending_by_GDP), "\n")
    cat("13th country (descending order): ", sorted_descending_by_GDP[13, "Long.Name"], "\n")
}

question4 <- function() {
    sorted_descending_by_GDP <- create_gdp_reversed_sorted()
    
    oecd_non_oecd_income_groups <- sorted_descending_by_GDP[sorted_descending_by_GDP$Income.Group %in% c("High income: OECD", "High income: nonOECD"), ]
    
    cat("Average GDP ranking\n")
    tapply(oecd_non_oecd_income_groups$Gross.domestic.product.2012, oecd_non_oecd_income_groups$Income.Group, mean)
}

question5 <- function() {
    installed_packages <- installed.packages()[, 1]
    if (!is.element("Hmisc", installed_packages)) {
        cat("Installing Hmisc package...\n")
        install.packages("Hmisc")
    }
    library(Hmisc)
    
    sorted_descending_by_GDP <- create_gdp_reversed_sorted()
    
    cat("Dividing countries into 5 quantile groups by GDP ranking...\n")
    sorted_descending_by_GDP$gdp_groups <- cut2(sorted_descending_by_GDP$Gross.domestic.product.2012, g = 5)
    
    lower_middle_income <- table(sorted_descending_by_GDP$gdp_groups, sorted_descending_by_GDP$Income.Group)[1, "Lower middle income"]
    cat("Number of countries that are lower middle income with highest GDP ranking: ", lower_middle_income, "\n")
}
