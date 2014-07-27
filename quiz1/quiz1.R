question1 <- function() {
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", "data/getdata-data-ss06hid.csv", method = "curl")
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf", "data/PUMSDataDict06.pdf", method = "curl")
    
    properties <- read.csv("data/getdata-data-ss06hid.csv")
    cat("Number of properties worth $1,000,000 or more: ", nrow(properties[which(properties$VAL == 24), ]), "\n")
}

question3 <- function() {
    require("xlsx")
    
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx", "data/getdata-data-DATA.gov_NGAP.xslx", method = "curl")
    row_index <- 18:23
    col_index <- 7:15
    dat <- read.xlsx("data/getdata-data-DATA.gov_NGAP.xslx", sheetIndex = 1, rowIndex = row_index, colIndex = col_index)
    sum(dat$Zip * dat$Ext, na.rm = TRUE)
}

question4 <- function() {
    library(XML)
    
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml", "data/getdata-data-restaurants.xml", method = "curl")
    
    document <- xmlTreeParse("data/getdata-data-restaurants.xml", useInternal = TRUE)
    root <- xmlRoot(document)
    
    zipcodes <- xpathSApply(root, "//zipcode", xmlValue)
    cat("Number of restaurants with ZIP code 21231: ", length(zipcodes[zipcodes == "21231"]), "\n")
}
