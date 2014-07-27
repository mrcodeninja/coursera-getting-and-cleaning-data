question4 <- function() {
    connection = url("http://biostat.jhsph.edu/~jleek/contact.html")
    html = readLines(connection)
    close(connection)
    
    cat("Number of characters in the 10th, 20th, 30th, and 100th lines of http://biostat.jhsph.edu/~jleek/contact.html: ", unlist(lapply(html[c(10, 20, 30, 100)], nchar)), "\n")
}

question5 <- function() {
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for", "data/getdata-wksst8110.for", method = "curl")
    wksst <- read.fwf(file = "data/getdata-wksst8110.for", skip = 4, width = c(10, 9, 4, 9, 4, 9, 4, 9, 4))
    
    cat("Sum of the 4th column: ", sum(wksst[, 4]))
}