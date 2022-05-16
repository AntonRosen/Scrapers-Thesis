library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(tm)
library(readr)
library(tidyr)

link <- "https://www.flashback.org/t2828333"
page <- read_html(link)

reg <- page %>% html_nodes(".post .post-user-info") %>% html_text() 
name <- page %>% html_nodes(".post .post-user-username") %>% html_text()
quote <- html_node(html_nodes(page, '.post .post-body .post_message'), '.post-bbcode-quote-wrapper') %>% html_text()
  
data <- data.frame(name, reg, quote, stringsAsFactors = FALSE)

data2 <- data.frame()

readUrl <- function(url) {
  out <- tryCatch(
    {   
      download.file(url, destfile = "scrapedpage2.html", quiet=TRUE)
      return(1)
    },
    error=function(cond) {
      
      return(0)
    },
    warning=function(cond) {
      return(0)
    }
  )    
  return(out)
}

options(scipen = 999)

for(page_result in seq (from = 2, to = 1227)) {
  
  url <- paste0("https://www.flashback.org/t2828333p", page_result)
  
  if(readUrl(url)==1) { 
    download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    page <- read_html("scrapedpage.html")
    reg <- page %>% html_nodes(".post .post-user-info") %>% html_text() %>% gsub('[\n\t]', '', .)
    name <- page %>% html_nodes(".post .post-user-username") %>% html_text() %>% gsub('[\n\t]', '', .)
    quote <- html_node(html_nodes(page, '.post .post-body .post_message'), '.post-bbcode-quote-wrapper') %>% html_text()
    cat("boom! ")
    data2 <- rbind(data2, data.frame(name, reg, quote, stringsAsFactors = FALSE))
    if (page_result %% 20 == 0) {
      closeAllConnections()
      Sys.sleep(30)
      next
    } else {
      Sys.sleep(sample(10, 1) * 0.2)
    }
  } else {er <- 1}
}

copy <- data2

datA <- rbind(data,data2)
write.csv(datA, "C:\\Users\\anton\\OneDrive\\Dokument\\R\\thesis\\scraper-for-thesis\\data-with-userinf.csv", row.names = TRUE, fileEncoding="UTF-8")

datA$index <- 1:nrow(datA)

datA$name <- gsub('[\n\t]', '', datA$name) 
datA$name <- gsub('[\r]', '', datA$name)
datA$reg <- gsub('[\t]', '', datA$reg) 

x <- stringr::str_split_fixed(datA$reg, "Inlägg", 2)
x <- x[,-2]

datA$numpost <- sub(".*Inlägg", "", datA$reg)
datA$numpost <- sub(".*:", "", datA$numpost)
datA$numpost <- gsub('[\r]', '', datA$numpost)
datA$numpost <- gsub('[\t]', '', datA$numpost) 
datA$numpost <- gsub('[\n\t]', '', datA$numpost)
datA$numpost <- gsub("[[:space:]]", "", datA$numpost)
datA$numpost <- as.numeric(datA$numpost)

datA$reg2 <- x
datA$reg2 <- gsub('[\r]', '', datA$reg2)
datA$reg2 <- gsub('[\t]', '', datA$reg2) 
datA$reg2 <- gsub('[\n\t]', '', datA$reg2)
datA$reg2 <- gsub("[^[:digit:]., ]", "", datA$reg2)
datA$reg2 <- gsub(" ", "", datA$reg2, fixed = TRUE)
datA$reg2 <- gsub("[[:space:]]", "", datA$reg2)
datA$reg2 <- as.numeric(datA$reg2)

datA$quotedummy <- ifelse(is.na(datA$quote), 0, 1)

