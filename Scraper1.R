library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(tm)
library(readr)
library(ggplot2)


get_robotstxt("www.flashback.org")

link <- "https://www.flashback.org/t2828333"
page <- read_html(link)

times <- page %>% html_nodes(".post-heading") %>% html_text() %>% gsub('[\n\t]', '', .)
posts <- page %>% html_nodes(".post_message")
toremove <- posts %>% html_nodes(".post-bbcode-quote")
xml_remove(toremove)
posts <- posts %>% html_text(trim=TRUE)
userinfos <- page %>% html_nodes("#posts .dropdown") %>% html_text() %>% gsub('[\n\t]', '', .) 

data <- data.frame(times, posts, userinfos, stringsAsFactors = FALSE)
COPY <- data.frame(data)

data2 <- data.frame()

readUrl <- function(url) {
  out <- tryCatch(
    {   
      download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
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
    times <- page %>% html_nodes(".post-heading") %>% html_text() %>% gsub('[\n\t]', '', .)
    posts <- page %>% html_nodes(".post_message")
    toremove <- posts %>% html_nodes(".post-bbcode-quote")
    xml_remove(toremove)
    posts <- posts %>% html_text(trim=TRUE)
    userinfos <- page %>% html_nodes("#posts .dropdown") %>% html_text() %>% gsub('[\n\t]', '', .)
    cat("boom! ")
    data2 <- rbind(data2, data.frame(times,posts,userinfos,stringsAsFactors = FALSE))
    if (page_result %% 20 == 0) {
      closeAllConnections()
      Sys.sleep(60)
      next
    } else {
      Sys.sleep(sample(10, 1) * 0.2)
    }
  } else {er <- 1}
}

COPY2<-data.frame(data2)

new_data <- rbind(data,data2)

write.csv(new_data, "C:\\Users\\anton\\OneDrive\\Dokument\\R\\thesis\\scraper-for-thesis\\data.csv", row.names = TRUE)

df2 <- read.csv("data.csv")

colnames(df2) <- c("post-nr", "date", "comment-text", "user")


df2$date <- gsub(",","",df2$date) #Cleaning dates column
df2$date <- gsub("#.*","",df2$date) #Cleaning dates column
dateC <- as.data.frame(df2$date)
post.nrC <- as.data.frame(df2$`post-nr`)
bind <- cbind(post.nrC, dateC)


df$date <- gsub(",","",df$date)
df$date <- gsub('[\n\t]', '', df$date) 

setPhase <- function(date){ #Function to set phase type
  if(date < as.Date("2017-04-22")){
    return("A")
  }
  else if(date >= as.Date("2017-05-19")){
    return("C")
  }
  else{
    return("B")
  }
}

df$phaseType <- sapply(as.Date(df$date), setPhase) #Create phases column
df$phaseType <- as.factor(df$phaseType)
counts <- table(df$phaseType) 
barplot(counts, main = "Posts distribution",
        xlab = "Phases")

df$user <- gsub(" .*", "", df$user) #Cleaning user column
df$user <- gsub('\\s+', '', df$user) 

df <- transform(df,id=as.numeric(factor(user))) #Creating anonymous ID column
copyUSERS <- data.frame(df$user) 
df <- subset(df, select = -c(user)) 


df$comment.text <- gsub('[\n\t]', ' ', df$comment.text) #Some initial cleaning of posts column
df$comment.text <- gsub("http\\S+\\s*", " ", df$comment.text) 
df$comment.text <- gsub("[^[:alnum:]]"," ", df$comment.text)
rubish <- c("Citat:", "Citat", ":Citat:")
df$comment.text <- removeWords(df$comment.text, rubish)
df$comment.text <- removePunctuation(df$comment.text)
df$comment.text <- removeNumbers(df$comment.text)
rubish2 <- c("U")
df$comment.text <- removeWords(df$comment.text, rubish2)
df$comment.text <- tolower(df$comment.text)
df$comment.text <- str_trim(df$comment.text)

df$n <-str_count(df$comment.text,'\\w+')#Creating n words per post column

df.COPY <- data.frame(df)
df <- df[df$n != 0,] #Subset df - retain only those with >0 word counts
df <- df[c("id", "post.nr", "date", "comment.text", "n", "phaseType")]

write_csv(df, "C:\\Users\\anton\\OneDrive\\Dokument\\R\\thesis\\scraper-for-thesis\\data-clean.csv")