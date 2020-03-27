#load packages for scraping pubmed
library(easyPubMed)
library(plyr)
library(dplyr)
library(kableExtra)
# Load the package required to read XML files.
library("XML")
# Also load the other required package.
library("methods")
#used for parsing xml files
library(xml2)
#convert to excel file
library(openxlsx)
#parse out ID's from xml code
library(qdapRegex)
library(curl)
library(rvest)

library(RSelenium)
#generic use for opening rsDriver
#RSelenium::rsDriver()
remDr <- rsDriver()

fprof <- makeFirefoxProfile(list(browser.download.folderList = 2L,
                                 browser.download.dir = gsub(x = getwd(), pattern =  "/", replacement =  "\\\\"),
                                 browser.helperApps.neverAsk.saveToDisk = "text/plain,application/octet-stream,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                 browser.download.manager.showWhenStarting = FALSE))
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4567L,
  extraCapabilities = fprof,
  browserName = "firefox"
)

remDr <- remoteDriver(port = 4567L, extraCapabilities = fprof)

#will need to navigate to original page again every time, start of for loop. 
#set working directory to a folder to hold all pdfs/excel file
remDr$open(silent = TRUE)
testnum <- length(AdolescentDepression_Authors$title) - 14824
testnum

setwd("/Users/georgeabitante/Desktop/SearchTest")
remDr$navigate("https://singlelogin.org/?from=booksc.xyz")
Login <- remDr$findElement(using = "css selector", "#username")
Login$sendKeysToElement(list("georgeabitante@gmail.com"))
Password <- remDr$findElement(using = "css selector", "#password")
Password$sendKeysToElement(list("q1w2e3r4!", "\uE006"))

for (i in 1500:1600) {
  remDr$navigate("https://book4you.org/?signAll=1")
  #general website, need to use url for when logged in
  #remDr$navigate("https://libgen.bban.top/")
  Sys.sleep(5)
  SearchEntry <- remDr$findElement(using = "css selector", "#searchFieldx")
  SearchEntry$sendKeysToElement(list(AdolescentDepression_Authors$title[i], "\uE006"))
  Sys.sleep(5)
  check <- try(remDr$findElement(using = "css", "#searchResultBox a"))
  if(class(check) == "try-error"){
    AdolescentDepression_Authors$keywords[i] <- "missing"
    next
  }
  else {
    Sys.sleep(5)
    webElem <- remDr$findElement(using = "css", "#searchResultBox a")
    webElem$clickElement()
  }
  Sys.sleep(5)
  check2 <- try(remDr$findElement(using = "css", ".addDownloadedBook"))
  if(class(check2) == "try-error"){
    AdolescentDepression_Authors$keywords[i] <- "missing"
    next
  }
    else {
      Sys.sleep(5)
      webElem2 <- remDr$findElement(using = "css", ".addDownloadedBook")
      webElem2$clickElement() 
      Sys.sleep(2)
      webElem2$sendKeysToElement(list("\uE006"))
      AdolescentDepression_Authors$keywords[i] <- "downloaded"
  }
}






testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

# Starting time: record
t.start <- Sys.time()

# Query pubmed and fetch many results adolescen*[tiab] AND depress*[tiab]
my_query <- 'adolescen*[tiab] AND depress*[tiab]' 
#my_query[length(my_query)] <-  
my_query <- get_pubmed_ids(my_query, api_key = "0e0464ff93883fbef4c648d491ced27ed909")
number <- (as.numeric(my_query$Count))
number <- ceiling(number)
number <- 1:number

final_df_noauthor <- data.frame(pmid=character(),
                                doi=character(),
                                title=character(),
                                abstract=character(),
                                year=character(),
                                month=character(),
                                day=character(),
                                jabbrv=character(),
                                journal=character(),
                                keywords=character(),
                                lastname=character(),
                                firstname=character(),
                                address=character(),
                                email=character())

for (i in number) {
  newdata <- fetch_pubmed_data(my_query, retstart = i-1, retmax = 1)
  newlist <- articles_to_list(newdata)
  new_df_noauthor <- do.call(rbind, lapply(newlist, article_to_df, 
                                             max_chars = -1, getAuthors = FALSE))
  final_df_noauthor <- rbind(final_df_noauthor, new_df_noauthor)
  #testit(20)
}

# Final time: record
t.stop <- Sys.time()
# How long did it take?
print(t.stop - t.start)

#Write to excel file
write.xlsx(final_df_noauthor, 'AdolescentDepression_Authors.xlsx')

# Perform operation (use lapply here, no further parameters) - getauthors true/false 
#final_df_noauthor <- do.call(rbind, lapply(check2, article_to_df, 
                                  #max_chars = -1, getAuthors = FALSE))

#final_df_author <- do.call(rbind, lapply(all_xml, article_to_df, 
                                           #max_chars = -1, getAuthors = TRUE))
#Take out the second row of the dataframe
#final_df_noauthor <- final_df_noauthor[-2,]



for (i in final_df_noauthor$pmid) {
  c <- list()
  d <- list()
  depression_search = paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pmc_refs&id=", final_df_noauthor$pmid[i], "&tool=my_tool&email=my_email@example.com", sep = "")
  tables = read_xml(depression_search)
  tablescheck2 <- xml_contents(tables)
  tablescheck2 <- as.character(tablescheck2)
  ids <- unlist(rm_between(tablescheck2, "<Id>", "</Id>\n", extract=TRUE))
  for (b in length(ids)) {
    depression_search2 = paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pmc_refs&id=", ids[b], "&tool=my_tool&email=my_email@example.com", sep = "")
    ables = read_xml(depression_search)
    ablescheck2 <- xml_contents(ables)
    ablescheck2 <- as.character(ablescheck2)
    ids2 <- unlist(rm_between(ablescheck2, "<Id>", "</Id>\n", extract=TRUE))
    c[[b]] <- length(ids2)
    print(b)
    testit(1)
  }
  c <- as.numeric(c)
  d[[i]] <- mean(c)
  print(i)
  testit(1)
}

depression_search = paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pmc_refs&id=", 28554910, "&tool=my_tool&email=my_email@example.com", sep = "")
tables = read_xml(depression_search)
tablescheck2 <- xml_contents(tables)
tablescheck2 <- as.character(tablescheck2)
tablescheck2
ids <- unlist(rm_between(tablescheck2, "<Id>", "</Id>\n", extract=TRUE))

sapply(tablescheck, size)

#for(i in 1:nrow(final_df_noauthor)){

  # Give the input file name to the function.
  #result <- ""
  #result2 <- read_xml(depression_search)
  #result3 <- xmlParse(result2, asText = TRUE)
  #result <- append(result, result2)
  #depression_search <- sub(final_df_noauthor[i, 1], final_df_noauthor[i+1,1], depression_search)
 # print(i)
  #testit(5)
#}


########UNUSED CODE MAY BE USEFUL######

# Give the input file name to the function.
#result <- xmlParse(file = "input.xml")
# Print the result.
#print(result)

## Can use to this add text to ends of strings - useful for changing titles easily in 
## consistent way
#for (i in length(data)) {
#  f <- "[Title]"
#  d <- unlist(strsplit(data[i], " "))
#  d <- paste(d, f, sep = "")
#  data[i] <- d
#}
