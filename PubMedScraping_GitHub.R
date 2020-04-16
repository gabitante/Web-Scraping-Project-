#Complete function for running code
PubMedScraping <- function(query, firefoxprofile, username, password, savelocation, filename, newsavelocation) {
  inputs <<- c(query, firefoxprofile, savelocation, filename, newsavelocation)
  if(is.null(username) == TRUE){
    LoadPackages()
    PubMedSearch(query)
    DownloadPDF_NoAccount(firefoxprofile, savelocation, filename, newsavelocation)
    WriteToExcel(newsavelocation, filename)
  }
  else {
    LoadPackages()
    PubMedSearch(query)
    DownloadPDF_Account(firefoxprofile, username, password, savelocation, filename, newsavelocation)
    WriteToExcel(newsavelocation, filename)
  }
}

#######Load Packages for All Functions#######

LoadPackages <- function(){
  #load packages for scraping pubmed
  library(easyPubMed)
  #used for parsing xml files
  library(xml2)
  #convert to excel file
  library(openxlsx)
  #parse out ID's from xml code
  library(qdapRegex)
  #run automated websearch
  library(RSelenium)
  #rename dataframe variables
  library(plyr)
  #used to rename pdf files
  library(R.utils)
  #used to check whether count is an integer
  library(ttutils)
}

########Run PubMed Search########

PubMedSearch <- function(query){
  
  # Query pubmed and fetch results for search string
  my_query <<- query
  #This api key should allow you to make large queries without issue
  #not positive what the maximum search is
  my_query <<- get_pubmed_ids(my_query, api_key = "0e0464ff93883fbef4c648d491ced27ed909")
  #obtain number of items that need to be iterated through in request
  number <<- 1:(as.numeric(my_query$Count))
  
  #generate empty dataframe to hold data
  final_df_noauthor <<- data.frame(query=character(),
                                   pmid=character(),
                                   doi=character(),
                                   title=character(),
                                   abstract=character(),
                                   year=character(),
                                   month=character(),
                                   day=character(),
                                   jabbrv=character(),
                                   journal=character(),
                                   PDFStatus=character(),
                                   Search=character(),
                                   firstname=character(),
                                   address=character(),
                                   email=character(),
                                   search=character())
  
  #for loop to pull all results. May be more/less efficient with larger/
  #smaller retmax (determines how many search results to pull at once)
  #If issues with pulling too many results at once, use Sys.sleep(seconds)
  #to include a pause in each loop
  for (i in number) {
    newdata <<- fetch_pubmed_data(my_query, retstart = i-1, retmax = 1)
    newlist <<- articles_to_list(newdata)
    new_df_noauthor <<- do.call(rbind, lapply(newlist, article_to_df, 
                                              max_chars = -1, getAuthors = FALSE))
    final_df_noauthor <<- rbind(final_df_noauthor, new_df_noauthor)
  }
 #these lines are just cleaning the data - renaming two columns to PDFStatus and Query,
 #adding the query used to the first cell of Query column, then eliinating columns that
 #weren't used at all. 
   final_df_noauthor <<- rename(final_df_noauthor, c("keywords"="PDFStatus", "lastname"="Query"))
  final_df_noauthor$Query[1] <<- query
  final_df_noauthor <<- final_df_noauthor[, c(1:11)]
}

########Pull PDF's Using Results From Search and Write to Excel File########
#This function loads the web browser and pulls pdfs using the results from the pubmed search
DownloadPDF_Account <- function(firefoxprofile, username, password, savelocation, filename, newsavelocation) {
  #I've had to run this to get the port to work correctly, not sure if 
  #genuinely necessary
  remDr1 <- rsDriver()
  
  #used to get the firefox profile you currently have -- any settings you want to apply
  #for this will have to be saved beforehand
  fprof <<- getFirefoxProfile(firefoxprofile, useBase = TRUE)

  #Initialize the remote driver that will open Firefox. In theory, 
  #extraCapabilities = fprof should load the specifications made
  #for Firefox to download files
  remDr <<- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4567L,
    extraCapabilities = fprof,
    browserName = "firefox"
  )
  #Browser sometimes quits due to error when running immediately after 
  #remDr, so need sleep time between them.
  Sys.sleep(15)
  #Open Firefox
  remDr$open(silent = TRUE)
  Sys.sleep(15)
  
  #This section enters your username/password for sci hub if you have them
  remDr$navigate("https://singlelogin.org/?from=booksc.xyz")
  Sys.sleep(5)
  Login <- remDr$findElement(using = "css selector", "#username")
  Login$sendKeysToElement(list(username))
  Password <- remDr$findElement(using = "css selector", "#password")
  Password$sendKeysToElement(list(password, "\uE006"))
  Sys.sleep(5)
  
  #Below code will navigate to website of choice, enter the titles from
  #final_df_noauthor in search bar, then navigate each window 
  #to download pdf if available.
  #When missing, it will enter "missing" in the PDFStatus variable in 
  #final_df_noauthor, when downloaded successfully it will enter 
  #as "downloaded"
  if(file.exists(file.path(newsavelocation, filename)) == TRUE) {
    oldfile <<- read.xlsx(file.path(newsavelocation, filename))
    for (i in 1:length(final_df_noauthor$title)) {
      if(is.na(oldfile$PDFStatus[i]) == FALSE){
        next
      }
      browsercheck <- try(remDr$navigate("https://book4you.org/?signAll=1"))
      if((class(browsercheck) != "NULL")){
        try(remDr1 <- rsDriver())
        Sys.sleep(5)
        remDr$open(silent = TRUE)
        Sys.sleep(5)
        remDr$navigate("https://singlelogin.org/?from=booksc.xyz")
        Sys.sleep(5)
        Login <- remDr$findElement(using = "css selector", "#username")
        Login$sendKeysToElement(list(username))
        Password <- remDr$findElement(using = "css selector", "#password")
        Password$sendKeysToElement(list(password, "\uE006"))
        Sys.sleep(2)
      }
      remDr$navigate("https://book4you.org/?signAll=1")
      Sys.sleep(2)
      ExactMatch <- remDr$findElement(using = "css selector", "#advSearch-control")
      ExactMatch$clickElement()
      ExactMatch2 <- remDr$findElement(using = "css selector", "#ftcb")
      ExactMatch2$clickElement()
      Sys.sleep(2)
      SearchEntry <- remDr$findElement(using = "css selector", "#searchFieldx")
      SearchEntry$sendKeysToElement(list(final_df_noauthor$title[i], "\uE006"))
      Sys.sleep(2)
      Articles <- remDr$findElement(using = "css selector", ".searchServiceStats")
      Articles$clickElement()
      Sys.sleep(2)
      check <- try(remDr$findElement(using = "css", "#searchResultBox a"))
      if(class(check) == "try-error"){
        final_df_noauthor$PDFStatus[i] <- "missing"
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
        final_df_noauthor$PDFStatus[i] <- "missing"
        next
      }
      else {
        Sys.sleep(5)
        webElem2 <- remDr$findElement(using = "css", ".addDownloadedBook")
        webElem2$clickElement() 
        Sys.sleep(2)
        webElem2$sendKeysToElement(list("\uE006"))
        final_df_noauthor$PDFStatus[i] <- "downloaded"
        pdfs <- list.files(savelocation, pattern = .pdf)
        pdfs <- data.frame(lapply(pdfs, as.character), stringsAsFactors=FALSE)
        if(nchar(pdfs[1, 1]) >= 5){
          pdf <- paste(final_df_noauthor$title[i], '.pdf', sep="")
          newname <- toString(pdf)
          renameFile(file.path(savelocation, pdfs[1,1]), file.path(newsavelocation, newname))
        }
      }
      if(i == length(final_df_noauthor)) {
        remDr$close()
      }
    }
  }
  else {
    for (i in 1:length(final_df_noauthor$title)) {
      browsercheck <- try(remDr$navigate("https://book4you.org/?signAll=1"))
      if((class(browsercheck) != "NULL")){
        try(remDr1 <- rsDriver())
        Sys.sleep(5)
        remDr$open(silent = TRUE)
        Sys.sleep(5)
        remDr$navigate("https://singlelogin.org/?from=booksc.xyz")
        Sys.sleep(5)
        Login <- remDr$findElement(using = "css selector", "#username")
        Login$sendKeysToElement(list(username))
        Password <- remDr$findElement(using = "css selector", "#password")
        Password$sendKeysToElement(list(password, "\uE006"))
        Sys.sleep(5)
      }
      remDr$navigate("https://book4you.org/?signAll=1")
      Sys.sleep(2)
      ExactMatch <- remDr$findElement(using = "css selector", "#advSearch-control")
      ExactMatch$clickElement()
      ExactMatch2 <- remDr$findElement(using = "css selector", "#ftcb")
      ExactMatch2$clickElement()
      Sys.sleep(2)
      SearchEntry <- remDr$findElement(using = "css selector", "#searchFieldx")
      SearchEntry$sendKeysToElement(list(final_df_noauthor$title[i], "\uE006"))
      Sys.sleep(2)
      Articles <- remDr$findElement(using = "css selector", ".searchServiceStats")
      Articles$clickElement()
      Sys.sleep(2)
      check <- try(remDr$findElement(using = "css", "#searchResultBox a"))
      if(class(check) == "try-error"){
        print("no result")
        final_df_noauthor$PDFStatus[i] <<- "missing"
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
        print("no result 2")
        final_df_noauthor$PDFStatus[i] <<- "missing"
        next
      }
      else {
        Sys.sleep(5)
        webElem2 <- remDr$findElement(using = "css", ".addDownloadedBook")
        webElem2$clickElement() 
        Sys.sleep(2)
        webElem2$sendKeysToElement(list("\uE006"))
        final_df_noauthor$PDFStatus[i] <<- "downloaded"
        pdfs <- list.files(savelocation, pattern = ".pdf")
        pdfs <- data.frame(lapply(pdfs, as.character), stringsAsFactors=FALSE)
        if(nchar(pdfs[1, 1]) >= 5){
          pdf <- paste(final_df_noauthor$title[i], '.pdf', sep="")
          newname <- toString(pdf)
          renameFile(file.path(savelocation, pdfs[1,1]), file.path(newsavelocation, newname))
        }
      }
      if(i == length(final_df_noauthor$title)) {
        remDr$close()
      }
    }
  }
}

#This function will be used when you don't have a scihub username/password - principle
#difference is that the maximum/day without an account is 10 pdfs, so will stop itself
#after successfully downloading 10. 
DownloadPDF_NoAccount <- function(firefoxprofile, savelocation, filename, newsavelocation) {
  #I've had to run this to get the port to work correctly, not sure if 
  #genuinely necessary
  remDr1 <- rsDriver()
  #This is supposed to make Firefox download files without asking you 
  #for permission, currently not working for me
  
  fprof <<- getFirefoxProfile(firefoxprofile, useBase = TRUE)
  
  #Initialize the remote driver that will open Firefox. In theory, 
  #extraCapabilities = fprof should load the specifications made
  #for Firefox to download files
  remDr <<- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4567L,
    extraCapabilities = fprof,
    browserName = "firefox"
  )
  
  #Browser sometimes quits due to error when running immediately after 
  #remDr, so need sleep time between them.
  Sys.sleep(15)
  #Open Firefox
  remDr$open(silent = TRUE)
  Sys.sleep(15)
  
  count <- 0
  if(file.exists(file.path(newsavelocation, filename)) == TRUE) {
    oldfile <<- read.xlsx(file.path(newsavelocation, filename))
    for (i in 1:length(final_df_noauthor$title)) {
      if(is.na(oldfile$PDFStatus[i]) == FALSE){
        next
      }
      if((isInteger(count/10) & (count >= 10)) == TRUE){
        remDr$close
        break
      }
      browsercheck <- try(remDr$navigate("https://libgen.bban.top/"))
      if((class(browsercheck) != "NULL")){
        try(remDr1 <- rsDriver())
        Sys.sleep(5)
        remDr$open(silent = TRUE)
        Sys.sleep(5)

      }
      remDr$navigate("https://libgen.bban.top/")
      ExactMatch <- remDr$findElement(using = "css selector", "#advSearch-control")
      ExactMatch$clickElement()
      ExactMatch2 <- remDr$findElement(using = "css selector", "#ftcb")
      ExactMatch2$clickElement()
      Sys.sleep(5)
      SearchEntry <- remDr$findElement(using = "css selector", "#searchFieldx")
      SearchEntry$sendKeysToElement(list(final_df_noauthor$title[i], "\uE006"))
      Sys.sleep(5)
      Articles <- remDr$findElement(using = "css selector", ".searchServiceStats")
      Articles$clickElement()
      Sys.sleep(5)
      check <- try(remDr$findElement(using = "css", "#searchResultBox a"))
      if(class(check) == "try-error"){
        final_df_noauthor$PDFStatus[i] <<- "missing"
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
        final_df_noauthor$PDFStatus[i] <<- "missing"
        next
      }
      else {
        Sys.sleep(5)
        webElem2 <- remDr$findElement(using = "css", ".addDownloadedBook")
        webElem2$clickElement() 
        Sys.sleep(2)
        webElem2$sendKeysToElement(list("\uE006"))
        final_df_noauthor$PDFStatus[i] <<- "downloaded"
        count <- count + 1
        pdfs <- list.files(savelocation, pattern = .pdf)
        pdfs <- data.frame(lapply(pdfs, as.character), stringsAsFactors=FALSE)
        if(nchar(pdfs[1, 1]) >= 5){
          pdf <- paste(final_df_noauthor$title[i], '.pdf', sep="")
          newname <- toString(pdf)
          renameFile(file.path(savelocation, pdfs[1,1]), file.path(newsavelocation, newname))
        }
      }
      if(i == length(final_df_noauthor)) {
        remDr$close()
      }
    }
  }
  else {
        for (i in 1:length(final_df_noauthor$title)) {
          browsercheck <- try(remDr$navigate("https://libgen.bban.top/"))
          if((class(browsercheck) != "NULL")){
            try(remDr1 <- rsDriver())
            Sys.sleep(5)
            remDr$open(silent = TRUE)
            Sys.sleep(5)
          }
          if((isInteger(count/10) & (count >= 10)) == TRUE){
            remDr$close
            break
          }
          remDr$navigate("https://libgen.bban.top/")
          ExactMatch <- remDr$findElement(using = "css selector", "#advSearch-control")
          ExactMatch$clickElement()
          ExactMatch2 <- remDr$findElement(using = "css selector", "#ftcb")
          ExactMatch2$clickElement()
          Sys.sleep(5)
          SearchEntry <- remDr$findElement(using = "css selector", "#searchFieldx")
          SearchEntry$sendKeysToElement(list(final_df_noauthor$title[i], "\uE006"))
          Sys.sleep(5)
          Articles <- remDr$findElement(using = "css selector", ".searchServiceStats")
          Articles$clickElement()
          Sys.sleep(5)
          check <- try(remDr$findElement(using = "css", "#searchResultBox a"))
          if(class(check) == "try-error"){
            final_df_noauthor$PDFStatus[i] <<- "missing"
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
            final_df_noauthor$PDFStatus[i] <<- "missing"
            next
          }
          else {
            Sys.sleep(5)
            webElem2 <- remDr$findElement(using = "css", ".addDownloadedBook")
            webElem2$clickElement() 
            Sys.sleep(2)
            webElem2$sendKeysToElement(list("\uE006"))
            final_df_noauthor$PDFStatus[i] <<- "downloaded"
            count <- count + 1
            pdfs <- list.files(savelocation, pattern = .pdf)
            pdfs <- data.frame(lapply(pdfs, as.character), stringsAsFactors=FALSE)
            if(nchar(pdfs[1, 1]) >= 5){
              pdf <- paste(final_df_noauthor$title[i], '.pdf', sep="")
              newname <- toString(pdf)
              renameFile(file.path(savelocation, pdfs[1,1]), file.path(newsavelocation, newname))
            }
          }
          if(i == length(final_df_noauthor$title)) {
            remDr$close()
          }
        }
  }
}

#This will write out the results of your search and whether pdfs were downloaded. 
#When running a second time, it will just append new results to the document you already have 
#rather than rewriting it. 
WriteToExcel <- function(newsavelocation, filename) {
  setwd(newsavelocation)
  if(file.exists(file.path(newsavelocation, filename)) == FALSE) {
    Output <- createWorkbook()
    addWorksheet(Output, "Search Results")
    addWorksheet(Output, "Search Inputs")
    writeData(Output, sheet = "Search Results", x = final_df_noauthor)
    writeData(Output, sheet = "Search Inputs", x = inputs)
    saveWorkbook(Output, filename)
  }
  else  {
    oldfile <<- read.xlsx(file.path("/Users/georgeabitante/Desktop/Judy Garber Search", "AdolescentDepression_JG.xlsx"))
    for(i in 1:length(final_df_noauthor)){
      if((final_df_noauthor$pmid[i] %in% oldfile$pmid) == TRUE) {
        next
      }
      else {
        oldfile <<- oldfile.append(final_df_noauthor[i, 1:11])
      }
    writein <- list("Search Results" = oldfile, "Search Inputs" = inputs)
    write.xlsx(writein, file=filename, sheetName='Search Inputs', append=TRUE)
    } 
  }
}


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
