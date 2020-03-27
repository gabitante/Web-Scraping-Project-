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

########Run PubMed Search########

# Starting time: record
t.start <- Sys.time()

# Query pubmed and fetch results for search string
my_query <- 'adolescen*[tiab] AND depress*[tiab]' 
#This api key should allow you to make large queries without issue
#not positive what the maximum search is
my_query <- get_pubmed_ids(my_query, api_key = "0e0464ff93883fbef4c648d491ced27ed909")
#obtain number of items that need to be iterated through in request
number <- 1:(as.numeric(my_query$Count))

#generate empty dataframe to hold data
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

#for loop to pull all results. May be more/less efficient with larger/
#smaller retmax (determines how many search results to pull at once)
#If issues with pulling too many results at once, use Sys.sleep(seconds)
#to include a pause in each loop
for (i in number) {
  newdata <- fetch_pubmed_data(my_query, retstart = i-1, retmax = 1)
  newlist <- articles_to_list(newdata)
  new_df_noauthor <- do.call(rbind, lapply(newlist, article_to_df, 
                                           max_chars = -1, getAuthors = FALSE))
  final_df_noauthor <- rbind(final_df_noauthor, new_df_noauthor)
}

# Final time: record
t.stop <- Sys.time()
# How long did it take?
print(t.stop - t.start)

########Pull PDF's Using Results From Search and Write to Excel File########

#I've had to run this to get the port to work correctly, not sure if 
#genuinely necessary
remDr <- rsDriver()

#This is supposed to make Firefox download files without asking you 
#for permission, currently not working for me
fprof <- makeFirefoxProfile(list(browser.download.folderList = 2L,
                                 browser.download.dir = gsub(x = getwd(), pattern =  "/", replacement =  "\\\\"),
                                 browser.helperApps.neverAsk.saveToDisk = "text/plain,application/octet-stream,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                 browser.download.manager.showWhenStarting = FALSE))

#Initialize the remote driver that will open Firefox. In theory, 
#extraCapabilities = fprof should load the specifications made
#for Firefox to download files
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4567L,
  extraCapabilities = fprof,
  browserName = "firefox"
)

#Open Firefox
remDr$open(silent = TRUE)

#Below code will navigate to website of choice, enter the titles from
#final_df_noauthor in search bar, then navigate each window 
#to download pdf if available.
#When missing, it will enter "missing" in the keywords variable in 
#final_df_noauthor.
for (i in length(final_df_noauthor$title)) {
  #general website, need to use url for when logged in
  remDr$navigate("https://libgen.bban.top/")
  Sys.sleep(5)
  SearchEntry <- remDr$findElement(using = "css selector", "#searchFieldx")
  SearchEntry$sendKeysToElement(list(final_df_noauthor$title[i], "\uE006"))
  Sys.sleep(5)
  check <- try(remDr$findElement(using = "css", "#searchResultBox a"))
  if(class(check) == "try-error"){
    final_df_noauthor$keywords[i] <- "missing"
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
    final_df_noauthor$keywords[i] <- "missing"
    next
  }
    else {
      Sys.sleep(5)
      webElem2 <- remDr$findElement(using = "css", ".addDownloadedBook")
      webElem2$clickElement() 
      Sys.sleep(2)
      webElem2$sendKeysToElement(list("\uE006"))
      final_df_noauthor$keywords[i] <- "downloaded"
  }
}

#Write to excel file
write.xlsx(final_df_noauthor, 'AdolescentDepression_Authors.xlsx')


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
