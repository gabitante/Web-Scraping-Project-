#Make sure you have the R code saved to your desktop 
#and that your working directory is to your desktop
#getwd()
#if getwd() returns anything different, use below
#code to switch to desktop
setwd("/Users/georgeabitante/Desktop")
#source will allow us to use functions saved in 
#the selected R code
source('PubMedScraping_GitHub.R')

#https://support.mozilla.org/en-US/kb/profiles-where-firefox-stores-user-data this 
#link will allow you to find the location where your firefox profile is saved.

#Run with a sci hub account
#Each entry should be a string
#IMPORTANT NOTE PLEASE READ - I made it so that you can have a single, set location where you download files from firefox
#initially and move them all to another folder. Eg if your downloads go to your download folder, this will 
#work just fine, but you should always make sure there are no other files in that location when you start running
#this code. The code checks for any pdf in the savelocation and moves them all, so having other pdfs there
#when the code starts would make it unreliable/move things you didn't want moved. 

PubMedScraping(query = 'enter the pubmed search you want to run here', 
               firefoxprofile = 'use the link above to find the location of your firefox profile, enter the path here as a string', 
               username = 'your scihub username', password = 'your scihub password', 
               savelocation = 'this is a relay location where your firefox browser will save your files by default', 
               filename = 'the name you want to use for the excel file',
               newsavelocation = 'this is the folder you want your files to end up in')

#Run without a sci hub account (download limit 10 pdfs/24 hours)
#IMPORTANT NOTE PLEASE READ - do not delete the entries below for username/password, as the code will not function
#if they don't have some input - the NULL input just lets the code know not to use the function that requires 
#a scihub account. 
PubMedScraping(query = 'enter the pubmed search you want to run here', 
               firefoxprofile = 'use the link above to find the location of your firefox profile, enter the path here as a string', 
               username = NULL, password = NULL, 
               savelocation = 'this is a relay location where your firefox browser will save your files by default', 
               filename = 'the name you want to use for the excel file',
               newsavelocation = 'this is the folder you want your files to end up in')

#The web browsers can be finnicky - if issue comes up with could not find port 4567L or something, 
#Quit out of R studio and reopen, that resolves the issue. 
