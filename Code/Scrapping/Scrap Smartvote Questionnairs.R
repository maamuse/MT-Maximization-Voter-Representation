############################################################
# Scrap IDs from Smartvote
############################################################

# install libraries
#install.packages("tidyverse")
#install.packages("rvest")
#install.packages("RSelenium")
#install.packages("stringi")

# imports
library(tidyverse)
library(rvest)
library(RSelenium)
library(stringi)

#setwd
setwd("C:\\Users\\Ueli\\Desktop\\Master Thesis\\Data and Code\\Smartvote")


############################################################
# Load helper functions
############################################################

scroll_to = function(rD, webElem){
  rD$executeScript("arguments[0].scrollIntoView(true);", 
                   args = list(webElem))
  webElem$highlightElement()
}


get_html = function(rD){
  rD$getPageSource() %>%
    .[[1]] %>%
    read_html()
}

get_data = function(source){
  
  candidate_name = Source %>% html_nodes(".candidate-title")  %>% html_text()
  
  #extract table
  
  SozialundFamilie = matrix(nrow = 6, ncol = 4)
  for(j in 1:6){
    SozialundFamilie[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-0"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Gesundheit = matrix(nrow = 5, ncol = 4)
  for(j in 1:5){
    Gesundheit[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-1"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Bildung = matrix(nrow = 4, ncol = 4)
  for(j in 1:4){
    Bildung[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-2"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Migration = matrix(nrow = 6, ncol = 4)
  for(j in 1:6){
    Migration[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-3"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Gesel = matrix(nrow = 5, ncol = 4)
  for(j in 1:5){
    Gesel[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-4"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Finanzen = matrix(nrow = 4, ncol = 4)
  for(j in 1:4){
    Finanzen[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-5"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Wirtschaft = matrix(nrow = 6, ncol = 4)
  for(j in 1:6){
    Wirtschaft[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-6"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Digital = matrix(nrow = 2, ncol = 4)
  for(j in 1:2){
    Digital[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-7"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Energie = matrix(nrow = 5, ncol = 4)
  for(j in 1:5){
    Energie[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-8"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Natur = matrix(nrow = 5, ncol = 4)
  for(j in 1:5){
    Natur[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-9"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Staat = matrix(nrow = 3, ncol = 4)
  for(j in 1:3){
    Staat[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-10"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Sicherheit = matrix(nrow = 5, ncol = 4)
  for(j in 1:5){
    Sicherheit[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-11"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Aussen = matrix(nrow = 4, ncol = 4)
  for(j in 1:4){
    Aussen[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-12"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Werte = matrix(nrow = 7, ncol = 7)
  for(j in 1:7){
    Werte[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-13"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  Budget = matrix(nrow = 8, ncol = 5)
  for(j in 1:8){
    Budget[j,] = Source %>% html_nodes(xpath = sprintf('//*[@id="cdk-accordion-child-14"]/div/div[%d]/sv-answers/div',j))  %>% html_attr("style")
  }
  
  joined = rbind(SozialundFamilie, Gesundheit, Bildung, Migration, 
                 Gesel, Finanzen, Wirtschaft, Digital, Energie,
                 Natur, Staat, Sicherheit, Aussen)
  joined[is.na(joined)] <- 0
  joined[!(joined == 0)] <- 1
  colnames(joined) <- c("Yees","Yes","No","Noo")
  
  Werte[is.na(Werte)] <- 0
  Werte[!(Werte == 0)] <- 1
  colnames(Werte) <- c("Nooo","Noo","No","Equal","Yes","Yees","Yees")
  
  Budget[is.na(Budget)] <- 0
  Budget[!(Budget == 0)] <- 1
  colnames(Budget) <- c("Yees","Yes","Equal","No","Noo")
  
  
  back = list(candidate_name, joined, Werte, Budget)
  
  return(back)
}

############################################################
# Create Links
############################################################

#load data
IDs = read.csv2("ID.csv", stringsAsFactors = FALSE)
IDs2 = stri_extract_last_regex(IDs$x, "\\d{11}")

candidate_links = c()

for (i in 1:length(IDs2)){
  link = paste0("https://smartvote.ch/de/group/2/election/19_ch_nr/db/candidates/", IDs2[i])
  candidate_links[i] = link
}


############################################################
# Setup RSelenium Server
############################################################

#lookup installed chrome versions if there is an error to switch
#binman::list_versions("chromedriver")

#start server
remDr = rsDriver(port = 7861L, 
                 browser = "chrome", 
                 chromever="78.0.3904.105")
rD <- remDr[["client"]]


############################################################
# Setup RSelenium Server
############################################################

#load page

counter = 0

for(i in 3000:3912){
  
  counter = counter + 1
  
  #go to candidate site
  rD$navigate(candidate_links[i])
  Sys.sleep(2)
  
  #extract name
  webElem = rD$findElement("xpath", '//*[@id="mat-tab-label-0-1"]/div')
  #webElem$highlightElement()
  webElem$clickElement()
  Sys.sleep(2)
  
  Source = get_html(rD)
  Sys.sleep(1)
  
  results = get_data(Source)
  
  write.csv2(results[2], file = paste0( "Q1\\", gsub( "[[:space:]]", "", results[1] ) ,".csv" )) 
  write.csv2(results[3], file = paste0( "Q2\\", gsub( "[[:space:]]", "", results[1] ) ,".csv" ))
  write.csv2(results[4], file = paste0( "Q3\\", gsub( "[[:space:]]", "", results[1] ) ,".csv" ))

}
  
############################################################
# Stop RSelenium Server
############################################################

rD$close()
remDr[["server"]]$stop()
