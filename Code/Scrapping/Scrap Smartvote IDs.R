############################################################
# Scrap Questionairs from Smartvote
############################################################

# install libraries
#install.packages("tidyverse")
#install.packages("rvest")
#install.packages("RSelenium")

# imports
library(tidyverse)
library(rvest)
library(RSelenium)

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

############################################################
# Setup RSelenium Server
############################################################

#lookup installed chrome versions if there is an error to switch
#binman::list_versions("chromedriver")

#start server
remDr = rsDriver(port = 7882L, 
         browser = "chrome", 
         chromever="78.0.3904.105")
rD <- remDr[["client"]]


############################################################
# Scrap Website
############################################################

#load page
rD$navigate("https://www.smartvote.ch/de/group/2/election/19_ch_nr/db/candidates")

#find search button
webElem = rD$findElement("css", ".mr-4")

#got to search button
webElem$sendKeysToElement(list(key = "end"))

#webElem$highlightElement()

#click search button
webElem$clickElement()

#jump to bottom of website so website loads
webElem$sendKeysToElement(list(key = "end"))

extract_id = function(rD){
  Source = get_html(rD)
  data = Source %>% html_nodes(".py-2") %>% html_attr('href')
  return(data)
}

#df = extract_id(rD)

df_backup = df

df = c(df, extract_id(rD))


############################################################
# Store ID's as CSV
############################################################

write.csv2(df, file = "ID.csv")

############################################################
# Stop RSelenium Server
############################################################

rD$close()
remDr[["server"]]$stop()
