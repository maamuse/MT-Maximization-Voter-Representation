#############################################################################################################
# Voting behaviour Accuracy
#############################################################################################################

#setwd
setwd("C:\\Users\\Marco\\Desktop\\Master\\Master\\Master Thesis\\Master Thesis\\Data")

#libraries
library(gridExtra)



############################################################
# Load Data 
############################################################

#extract filenames of candidate csv files
councillor_names = list.files(path = "Smartvote Downloads\\candidates Votes transformes\\Q1", pattern = NULL, all.files = FALSE,
                             full.names = FALSE, recursive = FALSE,
                             ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#import first questionnaire
councillor_quest = read.csv2(file = paste0("Smartvote Downloads\\candidates Votes transformes\\Q1\\", councillor_names[1]), stringsAsFactors = FALSE)
councillor_quest = councillor_quest[2]

#cbind remaining questionnaires to councillor_quest
for(i in 2:length(councillor_names)){
  
  councillor_quest_2 = read.csv2(file = paste0("Smartvote Downloads\\candidates Votes transformes\\Q1\\", councillor_names[i]), stringsAsFactors = FALSE)
  councillor_quest_2 = councillor_quest_2[2]
  councillor_quest = cbind(councillor_quest, councillor_quest_2)
}

#use councillor names as header
names(councillor_quest) <- councillor_names

#Map the answer no (0) to -1 to allow for neutral answers
councillor_quest[councillor_quest==0] =-1

rm(councillor_quest_2)

#import councillor voting data
councillor_votes = read.csv2(file = "Abstimmungen Nationalrat\\Unique Votes for R.csv", stringsAsFactors = FALSE)

#translate voting data into numerical representation
councillor_votes[councillor_votes=="Ja"] = 1
councillor_votes[councillor_votes=="Nein"] = -1
councillor_votes[councillor_votes == "Enthaltung"] = 0 
councillor_votes[councillor_votes == "Hat nicht teilgenommen"] = 0 
councillor_votes[councillor_votes == "Entschuldigt"] = 0 
councillor_votes[councillor_votes == "Der Präsident stimmt nicht"] = 0

#import matching table that relates parlimentary businesses with the smartvote questionnaire
matters_catalogue = read.csv2(file = "Vorlagenkategorisierung\\Vorlagenkategorisierung R.csv", stringsAsFactors = FALSE, check.names = FALSE)

#ids of parliamentary businesses that were matched to the Smartvote questionnaire.
matter = c("16.055", "16.3111", "15.3803", "08.432", "13.468", 
           "17.3047", "17.047", "18.075", "16.3006", "17.3971", 
           "16.489", "16.3865", "16.3007", "13.074", "14.319",
           "14.320", "16.056", "18.096", "15.3714",  "17.429",
           "15.3933", "18.3797", "15.3559")

#initialize empty lists
question = c()
row_question = c()
neg_row_question = c()

#Extract for each matter that was listed the the question and the row number. 
counter = 0
for(i in 1:length(matter)){
  question[i] = matters_catalogue[!is.na(matters_catalogue[matter[i]]),1]
  row_question[i] = which(matters_catalogue[matter[i]]==1|matters_catalogue[matter[i]]==-1)
  
  if(!is.na(match(TRUE, matters_catalogue[matter[i]]==-1))){
    counter = counter + 1
    neg_row_question[counter] = match(TRUE, matters_catalogue[matter[i]]==-1)
  }
  
}

#reverse the voting sign in cases wehere the Smartvote question is the inverse of the parliamentary vote
councillor_quest[neg_row_question,] = councillor_quest[neg_row_question,]*(-1)



############################################################
# Extract relevant Data Points
############################################################

#Create data extraction for voting data of first parliamentary business listed in matter
row_councillor_votes = which(councillor_votes$ID == matter[1])
extr_councillor_votes = councillor_votes[row_councillor_votes,]
extr_councillor_votes$ID = NULL

#rbind voting data of remaining parliamentary businesses listed in matter
for(i in 2:length(matter)){
  row_councillor_votes2 = which(councillor_votes$ID == matter[i])
  extr_councillor_votes2 = councillor_votes[row_councillor_votes2,]
  extr_councillor_votes2$ID = NULL
  extr_councillor_votes = rbind(extr_councillor_votes, extr_councillor_votes2)
}

#extract anwsers to parliamentary businesses from Smartvote questionnaire
extr_councillor_quest = councillor_quest[row_question,]

#clean evironment
rm(councillor_votes,i, row_councillor_votes2, row_councillor_votes, councillor_quest)


############################################################
# Data Description
############################################################

#Check number of politicians (must be 134)
number_politicians = length(councillor_names)
# number_politicians

#Check number of matched businesses (must be 23)
# number_bus = length(matter)
# number_bus

#percentage of votes that were answered with yes in the questionnaire
# y_share_quest = sum(extr_councillor_quest == 1)/(dim(extr_councillor_quest)[1]*dim(extr_councillor_quest)[2])

#percentage of votes that were answered with yes in parliament
# y_share_vote_vote = sum(extr_councillor_votes == 1)/(dim(extr_councillor_votes)[1]*dim(extr_councillor_votes)[2])

#percentage of neutral votes in parliament
# zero_share_vote_vote = sum(extr_councillor_votes == 0)/(dim(extr_councillor_votes)[1]*dim(extr_councillor_votes)[2])


############################################################
# Base Accuracy - Individual Voter Representability
############################################################

#Deviation from expected Smartvote Score. 
#1 means 100% as indicated by smartvote. 
#0 means the exact opposite as indicated by smartvote.

#change from char to num value
extr_councillor_votes <- data.frame(apply(extr_councillor_votes, 2, function(x) as.numeric(as.character(x))))

#calculate overlap between parliament and Smartvotefor on parliamentary businesses
overlap = 1 - (abs(extr_councillor_quest-extr_councillor_votes)/2)
bus_overlap = rowSums(overlap)/number_politicians

#summary stats overlap
mean(bus_overlap)
median(bus_overlap)
sd(bus_overlap)
hist(bus_overlap)



############################################################
# Base Accuracy - Individual Politicians Overlapp
############################################################

#Deviation from expected Smartvote Score. 
#1 means 100% as indicated by smartvote. 
#0 means the exact opposite as indicated by smartvote.

#calculate overlap between parliament and smartvote for each politician
indiv_overlap = colSums(overlap)/length(matter)

#summary stats overlap
mean(indiv_overlap)
median(indiv_overlap)
sd(indiv_overlap)
hist(indiv_overlap)

#list of all politicians inc overlap
indiv_overlap = as.data.frame(round(indiv_overlap,2))
grid.table(indiv_overlap)
