

#This function simulates voters answers to the questionnaire based on a predefined probability distribution. The size
#of the matrix depends on the number of parliamentary businesses and voters simulated.
generate_random_voters = function(num_bus, num_voters, distribution){
  
  #voters matrix
  set.seed(64)
  voters_matrix = matrix(sample(c(-1,1), num_bus*num_voters, replace = TRUE, prob = distribution), nrow = num_bus, ncol = num_voters)
  
  return(voters_matrix)
  
}



#This function loads the data on the parliamentary businesses that were matched with a question in the SMartvote Questionnaire.
load_matters = function(){
  #This CSV contains all relevant matters the National Council has voted on that can have a corresponding question on Smartvote
  matters_catalogue = read.csv2(file = "Vorlagenkategorisierung\\Vorlagenkategorisierung R.csv", stringsAsFactors = FALSE, check.names = FALSE)
  
  #the subset of matters that is actually used is defined here
  # matter = c("16.055", "16.3111", "15.3803",
  #            "08.432", "13.468", "17.3047", "17.047", "18.075", "16.3006",
  #            "17.3971", "16.3865", "16.3007", "13.074",
  #            "14.320", "16.056", "15.3714",
  #            "15.3933", "18.3797", "15.3559")
  
  matter = c("16.055", "16.3111", "15.3803", "08.432", "13.468", 
             "17.3047", "17.047", "18.075", "16.3006", "17.3971", 
             "16.489", "16.3865", "16.3007", "13.074", "14.319",
             "14.320", "16.056", "18.096", "15.3714",  "17.429",
             "15.3933", "18.3797", "15.3559")
  
  #Here we make all the Info surounding the Matters accessible
  question = c()
  row_question = c()
  neg_row_question = c()
  counter = 0
  
  for(i in 1:length(matter)){
    question[i] = matters_catalogue[!is.na(matters_catalogue[matter[i]]),1]
    row_question[i] = which(matters_catalogue[matter[i]]==1|matters_catalogue[matter[i]]==-1) #rows of smartvote questionnnaire with match
    
    if(!is.na(match(TRUE, matters_catalogue[matter[i]]==-1))){
      counter = counter + 1
      neg_row_question[counter] = match(TRUE, matters_catalogue[matter[i]]==-1) #inverse weighted matters
    }
  }
  return(list(matters_catalogue, matter, neg_row_question, row_question))
}



#loads councillors parliamentary voting data
load_councillor_votes = function(matter){
  
  #Here we import the councillors actual voting behaviour
  councillor_votes = read.csv2(file = "Abstimmungen Nationalrat\\Unique Votes for R.csv", stringsAsFactors = FALSE)
  
  #To being able to properly work with the councillors votes they have to be translated into numbers.
  #Yes is translated into 1
  councillor_votes[councillor_votes=="Ja"] = 1
  #No is translated to -1
  councillor_votes[councillor_votes=="Nein"] = -1
  #If a councillor has not voted on a matter we will see it as neutral
  councillor_votes[councillor_votes == "Enthaltung"] = 0 
  councillor_votes[councillor_votes == "Hat nicht teilgenommen"] = 0 
  councillor_votes[councillor_votes == "Entschuldigt"] = 0 
  councillor_votes[councillor_votes == "Der Präsident stimmt nicht"] = 0
  
  #Find the rows correpsonding to the proper matter ID
  row_councillor_votes = which(councillor_votes$ID == matter[1])
  extr_councillor_votes = councillor_votes[row_councillor_votes,]
  #remove ID now that we have the data
  extr_councillor_votes$ID = NULL
  
  for(i in 2:length(matter)){
    row_councillor_votes2 = which(councillor_votes$ID == matter[i])
    extr_councillor_votes2 = councillor_votes[row_councillor_votes2,]
    extr_councillor_votes2$ID = NULL
    extr_councillor_votes = rbind(extr_councillor_votes, extr_councillor_votes2)
  }
  
  #transform the data from char to numeric
  extr_councillor_votes = sapply(extr_councillor_votes, as.numeric)
  extr_councillor_votes = as.matrix(extr_councillor_votes)
  
  return(extr_councillor_votes)
  
}



#loads councillors answers to the Smartvote questionnaire
load_councillor_quest = function(neg_row_question,row_question){
  
  #Here we load in the Councillors smartvote profiles
  #First we load the filenames of the csvs
  councillor_names = list.files(path = "Smartvote Downloads\\Candidates Votes transformes\\Q1", pattern = NULL, all.files = FALSE,
                                full.names = FALSE, recursive = FALSE,
                                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  #Create first file where the others then are loaded into
  councillor_quest = read.csv2(file = paste0("Smartvote Downloads\\Candidates Votes transformes\\Q1\\", councillor_names[1]), stringsAsFactors = FALSE)
  councillor_quest = councillor_quest[2]
  
  for(i in 2:length(councillor_names)){
    
    councillor_quest_2 = read.csv2(file = paste0("Smartvote Downloads\\Candidates Votes transformes\\Q1\\", councillor_names[i]), stringsAsFactors = FALSE)
    councillor_quest_2 = councillor_quest_2[2]
    councillor_quest = cbind(councillor_quest, councillor_quest_2)
  }
  #here we add the names of the councillors to the voting data
  names(councillor_quest) = councillor_names
  #Here no is depicted as 0. However for the analysis we have to change it to -1.
  councillor_quest[councillor_quest==0] =-1
  
  councillor_quest[neg_row_question,] = councillor_quest[neg_row_question,]*(-1)
  
  #only relevant observations from councillor smartvote anwsers
  extr_councillor_quest = councillor_quest[row_question,]
  extr_councillor_quest = as.matrix(extr_councillor_quest)
  
  return(extr_councillor_quest)
}



#calculates political overlap between Politicians Smartvote Questionnaire and the simulated voters
#returns matrix of all politicians and their overlap with each simulated voter  
calc_quest_overlap = function(num_pol, num_bus, num_voters, party_aff, quest_matrix, voters_matrix){
  
  #intitalize matrix
  all_voter_pol_overlap = matrix(0, nrow=num_voters,ncol=num_pol)
  colnames(all_voter_pol_overlap) = party_aff[[1]] 
  
  for (i in 1:num_voters){
    all_voter_pol_dist = 1-abs(quest_matrix - voters_matrix[,i])/2
    all_voter_pol_overlap[i,] = colSums(all_voter_pol_dist)/num_bus
  }
  
  return(all_voter_pol_overlap)
  
}



#uses Smartvote recommendation to create a voting recommendation. Then picks the recommended politicians and calculatesthe true overlap.
#returns overall true overlap per voter based on voting recommendation
calc_panag_overlap = function(num_bus, num_voters, num_panag_votes, all_voter_pol_overlap, voters_matrix, votes_matrix){
  
  #initialize matrix
  panag_overlap = matrix(0, nrow=num_voters,ncol=num_panag_votes)
  
  for (i in 1:num_voters){
    best_overlap = order(all_voter_pol_overlap[i,], decreasing=TRUE) #create voting recommendation based Smartvote questionnaire
    panag_quest_index = best_overlap[1:num_panag_votes] #reduce the number of candidates acc to cantonal seats
    panag_voter_pol_dist = abs(votes_matrix[,panag_quest_index]- voters_matrix[,i])/2 #calculate pol overlap
    panag_overlap[i,] = 1-(colSums(panag_voter_pol_dist)/num_bus)
  }
  
  overlap_panag_per_voter = rowSums(panag_overlap)/num_panag_votes #Overall score per simulated voter
  
  return(overlap_panag_per_voter)
  
}




#calculates party mean per questions based on national councillors parliamentary voting data 
calc_party_average = function(num_bus, indices, councillor_votes, party_names){

  #initialize matrix
  Party_Agg = matrix(0,nrow=num_bus,ncol=length(indices))
  
  #party mean per questions based on national councillors parliamentary voting data 
  for (i in 1:length(indices)){
    Party_Agg[,i] = rowSums(councillor_votes[,indices[[i]]]) / length(indices[[i]])
  }
  
  colnames(Party_Agg) = party_names
  
  return(Party_Agg)
  
}



#calculates overlap of each voter with each party
calc_party_distance = function(num_voters, party_names, extr_rand_voters, Party_Agg){
  
  #initialize matrix
  distance_party = matrix(0,nrow=num_voters,ncol=length(party_names))
  
  #calculates overlap of each voter with each party
  for (i in 1:num_voters){
    distances = extr_rand_voters[,i]-Party_Agg
    distances = 1-(abs(distances)/2)
    for(j in 1:ncol(distances)){
      distance_party[i,j] = mean(distances[,j])
    }
  }
  
  colnames(distance_party) = party_names
  
  return(distance_party)
  
}


