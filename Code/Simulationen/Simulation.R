#############################################################################################################
# Voting Simulation
#############################################################################################################

#load library
library(ggplot2)
library(gridExtra)

#load functions
source("C:\\Users\\Marco\\Desktop\\Master\\Master\\Master Thesis\\Master Thesis\\Code\\Auswertung\\Helper Functions.R")

#setwd
setwd("C:\\Users\\Marco\\Desktop\\Master\\Master\\Master Thesis\\Master Thesis\\Data")



############################################################
# Define Parameters 
############################################################

#import data on councillors party affeliation
party_aff = read.csv2(file = "Party Affeliation\\Party Affeliation.csv", stringsAsFactors = FALSE)

#import data on parliamentary businesses
matters_catalogue = load_matters()[[1]]

#create parameters for Simulation
num_pol = dim(party_aff)[1] #number of politicians
num_voters = c(16000) #c(100, 500, 1000, 2000, 4000, 8000) 
num_panag_votes = c(2, 4, 8, 16, 35) #range of candidates in cantons
distributions = list(c(0.7,0.3)) #,c(0.5,0.5),c(0.3,0.7) #different probability distributions
num_bus = length(load_matters()[[2]]) #number of matters
neg_row_question = load_matters()[[3]] #inverse weighted matters
row_question = load_matters()[[4]] #rows of smartvote questionnnaire with match



############################################################
# load / generate data
############################################################

#load questionnaire data and parliamentary voting data of politicians
#Simulate voter data based on previosuly defined distributions
for (i in 1:length(num_voters)){
  for (j in 1:length(distributions)){
    
    #Real Data
    
    #load_councillor_votes()
    
    eval(parse(text=sprintf("quest_matrix_v%d_d%d = load_councillor_quest(neg_row_question, row_question)",
                            i, j))) #Smartvote Questionnaire Data
    eval(parse(text=sprintf("votes_matrix_v%d_d%d = load_councillor_votes(load_matters()[[2]])",
                            i, j))) #Parliamentary voting data
    eval(parse(text=sprintf("voters_matrix_v%d_d%d = generate_random_voters(num_bus, num_voters[i], distributions[[j]])",
                            i, j, j))) #voter data

    
  }
}

#Resulting Matrices:
#v1 means that the first element of the list num_voters was used for this matrix
#d1 means that the first element of the list distributions was used for this matrix



############################################################
# Voting Simulation
############################################################

#calculates political overlap between Politicians Smartvote Questionnaire and the simulated voters
#returns matrix of all politicians and their overlap with each simulated voter
for (i in 1:length(num_voters)){
  for (j in 1:length(distributions)){
    wow = sprintf("all_voter_pol_overlap_v%d_d%d = calc_quest_overlap(num_pol, num_bus, num_voters[%d], 
                  party_aff, quest_matrix_v%d_d%d, voters_matrix_v%d_d%d)", 
                  i,j,i,i,j,i,j)
    eval(parse(text=wow))
    
  }
}

# check = c()
# for (i in 1:num_voters){
#   check[i] = mean(sort(all_voter_pol_overlap_v1_d1[i,], decreasing = TRUE)[1:10])
# }
# 
# mean(check)

#uses Smartvote recommendation to create a voting recommendation. Then picks the recommended politicians and calculates the true overlap.
#returns overall true overlap per voter based on voting recommendation
for (k in 1:length(num_panag_votes)){
  for (i in 1:length(num_voters)){
    for (j in 1:length(distributions)){
      eval(parse(text=sprintf("overlap_panag_per_voter_v%d_d%d_panag%d = calc_panag_overlap(num_bus, 
                              num_voters[%d], num_panag_votes[%d], all_voter_pol_overlap_v%d_d%d, voters_matrix_v%d_d%d, 
                              votes_matrix_v%d_d%d)", i,j,k,i,k,i,j,i,j,i,j)))
    }
  }
}


# check = matrix(0, nrow=num_voters,ncol=35)
# 
# for (i in 1:num_voters){
#   best_overlap = order(all_voter_pol_overlap_v1_d1[i,], decreasing=TRUE) #create voting recommendation based Smartvote questionnaire
#   panag_quest_index = best_overlap[1:35] #reduce the number of candidates acc to cantonal seats
#   panag_voter_pol_dist = abs(votes_matrix_v1_d1[,panag_quest_index]- voters_matrix_v1_d1[,i])/2 #calculate pol overlap
#   check[i,] = 1-(colSums(panag_voter_pol_dist)/num_bus)
# }
# mean(check)



#intitialize matrix
results = matrix(0,nrow=3 ,ncol=(length(distributions)*length(num_voters)*length(num_panag_votes)))
names = c()
counter=0

#calculate summarizing statistics mean, median and sd for the true overlap per voter based on voting recommendation
for (k in 1:length(num_panag_votes)){
  for (i in 1:length(num_voters)){
    for (j in 1:length(distributions)){
      counter = counter+1
      eval(parse(text=sprintf("results[1,counter] = mean(overlap_panag_per_voter_v%d_d%d_panag%d)",i,j,k)))
      eval(parse(text=sprintf("results[2,counter] = median(overlap_panag_per_voter_v%d_d%d_panag%d)",i,j,k)))
      eval(parse(text=sprintf("results[3,counter] = sd(overlap_panag_per_voter_v%d_d%d_panag%d)",i,j,k)))
      names[counter]=sprintf("overlap_panag_per_voter_v%d_d%d_panag%d",i,j,k)
      
    }
  }
}

#label matrix
row_names = c("mean_p","median_p","sd_p")
rownames(results) = row_names
colnames(results) = names

#transpose results to make more readable
results=as.data.frame(t(results))

#plot means
ggplot(results,aes(y=mean_p,x=1:length(names)))+
  geom_point(shape=1,size=5)+
  ylab("Means of the  Panagiert Results") +
  xlab("Increasing Complexity") +
  ggtitle("This plots all results from lowest to highest Complexity")

#plot sd
ggplot(results,aes(y=sd_p,x=1:length(names)))+
  geom_point(shape=1,size=5)+
  ylab("Standard Deviation of the  Panagiert Results") +
  xlab("Increasing Complexity") +
  ggtitle("This plots all results from lowest to highest Complexity")


############################################################
# Stability
############################################################

#results from counter start_to counter end belong to the same group
counter_start = 1
counter_end = dim(results)[1]/length(num_panag_votes)

for (i in 1:length(num_panag_votes)){
  
  #extract standard deviations per group from results matrix
  eval(parse(text=sprintf("res_pan%d = results[counter_start:counter_end,3]",i,i)))
  
  #increase range for next group
  counter_start = counter_start + dim(results)[1]/length(num_panag_votes)
  counter_end = counter_end + dim(results)[1]/length(num_panag_votes)
}

#initialize list
std_within_panach = list()

#calculate the mean std per group
for (i in 1:length(num_panag_votes)){
  std_within_panach[i] = mean(eval(parse(text=sprintf("res_pan%d",i))))
}

#print results per group starting with the smallest "Canton"
print(std_within_panach)



############################################################
# Base Case
############################################################

#create index of politicians per party
index_SVP = which(party_aff$Fraktion=="SVP")
index_SP = which(party_aff$Fraktion=="SP")
index_FDP = which(party_aff$Fraktion=="FDP")
index_CVP = which(party_aff$Fraktion=="CVP")
index_G = which(party_aff$Fraktion=="G")

indices = list(index_SVP, index_SP, index_FDP, index_CVP, index_G)
party_names = c("SVP", "SP", "FDP", "CVP", "G")

#calculates party mean per questions based on national councillors parliamentary voting data 
for (i in 1:length(num_voters)){
  for (j in 1:length(distributions)){
    eval(parse(text=sprintf("party_average_v%d_d%d = calc_party_average(num_bus, indices, votes_matrix_v%d_d%d, party_names)", 
                  i,j,i,j)))
  }
}


#grid.table(round(party_average_v1_d1,2))


#calculates overlap of each voter with each party
for (i in 1:length(num_voters)){
  for (j in 1:length(distributions)){
    eval(parse(text=sprintf("party_distances_v%d_d%d = calc_party_distance(num_voters[%d], party_names, voters_matrix_v%d_d%d, party_average_v%d_d%d)",
                            i,j,i,i,j,i,j)))
  }
}

#initialize matrix
results_list = matrix(0,nrow=length(num_voters)*length(distributions),ncol=2)
names = c()


#match voter with the party they have the biggest overlap with
#return the mean overlap through list voting
counter=0
for (i in 1:length(num_voters)){
  for (j in 1:length(distributions)){
    counter=counter+1
    overlaps = c()
    
    for (g in 1:num_voters[i]){
      overlaps[g] = eval(parse(text=sprintf("max(party_distances_v%d_d%d[g,])",i,j))) 
    }
    
    names[counter]=sprintf("overlap_panag_per_voter_v%d_d%d",i,j)
    
    results_list[counter,1] = mean(overlaps)
    results_list[counter,2] = sd(overlaps)
  }
}

#label matrix
row_names = c("mean_l","sd_l")
colnames(results_list) = row_names
rownames(results_list) = names

#plot results
ggplot(as.data.frame(results_list),aes(y=mean_l,x=1:length(names)))+
  geom_point(shape=1,size=5)+
  ylab("Means of the list Results") +
  xlab("Increasing Complexity") +
  ggtitle("This plots all results from lowest to highest Complexity")

#clean environment
#rm(list=setdiff(ls(), c("results_list","results")))

#add additional columns to results matrix 
results["mean_l"] <- 0
results["sd_l"] <- 0

#merge results_list into results matrix
for (i in 1:dim(results_list)[1]){
  results[seq(i,dim(results)[1],dim(results_list)[1]),4] <- results_list[i,1]
}

for (i in 1:dim(results_list)[1]){
  results[seq(i,dim(results)[1],dim(results_list)[1]),5] <- results_list[i,2]
}

#round results to make them more comparable
results = round(results,2)

#png(filename = "output.png", width=600,height=900,bg = "white")
#grid.table(results)
#dev.off()


############################################################
# Optimal Result (Assumption - Smartvote is 100% accurate)
############################################################

for (i in 1:length(num_voters)){
  for (j in 1:length(distributions)){
    wow = sprintf("all_voter_pol_overlap_v%d_d%d = calc_quest_overlap(num_pol, num_bus, num_voters[%d], 
                  party_aff, votes_matrix_v%d_d%d, voters_matrix_v%d_d%d)", 
                  i,j,i,i,j,i,j)
    eval(parse(text=wow))
    
  }
}


for (k in 1:length(num_panag_votes)){
  for (i in 1:length(num_voters)){
    for (j in 1:length(distributions)){
      eval(parse(text=sprintf("overlap_panag_per_voter_v%d_d%d_panag%d = calc_panag_overlap(num_bus, 
                              num_voters[%d], num_panag_votes[%d], all_voter_pol_overlap_v%d_d%d, voters_matrix_v%d_d%d, 
                              votes_matrix_v%d_d%d)", i,j,k,i,k,i,j,i,j,i,j)))
    }
  }
}

check = matrix(0, nrow=num_voters,ncol=35)

for (i in 1:num_voters){
  best_overlap = order(all_voter_pol_overlap_v1_d1[i,], decreasing=TRUE) #create voting recommendation based Smartvote questionnaire
  panag_quest_index = best_overlap[1:35] #reduce the number of candidates acc to cantonal seats
  panag_voter_pol_dist = abs(votes_matrix_v1_d1[,panag_quest_index]- voters_matrix_v1_d1[,i])/2 #calculate pol overlap
  check[i,] = 1-(colSums(panag_voter_pol_dist)/num_bus)
}
mean(check)
