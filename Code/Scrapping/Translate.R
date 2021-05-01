############################################################
# Scrap IDs from Smartvote
############################################################

#setwd
setwd("C:\\Users\\Ueli\\Desktop\\Master Thesis\\Data and Code\\Smartvote")

#filenames of CSVs
candidate_names = list.files(path = "Candidates\\Candidates 1", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

for(i in 1:length(candidate_names)){

  candidate_votes = read.csv2(file = paste0("Candidates\\Candidates 1\\", candidate_names[i]))
  candidate_votes = candidate_votes[,2:5]
  
  candidates_binary = matrix(0, nrow = dim(candidate_votes)[1], ncol = 2)
  colnames(candidates_binary) = c("yes","no")
  
  for (j in 1:nrow(candidate_votes)){
    
    if(candidate_votes[j,1] == 1 || candidate_votes[j,2] == 1){
      candidates_binary[j,1] = 1
      }
    else{
      candidates_binary[j,2] = 1
      }
  }
  write.csv2(candidates_binary, file = paste0( "Adjusted\\Q1\\", candidate_names[i]) )
}


