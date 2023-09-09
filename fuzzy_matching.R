###############################################################  
### Fuzzy name matching algorithm
###############################################################  

# R version adapted from Darius Mehri's python name matching algorithm:
# https://github.com/dariusmehri/Name-Matching-Algorithm-using-Fuzzy-Logic

###############################################################  
# nameList -- list of names to find matches for
# nl -- list of names to search for matches in
# threshold -- name matching threshold (0-1)
# one.top.match -- keep only the top name match (T) or all name matches above threshold (F)
###############################################################  
library(fuzzywuzzyR)
fuzzy_name <- function(nameList, nl, threshold, one.top.match){
  
  matched_list <- matrix(0, nrow=length(nameList))
  
  for (j in (1:length(nameList))) {

    ratio_list <- c()
    for (i in (1:length(nl)) ) {
      #calculates a matching score
      init = SequenceMatcher$new(string1 = nameList[j], string2 = nl[i])
      ratio = init$ratio()
      
      name <- nl[i]
      #add the name to ration_list and its score
      ratio_list <- c(ratio_list, list(c(name, ratio)))
      
      if (i == (length(nl)-1)) {
        #sort score from highest to lowest
        ind <- with(ratio_list, order(sapply(ratio_list, "[[", 2), decreasing = T))
        ratio_list = ratio_list[ind]
      }
      # only keep match names of a certain threshold
      if (ratio_list[[1]][2] >= threshold) {
        if (one.top.match) {
          # chooses one top match
          matched_list[j,] <- ratio_list[[1]][1]
        }
        # all name matches that are above the threshold
        else {
          top_matches <- ratio_list[sapply(ratio_list, "[[", 2) >= threshold]
          ml <- sapply(top_matches, "[[", 1)
          matched_list[j,] <- paste(unique(ml), collapse = '; ')
        }
      }
    }
    # print(paste0("nameList: ", nameList[j]))
    # print(paste0("matched_list: ", matched_list[j]))
  }
  output <- as.data.frame(cbind(nameList, matched_list))
  return(output)
}
