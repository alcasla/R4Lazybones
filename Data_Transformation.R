# Tranform a categoric feature to binary features, for each label generate a new binary feature
#   Params:   feature - categoric feature with factors
#   Return:   data.frame with column number equal to 'feature' levels number
categoric2binary = function(feature){
  labls= levels(feature)    #get labels
  output = data.frame(matrix(nrow=length(feature), ncol=length(labls)))   #prealocate output structure
  
  output = sapply(1:length(labls), function(lbl){ generateBinByLabel(feature, labls[lbl]) })
  output = as.data.frame(output)
  
  return(output)
}

# Generate a binary column called as 'label' and the cases which 'label' is in 'data'
#   Params:   data - categoric feature with factors
#             label - one of the 'data' labels
#   Return:   list contains 1 if 'data' equal to 'label' or 0 otherwise, for each row
generateBinByLabel = function(data, label){
  binarized = data.frame(matrix(nrow=length(data), ncol=1))
  colnames(binarized) = label
  
  binarized[which(data==label),] = 1
  binarized[which(data!=label),] = 0
  
  return(binarized)
}

#Splot categoric features with low number of levels, I would use this only for features with less tahn 10 labels
categoric2binary(training$ISLA)
