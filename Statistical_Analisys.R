#Calcule percentage for each label/value
#     feature - whatever feature from a dataset, specifically categoric or few levels features
ClassPercentages = function(feature){
  labels = unique(feature)
  cat('Percentages from labels:', labels, '\n')
  sapply(labels, function(x) sum( feature == x ) / length(feature))
}

#Example
ClassPercentages(math.tra$PV1MATH)    #percentage labels from class column = PV1MATH and dataset = math.tra
apply(math.tra, MARGIN=2, ClassPercentages)     #apply function for each column


#Calcule number of NAs (missing values) to each feature
#     dataset - the full dataset
#     return a number list of NAs with the same features order in the dataset
numNA = function(dataset){
  cat('Number of NAs by feature')
  sapply(1:dim(dataset)[2], function(x){
    cat(labels(dataset)[[2]][x], sum(which(is.na(dataset[,x]))), '\n')
    return( sum(which(is.na(dataset[,x]))) )
  } )
}

#Example
numNA(math.tra)
