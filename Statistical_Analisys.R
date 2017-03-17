#Calcule percentage for each label/value
#     feature - whatever feature from a dataset, specifically categoric features
ClassPercentages = function(feature){
  labels = unique(feature)
  cat('Percentages from labels:', labels, '\n')
  sapply(labels, function(x) sum( feature == x ) / length(feature))
}

#Example
ClassPercentages(math.tra$PV1MATH)    #percentage labels from class column = PV1MATH and dataset = math.tra

