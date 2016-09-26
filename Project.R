#library(VIM)

GDELT <- read.csv("GDELT.csv")
summary(GDELT)
#aggr(GDELT)

do.preparation <- function(dataset, eventcode){
  
  Eventsubset <- dataset[dataset$EventCode == eventcode,]
  #Event Count
  time <- as.data.frame((table(dataset$MonthYear)))
  colnames(time) <- c("MonthYear","TotalFreq")
  Freq <- as.data.frame((table(Eventsubset$MonthYear)))
  colnames(Freq) <- c("MonthYear","Count")
  Event <- merge(time, Freq, by = "MonthYear", all.x=TRUE)
  Event[is.na(Event)] <- 0
  Event$Percent <- Event$Count/Event$TotalFreq
  
  # AvgTone
  AvgSum <- aggregate(Eventsubset$AvgTone, list(Eventsubset$MonthYear) ,sum)
  colnames(AvgSum) <- c("MonthYear","AvgSum")
  Event <- merge(Event,AvgSum, by = "MonthYear", all.x=TRUE)
  AvgMean <- aggregate(Eventsubset$AvgTone, list(Eventsubset$MonthYear) ,mean)
  colnames(AvgMean) <- c("MonthYear","AvgMean")
  Event <- merge(Event,AvgMean,  by = "MonthYear", all.x=TRUE)
  AvgSD <- aggregate(Eventsubset$AvgTone, list(Eventsubset$MonthYear) ,sd)
  colnames(AvgSD) <- c("MonthYear","AvgSD")
  Event <- merge(Event,AvgSD,  by = "MonthYear", all.x=TRUE)
  Event <- Event[,c(1,3,4,5,6,7)]
  # IsRootEvent
  isRoot <- aggregate(Eventsubset$IsRootEvent, list(Eventsubset$MonthYear) ,sum)
  colnames(isRoot) <- c("MonthYear","isRootSum")
  Event <- merge(Event,isRoot,  by = "MonthYear", all.x=TRUE)
  # NumMentions
  MentionSum <- aggregate(Eventsubset$NumMentions, list(Eventsubset$MonthYear) ,sum)
  colnames(MentionSum) <- c("MonthYear","MentionSum")
  Event <- merge(Event,MentionSum, by = "MonthYear", all.x=TRUE)
  MentionMean <- aggregate(Eventsubset$NumMentions, list(Eventsubset$MonthYear) ,mean)
  colnames(MentionMean) <- c("MonthYear","MentionMean")
  Event <- merge(Event,MentionMean,  by = "MonthYear", all.x=TRUE)
  MentionSD <- aggregate(Eventsubset$NumMentions, list(Eventsubset$MonthYear) ,sd)
  colnames(MentionSD) <- c("MonthYear","MentionSD")
  Event <- merge(Event,MentionSD,  by = "MonthYear", all.x=TRUE)
  # NumSource
  SourceSum <- aggregate(Eventsubset$NumSources, list(Eventsubset$MonthYear) ,sum)
  colnames(SourceSum) <- c("MonthYear","SourceSum")
  Event <- merge(Event,SourceSum, by = "MonthYear", all.x=TRUE)
  SourceMean <- aggregate(Eventsubset$NumSources, list(Eventsubset$MonthYear) ,mean)
  colnames(SourceMean) <- c("MonthYear","SourceMean")
  Event <- merge(Event,SourceMean,  by = "MonthYear", all.x=TRUE)
  SourceSD <- aggregate(Eventsubset$NumSources, list(Eventsubset$MonthYear) ,sd)
  colnames(SourceSD) <- c("MonthYear","SourceSD")
  Event <- merge(Event,SourceSD,  by = "MonthYear", all.x=TRUE)
  # NumArticles
  ArticleSum <- aggregate(Eventsubset$NumArticles, list(Eventsubset$MonthYear) ,sum)
  colnames(ArticleSum) <- c("MonthYear","ArticleSum")
  Event <- merge(Event,ArticleSum, by = "MonthYear", all.x=TRUE)
  ArticleMean <- aggregate(Eventsubset$NumArticles, list(Eventsubset$MonthYear) ,mean)
  colnames(ArticleMean) <- c("MonthYear","ArticleMean")
  Event <- merge(Event,ArticleMean,  by = "MonthYear", all.x=TRUE)
  ArticleSD <- aggregate(Eventsubset$NumArticles, list(Eventsubset$MonthYear) ,sd)
  colnames(ArticleSD) <- c("MonthYear","ArticleSD")
  Event <- merge(Event,ArticleSD,  by = "MonthYear", all.x=TRUE)
  # Set months without evnets as 0
  Event[is.na(Event)]<-0
  
  return(Event)
}
EC0211 <- do.preparation(GDELT, 211)
colnames(EC0211) <- c("MonthYear","0211Count","0211Percent","0211AvgSum","0211AvgMean","0211AvgSD","0211IsRootCount","0211MentionsSum","0211MentionsMean",
                      "0211MentionsSD","0211SourcesSum","0211SourcesMean","0211SourcesSD","0211ArticlesSum","0211ArticlesMean","0211ArticlesSD")
EC0231 <- do.preparation(GDELT, 231)
colnames(EC0231) <- c("MonthYear","0231Count","0231Percent","0231AvgSum","0231AvgMean","0231AvgSD","0231IsRootCount","0231MentionsSum","0231MentionsMean",
                      "0231MentionsSD","0231SourcesSum","0231SourcesMean","0231SourcesSD","0231ArticlesSum","0231ArticlesMean","0231ArticlesSD")
dataset <- merge(EC0211,EC0231,  by = "MonthYear")
EC0254 <- do.preparation(GDELT, 254)
colnames(EC0254) <- c("MonthYear","0254Count","0254Percent","0254AvgSum","0254AvgMean","0254AvgSD","0254IsRootCount","0254MentionsSum","0254MentionsMean",
                      "0254MentionsSD","0254SourcesSum","0254SourcesMean","0254SourcesSD","0254ArticlesSum","0254ArticlesMean","0254ArticlesSD")
dataset <- merge(dataset,EC0254,  by = "MonthYear")
EC0311 <- do.preparation(GDELT, 311)
colnames(EC0311) <- c("MonthYear","0311Count","0311Percent","0311AvgSum","0311AvgMean","0311AvgSD","0311IsRootCount","0311MentionsSum","0311MentionsMean",
                      "0311MentionsSD","0311SourcesSum","0311SourcesMean","0311SourcesSD","0311ArticlesSum","0311ArticlesMean","0311ArticlesSD")
dataset <- merge(dataset,EC0311,  by = "MonthYear")
EC0331 <- do.preparation(GDELT, 331)
colnames(EC0331) <- c("MonthYear","0331Count","0331Percent","0331AvgSum","0331AvgMean","0331AvgSD","0331IsRootCount","0331MentionsSum","0331MentionsMean",
                      "0331MentionsSD","0331SourcesSum","0331SourcesMean","0331SourcesSD","0331ArticlesSum","0331ArticlesMean","0331ArticlesSD")
dataset <- merge(dataset,EC0331,  by = "MonthYear")
EC061 <- do.preparation(GDELT, 61)
colnames(EC061) <- c("MonthYear","061Count","064Percent","061AvgSum","061AvgMean","061AvgSD","061IsRootCount","061MentionsSum","061MentionsMean",
                      "061MentionsSD","061SourcesSum","061SourcesMean","061SourcesSD","061ArticlesSum","061ArticlesMean","061ArticlesSD")
dataset <- merge(dataset,EC061,  by = "MonthYear")
EC071 <- do.preparation(GDELT, 71)
colnames(EC071) <- c("MonthYear","071Count","071Percent","071AvgSum","071AvgMean","071AvgSD","071IsRootCount","071MentionsSum","071MentionsMean",
                      "071MentionsSD","071SourcesSum","071SourcesMean","071SourcesSD","071ArticlesSum","071ArticlesMean","071ArticlesSD")
dataset <- merge(dataset,EC071,  by = "MonthYear")
EC1031 <- do.preparation(GDELT, 1031)
colnames(EC1031) <- c("MonthYear","1031Count","1031Percent","1031AvgSum","1031AvgMean","1031AvgSD","1031IsRootCount","1031MentionsSum","1031MentionsMean",
                      "1031MentionsSD","1031SourcesSum","1031SourcesMean","1031SourcesSD","1031ArticlesSum","1031ArticlesMean","1031ArticlesSD")
dataset <- merge(dataset,EC1031,  by = "MonthYear")
EC1054 <- do.preparation(GDELT, 1054)
colnames(EC1054) <- c("MonthYear","1054Count","1054Percent","1054AvgSum","1054AvgMean","1054AvgSD","1054IsRootCount","1054MentionsSum","1054MentionsMean",
                      "1054MentionsSD","1054SourcesSum","1054SourcesMean","1054SourcesSD","1054ArticlesSum","1054ArticlesMean","1054ArticlesSD")
dataset <- merge(dataset,EC1054,  by = "MonthYear")
EC1211 <- do.preparation(GDELT, 1211)
colnames(EC1211) <- c("MonthYear","1211Count","1211Percent","1211AvgSum","1211AvgMean","1211AvgSD","1211IsRootCount","1211MentionsSum","1211MentionsMean",
                      "1211MentionsSD","1211SourcesSum","1211SourcesMean","1211SourcesSD","1211ArticlesSum","1211ArticlesMean","1211ArticlesSD")
EC1244 <- do.preparation(GDELT, 1244)
colnames(EC1244) <- c("MonthYear","1244Count","1244Percent","1244AvgSum","1244AvgMean","1244AvgSD","1244IsRootCount","1244MentionsSum","1244MentionsMean",
                      "1244MentionsSD","1244SourcesSum","1244SourcesMean","1244SourcesSD","1244ArticlesSum","1244ArticlesMean","1244ArticlesSD")
dataset <- merge(dataset,EC1244,  by = "MonthYear")
EC1621 <- do.preparation(GDELT, 1621)
colnames(EC1621) <- c("MonthYear","1621Count","1621Percent","1621AvgSum","1621AvgMean","1621AvgSD","1621IsRootCount","1621MentionsSum","1621MentionsMean",
                      "1621MentionsSD","1621SourcesSum","1621SourcesMean","1621SourcesSD","1621ArticlesSum","1621ArticlesMean","1621ArticlesSD")
dataset <- merge(dataset,EC1621,  by = "MonthYear")
