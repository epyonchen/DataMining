library(RSNNS)
library(reshape)
library(DAAG)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

dataset2 <- read.csv("dataset.csv")
dataset2 = dataset2[,-(1:2)]
dataset = dataset2[sample(1:nrow(dataset2),length(1:nrow(dataset2))),1:ncol(dataset2)]
#dataset = dataset2
Value <- dataset[,-dataset$result]
Target <- decodeClassLabels(dataset$result)
data = splitForTrainingAndTest(Value, Target, ratio=0.1)
data = normTrainingAndTestSet(data)
model = mlp(data$inputsTrain, data$targetsTrain, size=5, inputsTest=data$inputsTest, targetsTest=data$targetsTest,linOut=T) 

plotIterativeError(model) 

#par(mar=numeric(4),family='serif')
#plot.nnet(model)

predictions = predict(model,data$inputsTest)

result = confusionMatrix(data$targetsTest,predictions)
result[1,1]
