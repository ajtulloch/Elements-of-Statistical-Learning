# Example preprocessing script.
# Load training and test data
zip.test <- as.data.frame(read.table(file="data/zip.test.wsv", header=FALSE))
zip.train <- as.data.frame(read.table(file="data/zip.train.wsv", header=FALSE))

colnames(zip.train) <- c("Y",paste("X.",1:256,sep=""))
zip.train.filtered <- subset(zip.train, Y == 2 | Y == 3)

colnames(zip.test) <- c("Y",paste("X.",1:256,sep=""))
zip.test.filtered <- subset(zip.test, Y == 2 | Y == 3)

cache("zip.test.filtered")
cache("zip.train.filtered")