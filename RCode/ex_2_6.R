# Load training data
zip.train <- as.data.frame(read.table(file="zip.train", header=FALSE))
colnames(zip.train) <- c("Y",paste("X.",1:256,sep=""))
zip.train.filtered <- subset(zip.train, Y == 2 | Y == 3)
# Create linear regression
mod <- lm(Y ~ ., data = zip.train.filtered)

# Load testing data
zip.test <- as.data.frame(read.table(file="zip.test", header=FALSE))
colnames(zip.test) <- c("Y",paste("X.",1:256,sep=""))
zip.test.filtered <- subset(zip.test, Y == 2 | Y == 3)
# Predict categories
zip.test.filtered$Ypred <- predict(mod, zip.test.filtered)

category_f = function(x) {
if (x > 2.5) 3 else 2
}
# Round predictions
zip.test.filtered$Yround = sapply(zip.test.filtered$Ypred, category_f)

##### KNN
knn.test.data <- subset(zip.test, Y == 2 | Y == 3)
knn.train.data <- subset(zip.train, Y == 2 | Y == 3)
knn.train.data$Y <- as.factor(knn.train.data$Y)

knn.results = sapply(1:15, function(k) { knn(train=knn.train.data, test=knn.test.data, knn.train.data$Y, k = k) })
install.packages("mclust")
errors = sapply(knn.results, function(classification) { classError(knn.test.data$Y, classification)$errorRate})
