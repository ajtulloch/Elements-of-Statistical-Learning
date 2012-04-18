library('ProjectTemplate')
load.project()

## Linear Regression
mod <- lm(Y ~ ., data = zip.train.filtered)

# Round predictions
category_f <- function(x) { if (x > 2.5) 3 else 2 }
predictions.lm.test <- as.character(sapply(predict(mod, zip.test.filtered), 
                                           category_f))
predictions.lm.train <- as.character(sapply(predict(mod, zip.train.filtered), 
                                            category_f))

## KNN
knn.train <- zip.train.filtered[, 2:257]
knn.test <- zip.test.filtered[, 2:257]

knn.train.Y <- as.factor(zip.train.filtered$Y)
knn.test.Y <- as.factor(zip.test.filtered$Y)

# KNN Predictions
predictions.knn.test <- sapply(1:15, function(k) { 
    knn(train = knn.train, 
        test = knn.test, 
        cl = knn.train.Y, 
        k = k) 
  })
predictions.knn.train <- sapply(1:15, function(k) { 
  knn(train = knn.train, 
      test = knn.train, 
      cl = knn.train.Y, 
      k = k) 
})

# Compute error rates
errors.xs <- 1:15

errors.knn.test <- apply(predictions.knn.test, 2, function(prediction) {
    classError(prediction, as.factor(zip.test.filtered$Y))$errorRate
  })
errors.knn.train <- apply(predictions.knn.train, 2, function(prediction) {
  classError(prediction, as.factor(zip.train.filtered$Y))$errorRate
})
errors.lm.test <- sapply(errors.xs, function(k) {
    classError(predictions.lm.test, as.factor(zip.test.filtered$Y))$errorRate
  })
errors.lm.train <- sapply(errors.xs, function(k) {
  classError(predictions.lm.train, as.factor(zip.train.filtered$Y))$errorRate
})

errors <- data.frame("K"=errors.xs, 
                     "KNN.Train"=errors.knn.train, 
                     "KNN.Test"=errors.knn.test,
                     "LR.Train"=errors.lm.train,
                     "LR.Test"=errors.lm.test)

# Create Plot
plot.data <- melt(errors, id="K") 
ggplot(data=plot.data,
       aes(x=K, y=value, colour=variable)) +
      geom_line() +
      xlab("k") + 
      ylab("Classification Error") +
      opts(title="Classification Errors for different methods on zipcode data")
      scale_colour_hue(name="Classification Method",
                       labels=c("k-NN (Train)",
                                "k-NN (Test)",
                                "Linear Regression (Train)",
                                "Linear Regression (Test)")
                        )
ggsave(file.path('graphs', 'exercise_2_8.pdf'))
ggsave(file.path('graphs', 'exercise_2_8.png'))