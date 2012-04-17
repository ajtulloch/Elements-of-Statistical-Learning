library('ProjectTemplate')
load.project()


# Create linear regression
mod <- lm(Y ~ ., data = zip.train.filtered)

category_f <- function(x) {
  if (x > 2.5) 3 else 2
}

# Round predictions
predictions.lm <- as.character(sapply(predict(mod, zip.test.filtered), category_f))

##### KNN
knn.train <- zip.train.filtered[, 2:257]
knn.test <- zip.test.filtered[, 2:257]

knn.train.Y <- as.factor(zip.train.filtered$Y)
knn.test.Y <- as.factor(zip.test.filtered$Y)

predictions.knn <- sapply(1:15, function(k) { 
    knn(train = knn.train, 
        test = knn.test, 
        cl = knn.train.Y, 
        k = k) 
  })

# values of K for KNN classification
errors.xs <- 1:15

errors.knn <- apply(predictions.knn, 2, function(prediction) {
    classError(prediction, as.factor(zip.test.filtered$Y))$errorRate
  })
errors.lm <- sapply(errors.xs, function(k) {
    classError(predictions.lm, as.factor(zip.test.filtered$Y))$errorRate
  })

errors <- data.frame("K"=errors.xs, "KNN"=errors.knn, "LR"=errors.lm)
plot.data <- melt(errors, id="K") 
plot.p <- ggplot(data=plot.data,
              aes(x=K, y=value, colour=variable)) +
            geom_line() +
            xlab("k") + 
            ylab("Classification Error") +
            scale_colour_hue(name="Classification Method",
                             labels=c("k-NN", "Linear Regression")
                              )
ggsave(file.path('graphs', 'exercise_2_8.pdf'))
ggsave(file.path('graphs', 'exercise_2_8.png'))
