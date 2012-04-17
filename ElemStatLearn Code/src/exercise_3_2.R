library('ProjectTemplate')
load.project()

# Raw data
simulation.xs <- c(1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969)
simulation.ys <- c(4835, 4970, 5085, 5160, 5310, 5260, 5235, 5255, 5235, 5210, 5175)
simulation.df <- data.frame(pop = simulation.ys, year = simulation.xs)

# Rescale years
simulation.df$year = simulation.df$year - 1964

# Generate regression, construct confidence intervals
fit <- lm(pop ~ year + I(year^2) + I(year^3), data=simulation.df)
xs = seq(-5, 5, 0.1)
fit.confidence = predict(fit, data.frame(year=xs), interval="confidence", level=0.95)


# Create data frame containing variables of interest
df = as.data.frame(fit.confidence)
df$year <- xs
df = melt(df, id.vars="year")

p <- ggplot() + geom_line(aes(x=year, y=value, colour=variable), df) + 
                geom_point(aes(x=year, y=pop), simulation.df)
p <- p + scale_x_continuous('Year') + scale_y_continuous('Population')
p <- p + opts(title="Cubic regression with confidence intervals")
p <- p + scale_color_brewer(name="Legend",
                            labels=c("Fit", 
                                     "95% Lower Bound", 
                                     "95% Upper Bound"), 
                            palette="Set1")
ggsave(file.path('graphs', 'exercise_3_2.pdf'))
ggsave(file.path('graphs', 'exercise_3_2.png'))
