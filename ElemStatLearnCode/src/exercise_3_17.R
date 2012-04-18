library("ProjectTemplate")
load.project()

library("lars") # For least-angle and lasso
library("MASS") # For ridge
library("pls") # For PLS and PCR

mod.ls <- lm(Y ~ . - 1, spam.train)
mod.ridge <- lm.ridge(Y ~ ., spam.train)
mod.pcr <- pcr(formula=Y ~ ., data=spam.train, validation="CV")
mod.plsr <- plsr(formula=Y ~ ., data=spam.train, validation="CV")
mod.lars <- lars(as.matrix(spam.train[,1:ncol(spam.train) - 1]), 
                 spam.train[,ncol(spam.train)], 
                 type="lar")
mod.lasso <- lars(as.matrix(spam.train[,1:ncol(spam.train) - 1]), 
                 spam.train[,ncol(spam.train)], 
                 type="lasso")

mods.coeffs <- data.frame(ls=mod.ls$coef,
                          ridge=mod.ridge$coef,
                          lasso=mod.lasso$beta[10,],
                          pcr=mod.pcr$coef[,,10],
                          plsr=mod.plsr$coef[,,10]
                          )

mods.coeffs$xs = row.names(mods.coeffs)
plot.data <- melt(mods.coeffs, id="xs")

ggplot(data=plot.data, 
       aes(x=factor(xs), 
           y=value, 
           group=variable, 
           colour=variable)) + 
        geom_line() + 
        geom_point() +
        xlab("Factor") + 
        ylab("Regression Coefficient") +
        opts(title = "Estimated coefficients for regression methods on spam data",
             axis.ticks = theme_blank(), 
             axis.text.x = theme_blank()) +
        scale_colour_hue(name="Regression Method",
                         labels=c("OLS",
                                  "Ridge",
                                  "Lasso",
                                  "PCR",
                                  "PLS")
                         )

ggsave(file.path('graphs', 'exercise_3_17.pdf'))
ggsave(file.path('graphs', 'exercise_3_17.png'))