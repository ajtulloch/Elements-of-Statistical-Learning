# Spam preprocessing 

spam <- as.data.frame(read.table(file="data/spam.wsv", header=FALSE))
colnames(spam) <- c(paste("X.",1:57,sep=""), "Y")


spam.normalized.y = scale(spam[58])
spam.normalized.x = scale(spam[1:57])

spam <- data.frame(spam.normalized.x, spam.normalized.y)
# Convert to factors
spam$Y = factor(spam$Y, labels=c("nospam", "spam"))
spam.sub = c(1:nrow(spam))[spam$Y == "spam"]
nospam.sub =   c(1:nrow(spam))[spam$Y == "nospam"]

# Convert back to numeric
spam$Y = as.numeric(spam$Y)

# Generate Training and test sets
train.spam = sample(spam.sub,floor(length(spam.sub)*2/3))
train.nospam = sample(nospam.sub,floor(length(nospam.sub)*2/3))
spam.training_indices = c(train.spam,train.nospam)

spam.train = spam[spam.training_indices,]
spam.test = spam[-spam.training_indices,]
