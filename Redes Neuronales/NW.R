library(caTools)
library(h2o)
library(ROCR)


dataset <- read.csv("../Data/pulsar_data_train.csv")
str(dataset)
colnames(dataset)[9] <- "TipoEstrella"
dataset$TipoEstrella <- factor(dataset$TipoEstrella, levels = c("0", "1"), labels = c("NoPulsar", "Pulsar"))
summary(dataset)
dataset[, c(1:8)] <- scale(dataset[, c(1:8)])
summary(dataset)

set.seed(1234)
split <- sample.split(dataset$TipoEstrella, SplitRatio = 0.80)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

table(training_set$TipoEstrella)
table(test_set$TipoEstrella)

h2o.init(nthreads = -1)

classifier = h2o.deeplearning(y = 'TipoEstrella',
                              training_frame = as.h2o(training_set),
                              activation = 'Rectifier',
                              hidden = c(5, 5),
                              epochs = 100,
                              train_samples_per_iteration = -2)

prob_pred <- h2o.predict(classifier, newdata = as.h2o(test_set))

y_pred <- as.vector(ifelse(prob_pred$predict == 'NoPulsar', 0, 1))

y_test_set <- ifelse(test_set$TipoEstrella == 'NoPulsar', 0, 1)

cm <- table(y_test_set, y_pred)

cm


pred1 <- prediction(as.numeric(y_pred), as.numeric(y_test_set))

perf1 <- performance(pred1, "tpr", "fpr")

plot(perf1)

h2o.shutdown()

