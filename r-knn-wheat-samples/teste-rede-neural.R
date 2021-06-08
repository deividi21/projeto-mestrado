library(tidyverse)
library(yardstick)
library(caTools)
library(caret)

library(tensorflow)
library(keras)
library(tfdatasets)

library(neuralnet)
library(randomForest)




####Importando Dados####
sample_21195 <- read_delim("dados/21195_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21196 <- read_delim("dados/21196_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21197 <- read_delim("dados/21197_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21198 <- read_delim("dados/21198_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21199 <- read_delim("dados/21199_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21200 <- read_delim("dados/21200_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21201 <- read_delim("dados/21201_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21202 <- read_delim("dados/21202_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21210 <- read_delim("dados/21210_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21227 <- read_delim("dados/21227_2.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_sadio <- read_delim("dados/sadio_2.csv",";", escape_double = FALSE, trim_ws = TRUE)

####Selecionando Colunas de Interesse####
sample_21195.subset <- sample_21195[20:37]
sample_21196.subset <- sample_21196[20:37]
sample_21197.subset <- sample_21197[20:37]
sample_21198.subset <- sample_21198[20:37]
sample_21199.subset <- sample_21199[20:37]
sample_21200.subset <- sample_21200[20:37]
sample_21201.subset <- sample_21201[20:37]
sample_21202.subset <- sample_21202[20:37]
sample_21210.subset <- sample_21210[20:37]
sample_21227.subset <- sample_21227[20:37]
sample_sadio.subset <- sample_sadio[20:37]

####Adicionando Identificadores####
sample_21195.subset <- mutate(sample_21195.subset, don = 1788)
sample_21195.subset <- mutate(sample_21195.subset, label = "21195")
sample_21195.subset <- tibble::rowid_to_column(sample_21195.subset, "id")
sample_21196.subset <- mutate(sample_21196.subset, don = 483)
sample_21196.subset <- mutate(sample_21196.subset, label = "21196")
sample_21196.subset <- tibble::rowid_to_column(sample_21196.subset, "id")
sample_21197.subset <- mutate(sample_21197.subset, don = 2113)
sample_21197.subset <- mutate(sample_21197.subset, label = "21197")
sample_21197.subset <- tibble::rowid_to_column(sample_21197.subset, "id")
sample_21198.subset <- mutate(sample_21198.subset, don = 1508)
sample_21198.subset <- mutate(sample_21198.subset, label = "21198")
sample_21198.subset <- tibble::rowid_to_column(sample_21198.subset, "id")
sample_21199.subset <- mutate(sample_21199.subset, don = 2009)
sample_21199.subset <- mutate(sample_21199.subset, label = "21199")
sample_21199.subset <- tibble::rowid_to_column(sample_21199.subset, "id")
sample_21200.subset <- mutate(sample_21200.subset, don = 1943)
sample_21200.subset <- mutate(sample_21200.subset, label = "21200")
sample_21200.subset <- tibble::rowid_to_column(sample_21200.subset, "id")
sample_21201.subset <- mutate(sample_21201.subset, don = 0)
sample_21201.subset <- mutate(sample_21201.subset, label = "21201")
sample_21201.subset <- tibble::rowid_to_column(sample_21201.subset, "id")
sample_21202.subset <- mutate(sample_21202.subset, don = 0)
sample_21202.subset <- mutate(sample_21202.subset, label = "21202")
sample_21202.subset <- tibble::rowid_to_column(sample_21202.subset, "id")
sample_21210.subset <- mutate(sample_21210.subset, don = 799)
sample_21210.subset <- mutate(sample_21210.subset, label = "21210")
sample_21210.subset <- tibble::rowid_to_column(sample_21210.subset, "id")
sample_21227.subset <- mutate(sample_21227.subset, don = 307)
sample_21227.subset <- mutate(sample_21227.subset, label = "21227")
sample_21227.subset <- tibble::rowid_to_column(sample_21227.subset, "id")
sample_sadio.subset <- mutate(sample_sadio.subset, don = 0)
sample_sadio.subset <- mutate(sample_sadio.subset, label = "sadio")
sample_sadio.subset <- tibble::rowid_to_column(sample_sadio.subset, "id")


####Unificando dados em um data frame####
wheat_dataset <- merge(sample_21195.subset,sample_21196.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_21197.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_21198.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_21199.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_21200.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_21201.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_21202.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_21210.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_21227.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_sadio.subset,all.x = TRUE, all.y = TRUE)

wheat_dataset <- wheat_dataset %>% rename(R = 'Cal. R (610nm)',
                                          S = 'Cal. S (680nm)',
                                          T = 'Cal. T (730nm)',
                                          U = 'Cal. U (760nm)',
                                          V = 'Cal. V (810nm)',
                                          W = 'Cal. W (860nm)',
                                          G = 'Cal. G (560nm)',
                                          H = 'Cal. H (585nm)',
                                          I = 'Cal. I (645nm)',
                                          J = 'Cal. J (705nm)',
                                          K = 'Cal. K (900nm)',
                                          L = 'Cal. L (940nm)',
                                          A = 'Cal. A (410nm)',
                                          B = 'Cal. B (435nm)',
                                          C = 'Cal. C (460nm)',
                                          D = 'Cal. D (485nm)',
                                          E = 'Cal. E (510nm)',
                                          F = 'Cal. F (535nm)')


####Rede Neural####

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

nn <- as.data.frame(lapply(wheat_dataset[2:20], min_max_norm))

nn <- as.data.frame(replace(nn, is.na(nn), 0))

nn <- mutate(nn,wheat_dataset$label)

nn.sample <- sample.split(nn$don, SplitRatio = .70)
nn.train <- as_tibble(subset(nn, nn.sample == TRUE) %>% select(-A,-L))
nn.test <- as_tibble(subset(nn, nn.sample == FALSE) %>% select(-A,-L))

write.csv(nn,"C:\\Users\\Deividi\\Documents\\dataset_normalizado.csv", row.names = TRUE)


spec <- feature_spec(nn.train, don ~ . ) %>% 
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()

spec


layer <- layer_dense_features(
  feature_columns = dense_features(spec), 
  dtype = tf$float32
)
layer(nn.train)



input <- layer_input_from_dataset(nn.train %>% select(-don))

output <- input %>% 
  layer_dense_features(dense_features(spec)) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax") %>%
  layer_dense(units = 1)

model <- keras_model(input, output)

summary(model)

model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )


build_model <- function() {
  input <- layer_input_from_dataset(nn.train %>% select(-don))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 10, activation = "softmax") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  model
}


# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

model <- build_model()

history <- model %>% fit(
  x = nn.train %>% select(-don),
  y = nn.train$don,
  epochs = 100,
  validation_split = 0.2,
  verbose = 0,
  batch_size=128,
  callbacks = list(print_dot_callback)
)


plot(history)


c(loss, mae) %<-% (model %>% evaluate(nn.test %>% select(-don), nn.test$don, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae * 1000))

test_predictions <- model %>% predict(nn.test %>% select(-don))


fr <- tibble(
  prediction = sapply(test_predictions[ , 1], round, digits=1), 
  actual = sapply(nn.test$don, round, digits=1), 
)

factor(fr$prediction)

l <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9,1.0)

confusionMatrix(factor(fr$prediction, levels = l), factor(fr$actual, levels = l), positive = NULL, dnn = c("Prediction", "Reference"))



confusionMatrix(factor(fr$prediction), factor(fr$actual), positive = NULL, dnn = c("Prediction", "Reference"))




time.start=proc.time()[[3]]

nn.model <- neuralnet(don~R+S+T+U+V+W+G+H+I+J+K+L+A+B+C+D+E+F,
                      data=nn.train,
                      hidden=c(5:1),
                      rep = 3,
                      stepmax=1e6)

time.end=proc.time()[[3]]
time.end-time.start

nn.model$result.matrix

plot(nn.model, rep = "best")

nn.results <- compute(nn.model, nn.test)

nn.final <- data.frame(actual = nn.test$don, prediction = nn.results$net.result)
nn.final

nn.final.rounded <- as.data.frame(sapply(nn.final, round, digits=1))

attach(nn.final.rounded)
table(actual,prediction)

####Randon Forest####

set.seed(8675309)

rf <- as.data.frame(scale(wheat_dataset[2:20]))

rf <- as.data.frame(replace(rf, is.na(rf), 0))

rf.sample <- sample.split(rf$don, SplitRatio = .90)
rf.train <- subset(rf, sample.rf == TRUE)
rf.test <- subset(rf, sample.rf == FALSE)


rf.model <- randomForest(don~R+S+T+U+V+W+G+H+I+J+K+L+A+B+C+D+E+F,
                         data = rf.train,
                         importance = TRUE,
                         proximity = TRUE,
                         keep.forest=TRUE)
print(rf.model)

y_hat <- predict(rf.model, rf.test)

test.rf_scored <- as_tibble(cbind(rf.test, y_hat))
glimpse(test.rf_scored)
RMSE_rf_TEST <- yardstick::rmse(test.rf_scored, truth=don, estimate=y_hat)
RMSE_rf_TEST

plot(rf.model)

####KNN####
set.seed(101)

knn <- wheat_dataset[2:20]

head(knn)

sample.knn <- sample.split(knn$don, SplitRatio = .70)
train.knn <- subset(knn, sample == TRUE)
test.knn <- subset(knn, sample == FALSE)

predicted.samples.knn <- knn(train.knn[1:18],test.knn[1:18],train.knn$don,k=11)
error.df.knn <- mean(test.knn$don != predicted.samples.knn)
error.df.knn
table(predicted.samples.knn,test.knn$don)

#Teste para k de 1 a 30
media <- c(1:30)

for (i in 1:30) {
  predicted.samples.knn <- knn(train.knn[1:18],test.knn[1:18],train.knn$don,k=i)
  media[i] <- mean(test.knn$don != predicted.samples.knn)
}

k.values <- 1:30

error.df <- data.frame(media,k.values)


ggplot(error.df,aes(x=k.values,y=media)) + geom_point()+ geom_line(lty="dotted",color='red')







