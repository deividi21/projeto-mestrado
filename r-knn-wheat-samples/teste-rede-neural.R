library(tidyverse)
library(yardstick)
library(caTools)
library(caret)

library(tensorflow)
library(keras)
library(tfdatasets)

library(neuralnet)
library(varhandle)
library(randomForest)
library(randomForestExplainer)
library(class)


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

wheat_dataset <- wheat_dataset %>% rename(R_610 = 'Cal. R (610nm)',
                                          S_680 = 'Cal. S (680nm)',
                                          T_730 = 'Cal. T (730nm)',
                                          U_760 = 'Cal. U (760nm)',
                                          V_810 = 'Cal. V (810nm)',
                                          W_860 = 'Cal. W (860nm)',
                                          G_560 = 'Cal. G (560nm)',
                                          H_585 = 'Cal. H (585nm)',
                                          I_645 = 'Cal. I (645nm)',
                                          J_705 = 'Cal. J (705nm)',
                                          K_900 = 'Cal. K (900nm)',
                                          L_940 = 'Cal. L (940nm)',
                                          A_410 = 'Cal. A (410nm)',
                                          B_435 = 'Cal. B (435nm)',
                                          C_460 = 'Cal. C (460nm)',
                                          D_485 = 'Cal. D (485nm)',
                                          E_510 = 'Cal. E (510nm)',
                                          F_535 = 'Cal. F (535nm)')


####Rede Neural####

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


#Regressao
nn <- as.data.frame(lapply(wheat_dataset %>% select(-id,-A_410,-L_940,-label), min_max_norm))


#Escolha binaria
nn <- as.data.frame(lapply(wheat_dataset %>% select(-id,-A_410,-L_940,-label), min_max_norm))
nn$don <- ceiling(nn$don)

head(nn)

#nn <- mutate(nn,wheat_dataset$label)

#Separando dataset
set.seed(42)
nn.sample <- sample.split(nn$don, SplitRatio = .70)
nn.train <- as_tibble(subset(nn, nn.sample == TRUE))
nn.test <- as_tibble(subset(nn, nn.sample == FALSE))


#Deixando dataset completo em um unico banco
#nn.train <- as_tibble(nn %>% select(-A_410,-L_940))
#rows <- sample(nrow(nn.train))
#nn.train <- nn.train[rows, ]

#write.csv(nn,"C:\\Users\\Deividi\\Documents\\dataset_normalizado.csv", row.names = TRUE)


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
  layer_dense(units = 9, activation = "softmax") %>%
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
    layer_dense(units = 9, activation = "softmax") %>%
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
  verbose = 0,
  batch_size=15,
  shuffle=TRUE
)

plot(history)

c(loss, mae) %<-% (model %>% evaluate(nn.test %>% select(-don), nn.test$don, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae * 100))

test_predictions <- model %>% predict(x=(nn.test %>% select(-don)), batch_size=20, verbose=2)

#Decisao
fr <- tibble(
  prediction = sapply(test_predictions[ , 1], round, digits=0), 
  actual = sapply(nn.test$don, round, digits=0),
)
factor(fr$prediction)
confusionMatrix(factor(fr$prediction), factor(fr$actual), positive = NULL, dnn = c("Prediction", "Reference"))


#Regressao
fr <- tibble(
  prediction = sapply(test_predictions[ , 1], round, digits=1), 
  actual = sapply(nn.test$don, round, digits=1), 
)
factor(fr$prediction)
l <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9,1.0)
confusionMatrix(factor(fr$prediction, levels = l), factor(fr$actual, levels = l), positive = NULL, dnn = c("Prediction", "Reference"))


####Randon Forest Classificacao####
set.seed(8675309)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

rf <- as.data.frame(lapply(wheat_dataset %>% select(-id,-A_410,-L_940,-label,-don), min_max_norm))

wdon <- sapply(wheat_dataset$don, as.character)

rf <- as_tibble(mutate(rf, don = wdon))

rf$don <-factor(rf$don, levels = c('0','307','483','799','1508','1788','1943','2009','2113'))

head(rf)

rf.sample <- sample.split(rf$don, SplitRatio = .70)
rf.train <- subset(rf, rf.sample == TRUE)
rf.test <- subset(rf, rf.sample == FALSE)

prop.table(table(rf.train$don))


rf.model <- randomForest(x = rf.train[,-17],
                         y = rf.train$don,
                         xtest = rf.test[,-17],
                         ytest = rf.test$don,
                         ntree = 150,
                         replace = TRUE)

plot(rf.model)

rf.model.predicted <- as_tibble(rf.model$test$predicted)

confusionMatrix(factor(rf.model$test$predicted), factor(rf.test$don), dnn = c("Prediction", "Reference"))


varImpPlot(rf.model)

####Randon Forest Binario####

set.seed(8675309)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

rf <- as.data.frame(lapply(wheat_dataset %>% select(-id,-A_410,-L_940,-label), min_max_norm))

rf$don <- ceiling(rf$don)
rf$don <- replace(rf$don, rf$don==1, "contamidado")
rf$don <- replace(rf$don, rf$don==0, "sadio")
rf$don <- factor(rf$don, levels = c('sadio','contamidado'))

head(rf)

rf.sample <- sample.split(rf$don, SplitRatio = .70)
rf.train <- subset(rf, rf.sample == TRUE)
rf.test <- subset(rf, rf.sample == FALSE)

prop.table(table(rf.train$don))


rf.model <- randomForest(x = rf.train[,-17],
                         y = rf.train$don,
                         xtest = rf.test[,-17],
                         ytest = rf.test$don,
                         ntree = 150,
                         replace = TRUE)

plot(rf.model)

rf.model.predicted <- as_tibble(rf.model$test$predicted)

confusionMatrix(factor(rf.model$test$predicted), factor(rf.test$don), dnn = c("Prediction", "Reference"))

varImpPlot(rf.model)



####KNN - Classificacao####
set.seed(101)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

knn <- as.data.frame(lapply(wheat_dataset %>% select(-id,-A_410 ,-L_940,-label,-don), min_max_norm))

knn <- mutate(knn, don=wheat_dataset$don)

head(knn)

sample.knn <- sample.split(knn$don, SplitRatio = .70)
train.knn <- subset(knn, sample.knn == TRUE)
test.knn <- subset(knn, sample.knn == FALSE)

predicted.samples.knn <- knn(train.knn[1:16],test.knn[1:16],train.knn$don,k=7)
error.df.knn <- mean(test.knn$don != predicted.samples.knn)
error.df.knn
table(predicted.samples.knn,test.knn$don)


confusionMatrix(factor(predicted.samples.knn), factor(test.knn$don), dnn = c("Prediction", "Reference"))


#Teste para k de 1 a 30
media_erro <- c(1:30)

for (i in 1:30) {
  predicted.samples.knn <- knn(train.knn[1:16],test.knn[1:16],train.knn$don,k=i)
  media_erro[i] <- mean(test.knn$don != predicted.samples.knn)
}

k.values <- 1:30

error.df <- data.frame(media_erro,k.values)

ggplot(error.df,aes(x=k.values,y=media_erro)) + geom_point()+ geom_line(lty="dotted",color='red')

####KNN Binario####
set.seed(101)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

knn <- as.data.frame(lapply(wheat_dataset %>% select(-id,-A_410 ,-L_940,-label), min_max_norm))

knn$don <- ceiling(knn$don)
knn$don <- replace(knn$don, knn$don==1, "contamidado")
knn$don <- replace(knn$don, knn$don==0, "sadio")
knn$don <- factor(knn$don, levels = c('sadio','contamidado'))


head(knn)

sample.knn <- sample.split(knn$don, SplitRatio = .70)
train.knn <- subset(knn, sample.knn == TRUE)
test.knn <- subset(knn, sample.knn == FALSE)

predicted.samples.knn <- knn(train.knn[1:16],test.knn[1:16],train.knn$don,k=15)
error.df.knn <- mean(test.knn$don != predicted.samples.knn)
error.df.knn
table(predicted.samples.knn,test.knn$don)


confusionMatrix(factor(predicted.samples.knn), factor(test.knn$don), dnn = c("Prediction", "Reference"))


#Teste para k de 1 a 30
media_erro <- c(1:30)

for (i in 1:30) {
  predicted.samples.knn <- knn(train.knn[1:16],test.knn[1:16],train.knn$don,k=i)
  media_erro[i] <- mean(test.knn$don != predicted.samples.knn)
}

k.values <- 1:30

error.df <- data.frame(media_erro,k.values)

ggplot(error.df,aes(x=k.values,y=media_erro)) + geom_point()+ geom_line(lty="dotted",color='red')









