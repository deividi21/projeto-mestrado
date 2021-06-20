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
library(e1071)


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




####Criando os bancos de teste e treino####
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

wdon <- sapply(wheat_dataset$don, as.character)

wheat_dataset.n <- as.data.frame(lapply(wheat_dataset %>% select(-id,-A_410,-L_940,-label), min_max_norm))

wheat_dataset.n <- mutate(wheat_dataset.n, id = wheat_dataset$id, label = wheat_dataset$label)

wheat_dataset.n <- mutate(wheat_dataset.n, don_label = wdon)

wheat_dataset.n$don_label <-factor(wheat_dataset.n$don_label, levels = c('0','307','483','799','1508','1788','1943','2009','2113'))

head(wheat_dataset.n)

set.seed(42)
sample <- sample.split(wheat_dataset.n$don, SplitRatio = .70)
train <- as_tibble(subset(wheat_dataset.n, sample == TRUE))
test <- as_tibble(subset(wheat_dataset.n, sample == FALSE))

train.b <- select(train, -id, -label, -don_label)
train.b$don <- ceiling(train.b$don)

test.b <- select(test, -id, -label, -don_label)
test.b$don <- ceiling(test.b$don)

#write.csv(wheat_dataset,"C:\\Users\\Deividi\\Documentswheat_dataset_v3.csv", row.names = FALSE)

####Rede Neural Binaria####

spec <- feature_spec(train.b, don ~ . ) %>% 
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()

spec


layer <- layer_dense_features(
  feature_columns = dense_features(spec), 
  dtype = tf$float32
)
layer(train.b)



input <- layer_input_from_dataset(train.b %>% select(-don))

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
  input <- layer_input_from_dataset(train.b %>% select(-don))
  
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
  x = train.b %>% select(-don),
  y = train.b$don,
  epochs = 100,
  verbose = 0,
  batch_size=15,
  shuffle=TRUE
)

plot(history)

c(loss, mae) %<-% (model %>% evaluate(test.b %>% select(-don), test.b$don, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae * 100))

test_predictions <- model %>% predict(x=(test.b %>% select(-don)), batch_size=20, verbose=2)

#Decisao
fr <- tibble(
  prediction = sapply(test_predictions[ , 1], round, digits=0), 
  actual = test.b$don,
)

fr.nn <- tibble(
  prediction = sapply(test_predictions[ , 1], round, digits=0), 
  actual = test$don,
  label = test$label,
  id = test$id,
)

confusion.m.nn <- confusionMatrix(factor(fr$prediction), factor(fr$actual), positive = NULL, dnn = c("Prediction", "Reference"))

confusion.m.nn

#Regressao
#fr <- tibble(
#  prediction = sapply(test_predictions[ , 1], round, digits=1), 
#  actual = sapply(test$don, round, digits=1), 
#)
#factor(fr$prediction)
#l <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9,1.0)
#confusionMatrix(factor(fr$prediction, levels = l), factor(fr$actual, levels = l), positive = NULL, dnn = c("Prediction", "Reference"))


####Randon Forest Classificacao####

rf.model.c <- randomForest(x = select(train,-don, -id, -label, -don_label),
                         y = train$don_label,
                         xtest = select(test,-don, -id, -label, -don_label),
                         ytest = test$don_label,
                         ntree = 150,
                         replace = TRUE)

plot(rf.model.c)

rf.model.c.predicted <- as_tibble(rf.model.c$test$predicted)

fr.rf.c <- tibble(
  prediction = rf.model.c$test$predicted, 
  actual = test$don_label,
  label = test$label,
  id = test$id,
)

confusion.m.rf.c <- confusionMatrix(factor(rf.model.c$test$predicted), factor(test$don_label), dnn = c("Prediction", "Reference"))

confusion.m.rf.c

varImpPlot(rf.model.c)

####Randon Forest Binario####

wheat_dataset.n$don_label <-factor(wheat_dataset.n$don_label, levels = c('0','307','483','799','1508','1788','1943','2009','2113'))

x.rf.b <- select(train,-don, -id, -label, -don_label)
xtest.rf.b <- select(test,-don, -id, -label, -don_label)

y.rf.b <- factor(train.b$don %>% replace(train.b$don==0, "0") %>% replace(train.b$don==1, "1"), levels = c('0','1'))
ytest.rf.b <- factor(test.b$don %>% replace(test.b$don==0, "0") %>% replace(test.b$don==1, "1"), levels = c('0','1'))


rf.model.b <- randomForest(x = x.rf.b,
                           y = y.rf.b,
                           xtest = xtest.rf.b,
                           ytest = ytest.rf.b,
                           ntree = 150,
                           replace = TRUE)

plot(rf.model.b)

rf.model.b.predicted <- as_tibble(rf.model.b$test$predicted)

fr.rf.b <- tibble(
  prediction = rf.model.b$test$predicted, 
  actual = test$don,
  label = test$label,
  id = test$id,
)

confusion.m.rf.b <- confusionMatrix(factor(rf.model.b$test$predicted), factor(ytest.rf.b), dnn = c("Prediction", "Reference"))

confusion.m.rf.b

varImpPlot(rf.model.b)



####KNN - Classificacao####

predicted.samples.knn <- knn(select(train,-don, -id, -label, -don_label),select(test,-don, -id, -label, -don_label),train$don_label,k=7)
error.df.knn <- mean(test$don_label != predicted.samples.knn)
error.df.knn
table(predicted.samples.knn,test$don_label)


confusionMatrix(factor(predicted.samples.knn), factor(test$don_label), dnn = c("Prediction", "Reference"))

#Teste para k de 1 a 30
media_erro <- c(1:30)

for (i in 1:30) {
  predicted.samples.knn <- knn(select(train,-don, -id, -label, -don_label),select(test,-don, -id, -label, -don_label),train$don_label,k=i)
  media_erro[i] <- mean(test$don_label != predicted.samples.knn)
}

k.values <- 1:30

error.df <- data.frame(media_erro,k.values)

ggplot(error.df,aes(x=k.values,y=media_erro)) + geom_point()+ geom_line(lty="dotted",color='red')

####KNN Binario####

predicted.samples.knn <- knn(select(train.b,-don),select(test.b,-don),train.b$don,k=9)
error.df.knn <- mean(test.b$don != predicted.samples.knn)
error.df.knn
table(predicted.samples.knn,test.b$don)


confusionMatrix(factor(predicted.samples.knn), factor(test.b$don), dnn = c("Prediction", "Reference"))


#Teste para k de 1 a 30
media_erro <- c(1:30)

for (i in 1:30) {
  predicted.samples.knn <- knn(select(train.b,-don),select(test.b,-don),train.b$don,k=i)
  media_erro[i] <- mean(test.b$don != predicted.samples.knn)
}

k.values <- 1:30

error.df <- data.frame(media_erro,k.values)

ggplot(error.df,aes(x=k.values,y=media_erro)) + geom_point()+ geom_line(lty="dotted",color='red')

####SVM Classificacao####

modelo_svm <- svm(don_label ~ ., data = select(train,-don, -id, -label), kernel="radial", cost=1.81, gamma=2)

summary(modelo_svm)

teste001 <- predict(modelo_svm, select(test,-don, -id, -label, -don_label))
confusionMatrix(factor(teste001), factor(test$don_label), dnn = c("Prediction", "Reference"))

tune_svm <- tune(svm, train.x = select(test,-don, -id, -label, -don_label), train.y = test$don_label, kernel = "radial", ranges = list(cost=(seq(0.1, 20, by=0.01)), gamma=c(0.0, 5,2)))

print(tune_svm)
 
####SVM Binario####
ytest.rf.b <- factor(test.b$don %>% replace(test.b$don==0, "0") %>% replace(test.b$don==1, "1"), levels = c('0','1'))

dados.svm.b <- train.b
dados.svm.b$don <- factor(train.b$don %>% replace(train.b$don==0, "0") %>% replace(train.b$don==1, "1"), levels = c('0','1'))


modelo_svm <- svm(don ~ ., data = dados.svm.b, kernel="radial", cost=1.81, gamma=2)

teste001 <- predict(modelo_svm, select(test.b,-don))
confusionMatrix(factor(teste001), factor(test.b$don), dnn = c("Prediction", "Reference"))

tune_svm <- tune(svm, train.x = select(train,-don, -id, -label, -don_label), train.y = train$don_label, kernel = "radial", ranges = list(cost=(seq(0.1, 20, by=0.01)), gamma=c(0.0, 5,2)))

print(tune_svm)









