library(tidyverse)
library(caTools)
library(class)
library(caret)
library(ggrepel)


####Importando Dados####
sample_21195 <- read_delim("dados/21195_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21196 <- read_delim("dados/21196_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21197 <- read_delim("dados/21197_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21198 <- read_delim("dados/21198_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21199 <- read_delim("dados/21199_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21200 <- read_delim("dados/21200_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21201 <- read_delim("dados/21201_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21202 <- read_delim("dados/21202_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21210 <- read_delim("dados/21210_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_21227 <- read_delim("dados/21227_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_giberela <- read_delim("dados/giberela_1.csv",";", escape_double = FALSE, trim_ws = TRUE)
sample_sadio <- read_delim("dados/sadio_1.csv",";", escape_double = FALSE, trim_ws = TRUE)


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
sample_giberela.subset <- sample_giberela[20:37]
sample_sadio.subset <- sample_sadio[20:37]

####Adicionando Identificadores####
sample_21195.subset <- mutate(sample_21195.subset, label = "21195")
sample_21195.subset <- tibble::rowid_to_column(sample_21195.subset, "id")
sample_21196.subset <- mutate(sample_21196.subset, label = "21196")
sample_21196.subset <- tibble::rowid_to_column(sample_21196.subset, "id")
sample_21197.subset <- mutate(sample_21197.subset, label = "21197")
sample_21197.subset <- tibble::rowid_to_column(sample_21197.subset, "id")
sample_21198.subset <- mutate(sample_21198.subset, label = "21198")
sample_21198.subset <- tibble::rowid_to_column(sample_21198.subset, "id")
sample_21199.subset <- mutate(sample_21199.subset, label = "21199")
sample_21199.subset <- tibble::rowid_to_column(sample_21199.subset, "id")
sample_21200.subset <- mutate(sample_21200.subset, label = "21200")
sample_21200.subset <- tibble::rowid_to_column(sample_21200.subset, "id")
sample_21201.subset <- mutate(sample_21201.subset, label = "21201")
sample_21201.subset <- tibble::rowid_to_column(sample_21201.subset, "id")
sample_21202.subset <- mutate(sample_21202.subset, label = "21202")
sample_21202.subset <- tibble::rowid_to_column(sample_21202.subset, "id")
sample_21210.subset <- mutate(sample_21210.subset, label = "21210")
sample_21210.subset <- tibble::rowid_to_column(sample_21210.subset, "id")
sample_21227.subset <- mutate(sample_21227.subset, label = "21227")
sample_21227.subset <- tibble::rowid_to_column(sample_21227.subset, "id")

sample_giberela.subset <- mutate(sample_giberela.subset, label = "giberela")
sample_giberela.subset <- tibble::rowid_to_column(sample_giberela.subset, "id")
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
wheat_dataset <- merge(wheat_dataset,sample_giberela.subset,all.x = TRUE, all.y = TRUE)
wheat_dataset <- merge(wheat_dataset,sample_sadio.subset,all.x = TRUE, all.y = TRUE)

####Normalizacao####
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

wheat_dataset.n1 <- scale(wheat_dataset[2:19])

wheat_dataset.n2 <- as.data.frame(lapply(wheat_dataset[,2:19], min_max_norm))

####Geracao de Graficos####

wheat_dataset_plot <- cbind(wheat_dataset.n2, wheat_dataset$id, wheat_dataset$label)

wheat_dataset_plot <- wheat_dataset_plot %>%
  pivot_longer(colnames(wheat_dataset_plot[1:18]), names_to = "wavelength", values_to = "response")
print(wheat_dataset_plot)

wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..R..610nm.', 610)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..S..680nm.', 680)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..T..730nm.', 730)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..U..760nm.', 760)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..V..810nm.', 810)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..W..860nm.', 860)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..G..560nm.', 560)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..H..585nm.', 585)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..I..645nm.', 645)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..J..705nm.', 705)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..K..900nm.', 900)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..L..940nm.', 940)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..A..410nm.', 410)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..B..435nm.', 435)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..C..460nm.', 460)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..D..485nm.', 485)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..E..510nm.', 510)
wheat_dataset_plot$wavelength <- replace(wheat_dataset_plot$wavelength, wheat_dataset_plot$wavelength=='Cal..F..535nm.', 535)

wheat_dataset_plot$wavelength <- as.numeric(wheat_dataset_plot$wavelength)
colnames(wheat_dataset_plot) <- c('id','label','wavelength','response')
sapply(wheat_dataset_plot, class)

ggplot(data = filter(wheat_dataset_plot, label == c('sadio','21227'))) + 
  geom_smooth(mapping = aes(x = wavelength, y = response, colour=label))

ggplot(data = filter(wheat_dataset_plot, label != c('giberela'))) + 
  geom_smooth(mapping = aes(x = wavelength, y = response, colour=label))

wheat_dataset_plot %>%
  ggplot(aes(x = wavelength, y = response, colour = label)) + 
  geom_smooth(size = 1.5)


ggplot(data = filter(wheat_dataset_plot, label == c('giberela'))) + 
  geom_bar(mapping = aes(x = wavelength, y = response, id=1))



####Teste 1 para o algoritmo knn#####
set.seed(101)

final.data <- cbind(wheat_dataset.n1,wheat_dataset[20])

head(final.data)

set.seed(101)

sample <- sample.split(final.data$label, SplitRatio = .70)
train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)

predicted.samples <- knn(wheat_dataset[2:19],sample_21195.subset[2:19],wheat_dataset$label,k=9)
error.df <- mean(test$label != predicted.samples)

table(predicted.samples)

####Teste 2 para o algoritmo knn#####
set.seed(123)
dat.d <- sample(1:nrow(wheat_dataset.n2),size=nrow(wheat_dataset.n2)*0.7,replace = FALSE) #random selection of 70% data.

train.loan <- wheat_dataset.n2[dat.d,] # 70% training data
test.loan <- wheat_dataset.n2[-dat.d,] # remaining 30% test data

#Creating seperate dataframe for 'Creditability' feature which is our target.
train.loan_labels <- wheat_dataset[dat.d,19]
test.loan_labels <-wheat_dataset[-dat.d,19]

knn.9 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=9)
acc.9 <- 100 * sum(test.loan_labels == knn.9)/NROW(test.loan_labels)

table(knn.9 ,test.loan_labels)


#Teste para k de 1 a 30
media <- c(1:30)

for (i in 1:30) {
  predicted.samples <- knn(train[1:18],test[1:18],train$label,k=i)
  media[i] <- mean(test$label != predicted.samples)
}

k.values <- 1:30

error.df <- data.frame(media,k.values)


ggplot(error.df,aes(x=k.values,y=media)) + geom_point()+ geom_line(lty="dotted",color='red')










