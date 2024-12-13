# Chargement des bibliothèques
library(ggplot2) # Visualisation de données
library(readr) # Lecture de fichiers CSV


# Chargement des données
db <- read.csv('C:/Users/pc/Documents/RG/diabetes.csv', header=TRUE)

# Inspection de la structure des données
str(db)
head(db) # Affiche les premières lignes des données
summary(db) # Affiche un résumé statistique des données

# Vérification des doublons et suppression
check_duplicates <- function(data) {
  duplicates <- data[duplicated(data), ]
  if (nrow(duplicates) > 0) {
    print("Doublons trouvés et supprimés.")
    data <- data[!duplicated(data), ]
  } else {
    print("Aucun doublon trouvé.")
  }
  return(data)
}
db <- check_duplicates(db)

# Vérification des valeurs manquantes
check_missing_values <- function(data) {
  missing_summary <- data.frame(
    Column = names(data),
    Missing_Values = sapply(data, function(col) sum(is.na(col))),
    Missing_Percentage = sapply(data, function(col) mean(is.na(col)) * 100)
  )
  
  print("Résumé des valeurs manquantes :")
  print(missing_summary)
  return(missing_summary)
}
missing_summary <- check_missing_values(db)





univariate_analysis <- function(data, numerical_cols, categorical_cols) {
  # Analyse univariée des variables numériques
  for (col in numerical_cols) {
    p <- ggplot(data, aes_string(x = col)) +
      geom_histogram(binwidth = 1, color = '#023047', fill = '#ffafcc') +
      ggtitle(paste('Histogram of', col)) +
      xlab(col)
    print(p)
  }
  
  # Analyse univariée des variables catégorielles
  for (col in categorical_cols) {
    p <- ggplot(data, aes_string(x = col)) +
      geom_bar(fill = '#ffafcc') +
      ggtitle(paste('Bar plot of', col)) +
      xlab(col) +
      ylab('Count')
    print(p)
  }
}

categorical_cols <- c('Outcome')
univariate_analysis(db, numerical_cols, categorical_cols)






# Création de la variable catégorielle `Age_Cat` basée sur l'âge
db$Age_Cat <- cut(db$Age, breaks = c(0, 21, 26, 31, 36, 41, 51, 61, 120),
                  labels = c("<21", "21-25", "25-30", "30-35", "35-40", "40-50", "50-60", ">60"))
table(db$Age_Cat)
ggplot(aes(x = Age_Cat), data = db) +
  geom_bar(fill='steelblue')




# Conversion de `Outcome` en facteur
db$Outcome <- as.factor(db$Outcome)









# Liste des variables continues à explorer
continuous_vars <- c('Pregnancies', 'Glucose', 'BloodPressure', 'SkinThickness',
                     'Insulin', 'BMI', 'DiabetesPedigreeFunction')

# Fonction pour créer des boxplots
plot_boxplots <- function(data, continuous_vars) {
  # Boucle à travers chaque variable continue
  for (var in continuous_vars) {
    # Création du boxplot
    p <- ggplot(data, aes(x = Outcome, y = .data[[var]], fill = Outcome)) +
      geom_boxplot() +
      labs(title = paste('Boxplot of', var, 'by Outcome'),
           x = 'Outcome',
           y = var) +
      theme_minimal() +
      scale_fill_manual(values = c("steelblue", "orange"))
    
    # Affichage du boxplot
    print(p)
  }
}

# Appel de la fonction pour créer les boxplots
plot_boxplots(db, continuous_vars)




# Conversion de `Outcome` en facteur
db$Outcome <- as.factor(db$Outcome)

# Liste des variables continues à explorer
continuous_vars <- c('Pregnancies', 'Glucose', 'BloodPressure', 'SkinThickness',
                     'Insulin', 'BMI', 'DiabetesPedigreeFunction')

# Fonction pour créer des boxplots
plot_boxplots <- function(data, continuous_vars) {
  # Boucle à travers chaque variable continue
  for (var in continuous_vars) {
    # Création du boxplot
    p <- ggplot(data, aes(x = Outcome, y = .data[[var]], fill = Outcome)) +
      geom_boxplot() +
      labs(title = paste('Boxplot of', var, 'by Outcome'),
           x = 'Outcome',
           y = var) +
      theme_minimal() +
      scale_fill_manual(values = c("steelblue", "orange"))
    
    # Affichage du boxplot
    print(p)
  }
}

# Appel de la fonction pour créer les boxplots
plot_boxplots(db, continuous_vars)


# Utilisation de by() pour calculer les statistiques descriptives de chaque variable continue groupée par `Outcome`
for (var in continuous_vars) {
  print(paste("Statistiques descriptives pour la variable", var, "groupée par Outcome:"))
  # Appliquer la fonction summary() à chaque groupe défini par `Outcome`
  stats_by_outcome <- by(db[[var]], db$Outcome, summary)
  print(stats_by_outcome)
}

ggplot(aes(x=Age_Cat, y = BMI), data = db) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,70))


db_cor <- round(cor(db[1:8]),1)
db_cor
install.packages(ggcorrplot)

library(ggcorrplot)
ggcorrplot(db_cor)


# Importation des packages nécessaires
library(stats)

# Liste des variables catégorielles à explorer
categorical_vars <- c('Age_Cat')

# Fonction pour effectuer le test du chi-carré entre deux variables catégorielles
chi_square_test <- function(data, var1, var2) {
  # Crée un tableau croisé entre les deux variables
  contingency_table <- table(data[[var1]], data[[var2]])
  
  # Effectue le test du chi-carré
  chi_test <- chisq.test(contingency_table)
  
  # Affiche les résultats du test
  print(paste("Test du chi-carré entre", var1, "et", var2))
  print(chi_test)
}

# Effectue le test du chi-carré entre `Outcome` et chaque autre variable catégorielle
for (var in categorical_vars) {
  chi_square_test(db, 'Outcome', var)
}


# Split dataset into train and test sets
require(caTools)
set.seed(3)
sample = sample.split(db$Outcome, SplitRatio=0.75)
train = subset(db, sample==TRUE)
test = subset(db, sample==FALSE)

nrow(db)
nrow(train)
nrow(test)
# distribution of Age category in Train set
table(train$Age_Cat)

# Structure of train set
str(train)
# Baseline model
table(db$Outcome)
# Baseline accuracy
baseline <- round(500/nrow(db),2)
baseline

# Fit model - using all independent variables

AllVar <- glm(Outcome ~ ., data = train, family = binomial)
summary(AllVar)


# Let's predict outcome on Training dataset

PredictTrain <- predict(AllVar, type = "response")
summary(PredictTrain)

# This computes the average prediction for each of the two outcomes

tapply(PredictTrain, train$Outcome, mean)


# Build confusion matrix with a threshold value of 0.5

threshold_0.5 <- table(train$Outcome, PredictTrain > 0.5)
threshold_0.5

# Accuracy
accuracy_0.5 <- round(sum(diag(threshold_0.5))/sum(threshold_0.5),2)
sprintf("Accuracy is %s",accuracy_0.5)

# Mis-classification error rate
MC_0.5 <- 1-accuracy_0.5
sprintf("Mis-classification error is %s",MC_0.5)

sensitivity0.5 <- round(118/(83+118),2)
specificity0.5 <- round(333/(333+42),2)
sprintf("Sensitivity at 0.5 threshold: %s", sensitivity0.5)
sprintf("Specificity at 0.5 threshold: %s", specificity0.5)


# Build confusion matrix with a threshold value of 0.7

threshold_0.7 <- table(train$Outcome, PredictTrain > 0.7)
threshold_0.7

# Accuracy
accuracy_0.7 <- round(sum(diag(threshold_0.7))/sum(threshold_0.7),2)
sprintf('Accuracy is %s', accuracy_0.7)

# Mis-classification error rate
MC_0.7 <- 1-accuracy_0.7
sprintf("Mis-classification error is %s",MC_0.7)

sensitivity0.7 <- round(78/(123+78),2)
specificity0.7 <- round(359/(359+16),2)
sprintf("Sensitivity at 0.7 threshold: %s", sensitivity0.7)
sprintf("Specificity at 0.7 threshold: %s", specificity0.7)


# Build confusion matrix with a threshold value of 0.2

threshold_0.2 <- table(train$Outcome, PredictTrain > 0.2)
threshold_0.2

# Accuracy
accuracy_0.2 <- round(sum(diag(threshold_0.2))/sum(threshold_0.2),2)
sprintf("Accuracy is %s", accuracy_0.2)

# Mis-classification error rate
MC_0.2 <- 1-accuracy_0.2
sprintf("Mis-classification error is %s",MC_0.2)

sensitivity0.2 <- round(180/(21+180),2)
specificity0.2 <- round(215/(215+160),2)
sprintf("Sensitivity at 0.2 threshold: %s",sensitivity0.2)
sprintf("Specificity at 0.2 threshold: %s",specificity0.2)


# Generate ROC Curves

library(ROCR)

ROCRpred = prediction(PredictTrain, train$Outcome)
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Adding threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
abline(a=0, b=1)

auc_train <- round(as.numeric(performance(ROCRpred, "auc")@y.values),2)
legend(.8, .2, auc_train, title = "AUC", cex=1)


# Making predictions on test set

PredictTest <- predict(AllVar, type = "response", newdata = test)

# Convert probabilities to values using the below

## Based on ROC curve above, selected a threshold of 0.5
test_tab <- table(test$Outcome, PredictTest > 0.5)
test_tab

accuracy_test <- round(sum(diag(test_tab))/sum(test_tab),2)
sprintf("Accuracy on test set is %s", accuracy_test)


# Compute test set AUC

ROCRPredTest = prediction(PredictTest, test$Outcome)
auc = round(as.numeric(performance(ROCRPredTest, "auc")@y.values),2)
auc

