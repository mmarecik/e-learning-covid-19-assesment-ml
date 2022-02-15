#czyszczenie środowiska
rm(list = ls())

#załadowanie pakietów
library(caret)
library(pROC)
library(dplyr)
library(psych)
library(klaR)
library(pdp)
library(ggplot2)
library(gridExtra)
library(rpart.plot)

#ustawienie odpowiedniego słownika - obsługa polskich znaków
if (.Platform$OS.type == 'windows') {
  Sys.setlocale(category = 'LC_ALL','English_United States.1250')
} else {
  Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
}


#preprocessing----

#wczytanie danych
ankieta <- read.csv("C:/Users/Magda/Desktop/P2/Projekt2/ankieta.csv", sep=";")
dane <- ankieta
dim(dane)
colnames(dane) <- c("typ_studiow", "rok_studiow", "rodzaj_studiow", 
                    "miejsce_zamieszkania", "plec", "poziom_wiedzy", 
                    "trudnosci_z_motywacja", "samodzielna_praca_stres", 
                    "dostosowanie_formy_zajec", "preferencje",
                    "inne_obowiazki", "nauka_hybrydowa_opinia", "ocena")

dane$ocena <- ifelse(dane$ocena == "Tak", 1, 0)
dane <- dane %>%
  mutate(
    typ_studiow = factor(typ_studiow),
    rok_studiow = factor(rok_studiow),
    rodzaj_studiow = factor(rodzaj_studiow),
    miejsce_zamieszkania = factor(miejsce_zamieszkania),
    plec = factor(plec))

#wybranie kolumn z pytaniami w skali Likerta
dane_num <- dane[, 6:ncol(dane)]

#-------------


#analiza respondentow ----------
plec.bplot <- ggplot(dane, aes(x=plec)) +
  geom_bar(stat="count", width=0.8) + 
  xlab("Płeć") + ylab("Liczność") + theme_minimal() + 
  scale_y_continuous(breaks=seq(20,120,20))

msc_zam.bplot <- ggplot(dane, aes(x=miejsce_zamieszkania)) +
  geom_bar(stat="count", width=0.8) + 
  xlab("Miejsce zamieszkania") + ylab("Liczność") + theme_minimal() + 
  scale_y_continuous(breaks=seq(20,120,20))

tryb_studiow.bplot <- ggplot(dane, aes(x=typ_studiow)) +
  geom_bar(stat="count", width=0.8) + 
  xlab("Tryb studiów") + ylab("Liczność") + theme_minimal() + 
  scale_y_continuous(breaks=seq(20,160,20))

rok_studiow.bplot <- ggplot(dane, aes(x=rok_studiow)) +
  geom_bar(stat="count", width=0.8) + 
  xlab("Rok studiów") + ylab("Liczność") + theme_minimal() + 
  scale_y_continuous(breaks=seq(20,60,10))

rodzaj_studiow.bplot <- ggplot(dane, aes(x=rodzaj_studiow)) +
  geom_bar(stat="count", width=0.8) + 
  xlab("Rodzaj studiów") + ylab("Liczność") + theme_minimal() + 
  scale_y_continuous(breaks=seq(20,80,10))

grid.arrange(plec.bplot, msc_zam.bplot, nrow=1)

grid.arrange(arrangeGrob(tryb_studiow.bplot, rok_studiow.bplot, ncol=2),
             arrangeGrob(rodzaj_studiow.bplot, ncol=1), nrow=2)


num.bplots <- vector('list', ncol(dane_num))
library(ggpubr)
for (i in seq_along(dane_num)) {
  message(i)
  num.bplots[[i]] <- local({
    i <- i
    p1 <- ggplot(dane_num, aes(x=dane_num[[i]])) +
      geom_bar(stat="count", width=0.8) + xlab(colnames(dane_num)[i])+
      theme_minimal() + scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7))
  })
}

grid.arrange(num.bplots[[1]], num.bplots[[2]], num.bplots[[3]], num.bplots[[4]], 
             num.bplots[[5]], num.bplots[[6]], num.bplots[[7]], num.bplots[[8]],
             ncol = 2, nrow = 4)

#podstawowe statystyki zmiennej Y
describe(dane_num)

#wpływ poszczególnych zmiennych kategorycznych na zmienną Y
describeBy(dane$ocena, group=dane$plec)
describeBy(dane$ocena, group=dane$miejsce_zamieszkania)
describeBy(dane$ocena, group=dane$typ_studiow)
describeBy(dane$ocena, group=dane$rok_studiow)
describeBy(dane$ocena, group=dane$rodzaj_studiow)
#--------------------


#macierz korelacji----
cor_matrix.p7 <- cor(dane_num, method = "kendall")
cor_matrix.p7
cor_matrix.p5 <- cor(dane_num[,-c(2,5)], method = "kendall")
cor_matrix.p5

#----


dane_num$ocena <- as.factor(dane_num$ocena)
elearning.full <- dane
elearning.full$ocena <- as.factor(elearning.full$ocena)
elearning <- dane_num[,-c(2, 5)]
elearning.factors <- elearning %>% 
  mutate( poziom_wiedzy = factor(poziom_wiedzy),
          samodzielna_praca_stres = factor(samodzielna_praca_stres),
          dostosowanie_formy_zajec = factor(dostosowanie_formy_zajec),
          inne_obowiazki = factor(inne_obowiazki),
          nauka_hybrydowa_opinia = factor(nauka_hybrydowa_opinia))



#analiza skupień----

dane.cluster <- dane[,-c(7, 10)] %>% 
  mutate( poziom_wiedzy = factor(poziom_wiedzy),
          samodzielna_praca_stres = factor(samodzielna_praca_stres),
          dostosowanie_formy_zajec = factor(dostosowanie_formy_zajec),
          inne_obowiazki = factor(inne_obowiazki),
          nauka_hybrydowa_opinia = factor(nauka_hybrydowa_opinia),
          ocena = factor(ocena))

?kmodes

library(purrr)
k.max <- 10
set.seed(16)
kmodes.wss <- map_dbl(2:k.max, function(k) {
  m <- kmodes(dane.cluster[,1:11], modes=k, iter.max = 10, weighted = FALSE)
  sum(m$withindiff)
})

#wykres zmiennosci wenwatrzgrupowej w zaleznosci od k
ggplot(data.frame(cluster = 2:k.max, withindiff = kmodes.wss), aes(cluster, withindiff)) +
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = 2:k.max) + 
  theme_minimal() + 
  labs(x="Liczba grup", y="Calkowita zmiennosc wewnatrz grup", 
       title="Miara zmiennosci wewnatrzgrupowej vs liczba grup")

set.seed(16)
cluster.results <-kmodes(dane.cluster[,1:11], modes=5, iter.max = 10, weighted = FALSE)
cluster.results

dane.cluster$grupa <- cluster.results$cluster


#analiza wynikow grupowania
get.mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

dane.cluster %>% group_by(grupa) %>%
  summarise(
    n = n(),
    plec = get.mode(plec),
    miejsce_zamieszkania = get.mode(miejsce_zamieszkania),
    typ_studiow = get.mode(typ_studiow),
    rok_studiow = get.mode(rok_studiow),
    rodzaj_studiow = get.mode(rodzaj_studiow))

summary(dane.cluster[dane.cluster$grupa=='1',-c(12)])
summary(dane.cluster[dane.cluster$grupa=='2',-c(12)])
summary(dane.cluster[dane.cluster$grupa=='3',-c(12)])
summary(dane.cluster[dane.cluster$grupa=='4',-c(12)])
summary(dane.cluster[dane.cluster$grupa=='5',-c(12)])

srednie.w.klastrach <- dane.cluster %>% 
  group_by(grupa) %>% 
  mutate( grupa=as.factor(grupa),
          poziom_wiedzy = as.numeric(poziom_wiedzy),
          samodzielna_praca_stres = as.numeric(samodzielna_praca_stres),
          dostosowanie_formy_zajec = as.numeric(dostosowanie_formy_zajec),
          inne_obowiazki = as.numeric(inne_obowiazki),
          nauka_hybrydowa_opinia = as.numeric(nauka_hybrydowa_opinia),
          ocena = as.numeric(ocena)) %>% 
  summarise( poziom_wiedzy = mean(poziom_wiedzy),
             samodzielna_praca_stres = mean(samodzielna_praca_stres), 
             dostosowanie_formy_zajec = mean(dostosowanie_formy_zajec), 
             inne_obowiazki = mean(inne_obowiazki), 
             nauka_hybrydowa_opinia = mean(nauka_hybrydowa_opinia), 
             ocena = mean(ocena))


#wykresy PCP
library(GGally)
ggparcoord(srednie.w.klastrach, columns = 2:7, groupColumn = 'grupa') + 
  theme_minimal() + xlab("Zmienne") + ylab("Wartość")


#Kobieta na kierunku ekonomicznym, ktora nie miala problemow motywacja
x1 <- dane %>%
  filter(plec=="Kobieta", rodzaj_studiow=="Ekonomiczne, biznesowe", trudnosci_z_motywacja < 4) %>%
  summarise(
    n = n(),
    sr.poziom_wiedzy = mean(poziom_wiedzy),
    sr.trudnosci_z_motywacja = mean(trudnosci_z_motywacja),
    sr.samodzielna_praca_stres = mean(samodzielna_praca_stres),
    sr.preferencje = mean(preferencje),
    sr.inne_obowiazki = mean(inne_obowiazki),
    sr.nauka_hybrydowa_opinia = mean(nauka_hybrydowa_opinia),
    sr.ocena=mean(ocena))
x1$nazwa <- c("Kobieta.St.Ekonomieczne.Trudnosci.Z.Motywacja<4")

#Mezczyzna na kierunku medycznym/przyrodniczym studiow dziennych
x2 <- dane %>%
  filter(plec=="Mezczyzna", rodzaj_studiow=="Scisle,  techniczne", typ_studiow == "Stacjonarne") %>%
  summarise(
    n = n(),
    sr.poziom_wiedzy = mean(poziom_wiedzy),
    sr.trudnosci_z_motywacja = mean(trudnosci_z_motywacja),
    sr.samodzielna_praca_stres = mean(samodzielna_praca_stres),
    sr.preferencje = mean(preferencje),
    sr.inne_obowiazki = mean(inne_obowiazki),
    sr.nauka_hybrydowa_opinia = mean(nauka_hybrydowa_opinia),
    sr.ocena=mean(ocena))
x2$nazwa <- c("Mezczyzna.St.Techniczne.Stacjonarne")


#Student 1 roku studiow mieszkajacy na wsi, ktory uwaza, ze studiujac stacjonarnie umialby wiecej
x3 <- dane %>%
  filter(miejsce_zamieszkania=="Wies", rok_studiow == "I", poziom_wiedzy > 4) %>%
  summarise(
    n = n(),
    sr.poziom_wiedzy = mean(poziom_wiedzy),
    sr.trudnosci_z_motywacja = mean(trudnosci_z_motywacja),
    sr.samodzielna_praca_stres = mean(samodzielna_praca_stres),
    sr.preferencje = mean(preferencje),
    sr.inne_obowiazki = mean(inne_obowiazki),
    sr.nauka_hybrydowa_opinia = mean(nauka_hybrydowa_opinia),
    sr.ocena=mean(ocena))
x3$nazwa <- c("Rok.I.Wies.Poziom.Wiedzy>4")

#Student niestacjonarny 2 stopnia z miasta

x4 <- dane %>%
  filter(miejsce_zamieszkania=="Miasto powyzej 100 tys. mieszkancow", typ_studiow=="Niestacjonarne", rok_studiow != "I" | rok_studiow != "II") %>%
  summarise(
    n = n(),
    sr.poziom_wiedzy = mean(poziom_wiedzy),
    sr.trudnosci_z_motywacja = mean(trudnosci_z_motywacja),
    sr.samodzielna_praca_stres = mean(samodzielna_praca_stres),
    sr.preferencje = mean(preferencje),
    sr.inne_obowiazki = mean(inne_obowiazki),
    sr.nauka_hybrydowa_opinia = mean(nauka_hybrydowa_opinia),
    sr.ocena=mean(ocena))
x4$nazwa <- c("DuzeMiasto.Niestacjonarne.Powyzej.II.Roku")

x.df <- NULL
x.df <- rbind(x.df, x1, x2, x3, x4)

ggparcoord(x.df, columns = 2:8, groupColumn = 'nazwa')
#---------------


#Klasyfikacja za pomoca ML

models.acc <- data.frame(model = "", test.acc = 0)
models.acc <- models.acc[-1,]

#Naiwny klasyfikator Bayes'a------

#podzial na zbior uczacy i testowy
set.seed(0)
index <- createDataPartition(y=elearning.factors$ocena, p=0.7, list=FALSE)

uczacy.elearning <- elearning[index,]
testowy.elearning <- elearning[-index,]

dim(uczacy.elearning)
summary(uczacy.elearning)

dim(testowy.elearning)
summary(testowy.elearning)

#konstruowanie modelu
set.seed(0)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
nbc <- train(ocena ~ ., data = uczacy.elearning, method = 'nb',trControl = control)
nbc

confusionMatrix(nbc)

#dokladnosc predykcji na zbiorze testowym
pred.nbc <- nbc %>% predict(testowy.elearning)
confusionMatrix(pred.nbc, testowy.elearning$ocena)

#tuning hiperparametrow modelu
search.grid <- expand.grid(usekernel = c(TRUE, FALSE), 
                           fL = seq(0, 3, 1),
                           adjust = seq(0, 1, 0.25))
set.seed(0)
nbc.tun <- train( ocena ~ .,
                  data=uczacy.elearning,
                  method = "nb",
                  trControl = control,
                  tuneGrid = search.grid)
nbc.tun

confusionMatrix(nbc.tun)

#dokladnosc predykcji na zbiorze testowym
pred.nbc.tun <- predict(nbc.tun, newdata = testowy.elearning)
confusionMatrix(pred.nbc.tun, testowy.elearning$ocena)

models.acc[nrow(models.acc)+1, ] <- c("NBC", mean(pred.nbc.tun == testowy.elearning$ocena))

#---------------------------


#Drzewa decyzyjne

#Pojedyncze drzewo decyzyjne------

#podzial na zbior uczacy i testowy
set.seed(0)
index <- createDataPartition(y=elearning$ocena, p=0.7, list=FALSE)

uczacy.elearning <- elearning[index,]
testowy.elearning <- elearning[-index,]

dim(uczacy.elearning)
summary(uczacy.elearning)

dim(testowy.elearning)
summary(testowy.elearning)


#budujemy model drzewa decyzyjnego na zbiorze testowym
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(0)
tree <- train(ocena ~ ., 
              data = uczacy.elearning, 
              method = "rpart", 
              trControl = control,
              tuneGrid = expand.grid(cp = seq(0,0.1,0.01)))
tree
plot(tree)
summary(tree$finalModel)

#wykres drzewa
rpart.plot(tree$finalModel, fallen.leaves = FALSE)

confusionMatrix(tree)
tree.pred.uczacy <- predict(tree, newdata = uczacy.elearning)
confusionMatrix(tree.pred.uczacy, uczacy.elearning$ocena)

tree.pred.testowy <- predict(tree, newdata = testowy.elearning)
confusionMatrix(tree.pred.testowy, testowy.elearning$ocena)

models.acc[nrow(models.acc)+1, ] <- c("Single tree", mean(tree.pred.testowy == testowy.elearning$ocena))


#zapisanie danych do wykresu krzywej ROC
tree.probs <- predict(tree, newdata = testowy.elearning, type="prob")
roc.tree <- roc(testowy.elearning$ocena, tree.probs[,"1"])


#drzewo stworzone na innym zbiorze uczacym
set.seed(10)
index <- createDataPartition(y=elearning$ocena, p=0.7, list=FALSE)

uczacy.elearning <- elearning[index,]
testowy.elearning <- elearning[-index,]

set.seed(10)
tree.2 = train(ocena ~ ., 
               data=uczacy.elearning, 
               method="rpart", 
               trControl = control,
               tuneGrid = expand.grid(cp = seq(0,0.1,0.01)))

rpart.plot(tree.2$finalModel, fallen.leaves = FALSE)

#---------------------------


#Bagging-------

set.seed(9)
index <- createDataPartition(y=elearning$ocena, p=0.7, list=FALSE)

uczacy.elearning <- elearning[index,]
testowy.elearning <- elearning[-index,]

dim(uczacy.elearning)
summary(uczacy.elearning)

dim(testowy.elearning)
summary(testowy.elearning)

set.seed(9)
bagging <- train( ocena ~ .,
                  data = uczacy.elearning,
                  method = "treebag",
                  trControl = control)

bagging
summary(bagging)

#dokladnosc predykcji
bagging.pred.uczacy <- predict(bagging, newdata = uczacy.elearning)
confusionMatrix(bagging.pred.uczacy, uczacy.elearning$ocena)

bagging.pred.testowy <- predict(bagging, newdata = testowy.elearning)
confusionMatrix(bagging.pred.testowy, testowy.elearning$ocena)

models.acc[nrow(models.acc)+1, ] <- c("Bagging p=5", mean(bagging.pred.testowy == testowy.elearning$ocena))

#wykres waznosci predyktorow
varImp(bagging)
plot(varImp(bagging))


#zapisanie danych do wykresu krzywej ROC
bagging.probs <- predict(bagging, newdata = testowy.elearning, type="prob")
roc.bagging <- roc(testowy.elearning$ocena, bagging.probs[,"1"])


#PDP
predicted.probs <- function(object, newdata) {
  mean(predict(object, newdata, type = "prob")[, "1"])
}


detach(package:purrr, unload=TRUE) #konflikt pakietow purrr i pdp

pdp1 <- partial(bagging, pred.var = "nauka_hybrydowa_opinia", pred.fun = predicted.probs, test = testowy.elearning, chull = TRUE)
pdp1 <- autoplot(pdp1, contour = TRUE) + xlim(1, 7) + ylim(0.1, 0.9)

pdp2 <- partial(bagging, pred.var = "inne_obowiazki", pred.fun = predicted.probs, test = testowy.elearning, chull = TRUE)
pdp2 <- autoplot(pdp2, contour = TRUE) + xlim(1, 7) + ylim(0.1, 0.9)

pdp3 <- partial(bagging, pred.var = "poziom_wiedzy", pred.fun = predicted.probs, test = testowy.elearning, chull = TRUE)
pdp3 <- autoplot(pdp3, contour = TRUE) + xlim(1, 7) + ylim(0.1, 0.9)

pdp4 <- partial(bagging, pred.var = "samodzielna_praca_stres", pred.fun = predicted.probs, test = testowy.elearning, chull = TRUE)
pdp4 <- autoplot(pdp4, contour = TRUE) + xlim(1, 7) + ylim(0.1, 0.9)

pdp5 <- partial(bagging, pred.var = "dostosowanie_formy_zajec", pred.fun = predicted.probs, test = testowy.elearning, chull = TRUE)
pdp5 <- autoplot(pdp5, contour = TRUE) + xlim(1, 7) + ylim(0.1, 0.9)

# pdp6 <- partial(bg.metryczka, pred.var = "plec", pred.fun = predicted.probs, test = testowy.elearning, chull = TRUE)
# pdp6 <- autoplot(pdp6, contour = TRUE) + ylim(0.1, 0.9)

grid.arrange(arrangeGrob(pdp1, pdp2, pdp3, ncol=3),
             arrangeGrob(pdp4, pdp5, ncol=3), nrow=2)



#bagging z uwzglednieniem pytan z metryczki (p=10)
set.seed(222)
index <- createDataPartition(y=elearning.full$ocena, p=0.7, list=FALSE)

uczacy.elearning <- elearning.full[index, -c(7, 10)]
testowy.elearning <- elearning.full[-index, -c(7, 10)]

summary(uczacy.elearning)
summary(testowy.elearning)


set.seed(222)
bg.metryczka <- train(ocena~., 
                      data=uczacy.elearning, 
                      method = "treebag",
                      trControl = control)
bg.metryczka

#dokladnosc predykcji
bg.metryczka.pred.uczacy <- predict(bg.metryczka, newdata = uczacy.elearning)
confusionMatrix(bg.metryczka.pred.uczacy, uczacy.elearning$ocena)

bg.metryczka.pred.testowy <- predict(bg.metryczka, newdata = testowy.elearning)
confusionMatrix(bg.metryczka.pred.testowy, testowy.elearning$ocena)

models.acc[nrow(models.acc)+1, ] <- c("Bagging p=10", mean(bg.metryczka.pred.testowy == testowy.elearning$ocena))

#wykres waznosci predyktorow
varImp(bg.metryczka)
plot(varImp(bg.metryczka))

#-------


#Lasy losowe----
library(randomForest)

#podzial na zbior uczacy i testowy
set.seed(77)
index <- createDataPartition(y=elearning$ocena, p=0.7, list=FALSE)

uczacy.elearning <- elearning[index,]
testowy.elearning <- elearning[-index,]

summary(uczacy.elearning)
summary(testowy.elearning)

#budowanie modelu RF
mtry <- c(2:(ncol(uczacy.elearning)-2))
tunegrid <- expand.grid(.mtry=mtry)

set.seed(77)
rf <- train(ocena~., 
            data=uczacy.elearning, 
            method="rf", 
            tuneGrid=tunegrid,
            trControl=control)
rf

rf.pred.uczacy <- predict(rf, newdata = uczacy.elearning)
confusionMatrix(rf.pred.uczacy, uczacy.elearning$ocena)

rf.pred.testowy <- predict(rf, newdata = testowy.elearning)
confusionMatrix(rf.pred.testowy, testowy.elearning$ocena)

models.acc[nrow(models.acc)+1, ] <- c("Random forest p=5", mean(rf.pred.testowy == testowy.elearning$ocena))


#las losowy z uwzglednieniem metryczki
set.seed(10)
index <- createDataPartition(y=elearning.full$ocena, p=0.7, list=FALSE)

uczacy.elearning <- elearning.full[index, -c(7, 10)]
testowy.elearning <- elearning.full[-index, -c(7, 10)]

summary(uczacy.elearning)
summary(testowy.elearning)

mtry <- c(2:(ncol(uczacy.elearning)-2))
tunegrid <- expand.grid(.mtry=mtry)

set.seed(10)
rf.metryczka <- train(ocena~., 
                      data=uczacy.elearning, 
                      method="rf", 
                      tuneGrid=tunegrid,
                      trControl=control)
rf.metryczka

rf.metryczka.pred.uczacy <- predict(rf.metryczka, newdata = uczacy.elearning)
confusionMatrix(rf.metryczka.pred.uczacy, uczacy.elearning$ocena)

rf.metryczka.pred.testowy <- predict(rf.metryczka, newdata = testowy.elearning)
confusionMatrix(rf.metryczka.pred.testowy, testowy.elearning$ocena)

models.acc[nrow(models.acc)+1, ] <- c("Random forest p=10", mean(rf.metryczka.pred.testowy == testowy.elearning$ocena))

#wykres waznosci predyktorow
varImp(rf.metryczka)
plot(varImp(rf.metryczka))

rf.probs <- predict(rf.metryczka, newdata = testowy.elearning, type="prob")
roc.rf <- roc(testowy.elearning$ocena, rf.probs[,"1"])

#---------------


#krzywa ROC -----

#prawdopodobienstwa
head(tree.probs)
head(bagging.probs)
head(rf.probs)

plot(roc.tree,col=c(7))
par(new = T)
plot(roc.bagging,col=c(5))
par(new = T)
plot(roc.rf,col=c(4))
par(new = T)
legend(0.6, 0.3, legend=c("Pojedyncze drzewo decyzyjne", "Bagging", "Random forest"),
       col=c(7, 5, 4), lty=1, cex=0.8)
par(new = T)

#pole obszaru po krzywa ROC
auc(roc.tree)
auc(roc.bagging)
auc(roc.rf) 

#----------


# Support Vector Classifier----

set.seed(11)
index <- createDataPartition(y=elearning$ocena, p=0.7, list=FALSE)

uczacy.elearning <- elearning[index,]
testowy.elearning <- elearning[-index,]

summary(uczacy.elearning)
summary(testowy.elearning)

getModelInfo("svmLinear")$svmLinear$parameters

#model z jadrem liniowym
set.seed(11)
grid <- expand.grid(C = seq(0.5, 2, 0.25))
svm.linear <- train(ocena ~., 
                    data = uczacy.elearning, 
                    method = "svmLinear",
                    preProcess = c("center","scale"),
                    trControl = control,
                    tuneGrid=grid)
svm.linear
plot(svm.linear)

confusionMatrix(svm.linear)

#dokladnosc predykcji
svm.linear.pred.uczacy <- svm.linear %>% predict(uczacy.elearning)
confusionMatrix(svm.linear.pred.uczacy, uczacy.elearning$ocena)

svm.linear.pred.testowy <- svm.linear %>% predict(testowy.elearning)
confusionMatrix(svm.linear.pred.testowy, testowy.elearning$ocena)

models.acc[nrow(models.acc)+1, ] <- c("SVM Linear", mean(svm.linear.pred.testowy == testowy.elearning$ocena))


#jadro radialne
getModelInfo("svmRadial")$svmRadial$parameters

set.seed(45)
index <- createDataPartition(y=elearning$ocena, p=0.7, list=FALSE)

uczacy.elearning <- elearning[index,]
testowy.elearning <- elearning[-index,]

summary(uczacy.elearning)
summary(testowy.elearning)

grid <- expand.grid(sigma = seq(0.1, 1, 0.1),
                    C = seq(0.25, 2, 0.25))
set.seed(45)
svm.radial <- train(ocena ~., 
                    data = uczacy.elearning, 
                    method = "svmRadial",
                    trControl = control,
                    preProcess = c("center","scale"),
                    tuneGrid = grid)

svm.radial
plot(svm.radial)

confusionMatrix(svm.radial)

#dokladnosc predykcji
svm.radial.pred.uczacy <- svm.radial %>% predict(uczacy.elearning)
confusionMatrix(svm.radial.pred.uczacy, uczacy.elearning$ocena)

svm.radial.pred.testowy <- svm.radial %>% predict(testowy.elearning)
confusionMatrix(svm.radial.pred.testowy, testowy.elearning$ocena)

models.acc[nrow(models.acc)+1, ] <- c("SVM radial", mean(svm.radial.pred.testowy == testowy.elearning$ocena))

#--------------------------






# bibliografia:
# https://christophm.github.io/interpretable-ml-book/index.html
# https://journal.r-project.org/archive/2017/RJ-2017-016/RJ-2017-016.pdf
# https://topepo.github.io/caret/model-training-and-tuning.html#example
# https://bookdown.org/mpfoley1973/data-sci/
# https://bradleyboehmke.github.io/HOML/index.html
# https://ema.drwhy.ai/introduction.html



