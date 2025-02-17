#BADANIE WYSTĘPOWANIA ZJAWISKA KONWERGENCJI  ----
# PROGNOZOWANIE LICZBY LAT POTRZEBNYCH DO ZREDUKOWANIA "OBECNYCH RÓŻNIC O POŁOWĘ"
###znak oddzielający część dziesiętną
options(OutDec=",")

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(readr)
library(reshape2)

dane <- read_csv2(file = 'Dane 5_2024.csv', col_types = "ffffnnnnnnnnnnnnnnnnnn") 
summary(dane) 


alfa <- 0.05

####wizualizacja----

dane %>%
  select(-c("Region","Nazwa","UE_do_2004","UE_od_2004")) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x = value, fill = key), color = "black", bins = 16) +
  facet_wrap(~ key, scales = "free") +
  ylim(0, 60) +                                                           #tutaj ujednolicamy ylim 
  labs(title = "HISTOGRAMY ZMIENNYCH", y = "Liczba", x = "Wartość") +
  labs(fill = "PKB per capita") +
  theme_minimal()

# zmiana układu danych do wykresu pudełkowego
dane_ciag <- melt(dane[,c(3,5:22)], id = "UE_do_2004", variable.name = "Zmienne", 
                  value.name = "Wartość")   
head(dane_ciag)

# wizualizacja danych - wykres pudełkowy
ggplot(dane_ciag, aes(x = Zmienne, y = Wartość, color = UE_do_2004)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "WYKRESY PUDEŁKOWE") +
  labs(color = "Państwa UE") +
  scale_colour_discrete(labels = c("UE_do_2004", "UE_od_2004")) +
  geom_boxplot()


#### wykresy dla pojedynczych grup----

##Filtrowanie danych dla UE_do_2004 = 1
dane_1 <- dane %>% filter(UE_do_2004 == 1) %>%
  select(-c("Region", "Nazwa", "UE_od_2004")) %>%
  keep(is.numeric) %>%
  gather(key = "rok", value = "wartosc")%>%
  mutate(UE_do_2004 = "UE do 2004")

##Filtrowanie danych dla UE_do_2004 = 0
dane_0 <- dane %>% filter(UE_do_2004 == 0) %>%
  select(-c("Region", "Nazwa", "UE_od_2004")) %>%
  keep(is.numeric) %>%
  gather(key = "rok", value = "wartosc")%>%
  mutate(UE_do_2004 = "UE od 2004")

##Wykres dla UE_do_2004 = 1
plot_1 <- ggplot(dane_1) +
  geom_histogram(mapping = aes(x = wartosc, fill = rok), color = "black", bins = 16) +
  facet_wrap(~ rok, scales = "free") +
  labs(title = "HISTOGRAMY (UE_DO_2004 = 1)", y = "Liczba", x = "Wartość") +
  theme_minimal()
plot_1

##Wykres dla UE_do_2004 = 0
plot_0 <- ggplot(dane_0) +
  geom_histogram(mapping = aes(x = wartosc, fill = rok), color = "black", bins = 16) +
  facet_wrap(~ rok, scales = "free") +
  labs(title = "HISTOGRAMY (UE_DO_2004 = 0)", y = "Liczba", x = "Wartość") +
  theme_minimal()
plot_0



#wspólny
#Połączenie obu zbiorów danych
dane_all <- bind_rows(dane_1, dane_0)

#Wspólny wykres z dwiema grupami
ggplot(dane_all, aes(x = wartosc, fill = UE_do_2004)) +
  geom_histogram(position = "identity", color = "black", bins = 16, alpha = 0.4) +
  facet_wrap(~ rok, scales = "free") +
  labs(title = "HISTOGRAMY ZMIENNYCH (PODZIAŁ NA CZŁONKOSTWO W UE)",
       x = "Wartość",
       y = "Liczba",
       fill = "UE_do_2004") +
  theme_minimal()


####BADANIE KONWERGENCJI TYPU BETA W WERSJI ABSOLUTNEJ----

analiza_konwergencji <- function(dane, zalezna, niezalezna, podgrupa, istotnosc, iloscprzejsc){
  model_nasz <- lm(data = dane, zalezna ~ niezalezna, subset = podgrupa) #przyklad (podgrupa == 1)
  summary(model_nasz)
  model_nasz$coefficients
  summary(model_nasz)[[4]][,4]
  # współczynnik regresji
  a1 <- model_nasz$coefficients[[2]]
  a1
  a1_p <- summary(model_nasz)[[4]][2,4]
  a1_p
  alfa <- istotnosc
  if (is.na(a1) != TRUE & is.na(a1_p) != TRUE){
    if (a1_p < alfa){
      if (a1 < 0){
        cat("Współczynnik regresji:", round(a1, 5), "\n")
        # współczynnik zbieżności
        beta <- -log(1+a1*T)/T
        cat("Współczynnik zbieżności:", round(beta, 5), "\n")
        # współczynnik połowicznej zbieżności
        T_polow.zb <- log(2)/beta
        cat("Współczynnik połowicznej zbieżności:", round(T_polow.zb, 5), "\n")
      } else {
        cat("Współczynnik regresji jest nieujemny.")
      } 
    } else {
      cat("Hipoteza zerowa")
    }
  } else {
    cat(" .")
  }
  cat("p_value przy niezaleznej: ", a1_p)
  
}






# ustalenie T -> ile przejsc
T <- dim(dane[,5:22])[2]-1 #tu  zmienic kolumny, od numerycznych 
T #liczba przejść
dane$niezalezna <- log(dane$R_2004)  #logarytm naturalny / pierwsza

dane$zalezna <- (log(dane$R_2021)-log(dane$R_2004))/T # ostatnia-pierwsza

### analiza konwergencji ogolem
analiza_konwergencji(dane, dane$zalezna, dane$niezalezna, (TRUE), alfa, T)

### analiza konwergencji stare - tego moze nie byc jak nei podzieli na podgrupy
analiza_konwergencji(dane, dane$zalezna, dane$niezalezna, (dane$UE_do_2004 == 1), alfa, T)
### analiza konwergencji nowe- tego moze nie byc jak nei podzieli na podgrupy
analiza_konwergencji(dane, dane$zalezna, dane$niezalezna, (dane$UE_od_2004 == 1), alfa, T)

# prosta regresji dla UE_nowe (UE_od_2004)- tego moze nie byc jak nei podzieli na podgrupy
model_nowe <- lm(data = dane, zalezna ~ niezalezna, subset = (UE_od_2004 == 1))

model_stare<- lm(data = dane, zalezna ~ niezalezna, subset = (UE_od_2004 == 0))

####WIZUALIZACJA MODELI REGRESJI----- 
model_ogolem <- lm(data = dane, zalezna ~ niezalezna)

# diagram korelacyjny
ggplot(dane, aes(x = niezalezna, y = zalezna)) + 
  labs(title = "DIAGRAM KORELACYJNY") +
  geom_point()

# diagram korelacyjny + podział na państwa UE_stare i UE_nowe
ggplot(dane, aes(x = niezalezna, y = zalezna, color = dane$UE_do_2004)) + 
  labs(title = "DIAGRAM KORELACYJNY") +
  labs(color = "Państwa UE") +
  scale_colour_discrete(labels = c("UE_do_2004", "UE_od_2004")) +
  geom_point()

# diagram korelacyjny + prosta regresji dla Ogółem + podział na państwa UE_stare i UE_nowe
ggplot(model_ogolem, aes_string(x = names(model_ogolem$model)[2], y = names(model_ogolem$model)[1], 
                                color = dane$UE_do_2004)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("MODEL OGÓŁEM: Skor.R^2 = ",signif(summary(model_ogolem)$adj.r.squared, 3),
                     "; Wyraz wolny =",signif(model_ogolem$coef[[1]], 3),
                     "; Wyraz kierunkowy =",signif(model_ogolem$coef[[2]], 3),
                     "; p-value =",signif(summary(model_ogolem)$coef[2,4], 3))) +
  labs(color = "Państwa UE") +
  scale_colour_discrete(labels = c("UE_do_2004", "UE_od_2004"))

# diagram korelacyjny + proste regresji dla UE_stare i UE_nowe
ggplot(model_ogolem, aes_string(x = names(model_ogolem$model)[2], y = names(model_ogolem$model)[1], 
                                fill = dane$UE_do_2004)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red") +
  labs(title = paste("MODEL UE_STARE: Skor.R^2 = ",signif(summary(model_stare)$adj.r.squared, 2),
                     "; Wyraz wolny =",signif(model_stare$coef[[1]], 2),
                     "; Wyraz kierunkowy = ",signif(model_stare$coef[[2]], 1),
                     "; p-value =",signif(summary(model_stare)$coef[2,4], 3))) +
  labs(subtitle = paste("MODEL UE_NOWE:  Skor.R^2 = ",signif(summary(model_nowe)$adj.r.squared, 3),
                        "; Wyraz wolny = ",signif(model_nowe$coef[[1]], 3),
                        "; Wyraz kierunkowy =",signif(model_nowe$coef[[2]], 2),
                        "; p-value =",signif(summary(model_nowe)$coef[2,4], 3))) +
  labs(fill = "Państwa UE") + 
  scale_fill_discrete(labels = c("UE_do_2004", "UE_od_2004")) +
  theme(plot.title = element_text(size = 12), plot.subtitle = element_text(size = 12))
#to tego momentu mozna przewinac


#### ANALIZA SKUPIEŃ - NIENADZOROWANE UCZENIE STATYSTYCZNE----

##### metoda hierarchiczna - metoda Warda----- tego tez nie bylo

# standaryzacja zmiennych



numeryczne_kol <- (5:22)



dane_stand <- dane
for (j in numeryczne_kol){
  dane_stand[,j] <- scale(dane[,j])
}
summary(dane_stand)


dendrogram_Ward <- function (dane_stand, numeryczne_kolumny){
  # miara odległości
  odl_Euklidesa <- dist(dane_stand[,5:22], method = "euclidean")
  
  # metoda Warda z kwadratem odległości Euklidesa
  dane_Ward <- hclust(odl_Euklidesa, method = 'ward.D2')
  
  # drzewo połączeń nr 1
  plot(dane_Ward, labels = NULL, cex = 0.6, 
       main = "Dendrogram", sub = NULL,
       xlab = "Regiony", ylab = "Wysokość")
  
  # przeskalowanie "Wysokości"
  dane_Ward$height <- 100*dane_Ward$height/max(dane_Ward$height)
  
  # drzewo połączeń nr 2
  plot(dane_Ward, labels = NULL, cex = 0.6, 
       main = "Dendrogram", sub = NULL,
       xlab = "Regiony", ylab = "Wysokość")
  
  # drzewo połączen nr 3
  plot(dane_Ward, labels = NULL, cex = 0.6, hang = -1, 
       main = "Dendrogram", sub = NULL,
       xlab = "Regiony", ylab = "Wysokość")
  
  # podział regionów na 2 skupienia
  rect.hclust(dane_Ward, k=2, border="black")
  # podział regionów na 3 skupienia
  rect.hclust(dane_Ward, k=3, border="red")
  # podział regionów na 4 skupienia
  rect.hclust(dane_Ward, k=4, border="blue")
  # podział regionów na 5 skupień
  rect.hclust(dane_Ward, k=5, border="green")
  # podział regionów na 7 skupień
  rect.hclust(dane_Ward, k=7, border="yellow")
  
}




dendrogram_Ward(dane_stand, numeryczne_kol)


#### metoda optymalizacyjna - metoda k-średnich----
k=3
numeryczne_kol <- (5:22)
lista_ilosci_iteracji <-c(1,5,10,50)
lista_ilosci_iteracji <-c(1) #tu podana liczba iteracji

dane_stand <- dane
for (j in numeryczne_kol){
  dane_stand[,j] <- scale(dane[,j])
}
summary(dane_stand)




KMEANS_plot <- function(dane_stand, kol_num, ilegrup, ile_iteracji){
  set.seed(1)
  dane_k_srednich <- kmeans(x = dane_stand[,kol_num], centers = ilegrup, nstart = ile_iteracji)
  # wizualizacja podziału na skupienia - iteracja nr nstart
  library(useful)
  plot11<- plot.kmeans(dane_k_srednich, data = dane_stand[,kol_num],
                       title =paste("Podział regionów", as.character(ilegrup), "skupienia metodą k-średnich - iteracja nr", as.character(ile_iteracji) , sep=" "),
                       xlab = "Główna składowa nr 1", ylab = "Główna składowa nr 2") 
  
  return(plot11)
}



KMEANS_dane_summ <- function(dane_stand, kol_num, ilegrup, ile_iteracji){
  set.seed(1)
  dane_k_srednich <- kmeans(x = dane_stand[,kol_num], centers = ilegrup, nstart = ile_iteracji)
  
  return(dane_k_srednich)
}






for (i in lista_ilosci_iteracji){
  
  print(KMEANS_dane_summ(dane_stand, numeryczne_kol, k, i))
  #print(KMEANS_plot(dane_stand, numeryczne_kol, k, i))
  
}
#######porownanie liczbenosci grup----jeszcze raz pokazuje ktora najliczniejsza
for (i in lista_ilosci_iteracji){
  print(i)
  print(KMEANS_dane_summ(dane_stand, numeryczne_kol, k, i)$size)
}

####### przynależność do skupień w iteracji nr 1----
for (i in lista_ilosci_iteracji){
  print(i)
  print(KMEANS_dane_summ(dane_stand, numeryczne_kol, k, i)$cluster)
}


###### charakterystyka skupień - wykres średnich wartości zmiennych w 3 skupieniach-----

# dla zmiennych standaryzowanych
chara_skupi<-function(dane_k_srednich, nazwa, ile_iteracji) {
  mean_class_stand <- t(dane_k_srednich$centers)
  colnames(mean_class_stand) <- c("Grupa 1","Grupa 2","Grupa 3")
  mean_class_stand_ciag <- melt(mean_class_stand) 
  colnames(mean_class_stand_ciag) <- c("Zmienna_standaryzowana","Grupa","Średnia_wartość")
  head(mean_class_stand_ciag)
  ggplot(data=mean_class_stand_ciag, aes(x=Grupa, y=Średnia_wartość, fill=Zmienna_standaryzowana)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(title = paste("WYKRES ŚREDNICH WARTOŚCI ZMIENNYCH W SKUPIENIACH", nazwa, "dla iteracji", ile_iteracji, sep = " "))
  
}
#stand
for (i in lista_ilosci_iteracji){
  print(i)
  print(chara_skupi(KMEANS_dane_summ(dane_stand, numeryczne_kol, k, i), nazwa="standaryzowane", ile_iteracji = i))
}
#oryg
for (i in lista_ilosci_iteracji){
  print(i)
  print(chara_skupi(KMEANS_dane_summ(dane, numeryczne_kol, k, i), nazwa="oryginalne", ile_iteracji=i))
}





## BADANIE KONWERGENCJI W GRUPACH REGIONÓW - podział na 3 skupienia-------------------


badanie_konwergencji_w_grupie <- function(dane1, zalezna, niezalezna, kmeans_dane, k){
  # dodanie zmiennej z numerami skupień
  dane1$Grupy <- as.factor(kmeans_dane$cluster)
  #print(dane$Grupy)
  
  for (i in 1:k){
    print("Grupa")
    print(i)
    print(analiza_konwergencji(dane1, zalezna, niezalezna, dane1$Grupy==i, alfa, T))
  }
}

#k<-3
#alfa=0.05
#T<-17

print(badanie_konwergencji_w_grupie(dane, dane$zalezna, dane$niezalezna, kmeans_dane = KMEANS_dane_summ(dane_stand,kol_num = numeryczne_kol, ilegrup = k, ile_iteracji = 1 ),k))
#KMEANS_dane_summ(dane_stand,kol_num = numeryczne_kol, ilegrup = k, ile_iteracji = 1 )

###### W RAZUE CZEGO regresja w grupach-------------
#dla 3
k=3

dane$Grupy <- as.factor(KMEANS_dane_summ(dane_stand,kol_num = numeryczne_kol, ilegrup = k, ile_iteracji = 1 )$cluster)


regression_analysis <- function(dane_group) {
  #dane_group <-gr1_3_dane
  # Przygotowanie zmiennych
  T <- dim(dane_group[, 5:22])[2] - 1
  dane_group$niezalezna <- log(dane_group$R_2004)
  dane_group$zalezna <- (log(dane_group$R_2021) - log(dane_group$R_2004)) / T
  
  # Model regresji
  model <- lm(data = dane_group, zalezna ~ niezalezna)
  
  # Wypisanie wyników
  cat("Wyniki regresji dla grupy:\n")
  print(summary(model))
  # Współczynnik regresji
  a1 <- model$coefficients[[2]]
  a1_p <- summary(model)[[4]][2, 4]
  cat("Współczynnik regresji:", round(a1, 5), "\n")
  
  # Jeśli współczynnik jest ujemny, obliczamy współczynniki konwergencji
  if (!is.na(a1) & !is.na(a1_p)) {
    if (a1_p < 0.05 && a1 < 0) {
      # Współczynnik zbieżności
      beta <- -log(1 + a1 * T) / T
      cat("Współczynnik zbieżności:", round(beta, 5), "\n")
      
      # Współczynnik połowicznej zbieżności
      T_polow_zb <- log(2) / beta
      cat("Współczynnik połowicznej zbieżności:", round(T_polow_zb, 5), "\n")
    } else {
      cat("  . \n")
    }
  }
}


gr1_3 <- dane[which(dane$Grupy == "1"),"Region"] %>%
  print(n = dim(dane[which(dane$Grupy == "1"),])[1])
#dane[which(dane$Region == "PL91"), "Nazwa"]
gr2_3 <-dane[which(dane$Grupy == "2"),"Region"] %>%
  print(n = dim(dane[which(dane$Grupy == "2"),])[1])
gr3_3<-dane[which(dane$Grupy == "3"),"Region"] %>%
  print(n = dim(dane[which(dane$Grupy == "3"),])[1])


gr1_3_dane <- dane %>%
  filter(Region %in% gr1_3$Region)
gr2_3_dane <- dane %>%
  filter(Region %in% gr2_3$Region)
gr3_3_dane <- dane %>%
  filter(Region %in% gr3_3$Region)
# Analiza regresji dla trzech grup
cat("\nREGRESJA DLA GRUPY 1 (cluster=1):\n") 
regression_analysis(gr1_3_dane )

cat("\nREGRESJA DLA GRUPY 2 (cluster=2):\n")
regression_analysis(gr2_3_dane )

cat("\nREGRESJA DLA GRUPY 3 (cluster=3):\n") 
regression_analysis(gr3_3_dane )

#####Dla k=2 ###############################
k=2
numeryczne_kol <- (5:22)
lista_ilosci_iteracji <-c(1)

dane_stand <- dane
for (j in numeryczne_kol){
  dane_stand[,j] <- scale(dane[,j])
}
summary(dane_stand)

KMEANS_dane_summ <- function(dane_stand, kol_num, ilegrup, ile_iteracji){
  set.seed(1)
  dane_k_srednich <- kmeans(x = dane_stand[,kol_num], centers = ilegrup, nstart = ile_iteracji)
  return(dane_k_srednich)
}

for (i in lista_ilosci_iteracji){
  print(KMEANS_dane_summ(dane_stand, numeryczne_kol, k, i))
}

for (i in lista_ilosci_iteracji){
  print(i)
  print(KMEANS_dane_summ(dane_stand, numeryczne_kol, k, i)$size)
}

for (i in lista_ilosci_iteracji){
  print(i)
  print(KMEANS_dane_summ(dane_stand, numeryczne_kol, k, i)$cluster)
}

chara_skupi<-function(dane_k_srednich, nazwa, ile_iteracji) {
  mean_class_stand <- t(dane_k_srednich$centers)
  colnames(mean_class_stand) <- c("Grupa 1","Grupa 2")
  mean_class_stand_ciag <- melt(mean_class_stand) 
  colnames(mean_class_stand_ciag) <- c("Zmienna_standaryzowana","Grupa","Średnia_wartość")
  ggplot(data=mean_class_stand_ciag, aes(x=Grupa, y=Średnia_wartość, fill=Zmienna_standaryzowana)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(title = paste("WYKRES ŚREDNICH WARTOŚCI ZMIENNYCH W SKUPIENIACH", nazwa, "dla iteracji", ile_iteracji, sep = " "))
}

for (i in lista_ilosci_iteracji){
  print(i)
  print(chara_skupi(KMEANS_dane_summ(dane_stand, numeryczne_kol, k, i), nazwa="standaryzowane", ile_iteracji = i))
}

badanie_konwergencji_w_grupie <- function(dane1, zalezna, niezalezna, kmeans_dane, k){
  dane1$Grupy <- as.factor(kmeans_dane$cluster)
  for (i in 1:k){
    print("Grupa")
    print(i)
    print(analiza_konwergencji(dane1, zalezna, niezalezna, dane1$Grupy==i, alfa, T))
  }
}

print(badanie_konwergencji_w_grupie(dane, dane$zalezna, dane$niezalezna, kmeans_dane = KMEANS_dane_summ(dane_stand,kol_num = numeryczne_kol, ilegrup = k, ile_iteracji = 1 ),k))

dane$Grupy <- as.factor(KMEANS_dane_summ(dane_stand,kol_num = numeryczne_kol, ilegrup = k, ile_iteracji = 1 )$cluster)

regression_analysis <- function(dane_group) {
  T <- dim(dane_group[, 5:22])[2] - 1
  dane_group$niezalezna <- log(dane_group$R_2004)
  dane_group$zalezna <- (log(dane_group$R_2021) - log(dane_group$R_2004)) / T
  model <- lm(data = dane_group, zalezna ~ niezalezna)
  cat("Wyniki regresji dla grupy:\n")
  print(summary(model))
  a1 <- model$coefficients[[2]]
  a1_p <- summary(model)[[4]][2, 4]
  cat("Współczynnik regresji:", round(a1, 5), "\n")
  if (!is.na(a1) & !is.na(a1_p)) {
    if (a1_p < 0.05 && a1 < 0) {
      beta <- -log(1 + a1 * T) / T
      cat("Współczynnik zbieżności:", round(beta, 5), "\n")
      T_polow_zb <- log(2) / beta
      cat("Współczynnik połowicznej zbieżności:", round(T_polow_zb, 5), "\n")
    } else {
      cat("  . \n")
    }
  }
}

gr1_2 <- dane[which(dane$Grupy == "1"),"Region"] %>%
  print(n = dim(dane[which(dane$Grupy == "1"),])[1])
gr2_2 <-dane[which(dane$Grupy == "2"),"Region"] %>%
  print(n = dim(dane[which(dane$Grupy == "2"),])[1])

gr1_2_dane <- dane %>%
  filter(Region %in% gr1_2$Region)
gr2_2_dane <- dane %>%
  filter(Region %in% gr2_2$Region)

cat("\nREGRESJA DLA GRUPY 1 (cluster=1):\n") 
regression_analysis(gr1_2_dane)

cat("\nREGRESJA DLA GRUPY 2 (cluster=2):\n")
regression_analysis(gr2_2_dane)




#iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii

#bankructwo drzewa, lasy----------------------------------------------------------------------------------------------------------------------------------------------

options(OutDec=",")

Dane1 <- read.csv2('Dane 6_2024.csv')
set.seed(123)

str(Dane1)
Dane1$Class <- as.factor(Dane1$Class)
Dane1$Sample <- as.factor(Dane1$Sample)
str(Dane1)

levels(Dane1$Class)
Dane1$Class <- ordered(Dane1$Class, levels=c('NB', 'B'))  # sukces ma byc drugi zeby przy zmianie na   dostała 1 bo my prognozujemy sukces: np bankruta
levels(Dane1$Class)
levels(Dane1$Sample)
# opcjonalne
Dane1$Sample<- ordered(Dane1$Sample, levels=c("U" , "T"))
levels(Dane1$Sample)
str(Dane1)

head(Dane1)
#ile jakiej klasy
table(Dane1$Class)
# warianty
table(Dane1$Sample)

## Część ucząca i część testowa

# Podział na dwa zbiory - zbiór uczący i zbiór testowy
# Zbiór uczący
Dane1_uczace <- subset(Dane1,Sample=="U")
# Proszę utworzyć zbiór testowy
Dane1_testowe <-subset(Dane1, Sample =="T")

dim(Dane1_uczace)
dim(Dane1_testowe)
head(Dane1_uczace)
head(Dane1_testowe)

# usunięcie zmiennej 'Sample'
Dane1_uczace <- Dane1_uczace[,-61]
Dane1_testowe <- Dane1_testowe[,-61]

head(Dane1_uczace)
head(Dane1_testowe)
#w gwoli wypadku sprawdzamy:
Dane1 <- Dane1[,-61]
levels(Dane1$Class)
levels(Dane1_uczace$Class)
levels(Dane1_testowe$Class)
table(Dane1_uczace$Class)
table(Dane1_testowe$Class)

# zmiana numeracji wierszy na ciągłą
Dane1_uczace_order <- Dane1_uczace
Dane1_testowe_order <- Dane1_testowe
row.names(Dane1_uczace_order)=c(1:length(Dane1_uczace_order[,1]))
head(Dane1_uczace_order)
Dane1_testowe_order <- Dane1_testowe
row.names(Dane1_testowe_order)=c(1:length(Dane1_testowe_order[,1]))
head(Dane1_testowe_order)


#### Drzewo klasyfikacyjne----

library(rpart)
#cp - parametr zlozonosci, za małe to moze zbyt sie dopasowac

Dane1_tree <- rpart(Class ~ ., data=Dane1_uczace_order, 
                    control=rpart.control(cp=0.005))

plot(Dane1_tree, uniform=TRUE, margin=0.05)
text(Dane1_tree, cex=0.75)
Dane1_tree

### Klasyfikacja i prognozowanie w razie czego $class do zmiany w 109
klasyfikacja_prognoza<- function(drzewo, dane_order_do_prognozy){
  Dane_order_pred <- predict(drzewo, 
                             dane_order_do_prognozy, 
                             type = "class")
  Tabela_uczace <- table(predicted = Dane_order_pred, 
                         actual = dane_order_do_prognozy$Class)
  print(Tabela_uczace)
  Skutecznosc_uczace <- (Tabela_uczace[1]+Tabela_uczace[4])/(Tabela_uczace[1]+Tabela_uczace[2]+Tabela_uczace[3]+Tabela_uczace[4])
  Czulosc_uczace <- Tabela_uczace[4]/(Tabela_uczace[3]+Tabela_uczace[4])
  Specyficznosc_uczace <- Tabela_uczace[1]/(Tabela_uczace[1]+Tabela_uczace[2])
  print("skutecznosc")
  print(Skutecznosc_uczace)
  print("czulosc")
  print(Czulosc_uczace)
  print("specyficznosc")
  print(Specyficznosc_uczace)
  table(Dane_order_pred)
}


# Dane uczące
klasyfikacja_prognoza(Dane1_tree, Dane1_uczace_order)


# Dane testowe 
klasyfikacja_prognoza(Dane1_tree, Dane1_testowe_order)




### Przykład 1 - tylko dwa wskaźniki finansowe

## Przykład 1.1
# cp = 0.005
Dane1_tree_short <- rpart(Class ~ Attr35 + Attr46, 
                          data=Dane1_uczace_order, 
                          control=rpart.control(cp=0.005))
plot(Dane1_tree_short, uniform=TRUE, margin=0.05)
text(Dane1_tree_short, cex=0.75)
Dane1_tree_short

# Klasyfikacja i prognozowanie

# Dane uczące
klasyfikacja_prognoza(Dane1_tree_short, Dane1_uczace_order)

# Dane testowe
klasyfikacja_prognoza(Dane1_tree_short, Dane1_testowe_order)

library("partykit") 
plot(as.party(Dane1_tree_short),tp_args=list(id=F))



### Las losowy----

library(randomForest)

set.seed(123)
rf <- randomForest(Class ~ ., data=Dane1_uczace_order)
rf
head(rf$err.rate)

## Wizualizacja błędóW

library(ggplot2)

# Dla ogółem     
error_df = data.frame(error_rate=rf$err.rate[,'OOB'], num_trees=1:rf$ntree)
graph <- ggplot(error_df, aes(x=num_trees, y=error_rate)) +
  geom_line()  +
  theme_bw()
graph

# Dla NB
error_df = data.frame(error_rate=rf$err.rate[,'NB'], num_trees=1:rf$ntree)
graph <- ggplot(error_df, aes(x=num_trees, y=error_rate)) +
  geom_line()  +
  theme_bw()
graph

# Dla B - Proszę uzupełnić kod dotyczący "B"
error_df = data.frame( error_rate=rf$err.rate[,'B'], num_trees=1:rf$ntree)
graph <- ggplot(error_df, aes(x=num_trees, y=error_rate)) +
  geom_line()  +
  theme_bw()
graph

## Wizualizacja danych
Pred <- predict(rf, Dane1_uczace_order, prob=TRUE)
rf_df <- cbind(Dane1_uczace_order, pred = Pred)

# Dla zmiennej "Class"
graph <- ggplot(data=rf_df, aes(x=Attr25, y=Attr28, 
                                shape=Class, color=Class, size=Class)) +
  geom_point(alpha=.8) +
  scale_color_manual(values = c('NB'='blue', 'B'='red')) +
  scale_shape_manual(values = c('NB'=0, 'B'=1)) +
  scale_size_manual(values = c('NB'=0.5, 'B'=2)) +
  scale_x_continuous(expand=c(0,0), lim=c(-1, 1)) + 
  scale_y_continuous(expand=c(0,0), lim=c(-5, 25)) + 
  guides(color = guide_legend(override.aes = list(size=2))) +
  theme_bw()
graph

# Dla zmiennej "Pred"
graph <- ggplot(data=rf_df, aes(x=Attr25, y=Attr28, 
                                shape=Pred, color=Pred, size=Pred)) +
  geom_point(alpha=.8) +
  scale_color_manual(values = c('NB'='#b8e186', 'B'='#d95f02')) +
  scale_shape_manual(values = c('NB'=0, 'B'=1)) +
  scale_size_manual(values = c('NB'=0.5, 'B'=2)) +
  scale_x_continuous(expand=c(0,0), lim=c(-1, 1)) + 
  scale_y_continuous(expand=c(0,0), lim=c(-5, 25)) + 
  guides(color = guide_legend(override.aes = list(size=2))) +
  theme_bw()
graph

## Klasyfikacja i prognozowanie

# Dane uczące
klasyfikacja_prognoza(rf, Dane1_uczace_order)

# Dane testowe
klasyfikacja_prognoza(rf, Dane1_testowe_order)

#### Ważność zmiennych accuracy, gini----

set.seed(123)     # *
rf_all <- randomForest(Class ~ ., data=Dane1_uczace_order, 
                       importance=TRUE)
rf_all

varImpPlot(rf_all, type=1)
varImpPlot(rf_all, type=2)

imp1 <- importance(rf_all, type=1)
imp2 <- importance(rf_all, type=2)
idx <- order(imp1[,1])
nms <- factor(row.names(imp1)[idx], levels=row.names(imp1)[idx])
imp <- data.frame(Predictor = rep(nms, 2),
                  Importance = c(imp1[idx, 1], imp2[idx, 1]),
                  Type = rep( c('Spadek dokładności', 
                                'Spadek wskaźnika Giniego'), 
                              rep(nrow(imp1), 2)))

graph <- ggplot(imp) + 
  geom_point(aes(y=Predictor, x=Importance), size=2, stat="identity") + 
  facet_wrap(~Type, ncol=1, scales="free_x") + 
  theme(
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line(linetype=3, color="darkgray") ) +
  theme_bw()
graph

#iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii


set.seed(123)
options(OutDec=",")


# bagging, boosting, bankructwo------------------------------------------------------------------------------------------------------------------------------
#
DANE3 <- read.csv2('Dane 6_2024.csv')
str(DANE3)
DANE3$Class <- as.factor(DANE3$Class)
DANE3$Sample <- as.factor(DANE3$Sample)
str(DANE3)

levels(DANE3$Class)
DANE3$Class <- ordered(DANE3$Class, levels=c('NB', 'B'))
levels(DANE3$Class)
levels(DANE3$Sample)
DANE3$Sample <- ordered(DANE3$Sample, levels=c('U', 'T'))
levels(DANE3$Sample)
str(DANE3)

## Część ucząca i część testowa

DANE3_uczace <- subset(DANE3,Sample=="U")
DANE3_testowe <- subset(DANE3,Sample=="T")
dim(DANE3_uczace)
dim(DANE3_testowe)
head(DANE3_uczace)
head(DANE3_testowe)
DANE3_uczace <- DANE3_uczace[,-61]
DANE3_testowe <- DANE3_testowe[,-61]
head(DANE3_uczace)
head(DANE3_testowe)
#DANE3 <- DANE3[,-61]
#levels(DANE3$Class)
levels(DANE3_uczace$Class)
levels(DANE3_testowe$Class)
table(DANE3_uczace$Class)
table(DANE3_testowe$Class)

# zmiana numeracji wierszy na ciągłą
DANE3_uczace_order <- DANE3_uczace
DANE3_testowe_order <- DANE3_testowe
row.names(DANE3_uczace_order)=c(1:length(DANE3_uczace_order[,1]))
head(DANE3_uczace_order)
row.names(DANE3_testowe_order)=c(1:length(DANE3_testowe_order[,1]))
head(DANE3_testowe_order)


#### Boosting - Wariant: XGBoost-----------------------------

library(xgboost)

predictors <- data.matrix(DANE3_uczace_order[,-60]) #objasniajace
label_uczace <- as.numeric(DANE3_uczace_order[,'Class']) - 1 # zamiana na 0 i 1 
xgb <- xgboost(data=predictors, label=label_uczace, objective='binary:logistic',
               eval_metric='error', nrounds=100)

### Klasyfikacja i prognozowanie

## Przykład 1 - prob = 0.5

# Dane uczące

DANE3_uczace_order_pred <- predict(xgb, data.matrix(DANE3_uczace_order[,-60]))
DANE3_uczace_order_pred <- as.numeric(DANE3_uczace_order_pred > 0.5)
Tabela_uczace <- table(predicted = DANE3_uczace_order_pred, actual = label_uczace)
Tabela_uczace
Skutecznosc_uczace <- (Tabela_uczace[1]+Tabela_uczace[4])/(Tabela_uczace[1]+Tabela_uczace[2]+Tabela_uczace[3]+Tabela_uczace[4])
Czulosc_uczace <- Tabela_uczace[4]/(Tabela_uczace[3]+Tabela_uczace[4])
Specyficznosc_uczace <- Tabela_uczace[1]/(Tabela_uczace[1]+Tabela_uczace[2])
Skutecznosc_uczace
Czulosc_uczace
Specyficznosc_uczace

# Dane testowe
DANE3_testowe_order_pred <- predict(xgb, data.matrix(DANE3_testowe_order[,-60]))
DANE3_testowe_order_pred <- as.numeric(DANE3_testowe_order_pred > 0.5)
label_testowe <- as.numeric(DANE3_testowe_order[,'Class']) - 1
Tabela_testowe <- table(predicted = DANE3_testowe_order_pred, actual = label_testowe)
Tabela_testowe
Skutecznosc_testowe <- (Tabela_testowe[1]+Tabela_testowe[4])/(Tabela_testowe[1]+Tabela_testowe[2]+Tabela_testowe[3]+Tabela_testowe[4])
Czulosc_testowe <- Tabela_testowe[4]/(Tabela_testowe[3]+Tabela_testowe[4])
Specyficznosc_testowe <- Tabela_testowe[1]/(Tabela_testowe[1]+Tabela_testowe[2])
Skutecznosc_testowe
Czulosc_testowe
Specyficznosc_testowe

## Prezentacja graficzna
library(ggplot2)

# Dane uczące
DANE3_uczace_order_pred <- predict(xgb, data.matrix(DANE3_uczace_order[,-60]))
xgb_df <- cbind(DANE3_uczace_order, pred_default = DANE3_uczace_order_pred > 0.5, 
                prob_default = DANE3_uczace_order_pred)
graph <- ggplot(data=xgb_df, aes(x=Attr25, y=Attr28, 
                                 color=pred_default, shape=pred_default, size=pred_default)) +
  geom_point(alpha=.8) +
  scale_color_manual(values = c('FALSE'='#b8e186', 'TRUE'='#d95f02')) +
  scale_shape_manual(values = c('FALSE'=0, 'TRUE'=1)) +
  scale_size_manual(values = c('FALSE'=0.5, 'TRUE'=2)) +
  scale_x_continuous(expand=c(0, 0), lim=c(-1, 1)) + 
  scale_y_continuous(expand=c(0, 0), lim=c(-5, 25)) + 
  guides(color = guide_legend(override.aes = list(size=2))) +
  theme_bw()
graph

# Dane testowe
DANE3_testowe_order_pred <- predict(xgb, data.matrix(DANE3_testowe_order[,-60]))
xgb_df <- cbind(DANE3_testowe_order, pred_default = DANE3_testowe_order_pred > 0.5, 
                prob_default = DANE3_testowe_order_pred)
graph <- ggplot(data=xgb_df, aes(x=Attr25, y=Attr28, 
                                 color=pred_default, shape=pred_default, size=pred_default)) +
  geom_point(alpha=.8) +
  scale_color_manual(values = c('FALSE'='#b8e186', 'TRUE'='#d95f02')) +
  scale_shape_manual(values = c('FALSE'=0, 'TRUE'=1)) +
  scale_size_manual(values = c('FALSE'=0.5, 'TRUE'=2)) +
  scale_x_continuous(expand=c(0, 0), lim=c(-1, 1)) + 
  scale_y_continuous(expand=c(0, 0), lim=c(-5, 25)) + 
  guides(color = guide_legend(override.aes = list(size=2))) +
  theme_bw()
graph

## Przykład 2 - prob = 0.2

# Dane uczące
DANE3_uczace_order_pred <- predict(xgb, data.matrix(DANE3_uczace_order[,-60]))
DANE3_uczace_order_pred <- as.numeric(DANE3_uczace_order_pred > 0.2)
Tabela_uczace <- table(predicted = DANE3_uczace_order_pred, actual = label_uczace)
Tabela_uczace
Skutecznosc_uczace <- (Tabela_uczace[1]+Tabela_uczace[4])/(Tabela_uczace[1]+Tabela_uczace[2]+Tabela_uczace[3]+Tabela_uczace[4])
Czulosc_uczace <- Tabela_uczace[4]/(Tabela_uczace[3]+Tabela_uczace[4])
Specyficznosc_uczace <- Tabela_uczace[1]/(Tabela_uczace[1]+Tabela_uczace[2])
Skutecznosc_uczace
Czulosc_uczace
Specyficznosc_uczace

# Dane testowe
DANE3_testowe_order_pred <- predict(xgb, data.matrix(DANE3_testowe_order[,-60]))
DANE3_testowe_order_pred <- as.numeric(DANE3_testowe_order_pred > 0.2)
Tabela_testowe <- table(predicted = DANE3_testowe_order_pred, actual = label_testowe)
Tabela_testowe
Skutecznosc_testowe <- (Tabela_testowe[1]+Tabela_testowe[4])/(Tabela_testowe[1]+Tabela_testowe[2]+Tabela_testowe[3]+Tabela_testowe[4])

Czulosc_testowe <- Tabela_testowe[4]/(Tabela_testowe[3]+Tabela_testowe[4])
Specyficznosc_testowe <- Tabela_testowe[1]/(Tabela_testowe[1]+Tabela_testowe[2])
Skutecznosc_testowe
Czulosc_testowe
Specyficznosc_testowe

## Prezentacja graficzna

# Dane uczące
DANE3_uczace_order_pred <- predict(xgb, data.matrix(DANE3_uczace_order[,-60]))
xgb_df <- cbind(DANE3_uczace_order, pred_default = DANE3_uczace_order_pred > 0.2, 
                prob_default = DANE3_uczace_order_pred)
graph <- ggplot(data=xgb_df, aes(x=Attr25, y=Attr28, 
                                 color=pred_default, shape=pred_default, size=pred_default)) +
  geom_point(alpha=.8) +
  scale_color_manual(values = c('FALSE'='#b8e186', 'TRUE'='#d95f02')) +
  scale_shape_manual(values = c('FALSE'=0, 'TRUE'=1)) +
  scale_size_manual(values = c('FALSE'=0.5, 'TRUE'=2)) +
  scale_x_continuous(expand=c(0, 0), lim=c(-1, 1)) + 
  scale_y_continuous(expand=c(0, 0), lim=c(-5, 25)) + 
  guides(color = guide_legend(override.aes = list(size=2))) +
  theme_bw()
graph

# Dane testowe
DANE3_testowe_order_pred <- predict(xgb, data.matrix(DANE3_testowe_order[,-60]))
xgb_df <- cbind(DANE3_testowe_order, pred_default = DANE3_testowe_order_pred > 0.2, 
                prob_default = DANE3_testowe_order_pred)
graph <- ggplot(data=xgb_df, aes(x=Attr25, y=Attr28, 
                                 color=pred_default, shape=pred_default, size=pred_default)) +
  geom_point(alpha=.8) +
  scale_color_manual(values = c('FALSE'='#b8e186', 'TRUE'='#d95f02')) +
  scale_shape_manual(values = c('FALSE'=0, 'TRUE'=1)) +
  scale_size_manual(values = c('FALSE'=0.5, 'TRUE'=2)) +
  scale_x_continuous(expand=c(0, 0), lim=c(-1, 1)) + 
  scale_y_continuous(expand=c(0, 0), lim=c(-5, 25)) + 
  guides(color = guide_legend(override.aes = list(size=2))) +
  theme_bw()
graph


#################################################
#### Bagging - wszystkie drzewa jednczesnie

library(ipred)

# Przygotowanie danych usuniecie uporzadkowania nb i b
class(DANE3_uczace_order$Class) <- "factor"
identical(DANE3_uczace_order$Class, factor(DANE3_uczace_order$Class))
str(DANE3_uczace_order)
levels(DANE3_uczace_order$Class)
class(DANE3_testowe_order$Class) <- "factor"
identical(DANE3_testowe_order$Class, factor(DANE3_testowe_order$Class))
str(DANE3_testowe_order)
levels(DANE3_testowe_order$Class)

bag <- bagging(Class ~ ., data=DANE3_uczace_order, nbagg = 20, coob=TRUE)
bag
bag$err

## Klasyfikacja i prognozowanie

# Dane uczące
DANE3_uczace_order_pred <- predict(bag, DANE3_uczace_order)
levels(DANE3_uczace_order$Class)
levels(DANE3_uczace_order_pred)
Tabela_uczace <- table(predicted = DANE3_uczace_order_pred, actual = DANE3_uczace_order$Class)
Tabela_uczace   # *
Skutecznosc_uczace <- (Tabela_uczace[1]+Tabela_uczace[4])/(Tabela_uczace[1]+Tabela_uczace[2]+Tabela_uczace[3]+Tabela_uczace[4])
Czulosc_uczace <- Tabela_uczace[4]/(Tabela_uczace[3]+Tabela_uczace[4])
Specyficznosc_uczace <- Tabela_uczace[1]/(Tabela_uczace[1]+Tabela_uczace[2])
Skutecznosc_uczace
Czulosc_uczace
Specyficznosc_uczace

# Dane testowe
DANE3_testowe_order_pred <- predict(bag, DANE3_testowe_order)
levels(DANE3_testowe_order$Class)
levels(DANE3_testowe_order_pred)
Tabela_testowe <- table(predicted = DANE3_testowe_order_pred, actual = DANE3_testowe_order$Class)
Tabela_testowe   # *
Skutecznosc_testowe <- (Tabela_testowe[1]+Tabela_testowe[4])/(Tabela_testowe[1]+Tabela_testowe[2]+Tabela_testowe[3]+Tabela_testowe[4])
Czulosc_testowe <- Tabela_testowe[4]/(Tabela_testowe[3]+Tabela_testowe[4])
Specyficznosc_testowe <- Tabela_testowe[1]/(Tabela_testowe[1]+Tabela_testowe[2])
Skutecznosc_testowe
Czulosc_testowe
Specyficznosc_testowe

#### Prezentacja graficzna błędu----------------------
# UWAGA: długo trwa 

# Dla danych uczących
nbagg <- 20                                                #200 np
error_rate <- c(1:nbagg)
for (i in 10:nbagg){
  bag <- bag <- bagging(Class ~ ., data=DANE3_uczace_order, nbag = i, coob=TRUE)
  error_rate[i] <- bag$err
}
error_df = data.frame(error_rate=error_rate, num_trees=1:nbagg)
graph <- ggplot(error_df[10:50,], aes(x=num_trees, y=error_rate)) +
  geom_line()  +
  theme_bw()
graph
print(error_rate[0:20])


