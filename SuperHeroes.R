library("jsonlite")
library(tidyverse)
library(naniar)
library(Amelia)

# Allocation d'un dataframe avec un premier superhéro pour l'utilisation de rbind dans la boucle
superheroes_raw_data = fromJSON("https://superheroapi.com/api/863227500790928/1")

for (x in c(2:731)) {
  # Construction de l'URL associée à un superhéro dont l'id est contenue dans x
  url_req <- paste0("https://superheroapi.com/api/863227500790928/",as.character(x))
  #Ajout de l'individu récupéré dans le dataframe crée
  superheroes_raw_data <- rbind(superheroes_raw_data,fromJSON(url_req))
}

#Convertion en tibble
superheroes_raw_data = as_tibble(superheroes_raw_data)

#Nettoyage du tibble (en créant un nouveau dataframe pour conserver le dataframe de base)
  #Conversion des colonnes listes en créant une colonne pour chaque élément de la liste
superheroes_raw_data <- superheroes_raw_data %>% select(-response) %>% unnest(c(id,name)) %>% unnest_wider(powerstats) %>% unnest_wider(biography) %>% unnest_wider(appearance) %>% unnest_wider(work) %>% unnest_wider(connections) %>% unnest_wider(image)
  #Suppression de colonnes avec beaucoup de valeurs manquantes à première vue ou avec des valeurs non observables + réordonnance des positions des colonnes
superheroes_data <- superheroes_raw_data %>% select(-id,-aliases,-'alter-egos',-'first-appearance',-'relatives',-occupation) %>% select(name,'full-name',everything())
  #Conversion des valeurs des colonnes weight et height en kg et cm uniquement (supprimant les valeurs en inch et lbs)
superheroes_data <- superheroes_data %>% unnest_wider(height) %>% rename('height-inch'=...1,'height-cm'=...2) %>% unnest_wider(weight) %>% rename('weight-lb'=...1,'weight-kg'=...2) %>% select(-'height-inch',-'weight-lb')
  #Conversion des types des colonnes weight et height de character à numeric
superheroes_data <- type_convert(superheroes_data, cols('weight-kg'=col_number(),'height-cm'=col_number()))

#Nettoyage des valeurs manquantes
  #On remplace toutes les valeurs nulles ou manquantes par NA
na_strings <- c("null","-")
superheroes_data <- superheroes_data %>% replace_with_na_all(condition = ~.x %in% na_strings) %>% replace_with_na(replace = list('height-cm' = 0,'weight-kg' = 0))
  #Compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(superheroes_data,function(x) sum(is.na(x)))
  #Affichage d'un graphe avec les valeurs manquantes vs les valeurs observées
missmap(superheroes_data, main = "Valeurs manquantes vs Valeurs observées")
  #Suppression des colonnes contenant le plus de valeurs manquantes
superheroes_data <- superheroes_data %>% select(-'place-of-birth',-base)
  #Suppression des lignes comportant des NA (nb obs sans enlever les colonnes: 102 obs. -> en enlevant: 260 obs. )
superheroes_sans_NA <- na.omit(superheroes_data)

