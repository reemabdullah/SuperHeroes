library("jsonlite")
library(tidyverse)
library(naniar)

# Allocation d'un dataframe avec un premier superhéro pour l'utilisation de rbind dans la boucle
superheroes_data = fromJSON("https://superheroapi.com/api/863227500790928/1")

for (x in c(2:731)) {
  # Construction de l'URL associée à un superhéro dont l'id est contenue dans x
  url_req <- paste0("https://superheroapi.com/api/863227500790928/",as.character(x))
  #Ajout de l'individu récupéré dans le dataframe crée
  superheroes_data <- rbind(superheroes_data,fromJSON(url_req))
}

#Convertion en tibble
superheroes_data = as_tibble(superheroes_data)

#Nettoyage du tibble 
  #Conversion des colonnes listes en créant une colonne pour chaque élément de la liste
superheroes_data <- superheroes_data %>% select(-response) %>% unnest(c(id,name)) %>% unnest_wider(powerstats) %>% unnest_wider(biography) %>% unnest_wider(appearance) %>% unnest_wider(work) %>% unnest_wider(connections) %>% unnest_wider(image)
  #Suppression de colonnes inutiles + réordonnance des positions des colonnes
superheroes_data <- superheroes_data %>% select(-id,-aliases,-'alter-egos',-'first-appearance',-'relatives') %>% select(name,'full-name',everything())
  #Conversion des valeurs des colonnes weight et height en kg et cm uniquement (supprimant les valeurs en inch et lbs)
superheroes_data <- superheroes_data %>% unnest_wider(height) %>% rename('height-inch'=...1,'height-cm'=...2) %>% unnest_wider(weight) %>% rename('weight-lb'=...1,'weight-kg'=...2) %>% select(-'height-inch',-'weight-lb')
  #Conversion des types des colonnes weight et height de character à numeric
superheroes_data <- type_convert(superheroes_data, cols('weight-kg'=col_number(),'height-cm'=col_number()))

#Nettoyage des valeurs manquantes 
  #On remplace toutes les valeurs nulles ou manquantes par NA
na_strings <- c("null","-")
superheroes_data <- superheroes_data %>% replace_with_na_all(condition = ~.x %in% na_strings) %>% replace_with_na(replace = list('height-cm' = 0,'weight-kg' = 0))
  #Suppression des lignes comportant des NA
superheroes_sans_NA <- na.omit(superheroes_data)

  #On crée un dataframe pour les Superheros du monde Marvel
marvel_data <- superheroes_sans_NA %>% filter(publisher=='Marvel Comics') %>% select(-publisher)

  #On crée un dataframe pour les Superheros du monde DC 
dc_data <- superheroes_sans_NA %>% filter(publisher=='DC Comics') %>% select(-publisher)




