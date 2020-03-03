library("jsonlite")
library(tidyverse)
library(naniar)
library(Amelia)
library(rvest)
library(polite)

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
superheroes_data <- superheroes_raw_data %>% select(-id,-aliases,-'alter-egos',-'first-appearance',-'relatives',-occupation,-'group-affiliation') %>% select(name,'full-name',everything())
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
  #Suppression des lignes comportant des NA
superheroes_sans_NA <- na.omit(superheroes_data)


  #---------Webscrapping--------

#Création manuellement d'une liste de tout les identifiants des personnages de notre dataset
characters = list('a-bomb/10-10060/','abe-sapien/10-956/','abomination/10-1/','absorbing-man/10-1448/','adam-strange/10-626/','agent-bob/10-10255/','ajax/10-10422/','alfred-pennyworth/10-628/','angel-dust/10-10405/','animal-man/10-632/',
                  'ant-man/10-857/','ant-man-ii/10-166/','apocalypse/10-852/','aqualad/10-1395/','aquaman/10-634/','arachne/10-882/','archangel/10-838/','ardina/10-10132/','atlas/10-878/','atom-ii/10-938/',
                  'aurora/10-1289/','banshee/10-26/','batgirl/10-1111/','batgirl-iv/10-1113/','batman/10-10441/','batman/10-639/','batman-ii/10-1496/','beast-fox/10-13906/','beast-boy/10-640/','big-barda/10-1169/',
                  'bishop/10-31/','bizarro/10-642/','black-bolt/10-243/','black-canary/10-1005/','black-canary/10-644/','black-cat/10-32/','black-knight-iii/10-246/','black-manta/10-10546/','black-panther/10-247/','black-widow/10-248/',
                  'blade/10-33/','blink/10-35/','bloodaxe/10-1509/','boom-boom/10-400/','booster-gold/10-647/','brainiac/10-648/','buffy/10-10430/','bullseye/10-624/','bumblebee/10-1542/','cable/10-40/',
                  'captain-america/10-274/','captain-atom/10-1007/','captain-britain/10-276/','captain-marvel-kingdom-come/10-13436/','captain-marvel/10-103/','captain-marvel-ii/10-279/','carnage/10-187/','catwoman/10-659/','chamber/10-44/','cheetah/10-661/',
                  'cheetah-ii/10-925/','cheetah-iii/10-926/','citizen-steel-cw/10-15408/','clock-king/10-10396/','colossus/10-48/','copycat/10-10423/','cottonmouth/10-10452/','crystal/10-305/','cyborg/10-1204/','cyclops/10-50/',
                  'daredevil/10-52/','darkhawk/10-53/','darkseid/10-668/','darkstar/10-169/','darth-vader/10-10444/','dash/10-824/','dazzler/10-1043/','deadman/10-1472/','deadpool/10-835/','deadshot/10-670/',
                  'deathlok/10-1304/','deathstroke/10-672/','diamondback-mcu/10-13956/','doc-samson/10-54/','doctor-doom/10-189/','doctor-fate/10-676/','doctor-octopus/10-622/','doctor-strange-classic/10-55/','domino/10-902/','doomsday/10-679/',
                  'drax-the-destroyer/10-10016/','elastigirl/10-870/','electro/10-7/','elektra/10-625/','enchantress/10-687/','etrigan/10-10429/','evil-deadpool/10-10090/','evilhawk/10-1516/','exodus/10-1517/','falcon/10-56/',
                  'firestar/10-60/','firestorm/10-10395/','flash/10-891/','flash-pre-crisis/10-17442/','flash-iv/10-893/','franklin-richards/10-63/','galactus-herald-supreme/10-17443/','gambit/10-64/','gamora/10-65/','ghost-rider/10-67/',
                  'gladiator/10-1521/','gorilla-grodd/10-694/','gravity/10-74/','green-arrow/10-696/','green-goblin/10-579/','green-lantern/10-700/','green-lantern/10-697/','han-solo/10-10456/','harley-quinn/10-701/','havok/10-77/',
                  'hawkeye/10-73/','hawkeye-ii/10-10043/','heat-wave/10-705/','hellboy/10-813/','hellcat/10-79/','hercules/10-78/','hulk/10-83/','human-torch/10-362/','husk/10-84/','hybrid/10-10493/',
                  'hyperion/10-1286/','iceman/10-816/','impulse/10-895/','ink/10-10337/','invisible-woman/10-620/','iron-fist/10-1120/','iron-man/10-85/','jack-of-hearts/10-86/','jack-jack/10-871/','james-t-kirk/10-10565/',
                  'jean-grey/10-814/','jessica-jones/10-10403/','green-lantern/10-873/','joker/10-719/','jubilee/10-87/','jubilee/10-87/','juggernaut/10-826/','justice/10-88/','killer-croc/10-723/','kingpin/10-623/','klaw/10-1539/',
                  'kraven-ii/10-1034/','kraven-the-hunter/10-1033/','krypto/10-725/','green-lantern/10-1409/','lady-deathstrike/10-810/','legion/10-10651/','lex-luthor/10-727/','lizard/10-11/','lobo/10-1127/','loki/10-928/','luke-cage/10-269/',
                  'luke-skywalker/10-10447/','magneto/10-12/','magneto/10-12/','martian-manhunter/10-733/','medusa/10-96/','mera/10-1388/','metallo/10-737/','mister-fantastic/10-408/','mister-terrific-cw/10-15407/','mockingbird/10-1328/',
                  'modok/10-10438/','moon-knight/10-415/','mr-immortal/10-10794/','mr-incredible/10-869/','mysterio/10-1039/','mystique/10-817/','namor/10-137/','nick-fury/10-326/','nightwing/10-851/','nova/10-109/','nova/10-10313/',
                  'odin/10-10388/','onslaught/10-13/','oracle/10-749/','penguin/10-753/','phoenix/10-828/','plantman/10-10487/','plastic-man/10-756/','poison-ivy/10-757/','polaris/10-1046/','power-girl/10-758/','professor-x/10-113/',
                  'professor-zoom/10-766/','psylocke/10-114/','punisher/10-112/','purple-man/10-10451/','question/10-760/','quicksilver/10-115/','ras-al-ghul-dark-knight/10-13879/','raven/10-764/','ray/10-10204/','red-arrow/10-1013/','red-hood/10-932/',
                  'red-hulk/10-1342/','red-robin/10-10009/','red-tornado/10-1149/','rhino/10-15/','robin/10-850/','robin-ii/10-849/','robin-iii/10-769/','robin-v/10-1473/','rocket-raccoon/10-10010/','ronin/10-1202/','rorschach/10-771/','sabretooth/10-17/',
                  'sandman/10-621/','scarecrow/10-819/','scarlet-spider/10-174/','scarlet-spider-ii/10-1536/','scarlet-witch/10-444/','scorpion/10-1041/','sentry/10-447/','shadow-lass/10-1195/','shadowcat/10-122/','shang-chi/10-454/','she-hulk/10-125/',
                  'she-thing/10-461/','shocker/10-19/','sif/10-1533/','silver-surfer/10-127/','sinestro/10-781/','siren/10-1387/','solomon-grundy/10-782/','spawn/10-842/','spider-girl/10-480/','spider-gwen/10-10507/','spider-man/10-133/','spider-woman/10-481/',
                  'spock/10-10566/','spyke/10-1455/','star-lord/10-10015/','starfire/10-786/','stargirl/10-1326/','static/10-1471/','storm-fox/10-14356/','sunspot/10-138/','superboy-prime/10-1535/','supergirl/10-790/',
                  'superman-1943/10-13081/','swarm/10-10428/','taskmaster/10-21/','thanos/10-1305/','the-comedian/10-1062/','thing/10-139/','thor/10-140/','thor-girl/10-1296/','tiger-shark/10-10047/','toad/10-1125/','toxin/10-848/',
                  'toxin/10-10494/','triton/10-513/','venom/10-22/','venom-iii/10-1042/','vibe/10-10509/','vindicator/10-150/','violet-parr/10-825/','vision/10-532/','vixen/10-10109/','vulture/10-1031/',
                  'walrus/10-10058/','war-machine/10-536/','warpath/10-541/','wasp-emh/10-14321/','winter-soldier/10-10027/','wolverine/10-161/','wonder-woman/10-807/','x-23/10-565/','yellowjacket/10-70/',
                  'yellowjacket-ii/10-178/','yoda/10-10454/','zatanna/10-809/')

# Avant de commencer on montre nos intentions au site pour être poli
superherodb_bow <- bow(
  url = "https://www.superherodb.com/",  # base URL
  user_agent = "Double R",  # identify ourselves
  force = TRUE
)

#Vérifions le résultat (results : the path is scrapable for this user)
superherodb_bow

#On crée une fonction 
classData <- function(n, bow = superherodb_bow) {
  # Faire un nod à chaque itération d'un vecteur d'id de superhéros
  session <- nod(
    bow = bow,
    path = n
  )
  # Scraper la page du nouveau path
  scraped_page <- scrape(session)
  # Rechercher la portion contenant la classe 
  # (Classification des superhéros en se basant sur leur force)
  class_html <- html_nodes(scraped_page, ".class")
  # Convertion en integer du résultat
  class <- as.integer(html_text(class_html))
  return(class)
}

#On créer un vecteur contenant le résultat des classes de chaque héro
scrap = vector()
for (i in characters) {
  scrap = c(scrap,classData(i))
}

#On rajoute le vecteur dans une colonne qu'on rajoute à notre dataset
col_class = tibble(class = scrap) 
superheroes_final_data <- bind_cols(superheroes_sans_NA,col_class)
superheroes_final_data <- superheroes_final_data %>% select(name,'full-name',class,everything())

#On sauvegarde notre dataset dans un fichier csv
write_csv(superheroes_final_data, path = "superheroes.csv")
