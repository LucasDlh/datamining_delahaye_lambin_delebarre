

# Importation des données

library(readr)
library(ggplot2)
setwd("~/Bureau/DataMining/DataMining")
customers <- read_csv("Données/olist_customers_dataset.csv")
geolocation <- read_csv("Données/olist_geolocation_dataset.csv")
sellers <- read_csv("Données/olist_sellers_dataset.csv")
order_items <- read_csv("Données/olist_order_items_dataset.csv")
products <- read_csv("Données/olist_products_dataset.csv")
orders <- read_csv("Données/olist_orders_dataset.csv")
order_payments <- read_csv("Données/olist_order_payments_dataset.csv")
order_reviews <- read_csv("Données/olist_order_reviews_dataset.csv")
translation_product_category <- read_csv("Données/product_category_name_translation.csv")


#Dans le fichier geolocalisation, certaines villes ont plusieurs lontitude et latitude. Pour faciliter les jointures, nous gardons seulement la dernière longitude et latitude de la geolocalisation de la ville. 

library(dplyr)
geolocationbis <- geolocation %>%
  arrange(geolocation_zip_code_prefix) %>% 
  group_by(geolocation_zip_code_prefix) %>% 
  summarise_all(last)

# Jointure des données

library("data.table")
merge1 <- merge(customers,geolocationbis,by.x="customer_zip_code_prefix",by.y="geolocation_zip_code_prefix",all.x=TRUE)
merge2 <- merge(merge1,orders,by="customer_id",all.x=TRUE)
merge3 <- merge(merge2,order_items,by="order_id",all.x=TRUE)
merge4 <- merge(merge3,products,by="product_id",all.x=TRUE)
merge5 <- merge(merge4,sellers,by="seller_id",all.x=TRUE)
data <- merge(merge5,translation_product_category,by="product_category_name",all.x=TRUE)

#Nous enlevons les produits sans catégorie de produit. En effet, sans la catégorie, nous ne pouvons pas faire un arbre de décision pertinant représentant cette catégorie. Cela représente une perte de 2 402 lignes (2,1% des données). 

library(tidyr)
data <- data %>% drop_na(product_category_name_english)


#Voici, après modification, à quoi ressemble notre base de données (chaque ligne représente un produit vendu) :

head(data)

summary(data)


#Nous allons aussi créer une autre table où chaque ligne représente un produit plutôt qu'un achat avec une colonne pour avoir le nombre de ventes du produit. C'est cette table qui permettra de créer les arbres de décision. 

data_prix <- data %>%
  arrange(product_category_name_english,product_id) %>% 
  group_by(product_category_name_english,product_id) %>% 
  summarise(nb_ventes=n())



# Qu'est-ce qu'un produit qui se vend bien ?

#Afin de créer un arbre de décision prévoyant si un produit va bien se vendre, il faut d'abord déterminer ce qu'est un produit qui se vend bien. Nous allons procéder en 3 étapes :

#1) calcul du prix moyen de chaque produit ;

#2) répartition des produits en 5 catégories (très peu cher, peu cher, prix moyen, cher, très cher) selon leur prix moyen et leur catégorie de produit ;

#3) répartition des produits en 3 catégories (pas bien vendu, moyennement vendu, bien vendu) selon la catégorie de prix et la catégorie de produit. 

#La première étape consiste donc à regrouper chaque produit ensemble pour avoir le prix moyen de vente du produit. En effet, au fil des années, le prix du produit peut varier, il peut aussi y avoir des promotions qui peuvent faire fluctuer le prix. Afin de simplifier l'analyse pour respecter le temps imparti, il est préférable de faire un prix moyen pour chaque produit.

data_prixmoyen <- data %>%
  group_by(product_category_name_english,product_id) %>% 
  summarise(prix_moyen=mean(price))

data_prix <- merge(data_prix,data_prixmoyen[,c("product_id","prix_moyen")],by="product_id",all.x=TRUE)

#Une fois le prix moyen ajouté à la base de données. Nous répartissons les produits en 5 catégories comme expliqués ci-dessus. Nous les répartissons selon la catégorie de produit. En effet, un produit qui est cher dans une catégorie ne les pas forcément dans une autre. Par exemple, dans la catégorie informatique le produit le plus cher peut etre de 2 000 € alors que dans la catégorie produit de beauté le produit le plus cher sera peut-etre de 100€.

#Pour cela, nous utilisons la fonction mutate avec l'option ntile=5. Cela permet de créer 5 catégories de prix en répartissant les produits afin d'avoir quasiment le meme nombre de produit dans chaque catégorie. Il aurait été préférable de regarder pour chaque catégorie de produit la répartition des prix pour adapter les catégories, mais cela prendrait plus de temps. 

for(categorie in unique(data_prix["product_category_name_english"])){
  categ <- data_prix[data_prix$product_category_name_english==categorie,]
  data_prix["categorie_prix"] <- ntile(data_prix$prix_moyen,n=5)
}


#Les catégories sont identifiées par un numéro, la correspondance est la suivante : 
  
#  - 1 = très peu cher,

#- 2 = peu cher,

#- 3 = prix moyen,

#- 4 = cher,

#- 5 = très cher.

#Maintenant, il faut compter le nombre de ventes pour chaque catégorie de produit et chaque catégorie de prix. Cela va permettre de savoir si un produit se vend plus que les autres selon ces catégories. 

data_nb_vente_categ <- data %>%
  arrange(product_category_name_english) %>% 
  group_by(product_category_name_english) %>% 
  summarise(nb_vente_categ=n())


#En comptant le nombre de ventes par catégorie de produit, nous nous rendons compte que certaines ne vendent que très peu de produit. La quantité vendue n'est pas assez importante pour réaliser un arbre. Nous allons donc supprimer les catégories qui représentent moins de 1% des ventes. 

categ_a_garder <- data_nb_vente_categ %>% filter(nb_vente_categ>nrow(data)*0.01)
data_prix <- merge(data_prix,categ_a_garder,by="product_category_name_english")

#Maintenant, pour savoir si le produit se vend bien dans sa catégorie, nous regardons le pourcentage de ventes qu'il représente. 

data_nb_vente_categ_prix <- data_prix %>%
  arrange(product_category_name_english,categorie_prix) %>% 
  group_by(product_category_name_english,categorie_prix) %>% 
  summarise(nb_vente_categ_prix=n())

data_nb_vente_produit <- data_prix %>%
  arrange(product_category_name_english,categorie_prix,product_id) %>% 
  group_by(product_category_name_english,categorie_prix,product_id) %>% 
  summarise(nb_vente_produit=n())

data_proportion_vente <- merge(data_nb_vente_produit,data_nb_vente_categ_prix,by=c("product_category_name_english","categorie_prix"),all.x=TRUE)

data_proportion_vente["proportion_vente"] = data_proportion_vente$nb_vente_produit/data_proportion_vente$nb_vente_categ_prix

#On ajoute ensuite les catégories selon cette proportion de la meme manière que pour la catégorie de prix. 

for(categorie_produit in unique(data_proportion_vente["product_category_name_english"])){
  for(categ_prix in unique(data_proportion_vente["categorie_prix"])){
    categ <- data_proportion_vente %>% filter(product_category_name_english==categorie_produit & categorie_prix==categ_prix)
    data_prix["categorie_vente"] <- ntile(data_proportion_vente$proportion_vente,n=3)
  }
}

#Nous avons donc 3 catégories : 
  
#  - 1 = pas bien vendu,

#- 2 = moyennement vendu,

#- 3 = bien vendu.

#Maintenant que les catégories sont faites, nous allons ajouter des variables qui pourraient etre pertinentes dans la construction de l'arbre de décision. 

# Ajout de variables

#Nous allons ajouter le temps estimé de livraison. En effet, un client pourraît être plus enclin à acheter un produit en ligne s'il est livré dans un jour plutôt que dans un mois. Et, nous allons, comme pour le prix, faire le temps moyen de livraison par produit.

data["delai_de_livraison"] <- as.numeric(difftime(data$order_estimated_delivery_date,data$order_purchase_timestamp),units="days")

data_delaimoyen <- data %>%
  group_by(product_category_name_english,product_id) %>% 
  summarise(delai_moyen_livraison=mean(delai_de_livraison))

data_prix <- merge(data_prix,data_delaimoyen[,c("product_id","delai_moyen_livraison")],by="product_id",all.x=TRUE)


#Nous allons aussi ajouter le nombre de vendeurs pour chaque produit. En effet, il est possible que s'il y a plusieurs types de vendeurs, il y a plus de stock et donc plus de probabilité d'achat. 

data_nb_vendeurs <- data %>%
  group_by(product_category_name_english,product_id) %>% 
  summarise("nb_vendeurs" = n_distinct(seller_id))

data_prix <- merge(data_prix,data_nb_vendeurs[,c("product_id","nb_vendeurs")],by="product_id",all.x=TRUE)


#Pour construire les arbres, il faut maintenant supprimer de la table data_prix, les variables qui ne servaient qu'à la construction des catégories de vente (peu vendu, bien vendu, moyennement vendu). 

data_prix <- data_prix[,c("product_category_name_english","product_id","prix_moyen","delai_moyen_livraison","nb_vendeurs","categorie_vente")]

#Il faut aussi ajouter les variables qui distinguent chaque produit qui peuvent être pertinentes pour la construction de l'arbre (taille du produit, taille de la description du produit, etc.).

data_descr_produit <- data %>% distinct(product_category_name_english,product_id,product_name_lenght,product_description_lenght,product_photos_qty,product_weight_g,product_length_cm,product_height_cm,product_width_cm)

data_prix <- merge(data_prix,data_descr_produit[,c("product_id","product_name_lenght","product_description_lenght","product_photos_qty","product_weight_g","product_length_cm","product_height_cm","product_width_cm")],by="product_id",all.x=TRUE)

# Construction de l'arbre

#Pour construire l'arbre, nous allons répartir notre jeu de données entre un échantillon de test et un échantillon d'entrainement. 


library(rpart)

liste_categ <- unique(data_prix["product_category_name_english"])
liste_arbres <- list()

for(categorie in liste_categ){
  data_prix_categ <- data_prix[data_prix$product_category_name_english==categorie,]
  
  # Suppression de la variable de catégorie
  data_prix_categ <- subset(data_prix_categ, select = -c(product_category_name_english,product_id))
  
  # Définir la taille de l'échantillon d'apprentissage pour chaque catégorie
  smp_size <- floor(0.75 * nrow(data_prix_categ))
  
  # Définir le vecteur de l'apprentissage pour chaque catégorie
  set.seed(245)
  
  train_ind <- sample(seq_len(nrow(data_prix_categ)), size = smp_size)
  
  # Deux tables apprentissage et test pour chaque catégorie
  data_prix_categ.train <- data_prix_categ[train_ind, ]
  data_prix_categ.test <- data_prix_categ[-train_ind, ]
  
  #Construction de l'arbre
  arbre<-rpart(categorie_vente~ ., data= data_prix_categ.train,control=rpart.control(minsplit=1,cp=-1), method="class")
  liste_arbres <- c(liste_arbres,arbre)
}

#### Importation des données
library(dplyr)
library(readr)
setwd("~/Bureau/DataMining/DataMining")
data <- read_csv("Données/data.csv")
data_prix <- read_csv("Données/data_prix.csv")

library(rpart)

#### Création de l'arbre pour la catégorie "bed_bath_table"
data_bed_bath_table <- data_prix[data_prix$product_category_name_english=="bed_bath_table",]

# Suppression de la variable de catégorie
data_bed_bath_table <- subset(data_bed_bath_table, select = -c(product_category_name_english,product_id))

# Définir la taille de l'échantillon d'apprentissage pour chaque catégorie pour garder une proportion équivalente de chaque catégorie
data_bed_bath_table_1 <- data_bed_bath_table[data_bed_bath_table$categorie_vente == 1,]
data_bed_bath_table_2 <- data_bed_bath_table[data_bed_bath_table$categorie_vente == 2,]
data_bed_bath_table_3 <- data_bed_bath_table[data_bed_bath_table$categorie_vente == 3,]

smp_size <- floor(nrow(data_bed_bath_table_3)*0.75)

# Définir le vecteur de l'apprentissage pour chaque catégorie
set.seed(245)

train_ind_1 <- sample(seq_len(nrow(data_bed_bath_table_1)), size = smp_size)
train_ind_2 <- sample(seq_len(nrow(data_bed_bath_table_2)), size = smp_size)
train_ind_3 <- sample(seq_len(nrow(data_bed_bath_table_3)), size = smp_size)

# Deux tables apprentissage et test pour chaque catégorie
data_bed_bath_table.train <- rbind(data_bed_bath_table_1[train_ind_1, ],data_bed_bath_table_2[train_ind_2, ],data_bed_bath_table_3[train_ind_3, ])
data_bed_bath_table.test <- rbind(data_bed_bath_table_1[-train_ind_1, ],data_bed_bath_table_2[-train_ind_2, ],data_bed_bath_table_3[-train_ind_3, ])

# Construction de l'arbre
arbre_bed_bath_table <-rpart(categorie_vente~ ., data= data_bed_bath_table.train,
                             control=rpart.control(minsplit=1,cp=-1), method="class")

# Simplification de l'arbre
arbre_bed_bath_table <-prune(arbre_bed_bath_table, cp= 0.00389408)

# matrice de confusion sur l’apprentissage
mat_conf_train_bed_bath_table <- table(data_bed_bath_table.train$categorie_vente, predict(arbre_bed_bath_table, data_bed_bath_table.train, type="class"))

# matrice de confusion sur le test
mat_conf_test_bed_bath_table <- table(data_bed_bath_table.test$categorie_vente, predict(arbre_bed_bath_table,data_bed_bath_table.test, type='class'))

### Création de l'arbre pour la catégorie "sports_leisure"

data_sports_leisure <- data_prix[data_prix$product_category_name_english=="sports_leisure",]

# Suppression de la variable de catégorie
data_sports_leisure <- subset(data_sports_leisure, select = -c(product_category_name_english,product_id))

# Définir la taille de l'échantillon d'apprentissage pour chaque catégorie pour garder une proportion équivalente de chaque catégorie
data_sports_leisure_1 <- data_sports_leisure[data_sports_leisure$categorie_vente == 1,]
data_sports_leisure_2 <- data_sports_leisure[data_sports_leisure$categorie_vente == 2,]
data_sports_leisure_3 <- data_sports_leisure[data_sports_leisure$categorie_vente == 3,]

smp_size <- floor(nrow(data_sports_leisure_3)*0.75)

# Définir le vecteur de l'apprentissage pour chaque catégorie
set.seed(245)

train_ind_1 <- sample(seq_len(nrow(data_sports_leisure_1)), size = smp_size)
train_ind_2 <- sample(seq_len(nrow(data_sports_leisure_2)), size = smp_size)
train_ind_3 <- sample(seq_len(nrow(data_sports_leisure_3)), size = smp_size)

# Deux tables apprentissage et test pour chaque catégorie
data_sports_leisure.train <- rbind(data_sports_leisure_1[train_ind_1, ],data_sports_leisure_2[train_ind_2, ],data_sports_leisure_3[train_ind_3, ])
data_sports_leisure.test <- rbind(data_sports_leisure_1[-train_ind_1, ],data_sports_leisure_2[-train_ind_2, ],data_sports_leisure_3[-train_ind_3, ])

# Construction de l'arbre
arbre_sports_leisure<-rpart(categorie_vente~ ., data= data_sports_leisure.train,
                            control=rpart.control(minsplit=1,cp=-1), method="class")

# Simplification de l'arbre
arbre_sports_leisure <-prune(arbre_sports_leisure, cp= 0.00493827)

# matrice de confusion sur l’apprentissage
mat_conf_train_sports_leisure <- table(data_sports_leisure.train$categorie_vente, predict(arbre_sports_leisure, data_sports_leisure.train, type="class"))

# matrice de confusion sur le test
mat_conf_test_sports_leisure <- table(data_sports_leisure.test$categorie_vente, predict(arbre_sports_leisure,data_sports_leisure.test, type='class'))

#### Construction de l'arbre pour la catégorie "furniture_decor"

data_furniture_decor <- data_prix[data_prix$product_category_name_english=="furniture_decor",]

# Suppression de la variable de catégorie
data_furniture_decor <- subset(data_furniture_decor, select = -c(product_category_name_english,product_id))

# Définir la taille de l'échantillon d'apprentissage pour chaque catégorie pour garder une proportion équivalente de chaque catégorie
data_furniture_decor_1 <- data_furniture_decor[data_furniture_decor$categorie_vente == 1,]
data_furniture_decor_2 <- data_furniture_decor[data_furniture_decor$categorie_vente == 2,]
data_furniture_decor_3 <- data_furniture_decor[data_furniture_decor$categorie_vente == 3,]

smp_size <- floor(nrow(data_furniture_decor_3)*0.75)

# Définir le vecteur de l'apprentissage pour chaque catégorie
set.seed(245)

train_ind_1 <- sample(seq_len(nrow(data_furniture_decor_1)), size = smp_size)
train_ind_2 <- sample(seq_len(nrow(data_furniture_decor_2)), size = smp_size)
train_ind_3 <- sample(seq_len(nrow(data_furniture_decor_3)), size = smp_size)

# Deux tables apprentissage et test pour chaque catégorie
data_furniture_decor.train <- rbind(data_furniture_decor_1[train_ind_1, ],data_furniture_decor_2[train_ind_2, ],data_furniture_decor_3[train_ind_3, ])
data_furniture_decor.test <- rbind(data_furniture_decor_1[-train_ind_1, ],data_furniture_decor_2[-train_ind_2, ],data_furniture_decor_3[-train_ind_3, ])

# Construction de l'arbre
arbre_furniture_decor<-rpart(categorie_vente~ ., data= data_furniture_decor.train,
                             control=rpart.control(minsplit=1,cp=-1), method="class")
data <- data %>%
  mutate(month = month(order_delivered_customer_date, label = TRUE, abbr = TRUE))
# Simplification de l'arbre
arbre_furniture_decor <-prune(arbre_furniture_decor, cp= 0.00448718)

# matrice de confusion sur l’apprentissage
mat_conf_test_furniture_decor <- table(data_furniture_decor.train$categorie_vente, predict(arbre_furniture_decor, data_furniture_decor.train, type="class"))

# matrice de confusion sur le test
mat_conf_train_furniture_decor <- table(data_furniture_decor.test$categorie_vente, predict(arbre_furniture_decor,data_furniture_decor.test, type='class'))

#### Code pour l'histogramme du nombre de produit par vente
# Création de la table pour avoir seulement la catégorie de produit et l'id de la commande
data_transaction <- data[,c("order_id","product_category_name_english")]

transactions_nb_item <- data_transaction %>%
  group_by(order_id) %>% 
  summarise(nb_ventes=n())

library(lubridate)
data <- data %>%
  mutate(month = floor_date(order_delivered_customer_date, "month"))


price_delai_cat <- data_prix %>%
  group_by(product_category_name_english) %>%
  summarize(price = mean(prix_moyen, na.rm = TRUE),
            delai = mean(delai_moyen_livraison, na.rm = TRUE))

monthly_data <- data %>%
  group_by(month) %>%
  summarize(monthly_price_sum = sum(price, na.rm = TRUE))


library(corrplot)
data_quant <- data %>% select_if(is.numeric)

# Calculer la matrice de corrélation
cor_matrix <- cor(data_quant, use = "complete.obs")




library(shiny)
library(rpart)
library(rpart.plot)
library(shiny)

ui <- fluidPage(
  titlePanel("Analyse des Données"),
  
  sidebarLayout(
    sidebarPanel(
      # Options de sélection pour les graphiques
      #selectInput("var", "Variable à visualiser", choices = names(data)),
      #selectInput("var_prix", "Variable de prix", choices = names(data_prix))
      selectInput("category", "Select Category:", choices = c("Furniture_Decor", "Sports_Leisure", "Bed_Bath_Table"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Résumé", 
                 tableOutput("summary"),
                 tableOutput("headdata_prix"),
                 h2("Base de données initiale"),
                 tableOutput("data_table"),
                 h3("Qu'est qu'un produit qui se vend bien ?"),
                 p("Afin de créer un arbre de décision qui prévoit si un produit va bien se vendre, il faut d'abord déterminer ce qu'est un produit qui se vend bien. Nous avons procéder en 3 étapes :"),
                 tags$ul(
                   tags$li("Stockage des données dans une table en les regroupant par la catégorie et l'id du produit en comptant le nombre de ventes"),
                   tags$li("Calcul du prix moyen de chaque produit"),
                   tags$li("Répartition des produits en 5 catégories (très peu cher, peu cher, prix moyen, cher, très cher) selon leur prix moyen et leur catégorie de produit"),
                   tags$li("Répartition des produits en 3 catégories (pas bien vendu, moyennement vendu, bien vendu) selon la catégorie de prix et la catégorie de produit. "),
                 ),
                 h3("Ajout de variables"),
                 p("Nous avons rajouter 2 variables avant la création de l'arbre :"),
                 tags$ul(
                   tags$li("Temps estimé moyen de livraison : un client pourraît être plus enclin à 
                acheter un produit en ligne s'il est livré dans un jour plutôt que dans un mois."),
                   tags$li("Nombre de vendeurs pour chaque produit : il est possible que s'il y a plusieurs types de vendeurs, il y a plus de stock et donc plus de probabilité d'achat."),
                 ),
                 plotOutput("corplot")
                 ### CHANGEMENT
                 ),
        tabPanel("Graphiques",
                 plotOutput("distPlot"),
                 plotOutput("trendPlot"),
                 plotOutput("boxPlot")),
        tabPanel("Analyse Avancée",
                 plotOutput("correlationPlot"),
                 plotOutput("treePlot"),
                 h2("Arbre de décisions"),
                 p("Dans la limite de temps, nous n'avons créer des arbres que pour les catégories de produit
        pour lesquelles les 3 catégories de vente sont représentées et qui sont dans le top 3 des catégories
        ayant le plus de vente."),
                 h3("Etape de la création de l'arbre :"),
                 tags$ul(
                   tags$li("Création d'un échantillon de test et d'apprentissage, l'échantillon d'apprentissage 
                contient chaque catégorie de vente de manière proportionnelle pour favoriser
                 l'apprentissage"),
                   tags$li("Construction de l'arbre"),
                   tags$li("Simplification de l'arbre avec la 1-SE rule"),
                   tags$li("Vérification avec la matrice de confusion")
                 ))
      )
    )
  )
  ,
)

# Serveur
server <- function(input, output) {
  
  output$summary <- renderTable({
    head(data)
  })
  
  output$headdata_prix <- renderTable({
    head(data_prix)
  })

  output$corplot <- renderPlot({
    corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")
  })
  
  output$distPlot <- renderPlot({
    ggplot(data, aes(x = price)) + 
      geom_histogram(binwidth = 20, fill = "blue", color = "black") + 
      theme_minimal() + xlim(0, 1000)+
      labs(title = "Nombre de produit par intervalle de prix") 
  })
  
  

  
  output$trendPlot <- renderPlot({
    ggplot(monthly_data, aes(x = month, y = monthly_price_sum)) + 
      geom_col(fill = "pink", color = "black") + 
      theme_minimal() + 
      labs(title = "Ventes par Mois", x = "Mois", y = "Ventes") 
  })
  

  
  output$boxPlot <- renderPlot({
    ggplot(data_prix, aes_string(x = "product_category_name_english", y = "prix_moyen")) + geom_boxplot() + theme_minimal() + ylim(0, 2000)
  }) 
  
  output$correlationPlot <- renderPlot({
    ggplot(price_delai_cat, aes_string(x = "price", y = "delai",color="product_category_name_english")) + geom_point() + geom_smooth(method = "lm") + theme_minimal()
 
  })
  
  output$treePlot <- renderPlot({
    category <- input$category
    
    if (category == "Furniture_Decor") {
      rpart.plot(arbre_furniture_decor)
    } else if (category == "Sports_Leisure") {
      rpart.plot(arbre_sports_leisure)
    } else if (category == "Bed_Bath_Table") {
      rpart.plot(arbre_bed_bath_table)
    }
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)





