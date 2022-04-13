library(tidyverse)
library(MatchIt)
library(glmnet)
library(randomForest)
library("lmtest") #coeftest
library("sandwich") #vcovCL
# Ci dessous une fonction qui test plusieurs modèles de matching
# Elle résume pour chaques modèles l'effet,p-value et R2.


formule_data_lm<-function(data.frame,traitement){
  #Transforme en formule les noms des colonnes d'un dataframe à partir d'une colonne
  #print(typeof(traitement))
  e1<-paste(names(data.frame))
  #print(e1)
  #print(e1[1])
  formule<-paste("")
  #print(formule)
  # On ajoute la target
  for (i in 1:length(e1)){
    #print(i)
    #print(e1[i]==traitement )
    if(e1[i]==traitement ){
      #print(i)
      formule<-str_trim(paste(formule,e1[i]))
      num <- i
    }
  }
  e1<- e1[-num]#on le retire
  # On ajoute les éléments
  formule<-paste0(formule," ~")
  paste(e1)
  # On ajoute la target
  for (i in 1:length(e1)){
    #print(i)
    formule<-str_trim(paste(formule,e1[i]))
    if(i!=length(e1)){
      formule<-str_trim(paste(formule,"+"))
    }
  }
  return(formule)
}

formule_data_matchit<-function(data.frame,traitement,variable_interet){
  #Transforme en formule les noms des colonnes d'un dataframe à partir d'une colonne variante metchit
  #print(typeof(traitement))
  e1<-paste(names(data.frame))
  #print(e1)
  #print(e1[1])
  formule<-paste("")
  #print(formule)
  # On ajoute la target
  for (i in 1:length(e1)){
    #print(i)
    #print(e1[i]==traitement )
    if(e1[i]==traitement ){
      #print(i)
      formule<-str_trim(paste(formule,e1[i]))
      num <- i
    }
  }
  e1<- e1[-num]#on le retire
  for (i in 1:length(e1)){
    if(e1[i]==variable_interet ){
      #print(i)
      num2 <- i
    }
  }
  #print(e1)
  e1<- e1[-num2]#on le retire
  #print(num2)
  #print(e1)
  # On ajoute les éléments
  formule<-paste0(formule," ~")
  paste(e1)
  # On ajoute la target
  for (i in 1:length(e1)){
    #print(i)
    formule<-str_trim(paste(formule,e1[i]))
    if(i!=length(e1)){
      formule<-str_trim(paste(formule,"+"))
    }
  }
  return(formule)
}


MetamatchingV2<-function(data.frame,traitement,variable_interet,ordre=TRUE,filtre=TRUE){
  print(paste('Le dataset contient',dim(data.frame)[1],'individus.'))
  liste_caliper<-seq(0.01,0.3,by=0.01)
  #liste_caliper<-0.05
  liste_distance<-list("glm","lasso", "ridge", "elasticnet","randomforest")
  #liste_distance<-("glm")
  formule_matchit<-formule_data_matchit(data.frame,traitement,variable_interet)
  #print(formule_matchit)
  formule_lm<-formule_data_lm(data.frame,variable_interet)
  #print(formule_lm)
  synthese_distance <- c()
  synthese_caliper<- c()
  synthese_effet<- c()
  synthese_R2<- c()
  synthese_pvalue<- c()
  synthese_nombre_match<- c()
  for (j in 1:length(liste_distance)) {
    for (i in 1:length(liste_caliper)) {
      #print(paste("essai :",i,",distance : ",liste_distance[j],",caliper :",liste_caliper[i]))
      m.out <- matchit(as.formula(formule_matchit) , data = data.frame,caliper=as.numeric(liste_caliper[i]), method = "nearest", distance = toString(liste_distance[j]))
      m.data1 <- match.data(m.out)
      fit1 <- lm(as.formula(formule_lm) , data = m.data1, weights = weights)
      result<-coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
      R2<-summary(fit1)$r.squared
      #print(paste("R2 =",R2))
      #print(result)
      effet_traitement<-result[2,1]
      #print(paste("effet =",effet_traitement))
      p_value_traitement<-result[2,4]
      #print(paste("p_value =",p_value_traitement))
      nombre_match<-(dim(m.data1)[1])/2
      #print(" ")
      synthese_distance <- append( synthese_distance,toString(liste_distance[j] ))
      #print(synthese_distance)
      synthese_caliper <- append( synthese_caliper , liste_caliper[i] )
      #print(synthese_caliper)
      synthese_effet <- append( synthese_effet ,effet_traitement )
      synthese_R2 <- append( synthese_R2 ,R2 )
      synthese_pvalue <- append( synthese_pvalue , p_value_traitement )
      synthese_nombre_match<- append( synthese_nombre_match ,  nombre_match)
      
    }
    
  }
  # for (i in 1:length(liste_caliper)) {
  #   print(paste("essai :",i+10))
  #   m.out <- matchit( as.formula("data.frame$treat~data.frame$age+data.frame$educ+data.frame$married"), data = data.frame, caliper=as.numeric(liste_caliper[i]),distance ="ridge")
  #   m.data1 <- match.data(m.out)
  #   fit1 <- lm(re78 ~ ., data = m.data1, weights = weights)
  #   coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
  #   R2<-summary(fit1)$r.squared
  #   print(paste("R2 =",R2))
  # }
  # for (i in 1:length(liste_caliper)) {
  #   print(paste("essai :",i+20))
  #   m.out <- matchit(treat ~ ., data = data.frame, caliper=as.numeric(liste_caliper[i]),distance ="randomforest")
  #   m.data1 <- match.data(m.out)
  #   fit1 <- lm(re78 ~ ., data = m.data1, weights = weights)
  #   coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
  #   R2<-summary(fit1)$r.squared
  #   print(paste("R2 =",R2))
  # }
  # print(mean(lalonde[,traitement]))
  #print(synthese_distance)
  #print(synthese_caliper)
  #print(synthese_effet)
  synthese <- data.frame(synthese_distance, synthese_caliper,synthese_effet,synthese_R2,synthese_pvalue,synthese_nombre_match)
  colnames(synthese) <- c("distance", "caliper","effet_ATT","R2","p_value","nombre_match")
  if(ordre){synthese<-synthese[order(synthese$p_value),] }
  print(paste("le métamatching a trouvé",nrow(subset(synthese, p_value<0.05)),"modèles avec une p_value inférieur à 0.05 sur",dim(synthese)[1],"modèles."))
  if(filtre){synthese <- synthese[synthese$p_value<=0.05,]}
  try(print(paste('ATT moyen :',mean(synthese$effet_ATT))))
  try(if(sign(mean(synthese$effet_ATT))==1){print("L impact est positif.")})
  try(if(sign(mean(synthese$effet_ATT))==0){print("L impact est négatif.")})
  return(synthese)
}


MetamatchingV2(lalonde,"treat","re78")


