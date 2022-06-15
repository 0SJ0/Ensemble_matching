# Metamatching

Ce projet "métamatching" a pour objectif d'estimer l'effet causal d'un traitement en se basant sur plusieurs modèles de matchings. La probabilité que l'individu reçoit le traitement peut être calculée à partir de différents modèles de régressions ( logistiques ou régularisés). L'effet causal est calculé en prenant en compte l'aggrégation des modèles utilisées.

Notre vision du matching est basé sur les travaux de Caliendo et Kopeining. Le matching est une méthode statististique d'évaluation d'impact. Cette méthode permet d'évaluer l'effet causal d'un traitement en comparant des individus traités et non-traités ayant des caractéristiques observables similaires.

![Cover](https://github.com/0SJ0/Images/blob/main/Implementation_matching.png)

 Caliendo et Kopeining (2005) proposent cette méthodologie pour implémenter le matching.
 
 Notre objetif avec le métamatching est de compiler plusieurs modèles de matching pour construire une estimation de l'effet causal la plus robuste possible. Pour cela, le programme de métamatching teste plusieurs modèles avec plusieurs méthodes de calculs des scores de propension et d'appariement.
 
 ![Cover](https://github.com/0SJ0/Images/blob/main/Metamatching.png)
 
 Une fois les modèles calculés, nous filtrons nos modèles par rapport à la p-value de l'effet causal (ATT) et à R2.
 

Lien du déploiement :
https://share.streamlit.io/0sj0/metamatching_app/main/Metamatching_app.py
