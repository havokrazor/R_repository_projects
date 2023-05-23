read.csv('pokemon.csv', stringsAsFactors = T) -> pokemon
library(dplyr)
View(pokemon)
str(pokemon)
sum(is.na(pokemon))

max(pokemon$Speed)
min(pokemon$Speed)


#selecting a pokemon

filter(pokemon, Type.1 == 'Grass' & Type.2 == 'Poison' & Speed == 180) -> a1

filter(pokemon, Type.1 == 'Water' & Type.2 == 'Flying' & Speed ==180) -> b1

filter(pokemon, Type.1 == 'Fire' & Type.2 == 'Psychic' & Speed == 180) -> c1

#Attack  vs Defence

library(caTools)

sample.split(pokemon$Attack , SplitRatio = 0.70) -> pokemon_split
subset(pokemon, pokemon_split == T) -> train_poke
subset(pokemon, pokemon_split == F) -> test_poke

lm(Attack~Defense , data = train_poke) -> model_poke1
summary(model_poke1)

predict(model_poke1 , newdata = test_poke) -> predict_poke1

cbind(Actual = test_poke$Attack , Predicted = predict_poke1) -> poke_Data1
class(poke_Data1)
as.data.frame(poke_Data1) -> poke_Data1

poke_Data1$Actual - poke_Data1$Predicted -> poke_error

cbind(poke_Data1, poke_error) -> poke_Data1

sqrt(mean(poke_Data1$poke_error)^2) ->rmse1_poke
rmse1_poke

#legendary  or not
library(rpart)


rpart(Legendary~. , data = train_poke) -> poke_tree
predict(poke_tree,newdata = test_poke , type = 'class') -> treepoke_result

table(test_poke$Legendary, treepoke_result)->cm1
cm1
sum(diag(cm1))/sum(cm1)->acc
acc
