library(arules)
library(arulesViz)

market_basket<-read.transactions(
  
  file='market_basket.csv',
  sep=',',
  quote="",
  format = 'basket',
  rm.duplicates = TRUE,
  skip=1
)

summary(market_basket)
class(market_basket)

#Compute the number of items that were purchased : 
18441 * 22347 * 0.0009914608

library(dplyr)
market_basket %>% head(n = 5) %>% inspect

itemFrequencyPlot(x     = market_basket,
                  topN  = 10,
                  # support =
                  type  = 'absolute',
                  horiz = TRUE
)

#b) Building 1st set of association rules

apriori(market_basket, parameter = list(support  = 0.005, confidence = 0.8)) -> rule_1

inspect(head(sort(rule_1, by = 'confidence'),5))
inspect(tail(sort(rule_1, by = 'confidence'),5))
inspect(head(sort(rule_1, by = 'lift'),5))
 
plot(rule_1, method = 'scatterplot')

#c) building 2nd set of association rules

apriori(market_basket, parameter = list(support = 0.009 , confidence = 0.3)) -> rule_2

inspect(head(sort(rule_2 , by = 'confidence'),5))
inspect(tail(sort(rule_2 , by = 'confidence'),5))

plot(rule_2, method = 'scatterplot')

#d) Building 3rd set of association rule 

apriori(market_basket, parameter = list(support = 0.02 , confidence = 0.5)) -> rule_3

inspect(head(sort(rule_3 , by = 'support'),5))
inspect(tail(sort(rule_3, by = 'support'),5))

plot(rule_3, method = 'scatterplot')
