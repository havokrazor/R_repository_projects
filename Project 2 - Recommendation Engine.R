#loading the dataset
read.csv('books.csv') -> books
read.csv('book_tags.csv') -> book_tags
read.csv('tags.csv') -> tags
read.csv('ratings.csv') -> ratings

View(books)
View(books_tags)
View(tags)
View(ratings)

str(ratings)
summary(ratings)

library(dplyr)
library(recommenderlab)
library(data.table)
library(tidyr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)

sapply(ratings, function(user_id) sum(is.na(user_id)))

#1) Data cleaning 
#Finding duplicate ratings

ratings %>% group_by(user_id, book_id) %>% mutate(N=n()) -> ratings
table(ratings$N)
ratings %>% filter(N>1) -> duplicate_ratings  

#Remove duplicate ratings
ratings %>% filter(N==1) -> ratings

#Removing users who rated fewer than 3 books

ratings %>% group_by(user_id) %>% mutate(Ratings_Given = n()) -> ratings
ratings %>% filter(Ratings_Given>2) -> ratings

#2) data exploration 
set.seed(1)

#a
0.02 -> user_fraction
unique(ratings$user_id)-> users
length(users)
sample(users, round(user_fraction  * length(users))) -> sample_users
length(sample_users)

ratings%>%filter(user_id %in% sample_users) -> ratings

#b
library(ggplot2)
ratings%>%ggplot(aes(x = rating , fill = factor(rating))) + geom_bar(col = 'green') + scale_fill_brewer(palette ='YlGnBu') + guides(fill = FALSE)

#c

ratings%>%group_by(book_id)%>%summarize(number_of_ratings_per_book  = n())%>% ggplot(aes(number_of_ratings_per_book)) + geom_bar(fill = 'yellow' , col = 'grey' , width = 1) + coord_cartesian(c(0,40))

#d
#finding no of genres
library(stringr)
str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", "Classics", "Comics", "Cookbooks", "Crime", "Fantasy", "Gay and Lesbian", "Graphic Novels", "Historical Fiction", "History", "Horror", "Humor and Comedy", "Manga", "Memoir", "Music", "Mystery", "Paranormal", "Philosophy", "Poetry", "Psychology", "Religion", "Romance", "Science", "Science Fiction", "Self Help", "Suspense", "Spirituality", "Sports", "Thriller", "Travel", "Young Adult"))-> genres
genres

genres[str_to_lower(genres) %in% tags$tag_name] -> avail_genres
tags$tag_id[match(avail_genres, tags$tag_name)] -> avail_tags

#plotting

tmp_tag %>% ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage)) + geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = 'YlOrRd') + labs(y = 'Percentage', x = 'Genre')

#e

books %>%arrange(-average_rating)%>% top_n(10, wt = average_rating) %>% select(title, ratings_count,average_rating) -> top10_ratings

top10_ratings
#f

books%>% arrange(-ratings_count) %>% top_n(10, wt = ratings_count) %>% select(title, ratings_count, average_rating) ->top10_popular_books

top10_popular_books

#3) Ubcf model
#restructuring the data for recommendation 
list(user_id = sort(unique(ratings$user_id)) , book_id = sort(unique(ratings$book_id))) -> dimen_names
dimen_names

spread(select(ratings, book_id, user_id , rating), book_id, rating) %>% select(-user_id) -> ratingmat

as.matrix(ratingmat) -> ratingmat
ratingmat[,-1] -> ratingmat

dimen_names -> dimnames(ratingmat)
ratingmat[1:5, 1:5]
dim(ratingmat)

#converting the rating matrix into a real rating matrix

ratingmat -> ratingmat1
dim(ratingmat1)
0 -> ratingmat1[is.na(ratingmat1)]

as(ratingmat1, 'sparseMatrix') -> sparse_ratings
new('realRatingMatrix', data = sparse_ratings) -> real_ratings
real_ratings


#3) Recommendations

#splitting data for ubcf model
sample(x=c(T,F),size=nrow(real_ratings),replace = T, prob = c(0.90,0.20)) ->split_books
real_ratings[split_books,]->recc_train
real_ratings[!split_books,]->recc_test

#Building t model
Recommender(data = recc_train,method="UBCF")->recc_model_ubcf

#number of recommendations
num_rec<-6

#predicting books
predict(object=recc_model_ubcf,newdata=recc_test,n=num_rec)->recc_predicted_ubcf

#recommending books for user 1
recc_predicted_ubcf@items[[1]]->user1_book_numbers
recc_predicted_ubcf@itemLabels[user1_book_numbers]

books %>% filter(id==6343) %>% select(original_title,authors)
books %>% filter(id==7482) %>% select(original_title,authors)
books %>% filter(id==2750) %>% select(original_title,authors)

#recommending books for user5

recc_predicted_ubcf@items[[5]]->user5_book_numbers
recc_predicted_ubcf@itemLabels[user5_book_numbers]

books %>% filter(id==4624) %>% select(original_title,authors)
books %>% filter(id==6867) %>% select(original_title,authors)
books %>% filter(id==7326) %>% select(original_title,authors)

