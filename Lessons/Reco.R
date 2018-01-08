library(data.table)
library(dplyr)
library(plotly)

df_rating <- fread("recommender system/BX-Book-Ratings.csv")
df_books <- read.csv("recommender system/BX-Books.csv",sep=';')
df_user <- fread("recommender system/BX-Users.csv")

df_rating$`User-ID` <- NULL
nrow(df_user)
df_rating <- data.table(unique(df_rating))

length(unique(df_rating$ISBN))


df_books <- df_books[,c(1:5)]

TOTAL_TABLE <- inner_join(df_books,df_rating)
t <- TOTAL_TABLE

Year_rating <- t %>% dplyr::group_by(Year.Of.Publication) %>% 
                     dplyr::summarise(yr=mean(as.integer(`Book-Rating`),na.rm=TRUE))
Year_rating <- Year_rating[-c(1:21),]
Year_rating <- Year_rating[c(1:99),]
str(Year_rating)
qplot(Year_rating$yr)
plot(Year_rating)


tss <- inner_join(t,Year_rating,by="Year.Of.Publication")
ts


length(unique(df_books$Publisher))
