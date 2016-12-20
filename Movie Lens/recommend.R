setwd("~/rnote/MachineLearning/Movie Lens/")
if(!require("dplyr")){
  install.packages(dplyr)
  library(dplyr)
}

movieLenz <- read.csv(file="./ml-100k/u.data",sep="\t",header=FALSE)

str(movieLenz)
colnames(movieLenz) <- c("id","movie","rating","create_date_time")
movieLenz$rating <- as.double(movieLenz$rating)
str(movieLenz)
library(dplyr)
movie.df <- tbl_df(movieLenz)
row_names <- summarise(group_by(movie.df,id)[1], count = n())[1]
row_vec <- row_names$id
length(row_vec)
col_names <- summarise(group_by(movie.df,movie)[2], count = n())[1]
col_vec <- col_names$movie
length(col_vec)
rating.df <-matrix(0.0,nrow=length(row_vec), ncol=length(col_vec))
str(rating.df)
df <- as.data.frame(arrange(movie.df,id))
str(df)

for ( i in 1:nrow(df)){
  row <- as.integer(df[i,1])
  col <- as.integer(df[i,2])
  rating <- as.numeric(df[i,3])
  rating.df[row,col] <- rating
  #print(paste(paste(row,col),rating))
}
rating.df.t <- t(rating.df)
str(rating.df.t)
recommend.raw.matrix <- rating.df.t %*% rating.df
recommend.raw.matrix.row <- nrow(recommend.raw.matrix);recommend.raw.matrix.row

recommend.matrix <- data.frame()
for(i in 1:recommend.raw.matrix.row){
  mat = data.frame("movie_id"=rep(i,recommend.raw.matrix.row),
                   "recommend_id"=1:recommend.raw.matrix.row,
                   "score"=t(t(recommend.raw.matrix[i,])))
  recommend.matrix <- rbind(recommend.matrix,mat);
}

recommend.matrix.df <- tbl_df(recommend.matrix)

find_recommend <- function(movie_id, n){
  res <- select(arrange(filter(recommend.matrix.df,movie_id == movie_id,
                        recommend_id != movie_id),desc(score)),recommend_id,score)[1:n,]
  return(as.data.frame(res))
}

find_recommend(132,30)


