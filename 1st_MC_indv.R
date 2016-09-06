setwd("/Users/Fangzhou/Desktop/project/R")
install.packages("markovchain")
library(markovchain)
##----select data-------
data <- read.csv("/Users/Fangzhou/Desktop/project/R/zhengli_data/data_cat16.csv", header = T, stringsAsFactors = F)
#data_08_1619 <- read.csv("/Users/Fangzhou/Desktop/project/R/zhengli_data/data_08_1619_cat16_30.csv", header = T, stringsAsFactors = F)
data_ori <-  as.data.frame(data)
##-----delete uncorrelated feature in ori data-------
data_ori$created_at <- NULL
data_ori$place_lat <- NULL
data_ori$place_lon <- NULL
data_ori$weidu <- NULL
data_ori$jingdu <- NULL
data_ori$area_zuobiao <- NULL
data_ori$hour <- NULL
data_ori$hour_cat <- NULL
#---------------------------

Test_table <-matrix(0,70,3)
data_ucll <- data_ori;
for( j in 5:70) {
  print(j)
###------[[[delete tweets less than J (and more than 300)]]]---------------------------
data_ucll <- data_ucll;
data_ucll$user_id_str[is.na(data_ucll$user_id_str)] <- 0
#data$user_id_str<-as.integer(data$user_id_str)
data_ucll <- rbind(data_ucll,c(0,0,0,0))
#data_matrix <- as.matrix(data_ucll)
n_row<-nrow(data_ucll)
n_rs <- (n_row-1)

id <- as.matrix(data_ucll$user_id_str);
c <- id[1,1];
for (i in 1:n_rs) {
  #print(i)
  a=id[i,1];
  b=id[i+1,1];
  if (a == b) { c=a }# con1
  else{
    L <- length(which(id == c)) 
    if (L<= j){id[id==c] <- 0 }
    #else if (L > 300){id[id == c] <- 0 }
    else { guo=1 }
    c=b;
  }# end for ELSE
}# end for for i:n_rs



data_ucll[is.na(data_ucll)] <- 0
data_ucll<- as.data.frame(data_ucll)
data_ucll$user_id_str <- id;
row_sub = apply(id, 1, function(row) all(row != 0 ));
##Subset as usual
data_ucll<-data_ucll[row_sub,];
#data_01$user_id_str<-as.numeric(data_01$user_id_str


#-------------------------------------------
data_ucll <- rbind(data_ucll,c(0,0,0,0))
data_matrix <- as.matrix(data_ucll)
n_data <- nrow(data_matrix)
n_rs <- n_data -1
##------initialize----
c =0 ;
N_sequence =0 ;
right = 0;
error = 0;
#----run for each id's sequence-----
for (i in 1:n_rs) {
  #print(i)
  a=data_matrix[i,1];
  b=data_matrix[i+1,1];
  if (a==b) {c<-c+1 }# count tweet of same id
  else {
    N_sequence <- N_sequence+1;# count the number of observation chains
    start = i-c;
    sequence_now <- as.matrix(data_matrix[start:i,2]); # get sequence of this id
    n <- nrow(sequence_now);
    sequence_train <- as.character(sequence_now[1:n-1,]);
    c = 0; # re-set to 0
    #----training, estimating transition matrix-----
    Fit_MC<-markovchainFit(data = sequence_train)#,confidencelevel=0.95);
    trans_M <-Fit_MC$estimate@transitionMatrix;
    #---testing-----------
    test_point <- as.character(sequence_now[n-1]);
    row_index <-which( rownames(trans_M)== test_point )
    #-----predicting------
    pre_index <- as.matrix(which.max(trans_M[row_index,]))
    pre<-as.numeric(row.names(trans_M)[pre_index])
    if (pre==sequence_now[n,1]) {right=right+1}
    else {error = error + 1}
   ##############compute logloss
   # trans_M[trans_M < 10^(-15)] <- 10^(-15)
   # log_trans_M <- as.matrix(log(trans_M))
    
     }# end for ELSE
}# end for for i:n_rs

#Test_Error <- error/N_sequence
Test_Accuracy <- right/N_sequence


#Test_Error_table <- as.matrix(0,3,50)

Test_table[j,1] <- j;
Test_table[j,2] <- N_sequence;
Test_table[j,3] <- Test_Accuracy;
} # end in J


#make plots of the result 
#plot(Test_table[,1],Test_table[,2], main = "Sequence amount vs Sequence length baseline", xlab = "sequence length baseline", ylab = "sequence amount")
plot(Test_table[5:70,1],Test_table[5:70,3], main = "Test accuracy vs Sequence length baseline",
     sub = "(1st Markov model)",
     ylab = "test accuracy", xlab = "sequence length baseline")

summary(Test_table[5:70,3])

Test_table_save <- as.data.frame(t(Test_table));
rownames(Test_table_save) <- c("sequence length baseline","number of toltal sequence", "accuracy")


setwd("/Users/Fangzhou/Desktop/project/R")
write.csv(Test_table_save, file = "1stMC_Result.csv")
          #row.names = FALSE)
#col.names = TRUE )
