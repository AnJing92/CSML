setwd("/Users/Fangzhou/Desktop/project/R")
install.packages('neuralnet')
install.packages("dummies")
library(dummies)
library("neuralnet")

data_cat16 <- read.csv("/Users/Fangzhou/Desktop/project/R/zhengli_data/data_cat16.csv", header = T, stringsAsFactors = F)
data_ori <-  as.data.frame(data_cat16)
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
area_dummy <- as.data.frame(dummy(data_ori$AREA))

#data_ori <- as.data.frame(cbind(data_ori, area_dummy))


Test_table <-matrix(0,50,3)
data_ucll <- data_ori;
area_dummy_R <- area_dummy
for( j in 5:50) {
  print(j)
  ###------[[[delete tweets less than J (and more than 300)]]]---------------------------
  data_ucll <- data_ucll;
  area_dummy_R <- area_dummy_R
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
  area_dummy_R <- area_dummy_R[row_sub,];
  #data_01$user_id_str<-as.numeric(data_01$user_id_str
  
  
  #-------------------------------------------
  data_ucll <- rbind(data_ucll,c(0,0,0,0))
  area_dummy_R <- rbind(area_dummy_R,c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
  
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
      dummy_now <- as.matrix(area_dummy_R[start:i,])
      
      n <- nrow(sequence_now);
      x_n <- n-2; y_n <- n-1;
      dummy_train_x <- dummy_now[1:x_n,];
      dummy_train_y <- dummy_now[2:y_n,];
      data_train <- as.data.frame(cbind(dummy_train_x, dummy_train_y))
      colnames(data_train) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16",
                                "y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16")
      c = 0; # re-set to 0
      #----training, estimating transition matrix-----
      
      NN <- neuralnet(y1+y2+y3+y4+y5+y6+y7+y8+y9+y10+y11+y12+y13+y14+y15+y16~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16,
                      data_train, 
                      hidden=c(16,16), 
                      threshold=0.01,
                      algorithm = "rprop+",
                      act.fct = "logistic")
      #print(NN)
      #plot(NN)
      
      #---testing-----------
      test_point <- as.data.frame(t(dummy_now[n-1,]));
      #-----predicting------
      NN_pre_pro<- compute(NN, test_point)
      #print(NN_pre_pro$net.result)
      pre_y <- as.numeric(which.max(NN_pre_pro$net.result))
      if (pre_y==sequence_now[n,1]) {right=right+1}
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
plot(Test_table[5:50,1],Test_table[5:50,3], main = "Test accuracy vs Sequence length baseline",
     sub = "(2 hidden layer with 16 neurons)",
     ylab = "test accuracy", xlab = "sequence length baseline")

#plot(Test_table[,2],Test_table[,3], main = "Test accuracy vs Sequence amount", ylab = "test accuracy", xlab = "sequence amout")

summary(Test_table[5:50,3])
Test_table_save <- as.data.frame(t(Test_table));
rownames(Test_table_save) <- c("sequence length baseline","number of toltal sequence", "accuracy")
write.csv(Test_table_save, file = "ANN_2H_BL_Result.csv")
#row.names = FALSE)
#col.names = TRUE )
