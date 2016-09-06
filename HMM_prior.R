setwd("/Users/Fangzhou/Desktop/project/R")
install.packages("HMM")
library(HMM)
#[0.]-----data---------------------------------------------------
##----select data-------
data <- read.csv("/Users/Fangzhou/Desktop/project/R/zhengli_data/data_cat16.csv", header = T, stringsAsFactors = F)

data_ori <-  as.data.frame(data)
remove(data)

##-----delete uncorrelated feature in ori data-------
data_ori$created_at <- NULL
data_ori$place_lat <- NULL
data_ori$place_lon <- NULL
data_ori$weidu <- NULL
data_ori$jingdu <- NULL
data_ori$area_zuobiao <- NULL
data_ori$hour <- NULL
#[1.]==================[initialize - transition matrix]======
#-------------------------------------------
#data_ucll <- rbind(data_ucll,c(0,0,0,0))
data_matrix <- as.matrix(data_ori)
n_data <- nrow(data_matrix)
n_rs <- n_data -1

N_cat <- 16 # number of area divided
trans_M_ini_count <- matrix(0, N_cat, N_cat) # creat a transition matrix with all zeros

for (i in 1:n_rs) {
  #print(i)
  a <- data_matrix[i,1]
  b <- data_matrix[i+1,1]
  
  x <- data_matrix[i,3]
  y <- data_matrix[i+1,3]
  if (a == b) {
    trans_M_ini_count[x,y] <- trans_M_ini_count[x,y]+1 }
  else 
  {here <- 0 }
}# end for for i:N Jingdu

#sum
v_rowsum <- as.vector(rowSums(trans_M_ini_count))
trans_M_ini<- matrix(0, N_cat, N_cat)
for (i in 1:N_cat) {
  print(i)
  trans_M_ini[i,] <- trans_M_ini_count[i,]/v_rowsum[i]
}

#[2.]==================[initialize - start probability]======

start_ini_count<- matrix(0,1,16);

for (i in 1:n_rs) {
  #print(i)
  a <- data_matrix[i,1]
  b <- data_matrix[i+1,1]
  
  x <- data_matrix[i,3]
  y <- data_matrix[i+1,3]
  if (a != b) {
    start_ini_count[1,y] <- start_ini_count[1,y]+1 }
  else 
  {here <- 0 }
}# end for for i:N Jingdu

#sum
v_rowsum <- as.numeric(rowSums(start_ini_count))

start_ini<- matrix(0, 1, N_cat)
for (i in 1:N_cat) {
  #print(i)
  start_ini[1,i] <- start_ini_count[i]/v_rowsum
}

#===========================================================

#[3.]==================[initialize - emission probability]
emiss_M_ini_count <- matrix(0, N_cat, 4) # creat a transition matrix with all zeros

for (i in 1:n_data) {
  #print(i)
  
  x <- data_matrix[i,3]
  y <- data_matrix[i,2]
  emiss_M_ini_count[x,y] <- emiss_M_ini_count[x,y]+1
}# end for for i:N Jingdu

#sum
v_rowsum <- as.vector(rowSums(emiss_M_ini_count))
emiss_M_ini<- matrix(0, N_cat, 4)
for (i in 1:N_cat) {
  print(i)
  emiss_M_ini[i,] <- emiss_M_ini_count[i,]/v_rowsum[i]
}





#=================all initialized============================
hmm_ini = initHMM(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),c(1,2,3,4),
                  transProbs=trans_M_ini,
                  emissionProbs=emiss_M_ini,
                  startProbs=start_ini )
#=========================================










Test_table <-matrix(0,70,4)
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
  

#[4.]==============[BM]=========================
##------initialize some value----
data_ucll <- rbind(data_ucll,c(0,0,0,0))
data_matrix <- as.matrix(data_ucll)
c =0 ;
N_sequence =0 ;
right_H = 0;
error_H = 0;
right_E = 0;
error_E = 0;
#----run for each id's sequence-----
for (i in 1:n_rs) {
  #print(i)
  a=data_matrix[i,1];
  b=data_matrix[i+1,1];
  if (a==b) {c<-c+1 }# count tweet of same id
  else {
    N_sequence <- N_sequence+1;# count the number of observation chains
    start = i-c;
    hidden_sequnce_now <- as.matrix(data_matrix[start:i,3]); # get sequence of this id
    observation_sequnce_now <- as.matrix(data_matrix[start:i,2]);
    n <- nrow(hidden_sequnce_now);
    observation_train <- observation_sequnce_now[1:n-1,];
    c = 0; # re-set to 0
    #----training, estimating transition matrix-----
      bw = baumWelch(hmm_ini,observation_train,maxIterations=15)
    trans_M <- bw$hmm$transProbs
    emiss_M <- bw$hmm$emissionProbs
    #---testing-----------
    test_hidden_point <- hidden_sequnce_now[n-1,1];
    #-----predicting------
    pre_hidden <- as.numeric(which.max(trans_M[test_hidden_point,]));
    pre_observ <- as.numeric(which.max(emiss_M[pre_hidden,]));
    if (pre_hidden == hidden_sequnce_now[n,1]){
      right_H=right_H+1
    } else {
      error_H = error_H+1
    };
    
    if (pre_observ == observation_sequnce_now[n,1]) {
      right_E=right_E+1
    } else {error_E = error_E + 1 
    }
    
  }# end for ELSE
}# end for for i:n_rs

#Test_Error_Hidden <- error_H/N_sequence
Test_Accuracy_Hidden <- right_H/N_sequence

#Test_Error_Emission <- error_E/N_sequence
Test_Accuracy_Emission  <- right_E/N_sequence

Test_table[j,1] <- j;
Test_table[j,2] <- N_sequence;
Test_table[j,3] <- Test_Hidden_Accuracy;
Test_table[j,4] <- Test_Emission_Accuracy;
} # end in J

plot(Test_table[5:70,1],Test_table[5:70,3], main = "Test Hideen accuracy vs Sequence length baseline",
     sub = "(HHM)",
     ylab = "test accuracy", xlab = "sequence length baseline")

plot(Test_table[5:70,1],Test_table[5:70,4], main = "Test Emission accuracy vs Sequence length baseline",
     sub = "(HHM)",
     ylab = "test accuracy", xlab = "sequence length baseline")

summary(Test_table[5:70,3])
summary(Test_table[5:70,4])


Test_table_save <- as.data.frame(t(Test_table));
rownames(Test_table_save) <- c("sequence length baseline","number of toltal sequence", "hidden accuracy","emission accuracy")

setwd("/Users/Fangzhou/Desktop/project/R")
write.csv(Test_table_save, file = "HHM_prior_Result.csv")
#row.names = FALSE)
#col.names = TRUE )


