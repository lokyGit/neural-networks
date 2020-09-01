
library(neuralnet)

bcd_data=data.frame()
#############################################################################

# Binary conversion function to change the input digits to binary and calculating the BCD sum

#############################################################################

binaryConv= function(x,y){
  binary=data.frame()
  binary[1,1:15]=0
  #print(paste0('right after entering the binary function:',x,y))
  binary[1,1]=x
  binary[1,2]=y
  binSum= x+y
  #print(paste0('binSum value - initial:',binSum))
  
 ### transforming x to binary
   i=6  # intializing the value for Num1_0 column
  while(x>=1){
   #print(paste0('value of x before entering loop in transform: ',x))
    binary[1,i] = x%%2
    #print(paste0('value of X in binary function ',binary[1,i]))
    if (x%%2==0){
      x = x/2  # Even digit logic
     #print( paste0 ('inside transform x if - loop: ',x))
    }
    else{
      x=(x-1)/2  # Odd digit logic
      #print(paste0('inside transform x else - loop: ',x))
    }
    i=i-1
    #print (paste0 ('i value in transform x loop: ',i))
  }


### transforming y to binary
   i=10
   while(y>=1){
    #print(paste0('value of y before entering loop in transform: ',y))
     binary[1,i] = y%%2
     #print(paste0('value of Y in binary function ',binary[1,i]))
     if (y%%2==0){
       y = y/2  # Even digit logic
      # print(paste0('inside transform y if - loop: ',y))
     }
     else{
       y=(y-1)/2 # Odd digit logic
       #print(paste0('inside transform y else - loop: ',y))
     }
     i=i-1 
     #print(paste0('i value in transform y loop: ',i))
   }
   #####################
   # BCD Output table #
   #####################
  if (binSum>9){
    binary[1,11]=1
    print(paste0('binSum value in binSum>9 loop before binSum%% 10: ',binSum))
    binSum= binSum%%10
    print(paste0('binSum value in binSum>9 loop: ',binSum))
  }
    i=15
    while(binSum>=1){
      binary[1,i] = binSum%%2
     print(paste0('value of binSum in binary function ',binary[1,i]))
      if (binSum%%2==0){
        binSum = binSum/2 # Even digit logic
        print(paste0('binSum value in binSum%%2 == 0 - if loop: ',binSum))
      }
      else{
        binSum=(binSum-1)/2 # Odd digit logic
        print(paste0('binSum value in binSum%%2 == 0 - else loop: ',binSum))
      }
      i=i-1
      print(paste0('i value in bcd loop: ',i))
    }
    
 bcd_data<<-rbind(bcd_data,binary)  ### binding the temp binary table to dataframe
}


# For loop for creating the BCD truth table
for (l in 0:9) {

  for (m in 0:9) {
    binaryConv(l,m)
    }
    
}


# Naming the BCD truth table

names(bcd_data)=c("Num1","Num2","Num1_3","Num1_2","Num1_1","Num1_0","Num2_3","Num2_2","Num2_1","Num2_0","C","BCD_S3","BCD_S2","BCD_S1","BCD_S0")

# Creating the test and train datsets for the model

bin_input=bcd_data
set.seed(12345)
training_ind =  sample(2, nrow(bin_input), replace = T, prob = c(0.7,0.3))
nn_train = input[training_ind == 1,]
nn_test = input[training_ind == 2,]
nn_train = nn_train[,-c(1:2)]
nn_test = nn_test[,-c(1:2)]
## Defining different Activation Functions
sigmoid = function(x) {
  1 / (1 + exp(-x))
}

tanh = function(x){
  2*sigmoid(2*x)-1
  
}

LeakyreLU = function(x){
  ifelse(x<0, 0.1*x,x)
}


# Preparing to train the simple model to predict the 5 bit output

allFeatures = (colnames(input))
predFeatures =allFeatures[!allFeatures%in% c("C","BCD_S3","BCD_S2","BCD_S1","BCD_S0","Num1","Num2")]
predFeatures = paste(predFeatures,collapse = "+")
outputFeatures = allFeatures[allFeatures%in% c("C","BCD_S3","BCD_S2","BCD_S1","BCD_S0")]
outputFeatures = paste(outputFeatures, collapse = "+")
outformula = as.formula(paste(outputFeatures,"~",predFeatures,collapse  = "+")) 


###############################################################
#Experiment-1:  with 1 hidden layers
###############################################################

# training with sigmoid function
out_nn = neuralnet(outformula, data=nn_train,hidden = 1, linear.output = F, act.fct = sigmoid)
# training with tanh function
out_nn_tanh = neuralnet(outformula, 
                        data=nn_train,hidden = 1, 
                        linear.output = F, act.fct = tanh)

# training with leaky reLU function
out_nn_leakyrelu = neuralnet(outformula, data=nn_train,hidden = 1, 
                           linear.output = F, 
                           act.fct = LeakyreLU)

#plotting the model with sigmoid function
plot(out_nn)  # error: 25.830389 Steps: 2067

#plotting the model with tanh function
plot(out_nn_tanh) # error: 27.540388 Steps: 3617

# plotting with leaky reLU function
plot(out_nn_leakyrelu) # error: 30.672798 Step: 533

#predictions with Sigmoid function
out_nn_prob =  compute(out_nn, nn_test) #[,c(3:10)]
out_nn_pred = out_nn_prob$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with sigmoid function
out_nn_MSE =  sum((out_nn_pred-out_nn_act)^2)/nrow(nn_test) # 0.8958955


#predictions with tanh function
out_nn_prob_tanh =  compute(out_nn_tanh, nn_test) 
out_nn_pred_tanh = out_nn_prob_tanh$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_tanh = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with tanh function
out_MSE_tanh =  sum((out_nn_pred_tanh-out_nn_act_tanh)^2)/nrow(nn_test) #0.8945729


#predictions with leakyreLU function
out_nn_prob_leakyreLU =  compute(out_nn_leakyrelu, nn_test)
out_nn_pred_leakyreLU = out_nn_prob_leakyreLU$net.result*(max(nn_test$C)-min(nn_test$C))+min(nn_test$C)
out_nn_test_act_leakyreLU = nn_test$C*(max(nn_test$C)-min(nn_test$C)+min(nn_test$C))


# Mean Squared error with leakyreLU function
out_MSE_leakyreLU =  sum((out_nn_pred_leakyreLU-out_nn_test_act_leakyreLU)^2)/nrow(nn_test)  # 1.158764


###############################################################
 #Experiment-2:  with 3 hidden layers
###############################################################

# training with sigmoid function
out_nn_2 = neuralnet(outformula, data=nn_train,hidden = 3, linear.output = F, act.fct = sigmoid)
# training with tanh function
out_nn_tanh_2 = neuralnet(outformula, 
                          data=nn_train,hidden = 3, 
                          linear.output = F, 
                          act.fct = tanh)

# training with leaky reLU function
out_nn_leakyrelu_2 = neuralnet(outformula, 
                               data=nn_train,hidden = 3,
                               linear.output = F, 
                               act.fct = LeakyreLU) #Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 

#plotting the model with sigmoid function
plot(out_nn_2)  # error: 20.479055 Steps: 6863


#predictions with Sigmoid function
out_nn_prob_2 =  compute(out_nn_2, nn_test)
out_nn_pred_2 = out_nn_prob_2$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_2 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)

# Mean Squared error with sigmoid function
out_nn_MSE_2 =  sum((out_nn_pred_2-out_nn_act_2)^2)/nrow(nn_test) #0.8976031

#plotting the model with tanh function
plot(out_nn_tanh_2) #error 23.355511 steps 75557

#predictions with tanh function
out_nn_prob_tanh_2 =  compute(out_nn_tanh_2, nn_test) 
out_nn_pred_tanh_2 = out_nn_prob_tanh_2$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_tanh_2 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with tanh function
out_MSE_tanh_2 =  sum((out_nn_pred_tanh_2-out_nn_act_tanh_2)^2)/nrow(nn_test) #1.253424




###############################################################
#Experiment-3:  with 5 hidden layers
###############################################################


# training with sigmoid function
out_nn_3 = neuralnet(outformula, data=nn_train,hidden = 5, linear.output = F, act.fct = sigmoid)
# training with tanh function
out_nn_tanh_3 = neuralnet(outformula, 
                          data=nn_train,hidden = 5,
                          linear.output = F, 
                          act.fct = tanh) #Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 

# training with leaky reLU function
out_nn_leakyrelu_3 = neuralnet(outformula, 
                               data=nn_train,hidden = 5,
                               linear.output = F, 
                               act.fct = LeakyreLU) #Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 

#plotting the model with sigmoid function
plot(out_nn_3)  # error: 4.941344 Steps: 7229

#plotting the model with tanh function
plot(out_nn_tanh_3) # error: 14.214515 Steps: 86283

# plotting with leaky reLU function
plot(out_nn_leakyrelu_3) # ERROR: weights were not calculated


#predictions with Sigmoid function
out_nn_prob_3 =  compute(out_nn_3, nn_test)
out_nn_pred_3 = out_nn_prob_3$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_3 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)

# Mean Squared error with sigmoid function
out_nn_MSE_3 =  sum((out_nn_pred_3-out_nn_act_3)^2)/nrow(nn_test) # 0.7823743

#predictions with tanh function
out_nn_prob_tanh_3 =  compute(out_nn_tanh_3, nn_test) 
out_nn_pred_tanh_3 = out_nn_prob_tanh_3$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_tanh_3 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with tanh function
out_MSE_tanh_3 =  sum((out_nn_pred_tanh_3-out_nn_act_tanh_3)^2)/nrow(nn_test) #1.406309


###############################################################
#Experiment-4:  with 2 hidden layers 1 neurons
###############################################################

# training with sigmoid function
out_nn_4 = neuralnet(outformula, data=nn_train,hidden = c(2,1), linear.output = F, act.fct = sigmoid)
# training with tanh function
out_nn_tanh_4 = neuralnet(outformula, 
                          data=nn_train,hidden = c(2,1), 
                          linear.output = F, 
                          act.fct = tanh) # Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 

# training with leaky reLU function
out_nn_leakyrelu_4 = neuralnet(outformula, 
                               data=nn_train,hidden = c(2,1),
                               linear.output = F, 
                               act.fct = LeakyreLU) #Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 

#plotting the model with sigmoid function
plot(out_nn_4)  # error: 25.40547 Steps: 10241

#plotting the model with tanh function
plot(out_nn_tanh_4) 

# plotting with leaky reLU function
plot(out_nn_leakyrelu_4) # ERROR: weights were not calculated

#predictions with Sigmoid function
out_nn_prob_4 =  compute(out_nn_4, nn_test)
out_nn_pred_4 = out_nn_prob_4$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_4 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)

# Mean Squared error with sigmoid function
out_nn_MSE_4 =  sum((out_nn_pred_4-out_nn_act_4)^2)/nrow(nn_test) # 0.9100843

#predictions with tanh function
out_nn_prob_tanh_4 =  compute(out_nn_tanh_4, nn_test) 
out_nn_pred_tanh_4 = out_nn_prob_tanh_4$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_tanh_4 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with tanh function
out_MSE_tanh_4 =  sum((out_nn_pred_tanh_4-out_nn_act_tanh_4)^2)/nrow(nn_test) #1.406309




###############################################################
#Experiment-5:  with 5 hidden layers, 4 neurons
###############################################################

# training with sigmoid function
out_nn_5 = neuralnet(outformula, data=nn_train,hidden = c(5,4), linear.output = F, act.fct = sigmoid)
# training with tanh function
out_nn_tanh_5 = neuralnet(outformula, 
                          data=nn_train,hidden = c(5,4), 
                          linear.output = F, 
                          act.fct = tanh) # Algorithm did not converge in 1 of 1 repetition(s) within the stepmax.
 
# training with leaky reLU function
out_nn_leakyrelu_5 = neuralnet(outformula, 
                               data=nn_train,hidden = c(5,4),
                               linear.output = F, 
                               act.fct = LeakyreLU) #Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 

#plotting the model with sigmoid function
plot(out_nn_5)  # error: 1.794207 Steps: 20928

#plotting the model with tanh function
plot(out_nn_tanh_5) # ERROR: weights were not calculated

# plotting with leaky reLU function
plot(out_nn_leakyrelu_5) # ERROR: weights were not calculated

#predictions with Sigmoid function
out_nn_prob_5 =  compute(out_nn_5, nn_test)#[,c(3:10)])
out_nn_pred_5 = out_nn_prob_5$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_5 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)

# Mean Squared error with sigmoid function
out_nn_MSE_5 =  sum((out_nn_pred_5-out_nn_act_5)^2)/nrow(nn_test) # 0.6263301


###############################################################
#Experiment-6:  with 10 hidden layers, 9 neurons
###############################################################

# training with sigmoid function
out_nn_6 = neuralnet(outformula, data=nn_train,hidden = c(10,9), linear.output = F, act.fct = sigmoid)

# training with tanh function
out_nn_tanh_6 = neuralnet(outformula, 
                          data=nn_train,hidden = c(10,9), 
                          linear.output = F, 
                          act.fct = tanh) # Algorithm did not converge in 1 of 1 repetition(s) within the stepmax.

# training with leaky reLU function
out_nn_leakyrelu_6 = neuralnet(outformula, 
                               data=nn_train,hidden = c(10,9),
                               linear.output = F, 
                               act.fct = LeakyreLU) #Algorithm did not converge in 1 of 1 repetition(s) within the stepmax. 

#plotting the model with sigmoid function
plot(out_nn_6)  # error: 0.514206 Steps: 453


#predictions with Sigmoid function
out_nn_prob_6 =  compute(out_nn_6, nn_test)#[,c(3:10)])
out_nn_pred_6 = out_nn_prob_6$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_6 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)


# Mean Squared error with sigmoid function
out_nn_MSE_6 =  sum((out_nn_pred_6-out_nn_act_6)^2)/nrow(nn_test) # 0.9354431


###############################################################
#Experiment-7:  with 1 hidden layers, 1 neurons
###############################################################

# training with sigmoid function
out_nn_7 = neuralnet(outformula, data=nn_test,hidden = c(1,1), linear.output = F, act.fct = sigmoid)

# training with tanh function
out_nn_tanh_7 = neuralnet(outformula, 
                          data=nn_test,hidden = c(1,1), 
                          linear.output = F, 
                          act.fct = tanh) 

# training with leaky reLU function
out_nn_leakyrelu_7 = neuralnet(outformula, 
                               data=nn_test,hidden = c(1,1),
                               linear.output = F, 
                               act.fct = LeakyreLU) # Algorithm did not converge in 1 of 1 repetition(s) within the stepmax.


#plotting the model with sigmoid function
plot(out_nn_7)  # error: 15.128518 Steps: 4053

#plotting the model with tanh function
plot(out_nn_tanh_7) # error: 16.565947 Steps: 430

# plotting with leaky reLU function
plot(out_nn_leakyrelu_7) # ERROR: weights were not calculated


#predictions with Sigmoid function
out_nn_prob_7 =  compute(out_nn_7, nn_test)#[,c(3:10)])
out_nn_pred_7 = out_nn_prob_7$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_7 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)


# Mean Squared error with sigmoid function
out_nn_MSE_7 =  sum((out_nn_pred_7-out_nn_act_7)^2)/nrow(nn_test) # 0.8644867


#predictions with tanh function
out_nn_prob_tanh_7 =  compute(out_nn_tanh_7, nn_test) 
out_nn_pred_tanh_7 = out_nn_prob_tanh_7$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_tanh_7 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with tanh function
out_MSE_tanh_7 =  sum((out_nn_pred_tanh_7-out_nn_act_tanh_7)^2)/nrow(nn_test) #0.9466255



###############################################################
#Experiment-8:  with 5 hidden layers, 3 neurons
###############################################################

# training with sigmoid function
out_nn_8 = neuralnet(outformula, data=nn_test,hidden = c(5,3), linear.output = F, act.fct = sigmoid)

# training with tanh function
out_nn_tanh_8 = neuralnet(outformula, 
                          data=nn_test,hidden = c(5,3), 
                          linear.output = F, 
                          act.fct = tanh) # Algorithm did not converge in 1 of 1 repetition(s) within the stepmax.

# training with leaky reLU function
out_nn_leakyrelu_8 = neuralnet(outformula, 
                               data=nn_test,hidden = c(5,3),
                               linear.output = F, 
                               act.fct = LeakyreLU) # Algorithm did not converge in 1 of 1 repetition(s) within the stepmax.


#plotting the model with sigmoid function
plot(out_nn_8)  # error: 1.688958 Steps: 34378


#predictions with Sigmoid function
out_nn_prob_8 =  compute(out_nn_8, nn_test)#[,c(3:10)])
out_nn_pred_8 = out_nn_prob_8$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_8 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)


# Mean Squared error with sigmoid function
out_nn_MSE_8 =  sum((out_nn_pred_8-out_nn_act_8)^2)/nrow(nn_test) # 0.09651188



###############################################################
#Experiment-9:  with 8 hidden layers, 7 neurons
###############################################################

# training with sigmoid function
out_nn_9 = neuralnet(outformula, data=nn_test,hidden = c(8,7), linear.output = F, act.fct = sigmoid)

# training with tanh function
out_nn_tanh_9 = neuralnet(outformula, 
                          data=nn_test,hidden = c(8,7), 
                          linear.output = F, 
                          act.fct = tanh,
                          threshold = T) 

# training with leaky reLU function
out_nn_leakyrelu_9 = neuralnet(outformula, 
                               data=nn_test,hidden = c(8,7),
                               linear.output = F, 
                               act.fct = LeakyreLU)  # Algorithm did not converge in 1 of 1 repetition(s) within the stepmax.


#plotting the model with sigmoid function
plot(out_nn_9)  # error: 0.009918 Steps: 212

#plotting the model with tanh function
plot(out_nn_tanh_9) # error: 13.566402 Steps: 33

#plotting the model with leakyreLU function
plot(out_nn_leakyrelu_9) # Error: weights were not calculated


#predictions with Sigmoid function
out_nn_prob_9 =  compute(out_nn_9, nn_test) #[,c(3:10)]
out_nn_pred_9 = out_nn_prob_9$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_9 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with sigmoid function
out_nn_MSE_9 =  sum((out_nn_pred_9-out_nn_act_9)^2)/nrow(nn_test) #0.0005667368


#predictions with tanh function
out_nn_prob_tanh_9 =  compute(out_nn_tanh_9, nn_test) 
out_nn_pred_tanh_9 = out_nn_prob_tanh_9$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_tanh_9 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with tanh function
out_MSE_tanh_9 =  sum((out_nn_pred_tanh_9-out_nn_act_tanh_9)^2)/nrow(nn_test) # 0.8662478




###############################################################
#Experiment-10:  with 10 hidden layers, 9 neurons
###############################################################

# training with sigmoid function
out_nn_10 = neuralnet(outformula, data=nn_test,hidden = c(10,9), linear.output = F, act.fct = sigmoid)

# training with tanh function
out_nn_tanh_10 = neuralnet(outformula, 
                          data=nn_test,hidden = c(10,9), 
                          linear.output = F, 
                          act.fct = tanh,
                          threshold = T) 

# training with leaky reLU function
out_nn_leakyrelu_10 = neuralnet(outformula, 
                               data=nn_test,hidden = c(10,9),
                               linear.output = F, 
                               act.fct = LeakyreLU)


#plotting the model with sigmoid function
plot(out_nn_10)  # error: 0.032012 Steps: 245

#plotting the model with tanh function
plot(out_nn_tanh_10) # error: 11.677758 Steps: 45

# plotting with leaky reLU function
plot(out_nn_leakyrelu_10) # error: 0.060945 steps 43768


#predictions with Sigmoid function
out_nn_prob_10 =  compute(out_nn_10, nn_test) #[,c(3:10)]
out_nn_pred_10 = out_nn_prob_10$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_10 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with sigmoid function
out_nn_MSE_10 =  sum((out_nn_pred_10-out_nn_act_10)^2)/nrow(nn_test) #0.002248111


#predictions with tanh function
out_nn_prob_tanh_10 =  compute(out_nn_tanh_10, nn_test) 
out_nn_pred_tanh_10 = out_nn_prob_tanh_10$net.result*(max(nn_test[,9:13])-min(nn_test[,9:13]))+min(nn_test,9:13)
out_nn_act_tanh_10 = nn_test[,9:13]*(max(nn_test[,9:13])-min(nn_test[,9:13])+min(nn_test[,9:13]))

# Mean Squared error with tanh function
out_MSE_tanh_10 =  sum((out_nn_pred_tanh_10-out_nn_act_tanh_10)^2)/nrow(nn_test) # 0.8996031


#predictions with leakyreLU function
out_nn_prob_leakyreLU_10 =  compute(out_nn_leakyrelu_10, nn_test)
out_nn_pred_leakyreLU_10 = out_nn_prob_leakyreLU_10$net.result*(max(nn_test$C)-min(nn_test$C))+min(nn_test$C)
out_nn_test_act_leakyreLU_10 = nn_test$C*(max(nn_test$C)-min(nn_test$C)+min(nn_test$C))


# Mean Squared error with leakyreLU function
out_MSE_leakyreLU_10 =  sum((out_nn_pred_leakyreLU_10-out_nn_test_act_leakyreLU_10)^2)/nrow(nn_test)  # 2.127063





