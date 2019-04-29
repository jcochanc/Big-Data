knitr::opts_chunk$set(fig.pos = 'H')
# Libraries.
pacman::p_load(dplyr, kableExtra, knitr,  
               ISLR, caret, tree, rpart,
               rpart.plot, randomForest,
               MASS, gbm, adabag, xgboost,
               class)
# Read in data.
q1_mat <- readRDS("C:/Users/jcochanc/Documents/Big-Data/Homework/hw4_q1_mat.RDS")

# Rename columns.
colnames(q1_mat) <- c("Y", "X1", "X2")


# Print to screen.
kable(q1_mat, booktabs = TRUE) %>%
  kable_styling(latex_options=c("hold_position"))
q1_mat <- as.data.frame(q1_mat)
q1_mat$Y <- ifelse(q1_mat$Y >0, 1, -1)

set.seed(2214)

# Initialize n.
n <- nrow(q1_mat)

# Initialize weights.
weights <- 1/n

# Initialize splitting possibility
splits <- c(0, 2, 4)

# Stumps
# Pick a stump.
pick_stump_2 <- sample(0:1, 1)

# Sample a split for either X1 or X2.
x1_stump <- sample(splits, 1)
x2_stump <- sample(splits, 1)

# Compute the cutoff.
cutoff <- x1_stump*(1-pick_stump_2) + pick_stump_2*x2_stump

# Compute h_t
if(pick_stump_2 == 0){
  # If we pick X1, then find if our observed value
  # is below or above the cutoff.
  h_t <- q1_mat$X1 > cutoff
}else{
  # Otherwise if we pick X2, check our observed values
  # against this cutoff.
    h_t <- q1_mat$X2 > cutoff
}

h_t <- ifelse(h_t == TRUE, 1, -1)

# Errors function
errors <- sum(weights * (q1_mat$Y != h_t))

# Voting weight of h_t:
alpha_t <- 0.5*log(1/errors -1)

# Results of weight, or the recomputed weights.
updated_weights <- weights*exp(-alpha_t * q1_mat$Y * h_t )
Z_t <- sum(updated_weights)
 
updated_weights <- updated_weights/Z_t

q1_mat_addition <- array(dim = c(5, 3*3))

# Weights
q1_mat_addition[, 1] <- rep(weights, 5)

# Voting power
q1_mat_addition[, 2] <- rep(alpha_t, 5)

# Error
q1_mat_addition[, 3] <- rep(errors, 5)

weights <- updated_weights

pick_x2 <- c()
cuts <- c()
predictions <- array(dim = c(5, 3))

pick_x2[1] <- pick_stump_2
cuts[1] <- cutoff
predictions[,1] <- h_t
val <- c(3,6)

for(i in  val){
  

# Weights
q1_mat_addition[, i+1] <- weights

  k <- which(val == i)
# Stumps
# Pick a stump.
pick_stump_2 <- sample(0:1, 1)

# Sample a split for either X1 or X2.
x1_stump <- sample(splits, 1)
x2_stump <- sample(splits, 1)

# Compute the cutoff.
cutoff <- x1_stump*(1-pick_stump_2) + pick_stump_2*x2_stump

pick_x2[k+1] <- pick_stump_2
cuts[k+1] <- cutoff

# Compute h_t
if(pick_stump_2 == 0){
  # If we pick X1, then find if our observed value
  # is below or above the cutoff.
  h_t <- q1_mat$X1 < cutoff
}else{
  # Otherwise if we pick X2, check our observed values
  # against this cutoff.
    h_t <- q1_mat$X2 < cutoff
}

h_t <- ifelse(h_t == TRUE, 1, -1)

# Errors function
errors <- sum(weights * (q1_mat$Y != h_t))

# Voting weight of h_t:
alpha_t <- 0.5*log(1/errors -1)

# Results of weight, or the recomputed weights.
updated_weights <- weights*exp(-alpha_t * q1_mat$Y * h_t )
Z_t <- sum(updated_weights)
 
updated_weights <- updated_weights/Z_t


# Voting power
q1_mat_addition[, i+ 2] <- rep(alpha_t, 5)

# Error
q1_mat_addition[, i+3] <- rep(errors, 5)

weights <- updated_weights
predictions[,1+k] <- h_t
}

q1_mat_addition <- as.matrix(q1_mat_addition)
colnames(q1_mat_addition) <-rep(c("Weight", "Vote", "Error"),3)

labels <- c()
for(i in 1:3){
  if(pick_x2[i] == 1){
    labels[i] <- paste0("X_2 > ", cuts[i]) 
  }else{
        labels[i] <- paste0("X_1 > ", cuts[i]) 
        }
 
}


kable(q1_mat_addition, digits = 2, booktabs = TRUE, format = "latex") %>%
    kable_styling(latex_options=c("hold_position"),  position = "center") %>%
  add_header_above(c("Iteration 1" = 3, "Iteration 2" = 3,
                     "Iteration 3" = 3)) %>%
  add_header_above(c("X_1 > 0" = 3, "X_2 > 4" = 3,
                     "X_1 > 4"= 3))
votes <- c(q1_mat_addition[1, 2],
           q1_mat_addition[1, 5],
           q1_mat_addition[1, 7])

final_prediction <- sign(predictions %*% votes)

# If we want it in 0,1 format; should also make the data as it was.
# final_prediction <- ifelse(final_prediction < 0, 0, 1)

final_error <- mean( (q1_mat$Y != final_prediction))

q1_b_mat <- cbind("Actual" = q1_mat$Y, "Predicted" = final_prediction)
colnames(q1_b_mat) <- c("Actual", "Predicted")


kable(q1_b_mat, digits = 2, booktabs = TRUE, format = "latex") %>%
    kable_styling(latex_options=c("hold_position"),  position = "center") %>%
  add_header_above(c(" " = 1, "Error = 0.2" = 1))

# Read in data.
q2_mat <- readRDS("C:/Users/jcochanc/Documents/Big-Data/Homework/hw4_q2_mat.RDS")

# Rename columns.
colnames(q2_mat) <- c("Y", "X1", "X2")

# Print to screen.
kable(q2_mat, booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"), position = "center")
# Set up the data frame.
q2_mat <- as.data.frame(q2_mat)

# Order X1 as we will split on that.
q2_mat <- q2_mat[order(q2_mat$X1),]

# Compute F_0, g1, k1.
q2_mat$`F0` <- 1.4
q2_mat$`g1` <- 2*(q2_mat$Y - 1.4)
q2_mat$`k1` <- 2

rownames(q2_mat) <- NULL

kable(q2_mat, digits = 2, booktabs = TRUE, format = "latex") %>%
    kable_styling(latex_options=c("hold_position"),  position = "center")

# Use all values and find optimal cut.
x1_splits <- c(8, 15, 16)
x2_splits <- 0

scores <- c()
for(i in 1:3){
  current_split <- ifelse(q2_mat$X1 >x1_splits[i], 1, 0)



G_0 <- sum(q2_mat$g1[current_split==0])
K_0 <- sum(q2_mat$k1[current_split==0])

G_1 <- sum(q2_mat$g1[current_split==1])
K_1 <- sum(q2_mat$k1[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
scores[i] <- -0.5*sum(G^2/K)
}

current_split <- ifelse(q2_mat$X2 >x2_splits[1], 1, 0)



G_0 <- sum(q2_mat$g1[current_split==0])
K_0 <- sum(q2_mat$k1[current_split==0])

G_1 <- sum(q2_mat$g1[current_split==1])
K_1 <- sum(q2_mat$k1[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
scores[4] <- -0.5*sum(G^2/K)

# Optimal cut at x1=8, calculate predictions
current_split <- ifelse(q2_mat$X1 >x1_splits[which.min(scores)], 1, 0)

G_0 <- sum(q2_mat$g1[current_split==0])
K_0 <- sum(q2_mat$k1[current_split==0])

G_1 <- sum(q2_mat$g1[current_split==1])
K_1 <- sum(q2_mat$k1[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
w <- -G/K

h1_yes <- w[2]
h1_no <- w[1]

q2_mat$`h1` <- ifelse(q2_mat$X1 > x1_splits[1], h1_yes, h1_no)

# Now we split on the remaining values
x1_splits <- c( 16)
x2_splits <- 0
scores <- c()

current_split <- ifelse(q2_mat$X1[q2_mat$X1>8] >x1_splits[1], 1, 0)

gvals <- q2_mat$g1[q2_mat$X1>8]
kvals <- q2_mat$k1[q2_mat$X1>8]
G_0 <- sum(gvals[current_split==0])
K_0 <- sum(kvals[current_split==0])

G_1 <- sum(gvals[current_split==1])
K_1 <- sum(kvals[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
scores[1] <- -0.5*sum(G^2/K)


current_split <- ifelse(q2_mat$X2[q2_mat$X1>8] >x2_splits[1], 1, 0)

gvals <- q2_mat$g1[q2_mat$X1>8]
kvals <- q2_mat$k1[q2_mat$X1>8]
G_0 <- sum(gvals[current_split==0])
K_0 <- sum(kvals[current_split==0])

G_1 <- sum(gvals[current_split==1])
K_1 <- sum(kvals[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
scores[2] <- -0.5*sum(G^2/K)

# Pick x1 at 16

current_split <- ifelse(q2_mat$X1[q2_mat$X1>8] >x1_splits[1], 1, 0)

gvals <- q2_mat$g1[q2_mat$X1>8]
kvals <- q2_mat$k1[q2_mat$X1>8]
G_0 <- sum(gvals[current_split==0])
K_0 <- sum(kvals[current_split==0])

G_1 <- sum(gvals[current_split==1])
K_1 <- sum(kvals[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)

w <- -G/K

h1_yes <- w[2]
h1_no <- w[1]


newvals <- ifelse(q2_mat$X1[q2_mat$X1>8] > x1_splits[1], h1_yes, h1_no)
newvals <- c(NA, NA, newvals)


for(i in 3:6){
  q2_mat[i,7] <- newvals[i]
}

kable(q2_mat, digits = 2, booktabs = TRUE, format = "latex") %>%
    kable_styling(latex_options=c("hold_position"),  position = "center") %>%
  add_header_above(c(" " = 3, "Step 1" = 4))

q2_mat$`F1` <- q2_mat$`F0` + 0.1*q2_mat$`h1`

q2_mat$`g2` <- 2*(q2_mat$Y - q2_mat$`F1`)

q2_mat$`k2` <- 2



kable(q2_mat, digits = 2, booktabs = TRUE, format = "latex") %>%
    kable_styling(latex_options=c("hold_position"),  position = "center")%>%
  add_header_above(c(" " = 3, "Step 1" = 4, "Step 2" = 3))


q2_mat_red <- q2_mat[,-c(4:7)]

# Use all values and find optimal cut.
x1_splits <- c(8, 15, 16)
x2_splits <- 0

scores <- c()
for(i in 1:3){
  current_split <- ifelse(q2_mat$X1 >x1_splits[i], 1, 0)



G_0 <- sum(q2_mat$g2[current_split==0])
K_0 <- sum(q2_mat$k2[current_split==0])

G_1 <- sum(q2_mat$g2[current_split==1])
K_1 <- sum(q2_mat$k2[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
scores[i] <- -0.5*sum(G^2/K)
}

current_split <- ifelse(q2_mat$X2 >x2_splits[1], 1, 0)



G_0 <- sum(q2_mat$g2[current_split==0])
K_0 <- sum(q2_mat$k2[current_split==0])

G_1 <- sum(q2_mat$g2[current_split==1])
K_1 <- sum(q2_mat$k2[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
scores[4] <- -0.5*sum(G^2/K)

# Optimal cut at x1=8, calculate predictions
current_split <- ifelse(q2_mat$X1 >x1_splits[which.min(scores)], 1, 0)

G_0 <- sum(q2_mat$g2[current_split==0])
K_0 <- sum(q2_mat$k2[current_split==0])

G_1 <- sum(q2_mat$g2[current_split==1])
K_1 <- sum(q2_mat$k2[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
w <- -G/K

h1_yes <- w[2]
h1_no <- w[1]

q2_mat$`h2` <- ifelse(q2_mat$X1 > x1_splits[1], h1_yes, h1_no)

# Now we split on the remaining values
x1_splits <- c( 16)
x2_splits <- 0
scores <- c()

current_split <- ifelse(q2_mat$X1[q2_mat$X1>8] >x1_splits[1], 1, 0)

gvals <- q2_mat$g2[q2_mat$X1>8]
kvals <- q2_mat$k2[q2_mat$X1>8]
G_0 <- sum(gvals[current_split==0])
K_0 <- sum(kvals[current_split==0])

G_1 <- sum(gvals[current_split==1])
K_1 <- sum(kvals[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
scores[1] <- -0.5*sum(G^2/K)


current_split <- ifelse(q2_mat$X2[q2_mat$X1>8] >x2_splits[1], 1, 0)

gvals <- q2_mat$g2[q2_mat$X1>8]
kvals <- q2_mat$k2[q2_mat$X1>8]
G_0 <- sum(gvals[current_split==0])
K_0 <- sum(kvals[current_split==0])

G_1 <- sum(gvals[current_split==1])
K_1 <- sum(kvals[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)
scores[2] <- -0.5*sum(G^2/K)
# Pick x1 at 16
current_split <- ifelse(q2_mat$X1[q2_mat$X1>8] >x1_splits[1], 1, 0)

gvals <- q2_mat$g2[q2_mat$X1>8]
kvals <- q2_mat$k2[q2_mat$X1>8]
G_0 <- sum(gvals[current_split==0])
K_0 <- sum(kvals[current_split==0])

G_1 <- sum(gvals[current_split==1])
K_1 <- sum(kvals[current_split==1])

G <- c(G_0, G_1)
K <- c(K_0, K_1)

w <- -G/K

h1_yes <- w[2]
h1_no <- w[1]

newvals <- ifelse(q2_mat$X1[q2_mat$X1>8] > x1_splits[1], h1_yes, h1_no)
newvals <- c(NA, NA, newvals)


for(i in 3:6){
  q2_mat[i,11] <- newvals[i]
}


kable(q2_mat, digits = 2, booktabs = TRUE, format = "latex") %>%
    kable_styling(latex_options=c("hold_position"),  position = "center")%>%
  add_header_above(c(" " = 3, "Step 1" = 4, "Step 2" = 4))

vals <- list()
for(i in c(5,6,9,10)){
  vals[[i]] <- c(q2_mat[1,i] + q2_mat[2,i], q2_mat[3,i] + q2_mat[4,i],
          q2_mat[5,i] + q2_mat[6,i])
}
G1 <- unlist(vals[[5]])
G2 <- unlist(vals[[9]])
K1 <- unlist(vals[[6]])
K2 <-unlist(vals[[10]])

(-0.5*sum(G1^2/(K1+0.1))--0.5*sum(G2^2/(K2+0.1)))/(6)


# Load in the data.
df <- Auto
df2 <- df %>%
  mutate(mpg01 = ifelse(mpg > median(mpg), 1, -1)) %>%
  dplyr::select(-mpg)
# Create binary as stated above.
df2 <- df %>%
  mutate(mpg01 = ifelse(mpg > median(df$mpg), 1, -1)) %>%
  mutate(mpg01 = as.factor(mpg01)) %>%
  dplyr::select(-mpg, -name) %>%
  mutate(origin = as.factor(origin))

sq<-glm(mpg01~., data = df2, family = binomial)
model <- step(sq,direction = "both", trace = 0)
pairs(df, col = "darkblue")
df2 <- df2 %>%
  dplyr::select(horsepower, weight, year, origin, mpg01)

set.seed(14)
# Random sample.
train_set <- sample(1:nrow(df2), floor(0.8*nrow(df2)))

train <- df2[train_set,]

test <- df2[-train_set,]
set.seed(22)

# We randomly sample a set within the training.
looping_set <- sample(1:nrow(train), 250)

# Split the training for k-search.
training_data <- train[looping_set,]

# Pull the outcome.
outcome_train <- training_data %>% pull(mpg01) 

# Pull the testing for k-search. 
test_data <- train[-looping_set,]

# Initialize error for plotting.
error_rate <- c()

# Loop through all the rows available.
for(i in 1:250){
  # Predict for k=i
  knn_pred_y = knn(training_data,test_data,outcome_train,k=i)
  
  # Store the error rate.
  error_rate[i] = mean(test_data$mpg01 != knn_pred_y)
}

plot(error_rate, type = "l", xlab = "K values", ylab = "Error Rate",
     main = "Error rates for varying K-values")
points(which(error_rate == min(error_rate)),
       error_rate[which(error_rate == min(error_rate))], 
       col = "red", pch = "*")
table <- rbind("K" = which(error_rate == min(error_rate)),
"Error" = error_rate[which(error_rate == min(error_rate))])
kable(table, digits =2, booktabs = TRUE, format = "latex") %>%
    kable_styling(latex_options=c("hold_position"),  position = "center")
outcome_train <- train %>% pull(mpg01) 

knn_pred_y = knn(train,test,outcome_train,k=3)
error_rate[i] = mean(test$mpg01 != knn_pred_y)
confusionmat <- table( prediction = knn_pred_y, truth =test$mpg01)

accuracy <- sum(diag(confusionmat))/sum(confusionmat)

confusionmat
df <- Caravan
# First 1000.
train <- df[1:1000,]

# The remaining.
test <- df[-c(1:1000),]
# Model.
ada_model <- boosting(Purchase~., data=train, boos=FALSE, 
                      mfinal=100)

# How many of the 85 variables have zero importance.
sum(ada_model$importance == 0)

# Grab the top 5 important variables to compare.
imp_var <- ada_model$importance[order(ada_model$importance, 
                                      decreasing = T)]
imp_var <- imp_var[1:5]

# Plot importance.
barplot(imp_var,horiz=TRUE, cex.names=0.4)
# We begin by getting the data ready for use.
# Train data without outcome.
train_data <- train %>%
  dplyr::select(-Purchase)

# Train outcome as indicator.
train_label <- ifelse(train$Purchase == "Yes", 1, 0)

set.seed(14)
# Apply the xg model.
xg_model <- xgboost(data = as.matrix(train_data),
                    label = train_label, max.depth = 2, eta = 1,
                    nthread = 2, nrounds = 2, 
                    objective = "binary:logistic")

# Grab the important variables.
importance_matrix <- xgb.importance(model = xg_model)

# Predict using adaboost.
pred <- predict(ada_model, test)

# Bind th results.
result <- cbind(test, prob = pred$prob, pred = pred$class)

# We grab the probability of Purchasing == Yes.
# We will use this to change our cut-off at 0.2.
prediction <- pred$prob[,2]

prediction <- ifelse(prediction > 0.2, "Yes", "No")

table1 <- table( pred = prediction, truth = result$Purchase)

accuracy <- sum(diag(table1))/sum(table1)
accuracy
# Mutate the test data for xgboost.
test_data <-  test %>%
  dplyr::select(-Purchase)

pred <- predict(xg_model, as.matrix(test_data))

prediction <- ifelse(pred > 0.2, "Yes", "No")

table2 <- table( pred = prediction, truth = test$Purchase)

accuracy <- sum(diag(table2))/sum(table2)
accuracy
table(test$Purchase)/nrow(test)
logit_model <- glm(Purchase ~ ., data = train,
                   family = binomial(link = "logit"))

purchase_predicted <- predict(logit_model, newdata = test,
                              type = "response")

purchase_predicted <- ifelse(purchase_predicted > 0.2, 1, 0)

table(Predicted = purchase_predicted, Actual = test$Purchase)
