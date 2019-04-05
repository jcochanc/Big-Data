# Libraries.
pacman::p_load(dplyr, kableExtra, knitr, ggplot2, 
               ISLR, caret, scatterplot3d, rgl, calibrate, plotrix,
               pROC, e1071, usdm)
# First we initialize x1 and x2
x1 <- x2 <- seq(-1.5, 1.5, length.out = 10)

# Place points on a grid to plot.
grid <- expand.grid("x1" = x1,
                    "x2" = x2 )

# Create a classifier based on the question.
classifier <- grid$x2 <= (1+3*grid$x1)

plot(grid$x1[classifier == TRUE], grid$x2[classifier == TRUE],
     col = "lightblue", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5),
     xlab = expression(italic(X)[1]), ylab = expression(italic(X)[2]))
points(grid$x1[classifier == FALSE], grid$x2[classifier == FALSE],
       col = "lightpink")
abline(a = 1, b = 3, col ="black")
legend("topright", legend = c(expression(1 + 3*X[1] - X[2] >= 0),
                              expression(1 + 3*X[1] - X[2] < 0)),
       col = c("lightblue", "lightpink"),  pch = c(1,1),
       cex = 0.8,
       bg = "white")
# Create a classifier based on the question.
classifier_2 <- grid$x2 <=  (1-0.5*grid$x1)


plot(grid$x1[classifier == TRUE & classifier_2 == TRUE ],
     grid$x2[classifier == TRUE & classifier_2 == TRUE ],
     col = "lightblue", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5),
     xlab = expression(italic(X)[1]), ylab = expression(italic(X)[2]))
points(grid$x1[classifier == FALSE & classifier_2 == FALSE ],
       grid$x2[classifier == FALSE & classifier_2 == FALSE],
       col = "lightpink")
points(grid$x1[classifier == TRUE & classifier_2 == FALSE ],
       grid$x2[classifier == TRUE & classifier_2 == FALSE],
       col = "green")
points(grid$x1[classifier == FALSE & classifier_2 == TRUE ],
       grid$x2[classifier == FALSE & classifier_2 == TRUE],
       col = "purple")
abline(a = 1, b = 3, col ="black") # hyperplane 1
abline(a = 1, b = -0.5, col ="red") # hyperplane 2
legend("bottomright", legend = c(expression(1 + 3*X[1] - X[2] == 0),
                              expression(-2 + X[1] + 2*X[2] == 0)),
       col = c("black", "red"),  lty = c(1, 1),
       cex = 0.8,
       bg = "white")
# First we initialize x1 and x2
x1 <- seq(-4, 4, length.out = 1000)
x2 <- seq(-4, 8, length.out = 1000)
# Place points on a grid to plot.
grid <- expand.grid("x1" = x1,
                    "x2" = x2)

# Name the points we are looking for.
names <- c("A", "B", "C", "D")
x_id <- c(0, -1, 2, 3)
y_id <- c(0, 1, 2, 8)

# Plot.
plot(grid$x1, grid$x2, col = "blue", 
     xlab = expression(italic(X)[1]), ylab = expression(italic(X)[2]))
draw.circle(-1,2,radius = 2, col = "red")
textxy(x_id, y_id, labs = names, cx = 1, dcol = "black")
legend("bottomright", legend = c(expression((1 + X[1])^2 + (2 - X[2])^2 > 4),
                              expression((1 + X[1])^2 + (2 - X[2])^2 <= 4)),
       col = c("blue", "red"),  pch = c(20, 20),
       cex = 0.8,
       bg = "white")
# Set a seed so we can reproduce results.
set.seed(14)

# Draw as book suggests.
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1*(x1^2 - x2^2 > 0)
# First we plot the red and then the blue points.
plot(x1[y == 1], x2[y == 1], col = "red",
     xlab = expression(italic(X)[1]),
     ylab = expression(italic(X)[2]))
points(x1[y != 1], x2[y != 1], col = "blue")
legend("right", legend = c(expression(X[1]^2 - X[2]^2 > 0),
                              expression(X[1]^2 - X[2]^2 <= 0)),
       col = c("red", "blue"),  pch = c(1, 1),
       cex = 0.8,
       bg = "white")
# We wrangle the data.
df <- data.frame(cbind(y, x1, x2))

# Fit the logit.
fit_train <- glm(y ~ x1 + x2,
                 family = binomial(link = "logit"),
                 data = df)
# We predict using the model.
pred <- predict(fit_train, type = "response")

# Grab the best threshold on the training.
auc_roc <- roc(df$y, pred)

best_thresh <- coords(auc_roc, "best", ret= "threshold")

# Then use the threshold on the predicted data for training.
# We need this to classify i.e. color in the points.
y_pred <- ifelse(pred >= best_thresh, 1, 0)

# Finally, we plot.
plot(x1[y_pred == 1], x2[y_pred == 1], col = "red",
     xlab = expression(italic(X)[1]),
     ylab = expression(italic(X)[2]))
points(x1[y_pred != 1], x2[y_pred != 1], col = "blue")
legend("right", legend = c("predicted >= 0.47",
                            "predicted < 0.47"),
       col = c("red", "blue"),  pch = c(1, 1),
       cex = 0.8,
       bg = "white")
nonlinear_fit <- glm(y ~ poly(x1, 3, raw = TRUE) + poly(x2, 3, raw = TRUE)
                     + x1:x2,
                     family = binomial(link ="logit"),
                     data = df)


select_vars <- step(nonlinear_fit, direction = "both")
# Predict using just the cubic terms.
pred <- predict(nonlinear_fit, type = "response")

# Grab the new threshold.
auc_roc <- roc(df$y, pred)

best_thresh <- coords(auc_roc, "best", ret = "threshold")

# Classify using the new threshold.
y_pred <- ifelse(pred >= best_thresh, 1, 0)

# Plot.
plot(x1[y_pred == 1], x2[y_pred == 1], col = "red",
     xlab = expression(italic(X)[1]),
     ylab = expression(italic(X)[2]))
points(x1[y_pred != 1], x2[y_pred != 1], col = "blue")
legend("right", legend = c(expression(hat(P) >= 0.5),
                              expression(hat(P) < 0.5)),
       col = c("red", "blue"),  pch = c(1, 1),
       cex = 0.8,
       bg = "white")
# We rewrite the outcome as -1 or 1 as the book does.
df2 <- df %>%
  mutate(y = ifelse(y == 1, 1, -1)) %>%
  mutate(y = as.factor(y))

# Train the cost.
tune_out <- tune(svm , y~ . , 
                 data = df2,
                 kernel = "linear",
                 ranges = list(cost=c(0.001, 0.01, 1:10)))

# Grab the tuned costs in case we make a table.
costs <- summary(tune_out)

# Grab the best_cost.
best_cost <- costs$best.parameters

# Grab the best model.
best_model <- costs$best.model

# Plot using the best model on the training.
plot(best_model, df2)
# We fit a polynomial of degree 3 and 2, while tuning cost and gamma.
tune_out <- tune(svm , y~ . , 
                 data = df2,
                 kernel = "polynomial",
                 degree = 2:3,
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 2),
                               gamma = c(0.5,1,2,3,4) ))

costs <- summary(tune_out)
best_cost <- costs$best.parameters
best_model <- costs$best.model


plot(best_model, df2)
df <- Auto
# Create binary as stated above.
df2 <- df %>%
  mutate(above_median = ifelse(mpg > median(df$mpg), 1, -1)) %>%
  mutate(above_median = as.factor(above_median)) %>%
  dplyr::select(-mpg, -name) %>%
  mutate(origin = as.factor(origin))

sq<-glm(above_median~., data = df2, family = binomial)
model <- step(sq,direction = "both", trace = 0)
pairs(df, col = "darkblue")
set.seed(14)
tune_out <- tune(svm , above_median ~ horsepower + weight, 
                 data = df2,
                 kernel = "linear",
                 ranges =list(cost=seq(1, 100, length.out = 20)))

costs <- summary(tune_out)

# save the best model for later.
best_linear_model <- costs$best.model

table <- costs$performances[1:5,]
kable(table, caption = "Performance for Linear Kernel",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))

set.seed(14)
tune_out <- tune(svm , above_median ~ horsepower + weight,  
                 data = df2,
                 kernel = "polynomial",
                 ranges = list(cost = seq(1, 100, length.out = 20),
                              degree = 2:3))

costs <- summary(tune_out)

# save the best model for later.
best_poly_model <- costs$best.model

table <- costs$performances[c(9:11, 29:31),]

rownames(table) <- NULL

kable(table, caption = "Performance for Polynomial Kernel",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))
set.seed(14)
tune_out <- tune(svm , above_median ~ horsepower + weight, 
                 data = df2,
                 kernel = "radial",
                 ranges =list(cost=seq(1, 100, length.out = 20),
                              gamma = c(0.5,1,2,3,4)))

costs <- summary(tune_out)

# save the best model for later.
best_radial_model <- costs$best.model

table <- costs$performances[c(3, 23, 43, 63, 83),]
rownames(table) <- NULL

kable(table, caption = "Performance for Radial Kernel",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))

plot(best_linear_model, df2 , horsepower~weight)

plot(best_poly_model, df2 , horsepower~weight)

plot(best_radial_model, df2 , horsepower~weight)

# First we load in the data.
df <- OJ

names(df) <- tolower(names(df))
df <- df %>%
  mutate(storeid = as.factor(storeid),
         store = as.factor(store))
# We set a seed for reproducibility.
set.seed(13)

# Now we sample 800 obs.
in_train <- sample(nrow(df), 800, replace = FALSE)

# Create the training test.
train <- df[in_train, ]

# Create the test set.
test <- df[-in_train, ]
# Summary of the SVM.
summary(svm(purchase ~ ., data = train, kernel = "linear",cost = 0.01))

set.seed(14)
# The tuning performance.
tune_out <- tune(svm , purchase~ . ,
                 data = train,
                 kernel = "linear",
                 ranges = list(cost=c(0.01),
                               gamma = c(0.5,1,2,3,4)))

# Summary of the varying gammas.
costs <- summary(tune_out)

# Grab the best model.
best_model <- costs$best.model
# First we grab the predictions using our best model for test.
purchase_pre <- predict (best_model, test)

# The compute the error rates.
test_errors <- confusionMatrix(reference = purchase_pre,
                               data = test$purchase)

test_error_rate <- 1-test_errors$overall[1]

# We do the same for the training.
purchase_pre_train <- predict (best_model , train)

train_errors <- confusionMatrix(reference = purchase_pre_train,
                                data = train$purchase)

train_error_rate <- 1-train_errors$overall[1]

# Wrap them to make a table.
tables <- cbind(train_error_rate, test_error_rate)
rownames(tables) <- NULL

kable(tables, caption = "Error Rates with Fixed Cost, Optimized Gamma", booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))

set.seed(14)
tune_out <- tune(svm , purchase~ . , 
                 data = train,
                 kernel = "linear",
                 ranges = list(cost = seq(0.01, 10, length.out = 10)))

costs <- summary(tune_out)
best_model <- costs$best.model
linear_model <- best_model

table <- costs[["performances"]]
rownames(table) <- NULL

kable(table, caption = "Fixed Cost",
      booktabs = TRUE, format = "latex", digits = 4) %>%
  kable_styling(latex_options=c("hold_position"))

# First we grab the predictions using our best model for test.
purchase_pre <- predict (best_model , test)

# The compute the error rates.
test_errors <- confusionMatrix(reference = purchase_pre,
                               data = test$purchase)

test_error_rate <- 1-test_errors$overall[1]

# We do the same for the training.
purchase_pre_train <- predict (best_model , train)

train_errors <- confusionMatrix(reference = purchase_pre_train,
                                data = train$purchase)

train_error_rate <- 1-train_errors$overall[1]

# Wrap them to make a table.
tables <- cbind(train_error_rate, test_error_rate)
rownames(tables) <- NULL

kable(tables, caption = "Error Rates with Optimal Cost",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))

set.seed(14)
radial <- svm(purchase ~ ., 
              data = train,
              kernel = "radial",
              cost = 0.01)
set.seed(14)
tune_out <- tune(svm , purchase ~ ., 
                 data = train,
                 kernel = "radial",
                 ranges =list(cost = seq(0.01, 10, length.out = 10)))

costs <- summary(tune_out)

table <- costs$performances
best_model <- costs$best.model
radial_model <- best_model

kable(table, caption = "Performance for Radial Kernel",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))
# First we grab the predictions using our best model for test.
purchase_pre <- predict (best_model, test)

# The compute the error rates.
test_errors <- confusionMatrix(reference = purchase_pre,
                               data = test$purchase)

test_error_rate <- 1-test_errors$overall[1]

# We do the same for the training.
purchase_pre_train <- predict (best_model , train)

train_errors <- confusionMatrix(reference = purchase_pre_train,
                                data = train$purchase)

train_error_rate <- 1-train_errors$overall[1]

# Wrap them to make a table.
tables <- cbind(train_error_rate, test_error_rate)
rownames(tables) <- "Cost = 8.89"

# Repeat this process for the fixed cost radial model.
# First we grab the predictions using our best model for test.
purchase_pre <- predict(radial, test)

# The compute the error rates.
test_errors <- confusionMatrix(reference = purchase_pre,
                               data = test$purchase)

test_error_rate <- 1-test_errors$overall[1]

# We do the same for the training.
purchase_pre_train <- predict (radial , train)

train_errors <- confusionMatrix(reference = purchase_pre_train,
                                data = train$purchase)

train_error_rate <- 1-train_errors$overall[1]

table1 <- cbind(train_error_rate, test_error_rate)
rownames(table1) <- "Fixed Cost"

tables <- rbind(table1, tables)

kable(tables, caption = "Error Rates for Radial Kernel",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))
set.seed(14)
poly <- svm(purchase ~ ., 
              data = train,
              kernel = "poly",
              cost = 0.01,
              degree = 2)
set.seed(14)
tune_out <- tune(svm , purchase ~ ., 
                 data = train,
                 kernel = "poly",
                 ranges = list(cost = seq(0.01, 10, length.out = 10),
                               degree = 2))

costs <- summary(tune_out)

table <- costs$performances
best_model <- costs$best.model
poly_model <- best_model

kable(table, caption = "Performance for Radial Kernel",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))
# First we grab the predictions using our best model for test.
purchase_pre <- predict (best_model, test)

# The compute the error rates.
test_errors <- confusionMatrix(reference = purchase_pre,
                               data = test$purchase)

test_error_rate <- 1-test_errors$overall[1]

# We do the same for the training.
purchase_pre_train <- predict (best_model , train)

train_errors <- confusionMatrix(reference = purchase_pre_train,
                                data = train$purchase)

train_error_rate <- 1-train_errors$overall[1]

# Wrap them to make a table.
tables <- cbind(train_error_rate, test_error_rate)
rownames(tables) <- "Cost = 8.89"

# Repeat this process for the fixed cost radial model.
# First we grab the predictions using our best model for test.
purchase_pre <- predict(poly, test)

# The compute the error rates.
test_errors <- confusionMatrix(reference = purchase_pre,
                               data = test$purchase)

test_error_rate <- 1-test_errors$overall[1]

# We do the same for the training.
purchase_pre_train <- predict (poly , train)

train_errors <- confusionMatrix(reference = purchase_pre_train,
                                data = train$purchase)

train_error_rate <- 1-train_errors$overall[1]

table1 <- cbind(train_error_rate, test_error_rate)
rownames(table1) <- "Fixed Cost"

tables <- rbind(table1, tables)

kable(tables, caption = "Error Rates for Radial Kernel",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))
## NA
