## ---- echo = FALSE-------------------------------------------------------
# Libraries.
pacman::p_load(dplyr, kableExtra, knitr, ggplot2, 
               ISLR, corrplot, caret, glmnet, MASS,
               usdm, leaps, pROC)

## ------------------------------------------------------------------------
# We set the seed as the problem asks.
set.seed(1)

## ------------------------------------------------------------------------
# Drawing 100 psuedo RV from N(0, 1).
x <- rnorm(100)

## ------------------------------------------------------------------------
# Drawing 100 psuedo RS from N(0, 0.5^2).
eps <- rnorm(100, mean = 0, sd = 0.5)

## ------------------------------------------------------------------------
# Build y according to the model.
y <- -1 + 0.5*x + eps

## ------------------------------------------------------------------------
length(y)

## ---- echo = FALSE, fig.align = "center", fig.asp = .68, fig.width=4.5, warning = FALSE----
# Bind the data for ease in ggplot.
df <- data.frame(cbind(y, x))

# Scatterplot with ggplot.
ggplot(df, aes(x, y)) +
  geom_point() +
  theme_minimal()

## ---- echo = FALSE-------------------------------------------------------
# Now we fit a model to the data.
lm_fit <- lm(y ~ x, data = df)

# Let's examine the estimated intercept and slope.
coefs <- round(summary(lm_fit)$coefficients, 2)

# Clean up p-vals.
coefs[,4] <- "<0.0001"

# Make table.
kable(coefs, caption = "Estimated Coefficients",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))

## ---- echo = FALSE, fig.align = "center", fig.asp = .68, fig.width=4.5, warning = FALSE----

# Scatterplot with ggplot.
plot(df$x, df$y, pch  = 20, xlab = "X", ylab = "Y")
abline(a = as.numeric(coefs[1, 1]),
       b = as.numeric(coefs[2,1]), col="red")
abline(a =-1, b = 0.5, col="blue")
legend(x = 0.75, y =-1.5, legend = c("Regresion Line", "Population Line"), lty=c(1,1), col = c("red", "blue"), cex=0.6,
       box.lty=0)

## ------------------------------------------------------------------------
# Fit a quadratic model.
quad_fit <- lm(y ~ poly(x, 2, raw = TRUE), data = df)

## ------------------------------------------------------------------------
summary(quad_fit)$coefficients

## ------------------------------------------------------------------------
anova(lm_fit, quad_fit)

## ------------------------------------------------------------------------
# Following the book.
set.seed(1)

# Drawing 100 PRV from a U(0, 1).
x1 <- runif(100)

# Creating x2 which follows a model:
# Y = 0.5*x1 + N(0,1)/10
x2 <- 0.5 * x1 + rnorm (100) / 10

# Builing y according to the model below.
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

## ------------------------------------------------------------------------
cor(x1, x2)

## ---- echo = FALSE, fig.align = "center", fig.asp = .68, fig.width=4.5, warning = FALSE----
# Bind the new data.
df2 <- data.frame(cbind(x1, x2, y))

# Scatterplot with ggplot.
ggplot(df2, aes(x1, x2)) +
  geom_point() 


## ------------------------------------------------------------------------
# The linear model on x1 and x2.
fit <- lm(y ~ x1 + x2, data = df2)

## ---- echo = FALSE-------------------------------------------------------
# Let's examine the estimated intercept and slope.
coefs <- round(summary(fit)$coefficients, 2)

# Clean up p-vals.
coefs[1,4] <- "<0.0001"

# Make table.
kable(coefs, caption = "Estimated Coefficients",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))

## ------------------------------------------------------------------------
# Fitting model with only x1.
fit_x1 <- lm(y ~ x1, data = df2)

## ---- echo = FALSE-------------------------------------------------------
# Let's examine the estimated intercept and slope.
coefs <- round(summary(fit_x1)$coefficients, 2)

# Clean up p-vals.
coefs[,4] <- "<0.0001"

# Make table.
kable(coefs, caption = "Estimated Coefficients",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))

## ------------------------------------------------------------------------
# Fit model only on x2.
fit_x2 <- lm(y ~ x2, data = df2)

## ---- echo = FALSE-------------------------------------------------------
# Let's examine the estimated intercept and slope.
coefs <- round(summary(fit_x2)$coefficients, 2)

# Clean up p-vals.
coefs[,4] <- "<0.0001"

# Make table.
kable(coefs, caption = "Estimated Coefficients",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))

# Save the three fits for later comparison.
fits_100 <- list(fit, fit_x1, fit_x2)

## ------------------------------------------------------------------------
x1 <- c(x1 , 0.1)
x2 <- c(x2 , 0.8)
y <- c(y,6)

# Save them for later use.
df3 <- data.frame(cbind(x1, x2, y))

## ---- echo = FALSE-------------------------------------------------------
# Refitting the three models.
fit <- lm(y ~ x1 + x2, data = df3)

fit_x1 <- lm(y ~ x1, data = df3)

fit_x2 <- lm(y ~ x2, data = df3)

# Save for later use.
fits_101 <- list("fit" = fit, "fit_x2" = fit_x1, "fit_x2" = fit_x2)

## ---- echo = FALSE-------------------------------------------------------
# Let's examine the estimated intercept and slope.
coefs <- round(summary(fit)$coefficients, 2)
coefs_x1 <- round(summary(fit_x1)$coefficients, 2)
coefs_x2 <- round(summary(fit_x2)$coefficients, 2)

all_coefs <- rbind(coefs, coefs_x1, coefs_x2)

# Clean up p-vals.
all_coefs[-c(2,3),4] <- "<0.0001"

# Make table.
kable(all_coefs, caption = "Estimated Coefficients",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position")) %>%
  group_rows("Full Model", 1, 3) %>%
  group_rows("X_1 Model", 4, 5) %>%
  group_rows("X_2 Model", 6, 7)

## ---- echo = FALSE, fig.width=8, fig.height=3----------------------------
par(mfrow=c(1 ,3))

for(i in 1:3){
    plot(studres(fits_101[[i]]), main = names(fits_101)[i],
         ylab = "Studentized Residuals",
         xlab = "Fitted Values") +
    abline(h = 3, col = "red") +
    abline(h = -3, col = "red")
  
  }
par(mfrow=c(1,3))


## ---- echo = FALSE, fig.width=8, fig.height=3----------------------------
par(mfrow=c(1,3))
plot(fit, 5, main = "fit")
plot(fit_x1, 5, main ="fit_x1")
plot(fit_x2, 5, main ="fit_x2")
par(mfrow=c(1,3))

## ------------------------------------------------------------------------
# Bring in the data.
df <- College

# Clean the variable names.
names(df) <- tolower(names(df))

## ------------------------------------------------------------------------
set.seed(1)

# We will split the data 80-20 train vs test.
in_training <- createDataPartition(df$apps, p = .80,
                                   list = F, times = 1)
# Split into training.
train <- df[in_training, ]

# Split into testing.
test <- df[-in_training, ]

## ---- echo = FALSE, fig.align = "center", fig.asp = .68, fig.width=4.5, warning = FALSE----
M <- cor(df %>% dplyr::select(-private))
corrplot(M, method = "circle")

## ---- echo = FALSE-------------------------------------------------------
fit <- lm(apps ~ ., data = train)

fit.pred <- predict(fit, newdata = test)

mean((test$apps-fit.pred)^2)

## ---- echo = FALSE, fig.align = "center", fig.asp = .68, fig.width=4.5, warning = FALSE----
# We set a grid; for now we use one the book ISLR uses.
grid <- 10^seq (10,-2, length =100)

design_matrix <- model.matrix(apps ~ ., data = train) 
design_matrix <- design_matrix[, -1]# remove int.

# Setting alpha = 0 chooses the ridge within the glmnet.
lambda_cv <- cv.glmnet(design_matrix, train$apps,
                       lambda = grid, nfolds = 5, alpha = 0)

plot(lambda_cv)

## ------------------------------------------------------------------------
lambda_cv$lambda.min

## ------------------------------------------------------------------------
lambda_cv$lambda.1se

## ------------------------------------------------------------------------
ridge <- glmnet(design_matrix, 
                train$apps, alpha = 0, lambda = lambda_cv$lambda.1se)

test_design_matrix <- model.matrix(apps ~ ., data = test)

test_design_matrix <- test_design_matrix[ , -1]

ridge.pred <- predict.glmnet(ridge, newx = test_design_matrix)

mean((test$apps-ridge.pred)^2)

## ---- echo = FALSE, fig.align = "center", fig.asp = .68, fig.width=4.5, warning = FALSE----
# Setting alpha = 1 chooses the lasso within the glmnet.
lambda_cv <- cv.glmnet(design_matrix, train$apps,
                       lambda = grid, nfolds = 5, alpha = 1)

plot(lambda_cv)

## ------------------------------------------------------------------------
lambda_cv$lambda.min

lambda_cv$lambda.1se

## ------------------------------------------------------------------------
lasso <- glmnet(design_matrix, 
                train$apps, alpha = 1, lambda = lambda_cv$lambda.1se)

lasso_pred <- predict.glmnet(lasso, newx = test_design_matrix)

mean((test$apps-lasso_pred)^2)

## ------------------------------------------------------------------------
# Load in the ISLR Weekly data.
df <- Weekly

# Fix names for ease.
names(df) <- names(df) %>% tolower()

## ---- echo = FALSE, fig.align = "center", fig.asp = .78, fig.width=6.5, warning = FALSE----
# Pairs plot.
pairs(df %>% dplyr::select(-direction), cex.labels=1, col="dark blue")

## ---- echo = FALSE-------------------------------------------------------

vifval <- vif(df %>% dplyr::select(-direction))

# Make table.
kable(vifval, caption = "Variance Inflation Factor", digits = 3,
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))


## ---- echo = FALSE, fig.align = "center", fig.asp = .68, fig.width=4.5, warning = FALSE----

M <- cor(df %>% dplyr::select(-direction, -year))
corrplot(M, method = "ellipse")

## ----echo = FALSE--------------------------------------------------------
logit_fit <- glm(direction ~ . - today - year, data = df,
                 family = binomial(link = "logit")) 

# Let's examine the estimated intercept and slope.
coefs <- round(summary(logit_fit)$coefficients, 2)

# Clean up p-vals.
coefs[1 , 4] <- "<0.001"

# Make table.
kable(coefs, caption = "Estimated Coefficients",
      booktabs = TRUE, format = "latex") %>%
  kable_styling(latex_options=c("hold_position"))

## ---- echo = FALSE-------------------------------------------------------
prediction <- predict(logit_fit, data = df, type =  "response")
predicted_direction <- ifelse(prediction >0.5, 1, 0) %>% as.factor()
levels(predicted_direction) <- c("Down", "Up")

confusionMatrix(predicted_direction, df$direction)$table

## ---- echo = FALSE-------------------------------------------------------
# Filter to data only up to 2208 including.
train <- df %>% filter(year < 2009 )  %>% dplyr::select(-today, -year)

# Remaining is test.
test <- df %>% filter(!year < 2009 )  %>% dplyr::select(-today, -year)


logit_fit <- glm(direction ~ lag2, data = train,
                 family = binomial(link = "logit"))

prediction <- predict(logit_fit,
                      newdata = test %>% dplyr:: select(direction, lag2),
                      type =  "response")

predicted_direction <- ifelse(prediction >0.5, 1, 0) %>% as.factor()
levels(predicted_direction) <- c("Down", "Up")
confusionMatrix(predicted_direction, test$direction)$table

## ---- echo = FALSE, fig.width=8, fig.height=3----------------------------
par(mfrow=c(1 ,2))
plot(df$volume, ylab = "volume", main = "No Tranform")
plot(log(df$volume), ylab = "log volume", main = "Log Tranform")

par(mfrow=c(1,2))


## ---- echo = FALSE, include = FALSE--------------------------------------
set.seed(14)
fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

glm_fit <- train(direction ~ (.-volume+I(log(volume)))^2, data = train, 
                    method = "glmStepAIC", 
                    trControl = fitControl,
                    na.action = na.omit,
                    metric = "ROC")

## ------------------------------------------------------------------------
glm_fit$finalModel

## ---- echo = FALSE-------------------------------------------------------
final_model <-  glm(direction ~ lag1 +lag2 + lag5:volume,
                data = train,
                family=binomial)



test$pred <- predict(final_model, newdata = test, type = "response")

auc_roc <- roc(test$direction, test$pred)

auc(auc_roc)

coords(auc_roc, "best", ret=c("threshold", "specificity", "1-npv"))


## ---- echo = FALSE-------------------------------------------------------
predicted_direction <- ifelse(test$pred >0.5293889, 1, 0) %>% as.factor()
levels(predicted_direction) <- c("Down", "Up")
confusionMatrix(predicted_direction, test$direction)$table

## ----code = readLines(knitr::purl("C:/Users/jcochanc/Documents/Big-Data/Homework/hw1.Rmd", documentation = 1)), echo = T, eval = F----
## NA

