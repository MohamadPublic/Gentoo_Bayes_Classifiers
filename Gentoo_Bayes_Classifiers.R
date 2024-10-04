library(palmerpenguins)
library(mnormt)
library(plotly)
library(class)

df = palmerpenguins::penguins
df = subset(df, df$species == 'Gentoo')
df <- data.frame(Sex = df$sex,
                 Flipper_mm = df$flipper_length_mm,
                 Bill_mm = df$bill_length_mm)
df = na.omit(df)

male = subset(df, df$Sex =='male')
female = subset(df, df$Sex =='female')

mu_m  = c(mean(male$Flipper_mm), mean(male$Bill_mm))
mu_f  = c(mean(female$Flipper_mm), mean(female$Bill_mm))

X_m = data.frame(Flipper_mm = male$Flipper_mm,
                 Bill_mm = male$Bill_mm)

X_f = data.frame(Flipper_mm = female$Flipper_mm,
                 Bill_mm = female$Bill_mm)

sigma_m = cov(X_m)
sigma_f = cov(X_f)


x = seq(200, 240, 0.5)

y = seq(40, 60, 0.5)

f_m <- function(x, y) dmnorm(cbind(x, y), mu_m, sigma_m)
f_f <- function(x, y) dmnorm(cbind(x, y), mu_f, sigma_f)

z_m <- outer(x, y, f_m)
z_f <- outer(x, y, f_f)


par(mfrow=c(1,2))
contour(x, y, z_m,  xlab ="Flipper Length (mm)", ylab="Bill Length (mm)", main = 'Male Gentoo Penguins')
contour(x, y, z_f, xlab ="Flipper Length (mm)", ylab="Bill Length (mm)", main = 'Female Gentoo Penguins')


par(mfrow=c(1,1))
grid_data <- expand.grid(Flipper_mm = x, Bill_mm = y)
z_m_flat <- as.vector(z_m)
z_f_flat <- as.vector(z_f)


fig <- plot_ly() %>%
  add_surface(x = ~x, y = ~y, z = ~z_m, colorscale = list(c(0, 1), c('blue', 'blue')), showscale = FALSE, opacity = 0.5, name = 'Male Gentoo') %>%
  add_surface(x = ~x, y = ~y, z = ~z_f, colorscale = list(c(0, 1), c('red', 'red')), showscale = FALSE, opacity = 0.5, name = 'Female Gentoo') %>%
  layout(scene = list(
    xaxis = list(title = 'Flipper Length (mm)', range = c(min(x), max(x))),
    yaxis = list(title = 'Bill Length (mm)', range = c(min(y), max(y))),
    zaxis = list(title = 'Density'),
    title = '3D Distributions of Gentoo Penguins'),
    legend = list(x = 0.1, y = 0.9)
  )

fig

QDA = function(X, pi_m, mu_m, sigma_m, mu_f, sigma_f){
  
  likelihood_male = dmnorm(X, mean = mu_m, varcov = sigma_m)
  
  likelihood_female = dmnorm(X, mean = mu_f, varcov = sigma_f)
  
  prior = pi_m
  
  evidence = likelihood_male*prior + likelihood_female*(1-prior)
  
  posterior = (likelihood_male*prior)/evidence
  
  return(posterior)
  
}

pi_m = nrow(male)/nrow(df)

X = data.frame(Flipper_mm = df$Flipper_mm,
               Bill_mm = df$Bill_mm)

preds = QDA(X, pi_m, mu_m, sigma_m, mu_f, sigma_f)

ytrue = ifelse(df$Sex == 'male', 1, 0)
ypred = round(preds)
paste('The accuracy achieved is ', mean(ytrue==ypred))

predictors = df[,c("Flipper_mm", "Bill_mm" )]
grid = expand.grid(x=201:240, y=41:60) # make a list of grid points

prob.grid = QDA(grid, pi_m, mu_m, sigma_m, mu_f, sigma_f)

contour(x=201:240, y=41:60, z=matrix(prob.grid, nrow=40), levels=0.5,
        col="purple", drawlabels=FALSE, lwd=2, xlab = "Flipper Length (mm)", ylab = "Bill Length (mm)")
pi_m = nrow(male)/nrow(df) #Prior (proportion of males in training set)


QDA = function(X, pi_m, mu_m, sigma_m, mu_f, sigma_f){
  
  likelihood_male = dmnorm(X, mean=mu_m, varcov = sigma_m)
  
  likelihood_female = dmnorm(X, mean=mu_f, varcov = sigma_f)
  
  prior = pi_m
  
  evidence = likelihood_male*prior + likelihood_female*(1-prior)
  
  posterior = (likelihood_male*prior)/evidence 
  
  return (posterior)
}


preds = QDA(X, pi_m, mu_m, sigma_m, mu_f, sigma_f)

ytrue = ifelse(df$Sex == 'male', 1, 0)
ypred = round(preds)
paste('The accuracy achieved is ', mean(ytrue==ypred))



library(class)
predictors = df[,c("Flipper_mm", "Bill_mm" )]
grid = expand.grid(x=201:240, y=41:60) # make a list of grid points

prob.grid = QDA(grid, pi_m, mu_m, sigma_m, mu_f, sigma_f)

contour(x=201:240, y=41:60, z=matrix(prob.grid, nrow=40), levels=0.5,
        col="purple", drawlabels=FALSE, lwd=2, xlab = "Flipper Length (mm)", ylab = "Bill Length (mm)")


points(df$Flipper_mm, df$Bill_mm, col=df$Sex)
legend('bottomright',legend = c("Female", "Male"), col = 1:2, lty=1)


###################### VARIANT: LDA ###############################

sigma_pooled = (1/2)*(sigma_f+sigma_m)

preds = QDA(X, pi_m, mu_m, sigma_pooled, mu_f, sigma_pooled)

ytrue = ifelse(df$Sex == 'male', 1, 0)
ypred = round(preds)
paste('The accuracy achieved is ', mean(ytrue==ypred))


predictors = df[,c("Flipper_mm", "Bill_mm" )]
grid = expand.grid(x=201:240, y=41:60) # make a list of grid points

prob.grid = QDA(grid, pi_m, mu_m, sigma_pooled, mu_f, sigma_pooled)

contour(x=201:240, y=41:60, z=matrix(prob.grid, nrow=40), levels=0.5,
        col="purple", drawlabels=FALSE, lwd=2, xlab = "Flipper Length (mm)", ylab = "Bill Length (mm)")


points(df$Flipper_mm, df$Bill_mm, col=df$Sex)
legend('bottomright',legend = c("Female", "Male"), col = 1:2, lty=1)

############ VARIANT: NB Per-Class Variances #######################

var_flipper_m = var(male$Flipper_mm)
var_bill_m = var(male$Bill_mm)
sigma_m_diag = matrix( c(var_flipper_m, 0,0, var_bill_m),
                       nrow=2,
                       ncol=2)


var_flipper_f = var(female$Flipper_mm)
var_bill_f = var(female$Bill_mm)
sigma_f_diag = matrix( c(var_flipper_f, 0,0, var_bill_f),
                       nrow=2,
                       ncol=2)

preds = QDA(X, pi_m, mu_m, sigma_m_diag, mu_f, sigma_f_diag)

ytrue = ifelse(df$Sex == 'male', 1, 0)
ypred = round(preds)
paste('The accuracy achieved is ', mean(ytrue==ypred))


predictors = df[,c("Flipper_mm", "Bill_mm" )]
grid = expand.grid(x=201:240, y=41:60) # make a list of grid points

prob.grid = QDA(grid, pi_m, mu_m, sigma_m_diag, mu_f, sigma_f_diag)

contour(x=201:240, y=41:60, z=matrix(prob.grid, nrow=40), levels=0.5,
        col="purple", drawlabels=FALSE, lwd=2, xlab = "Flipper Length (mm)", ylab = "Bill Length (mm)")


points(df$Flipper_mm, df$Bill_mm, col=df$Sex)
legend('bottomright',legend = c("Female", "Male"), col = 1:2, lty=1)


############ VARIANT: NB Pooled Variance #######################

sigma_pooled = (1/2)*(sigma_m_diag + sigma_f_diag)

preds = QDA(X, pi_m, mu_m, sigma_pooled, mu_f, sigma_pooled)

ytrue = ifelse(df$Sex == 'male', 1, 0)
ypred = round(preds)
paste('The accuracy achieved is ', mean(ytrue==ypred))


predictors = df[,c("Flipper_mm", "Bill_mm" )]
grid = expand.grid(x=201:240, y=41:60) # make a list of grid points

prob.grid = QDA(grid, pi_m, mu_m, sigma_pooled, mu_f, sigma_pooled)

contour(x=201:240, y=41:60, z=matrix(prob.grid, nrow=40), levels=0.5,
        col="purple", drawlabels=FALSE, lwd=2, xlab = "Flipper Length (mm)", ylab = "Bill Length (mm)")


points(df$Flipper_mm, df$Bill_mm, col=df$Sex)
legend('bottomright',legend = c("Female", "Male"), col = 1:2, lty=1)









