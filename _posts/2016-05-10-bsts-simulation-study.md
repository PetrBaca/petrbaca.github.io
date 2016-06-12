---
layout: post
title: How to deal with confusion when forecasting time series data
author: Petr Baca
published: true
status: publish
draft: false
tags: R
---
##### How to choose the right explanatory variables?
 
One of the key issues when forecasting time series data is to find relevant information that may help with this difficult task.
 
Of course, we could for example simply calculate correlation coefficients between the series of interest and prospective explanatory variables. Or we could perform Granger causality tests.
 
But if there's many potential explanatory series available, these tools may not be efficient enough. And to be honest, they seem to be a bit outdated:-).
 
##### Use 'bsts'!
 
Fortunatelly, a group of researchers at Google led by [Steven L. Scott](http://research.google.com/pubs/author57989.html) put together [this](http://cran.r-project.org/web/packages/bsts/bsts.pdf) package. Its name, 'bsts', stands for **B**ayesian **S**tructural **T**ime **S**eries models.
 
A 'bsts' time series model generally comprises several 'building blocks'. These may or may not include trend, seasonality and regression.
 
The purpose of the text below is to perform a simple simulation study that will verify how good the functions in the 'bsts' library actually are in reconstructing the underlying mechanism that actually generated the data.
 
##### The design of our experiment
 
We will basically create an artificial time series that would consist of two components - trend and regression.
 
At the same time, we will try to 'confuse' the function by considering irrelevant information. That is, we will also include explanatory variables that we know (in advance) have no effect on the time series of interest. Consequently, we will check how good the algorithm is at identification of the 'right' explanatory variables.
 
Of course, the proposed study is not perfect. But in the vast majority of 'real life' situations, we want to predict a behaviour of some time series and do not have a clue (for example in a form of some theory) which variables could help us. But if we have some algorithm at hand that claims it could select the right explanatory variables for us, we would expect it would perform reasonably well in a 'laboratory' conditions sketched above.
 
Ok, so let's dig in. Hopefully, the code below will shed a bit more light on our trial.
 

 
So, we generated 50*192 = 9600 values from normal distribution, converted them into 50 variables (with length 192 = length(ind)) and saved this into matrix __X__ (which therefore has dimensions of 192 x 50).
 
Then, we created (50 x 1) matrix of regression coefficients, or __beta__. Than we randomly picked ten of those fifty parameters and set their values not equal to zero.
 
Then we calculated response variable __y__ in such a way that it consists of two deterministic components - trend and regression.
 
The first one is classic "random walk" model (cummulative sum of random shocks from normal distribution centered at zero) which is basically the trend of variable __y__. The second component is regression component obtained by multiplication of matrix __X__ by matrix __beta__.
 
Finally, we add "white noise" component, i.e. 192 values randomly drawn from normal distribution.
 
In the end, we converted all relevant data to 'xts' objects (and we finally used __ind__ variable that we actually created first). Our data is therefore monthly data that cover years 2000 - 2015.
 
Ok, so let's plot the data first.
 

{% highlight r %}
op <- par(no.readonly = T)
par(mfrow = c(2,1), cex = 0.7, adj = 1, bty = "n", mar = c(4,4,2,2), las = 1)
 
plot(y, auto.grid = F, main = "Variable of interest (y)")
 
x_col <- rep("#AAAAAA50", n_var)
x_col[beta != 0] <- 1:10 + 1
plot(X[,1], type = "n", ylim = range(X), main = "Explanatory variables (X)", auto.grid = F)
for(i in 1:n_var) {
  lines(X[,i], col = x_col[i])
}
{% endhighlight %}

![plot of chunk plot_data](/figures/plot_data-1.png)

{% highlight r %}
par(op)
{% endhighlight %}
 
For greater clarity, the variables whose beta coefficients are not equal to zero are coloured (see the bottom panel of the plot). So as you can see, the task to pick the 'right' variables is pretty simple :-).
 
Ok, so let's try the 'bsts' algorithm.
 

{% highlight r %}
library("bsts")
 
# first convert data back to matrices
y <- coredata(y)
X <- coredata(X)
 
# prior for trend
trend_prior <- AddLocalLevel(state.specification = list(), y = y)
 
# prior for regression component
reg_prior <- SpikeSlabPrior(X, y,
                            expected.model.size = 10, # expect 10 nonzero predictors
                            diagonal.shrinkage = 0, # use Zellner's prior
                            optional.coefficient.estimate = rep(0, n_var)) # shrink to 0
 
# modelling
model <- bsts(y ~ X - 1,
              state.specification = trend_prior,
              prior = reg_prior,
              niter = 1e4, ping = 0)
 
# extract coeffs after burn-in period
coefs <- model$coefficients[-1:-0.5e4,]
 
# calculate inclusion probability
incl_prob <- (colSums(coefs!=0) / 0.5e4)
 
# get coef stats if not zero
coef_val <- t(apply(coefs, 2, function(x) quantile(x[x!=0], probs = c(0.025, 0.50, 0.975))))
 
# merge results
results <- cbind(incl_prob, coef_val, beta)
colnames(results) <- c("inclusion probability",
                       "5%", "50%", "95%",
                       "true beta")
 
op <- par(no.readonly = T)
par(cex = 0.7, adj = 1, bty = "n", mar = c(4,4,2,6), las = 1)
 
plot(beta, col = 3, cex = 1.5, pch = 1, type = "p",
     ylim = c(-3, 3), yaxt = "n",
     main = "Model vs. THE Truth",
     xlab = "index of beta", ylab = "")
axis(2, line = 1)
lines(coef_val[,2], lwd = 2, type = "p", pch = 19, cex = 0.6)
#lines(coef_val[,1], lty = 3)
#lines(coef_val[,3], lty = 3)
abline(h = 0, col = "#CCCCCC")
abline(v = nonzero_beta, col = "#CCCCCC", lty = 3)
 
par(new = T)
plot(incl_prob, col = 2, yaxt = "n", xlab = "", ylab = "", pch = 3)
axis(4, line = 1, col = 2, col.axis = 2)
#abline(h = 0:5*0.2, col = "#FF000050", lty = 2)
 
legend(x = 1, y = 0.3,
       legend = c("true beta", "estimate median", "incl. prob. (r.axis)"),
       bg = "#FFFFFF", box.col = "#FFFFFF",
       pch = c(1, 19,  3),
       #lty = c(NA, 1, NA),
       #lwd = c(NA, 2, NA),
       col = c(3, 1, 2))
{% endhighlight %}

![plot of chunk simulation_study](/figures/simulation_study-1.png)

{% highlight r %}
par(op)
{% endhighlight %}
 
##### Results
 
It is clear from the picture that in the case above the model does a pretty good job in identification of the underlying process that has generated the data!
 
The last plot shows not only that the model in the vast majority of cases unveiled which regression coefficients likely are non-zero (red crosses show the inclusion probability - right axis), but also very precisely estimated their values (left axis, green and black dots).
 
Of course, this is just one experiment and to be able to assess the performance of functions in the __bsts__ library, we should probably do more simulations like the one above.
 
But our example shows that the functions in the __bsts__ library truly are more than promising!
