---
layout: post
title: What is the valuable fraction of the barrel
author: Petr Baca
published: true
status: publish
draft: false
tags: R
---
##### What is the valuable fraction of the barrel
 
Demand for oil is in fact being driven by demand for oil products. That said, oil in its crude form has no use. If someone buys oil, he or she wants to transform crude oil into oil products (let's ignore traders) and then sell these products to end consumers. The process of conversion takes place in oil refineries. Oil products consumers are familliar with are the light-ends (incl. transportation fuels such as gasoline) and middle distillates (incl. heating oil, diesel, etc.).
 
The general rule is that the former is usually more expensive than the latter. This also makes light (less dense) sweet (less sulfur) crude oil more expensive than heavy (more dense) sour (more sulfur) crude oil as it is easier to convert the former into what is usually more valuable product (i.e. gasoline).
 
Because gasoline as well as heating oil have their on prices, we can easily calculate the difference between prices of products (i.e. final products or the output) and crude oil (i.e. the input). This is what is usually called a 'crack spread'. It is not only a proxy for refinery margins but it also allows us to find out what is currently the most valuable component of the barrel and even more importantly, it allows us to analyse its dynamics.
 
##### Crack spreads
 
A more formal definition of the crack spread is provided by the [EIA](https://www.eia.gov/todayinenergy/includes/CrackSpread_Explain.cfm) which says that '[a] crack spread measures the difference between the purchase price of crude oil and the selling price of finished products, such as gasoline and distillate fuel, that a refinery produces from the crude oil'.
 
Crack spreads are not only some calculated indicators but are also traded on [exchanges](http://www.cmegroup.com/trading/energy/crack-spread-handbook.html). 
So, for example, 3:2:1 crack spread means that we subtract the price of three barrels of crude oil from the price of two barrels of gasoline plus barrel of heating oil (and then divide the result by 3 to get the price per one barrel).
 
Accordingly, 2:1:1 crack spread means that we have one barrel of gasoline and one barrel of heating oil on the 'product side' of the equation.
 
In our analysis, we will use R and data from EIA API (for more details on how to download data, see the previous [post](http://petrbaca.github.io/2016/03/27/EIA-data/)). We will focus on futures based prices.
 

 

{% highlight r %}
library("xts")
tail(dat)
{% endhighlight %}



{% highlight text %}
##            Crude Gasoline HeatingOil
## 2016-04-19 41.08    1.480      1.263
## 2016-04-20 42.63    1.507      1.332
## 2016-04-21 43.18    1.516      1.300
## 2016-04-22 43.73    1.531      1.309
## 2016-04-25 42.64    1.513      1.290
## 2016-04-26 44.04    1.566      1.333
{% endhighlight %}
 
Ok, now we have our data so let's calculate price spreads and also crack spreads. To do that, we first have to convert gasoline and heating oil prices from USD/gallon to USD/barrel (by multiplying by 42).
 

{% highlight r %}
dat[,c("Gasoline", "HeatingOil")] <- dat[,c("Gasoline", "HeatingOil")] * 42
 
# differences between gasoline and heating oil prices vs crude
dat$GasDiff <- dat[,"Gasoline"] - dat[,"Crude"]
dat$HoDiff <- dat[,"HeatingOil"] - dat[,"Crude"]
 
# 3:2:1 and 2:1:1 crack spreads
dat$CS_321 <- 2/3 * dat[,"GasDiff"] + 1/3 * dat[,"HoDiff"]
dat$CS_211 <- 1/2 * dat[,"GasDiff"] + 1/2 * dat[,"HoDiff"]
 
# weekly averages
dat_w <- apply.weekly(dat, mean, na.rm=T)
# monthly averages
dat_m <- apply.monthly(dat, mean, na.rm=T)
 
# select data for plotting
plot_data <- dat_m
 
op <- par(no.readonly = T)
par(adj = 1, cex = 0.8, bty = "n", lwd = 2)
plot(plot_data[,"CS_211"], auto.grid = F,
     ylim = range(plot_data[,c("GasDiff","HoDiff")]),
     main = "Spreads vs. crude oil price", ylab = "USD/barrel")
lines(plot_data[,"GasDiff"], lty = 2, lwd = 1)
lines(plot_data[,"HoDiff"], lty = 3, lwd = 1)
abline(h=0, col = "#BBBBBB")
rect(xleft = as.POSIXct("2007-11-01"), ybottom = -100,
     xright = as.POSIXct("2009-06-30"), ytop = 100,
     col = "#AAAAAA50", border = NA)
legend("topleft",
       legend = c("2:1:1 crack spread", "gasoline spread","heating oil spread"),
       lty = 1:3, lwd = c(2,1,1),
       bty = "n")
{% endhighlight %}

![plot of chunk crk_spd_calc](/figures/crk_spd_calc-1.png)

{% highlight r %}
par(op)
{% endhighlight %}
 
##### What do the spreads tell us?
 
The plot above shows what actually drives the evolution of the crack spread (crack spread is just an average of gasoline and heating oil spreads). The plot clearly shows that processing crude into heating oil has been less and less profitable.
 
The plot also shows that the recent surge in oil prices has probably had something to do with rising gasoline prices. On the other hand, it is not clear if this year's situation is extraordinary. To check this we will seasonally decompose the series (STL) and do the seasonality plot.
 

{% highlight r %}
seas_dat <- dat_m[,"GasDiff"]
seas_dat <- ts(coredata(seas_dat),
               start = as.numeric(unlist(strsplit(as.character(start(seas_dat)), "-"))[1:2]),
               freq = 12)
 
 
seas_dat_sa <- stl(seas_dat[,1], s.window = 11)
plot(seas_dat_sa, main = "Gasoline difference, seasonal decomposition")
{% endhighlight %}

![plot of chunk seasonality_check](/figures/seasonality_check-1.png)

{% highlight r %}
op <- par(no.readonly = T)
par(bty = "n", cex = 0.8, adj = 1)
boxplot(window(seas_dat, end = c(2015, 12)) ~ cycle(window(seas_dat, end = c(2015, 12))), col = "#EEEEEE", border = "#AAAAAA",
        main = "Seasonality plot (2006 - 2015), gasoline difference",
        ylab = "USD/barrel", xlab = "month")
lines(coredata(window(seas_dat, start = c(2015, 1))), col = 1, lwd = 3, lty = 3)
lines(coredata(window(seas_dat, start = c(2016, 1))), col = 1, lwd = 3)
legend("topleft", legend = c("2015" ,"2016"), lty = c(1,3), lwd = 3, bty = "n")
{% endhighlight %}

![plot of chunk seasonality_check](/figures/seasonality_check-2.png)

{% highlight r %}
par(op)
{% endhighlight %}
 
While the first plot clearly shows there's a strong seasonal component, the second plot unveils that this year's situation is not extraordinary at all. Quite contrary, despite its recent surge, this year's gasoline crack spread is still below its medium-term median and below the levels observed last year. And that holds in spite of the fact that the most recent monhly set of data from the Energy Information Administration showed the largest increase in US gasoline consumption in about 40 years. So, at least for now, the US refineries seem to be well prepared for high summer demand for gasoline.
 
 
 
 
 
 
 
 
 