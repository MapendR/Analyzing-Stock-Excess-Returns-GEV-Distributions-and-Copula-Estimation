#' ---
#' title: "Fitting Extreme Value Distributions & Copula Estimation"
#' author: "Marcel R. Penda"
#' date: "2024-05-01"
#' output:
#'   html_document: default
#'   pdf_document: default
#' ---
#' 
#' # Setup & Libraries
## --------------------------------------------------------------------------------------------------------------------------------------------
#Libraries
library(pacman)
p_load(
  # General
  readxl, stargazer, skimr, dplyr, tidyverse, psych, skimr, ggpubr, 
  
  # Time Series
  xts, POT, astsa, tis, plm, extRemes, fExtremes, nonlinearTseries, 
  
  # Distributions
  fitdistrplus, actuar, VGAM, ismev, VaRES, car, CircStats, qqconf,
  
  # Copulas
  copula, scatterplot3d, ggplot2, grid, MASS, mnormt, fGarch, VineCopula, BiocManager, mvtnorm, sn, 
  
  # Markov Regime Switching?
  tsDyn, xts, dygraphs, ggpubr, olsrr, broom, nonlinearTseries, MSwM, readxl,
  
  # Tests
  strucchange,
  
  #Marx
  matlab, metRology
)


# Deactivate scientific notation
options(scipen = 999) 

# Set Working Directory
# wd = "YOUR-WORKING-DIRECTORY"
# setwd("")


#' 
#' 
#' 
#' # Read Data
## --------------------------------------------------------------------------------------------------------------------------------------------
# Read capm data
capm <- read_excel("C:/Users/marce/OneDrive - UT Cloud/GitHub Projects/08_Analyzing Stock Excess Returns - Extreme Distributions & Copula Estimation/capm.xls",
                   range = "A2:G138",
                   col_names = TRUE)

#' 
#' 
#' ## Data Preperation
#' 
#' We select the Ford stock as our index of interst. To examine the excess returns, we must first of all compute the excess stock return for the Ford index compared to the USTB3M (risk-free asset). In the first step, we calculate the excess returns for the index of interest (Ford) and the market index (S&P500).
## --------------------------------------------------------------------------------------------------------------------------------------------
# Copmute monthly growth rate of Ford index to get same unit (percentage) as for USTB3M 
Ford_gr <- 100*diff(log(capm$FORD), lag = 1)
SP_gr <- 100*diff(log(capm$SANDP), lag = 1)

# Compute the excess returns (excluding first value of USTB3M since grwoth rates for Ford and S&P500 are missing)
Fordex <- Ford_gr - capm$USTB3M[-1]
SPex <- SP_gr - capm$USTB3M[-1]


# Create dataframe with the indexes and corresponding dates
returns <- data.frame(Date = capm$Date[-1], 
                      Fordex = Fordex, 
                      SPex = SPex)

#' 
#' ## 1.1 Preliminary Analysis
## --------------------------------------------------------------------------------------------------------------------------------------------
## Basic statistics
skim(returns)

#' For the Ford & S&P500 excess returns (in the following Fordex and SPex), the basic descriptive statistics show a mean of -1.586154 & -1.388684 and a standard deviation of 15.867397 & 4.938581. Thus, we can see that the Fordex time series is relatively more volatile compared to the SPex. This is supported when comparing the respective quartiles and the histogram indicator (last column) of the skim function output.
#' 
#' ### i) Box-Plots
## --------------------------------------------------------------------------------------------------------------------------------------------
boxplot(returns$Fordex, main = "Boxplot - Ford Excess Returns")
boxplot(returns$SPex,main = "Boxplot - S&P500 Excess Returns")
boxplot(returns$Fordex, returns$SPex)

#' The box plot for the Fordex reveals three outliers which lie above (> q_0.75 + 1.5IQR) and two outliers below (i.e. < q_0.25 - 1.5IQR) the maxima and minima. In contrast, the SPex box plot shows only two outliers below the IQR. The third graph, comparing the two box plots shows that the data for the Fordex is more dispersed or volatile compared to the S&P500. This makes sense, since the single Ford stock will show more volatile market dynamics compared to the aggregated S&P index of 500 companies.
#' 
#' ### ii) Distirbutions & Histogram
## --------------------------------------------------------------------------------------------------------------------------------------------
plot(density(returns$Fordex, kernel="epanech"), main = "Density Plot - Ford Excess Returns")
hist(returns$Fordex, breaks = 30, main = "Histogram - Ford Ford Excess Returns", xlab = "Excess Returns")

plot(density(returns$SPex, kernel="epanech"), main = "Density Plot - S&P500 Excess Returns")
hist(returns$SPex, breaks = 30, main = "Histogram - S&P500 Excess Returns", xlab = "Excess Returns")

#' The density plot of the Fordex time series shows a non-continuous pdf function, indicating some outliers on the right and left tails as well as a rather too small sample size. Moreover, the estimated distribution is very dispersed and has shows an upper extreme value. Against this background, we could suggest that the Fordex might be well described by a student-t distribution. In contrast, the density plot of the S&P500 time series looks more like a normal distribution. However, we can see that the data is slightly right-skewed, i.e. we have a slightly fat tail on the left to the data. Nonetheless, a normal distribution might describe the S&P500 excess returns sufficiently well. The respective histograms yield similar interpretation.
#' 
#' 
#' ### iii) Time Series Graphs
## --------------------------------------------------------------------------------------------------------------------------------------------
plot.ts(returns$Fordex)
plot.ts(returns$SPex)

#' Comparing the Fordex and SPex time series, we can observe high
#' volatility in the data. However, the Ford excess returns show relatively higher volatility (amplitudes between -80 and 80)compared to the S&P500 market index (amplitudes between -20 and 10). Even though this might be partially explained by the larger number of outliers in the Fordex time series, the rather moderate time series data (e.g. between 20 and 60) still shows noticeably higher volatility. For the Fordex, we can observe a sudden maximum peak at approx. 90 (90th observation),i.e. April-2006 with a excess return rate of 87%. Moreover, we can observe a sudden minimum excess return (at approx. 80) of -87% in October-2008 (most likely linked to the financial crises of 2008). Since the financial crises affected the whole economy, the latter minimum can also be observed in the S&P500 market index.
#' 
#' ### iv) QQ-Plots
## --------------------------------------------------------------------------------------------------------------------------------------------
qqPlot(returns$Fordex, main = "QQ Plot - Ford Excess Returns", pch = 20, ylab = "Ford Sample", xlab = "Theoretical Normal Distribution Quantiles")

qqPlot(returns$SPex, main = "QQ Plot - S&P500 Excess Returns", pch = 20, ylab = "S&P500 Sample", xlab = "Theoretical Normal Distribution Quantiles")

#' Since many points of the Ford excess returns sample lie outside the confidence interval of the QQ-plot, we can conclude that the
#' distribution of the Ford excess returns does not follow a normal
#' distribution. For the S&P500 excess returns, we can observe a similar behavior. However, compared to the Ford excess returns, the S&P500 excess returns show only few values (more precisely, ) that lie outside the confidence bands. Thus, we could argue that a normal distribution describes our S&P500 excess returns sample sufficiently well. However, to thoroughly evaluate if the S&P500 excess returns follow a normal distribution, a larger sample size might be of good use. This is in line with our box plots, since we can observe more outliers for the Ford excess returns compared to the S&P500 excess returns.
#' 
#' 
#' ### vi) Bivariate Scatterplot
## --------------------------------------------------------------------------------------------------------------------------------------------
plot(returns$SPex, returns$Fordex, xlab = "Excess Returns Ford", ylab = "Excess Returns S&P500", pch = 20)

#' 
#' The bivariate scatter plot shows rather disperse data. However, one might argue to observe a weak positive relationship between the two excess return rates. However, this is only a visual analysis. To further examine the relationship between the two time seris, more sophisticated methods such as corpulas must be considered.
#' 
#' ### vii) ACF-Plots
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------
Fordex_acf <- acf(returns$Fordex, plot = FALSE)
plot(Fordex_acf, main = "Autocorrelation Function (ACF) - Ford Excess Returns", xlab = "Lag", ylab = "ACF")

SPex_acf <- acf(returns$SPex, plot = FALSE)
plot(SPex_acf, main = "Autocorrelation Function (ACF) - S&P500 Excess Returns", xlab = "Lag", ylab = "ACF")

#' 
#' The autocorrelation function examines a potential correlation between the time series values with some lagged values of the very same time series. For the Fordex, we can see that some lags are not in the confidence interval, indicating a correlation (and thus a potential candidate for a time series model). We can observe that the time series data at time t might be correlated with its 6th (negative correlation), 7th (positive correlation) and 16th (slightly positive) lagged value. For the SPex data, we can see that lag 1, 3, 4,8, 15 and 19 might be correlated with the time series. Since our time series contains monthly data, these rather large (because it is financial data with usually fast dynamics) and significant lags might describe seasonal patterns.
#' 
#' ### viii) PP-Plots
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------
pp_conf_plot(
  obs = returns$Fordex, 
  distribution = pnorm
)

pp_conf_plot(
  obs = returns$SPex, 
  distribution = pnorm
)


#' 
#' The PP-Graphs plot the expected probabilities of the sample data (i.e. our respective time series) against the theoretical probabilities of a normal distribution. For the Fordex, we can observe some observations that lie outside the confidence interval (grey area) indicating that the the Fordex time series does not follow a normal distribution. In contrast, fromt he SPex PP-plot, we can assume that only very few observations fall outside the confidence interval. These are most likely the above-mentioned outliers. The PP-plots are therefore in line with the ourn interpretation of the QQ-plots, i.e. the SPex time series might
#' be sufficiently well described by a normal distribution.
#' 
#' 
#' ## 1.2 Fitting Extreme Distribution
#' 
#' The suggested extreme value distributions, namely Weibull, log-normal and Gamma, can only be fitted to positive data. Since both the SPex and Fordex time series contain negative values, we need to change the data such that we obtain solely positive values for the distribution fitting. 
#' 
#' Therefore, we have several alternatives such as (1) adding a value to shift the data (to the right), (2) extracting only the positive values, or (3) fitting the distribution to the absolute values of the time series. For the fitting of the distribution, we use the maximum likelihood estimator.
#' 
#' 
#' A) Ford Excess Returns 
#' 
#' To compare the fit of the suggested, we use combine two for loops. The first loop will iterate over the suggested approaches, while the second loop will fit the suggested distributions. Subsequently, the results will be stored and compared.
## --------------------------------------------------------------------------------------------------------------------------------------------
distributions = c("weibull", "lnorm", "gamma")
approaches = list(returns$Fordex+100, returns$Fordex[returns$Fordex>0], abs(returns$Fordex))
fit_results <- list()

for (approach in seq_along(approaches)) {
  data <- approaches[[approach]]
  approach_name <- switch(approach, "shift", "pos", "abs")
  
  for (dist in distributions) {
    
    fit <- fitdist(data,
                 distr = dist,
                 method = "mle")
  
  # Store fit results
  fit_results[[paste("Fordex_", dist, "_", approach_name, sep = "")]] <- fit
    
  # Print summary
  cat("\nSummary for", paste("Fordex_", dist, "_", approach_name, sep = ""), ":\n")
    print(summary(fit))
    
    # Plot
    cat("\nPlot for", paste("Fordex_", dist, "_", approach_name, sep = ""), ":\n")
    plot(fit)
    }
}

#' 
#' Given the AIC results, we observe that for each approach a different extreme distribution (among the three candidates) describes the data best: 
#' 
#' (1) When shifting the data, the Weibull distribution describes the data best (AIC = 1166.93). 
#' 
#' (2) When using only positive values, the
#' log-normal describes the data best (AIC = 379.3569). 
#' 
#' (3) When using the absolute values, the gamma distribution describes the data baste (AIC = 909.5253). 
#' 
#' However, no matter the approach, the AIC values are very similar,
#' suggesting no significant differences in the distribution fit. Looking closer at the QQ- and PP-Plots, i.e. the fit of the log-normal to the positive values of the Fordex time series, show that the empirical distribution (data points) is well described by (are similarly distributed as) the theoretical distribution.
#' 
#' B)  S&P500 Excess Returns
## --------------------------------------------------------------------------------------------------------------------------------------------
distributions = c("weibull", "lnorm", "gamma")
approaches = list(returns$SPex+100, returns$SPex[returns$SPex>0], abs(returns$SPex))
fit_results <- list()

for (approach in seq_along(approaches)) {
  data <- approaches[[approach]]
  approach_name <- switch(approach, "shift", "pos", "abs")
  
  for (dist in distributions) {
    
    fit <- fitdist(data,
                 distr = dist,
                 method = "mle")
  
  # Store fit results
  fit_results[[paste("SPex_", dist, "_", approach_name, sep = "")]] <- fit
    
  # Print summary
  cat("\nSummary for", paste("SPex_", dist, "_", approach_name, sep = ""), ":\n")
    print(summary(fit))
    
    # Plot
    cat("\nPlot for", paste("SPex_", dist, "_", approach_name, sep = ""), ":\n")
    plot(fit)
    }
}

#' 
#' For the S&P500 excess returns, we obtain an overall similar picture. For all approaches, we obtain a similar quality for the distribution fit. However, for the first and last approach (shifting and absolute values), the gamma distributions describes the data best. If fitting a distribution only to the positive values, the Weibull distribution describes our data best.
#' 
#' However, examining all approach-distribution combinations, the
#' comparison between the theoretical and empirical CDFs, PDFs as well as QQ- and PP-plots for the gamma distribution fitted to the shifted time series values could be suggested as the best fit. The density function fits well the empirical histogram and almoast all data points lie on the theoratical CDF and QQ-/PP- probabilities.
#' 
#' 
#' ## 1.3 Fitting GEV Distribution
#' 
#' Generalized extreme value distributions are usually fitted to extreme value distributions extracted by the Block Maxima method. To fit such an GEV to our time series, we might want to follow this Block Maxima method. Given the small sample size of 135 observations, we should aim at keeping as many data points as possible. Thus, we will extract the quarterly block maxima:
## --------------------------------------------------------------------------------------------------------------------------------------------
# Apply blockmax function to extract (e.g. quarterly) maximum values
blockmax<- blockmaxxer(returns, # data frame
              which = "Fordex", # define variable of interest
              blocks = NULL, # self defined blocks
              blen = 3, # block length
              span = 45) #number of blocks
# To get quarterly data, we choose blen = 12/4 = 3 and thus span = 135/3 = 45

#' 
#' Subsequently, we fit an GEV distribution to our block maxima data.
## --------------------------------------------------------------------------------------------------------------------------------------------
gev <-gevFit(blockmax$Fordex, 
       block = 1, 
       type = c("mle"), 
       title = NULL, 
       description = NULL)
summary(gev)

plot(density(blockmax$Fordex, kernel = "epanech"))
hist(blockmax$Fordex, breaks = 30, main = "Histogram of Block Maxima", xlab = "Excess Returns", ylab = "Density")

#' 
#' xi is the shape parameter, 
#' mu the location parameter, 
#' beta is the scale parameter.
#' 
#' Given the positive scale parameter beta \> 0, the quarterly extreme value distirbution of the Fordex time series is best described by a Fréchet distribution (fat tailed). This suggestion is supported by the density plot which clearly shows a fat tail left to the data. 
#' 
#' Alternatively, we could fit a GEV distribution to the complete Fordex time series and examine the results. From the preliminary analysis, we know that the Fordex time series is rather normally distributed (density plot). Thus, we expect that a GEV distribution will not describe the data well.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------
gev2 <-gevFit(returns$Fordex, 
       block = 1, 
       type = c("mle"), 
       title = NULL, 
       description = NULL)
summary(gev2)

plot(density(returns$Fordex, kernel = "epanech"), main = "Density of Ford Excess Returns")
hist(returns$Fordex, breaks = 30, main = "Histogram of Ford Excess Returns", xlab = "Excess Returns", ylab = "Density")

#' 
#' As expected, we can observe that the whole Fordex time series is poorly described by a GEV distribution. This can be seen by the QQ-Plot as well as the comparison of the cumulative distribution functions of the theoretical and empirical distributions.
#' 
#' 
#' ## 1.4 Eliptical Copula
#' 
#' Data Transformation
## --------------------------------------------------------------------------------------------------------------------------------------------
#Create data frame with both time series
returns_vec <- cbind(returns$Fordex, returns$SPex)

# Transform the data to uniform values between [0,1]
margins <- pobs(as.matrix(returns_vec))

#' 
## --------------------------------------------------------------------------------------------------------------------------------------------
# Define Guassian and t Copula
cop_norm <- normalCopula(dim = 2) # dim = 2, since we have two margins
cop_t <- tCopula(dim=2)

# Fitting the Copulas to our margins using MLE
fitml_norm <- fitCopula(cop_norm, margins, method = "ml")
fitml_t <- fitCopula(cop_t, margins, method = "ml")

# Compare the models based on log-likelihood
logli_comp <- c(logLik(fitml_norm), logLik(fitml_t))
names(logli_comp) <- c("norm", "t")

# Get the AIC values
AIC <- AIC(fitml_norm, fitml_t)

# Get quality results
print(logli_comp)
print(AIC)

# Obtain Parameters for the normal copula (best AIC & Log-Liklihood values)
summary(fitml_norm)
summary(fitml_t)

#' 
#' The t-copula provides the better fit with an AIC of -72.81838 (compared to the normal copular with an AIC of -54.99207) and a log-liklihood of 38.41 (compared to 28.5). Thus, we will compute the Kendall's tau based on the tCopula and it's estimated roh parameter of 0.605. Moreover, the degrees of freedom of 2.090, indicates that the t copula does not behave like a normal copula but the has heavier tails.
#' 
#' Estimate Kendall's Tau
## --------------------------------------------------------------------------------------------------------------------------------------------
# Check Kendall's tau value for the best fitted copula, here the t-copula
tau(tCopula(param = 0.605))

#' 
#' We obtain a Kendall's tau value of 0.4136538 > 0, indication comonotonicity of the two time series. Put differently, the excess
#' returns of the Ford stock and the S&P500 market index show positive dependence, i.e. if the S&P500 market index is in a bull market, the Ford excess returns will be promissing as well.
#' 
#' ## 1.5 Archimedian Copulas
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------
# Define Clayton and Gumbel Copulas
cop_clay <- claytonCopula(dim = 2) # dim = 2, since we have two margins
cop_gumb <- gumbelCopula(dim=2)

# Fitting the Copulas to our margins using MLE
fitml_clay <- fitCopula(cop_clay, margins, method = "ml")
fitml_gumb <- fitCopula(cop_gumb, margins, method = "ml")

# Compare the models based on log-likelihood
logli_comp <- c(logLik(fitml_clay), logLik(fitml_gumb))
names(logli_comp) <- c("clayton", "gumbel")

# Get the AIC values
AIC <- AIC(fitml_clay, fitml_gumb)

# Get quality results
print(logli_comp)
print(AIC)

# Obtain Parameters for the normal copula (best AIC & Log-Liklihood values)
summary(fitml_clay)
summary(fitml_gumb)

#' 
#' For our two the (archimedian) Clayton and Gumbel copulas, we obtain a log-likelihood of 23.44808 and 32.84128, respectively. The corresponding AIC values of -44.89616 and -63.68256 confirm these results and identify the Gumbel copula with an alpha value of 1.751 as the better fit.
#' 
#' Estimate Kendall's Tau
## --------------------------------------------------------------------------------------------------------------------------------------------
# Check Kendall's tau value for the best fitted copula, here the t-copula
tau(gumbelCopula(param = 1.751))

#' 
#' With a Kendall's tau estimate of 0.4288978 > 0, the archimedian Gumbel copula yields a similar result as the elliptical t Copula. I.e. the S&P500 and Ford excess returns show a positive dependence relation. In other words, the Kendall's tau greater than zero indicates comonotonicity of the two time series.
