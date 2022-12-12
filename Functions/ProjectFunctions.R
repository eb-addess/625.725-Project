# load libraries
library(tidyverse)

# experiment1 function 
experiment1 <- function(sd, data){
        
        # set error
        e <- rnorm(length(data$x1), mean = 0, sd = sd)
        
        # generate dependent variable from regression model
        y_sim <- coefs1[1] + (coefs1[2] * data$x1)  + (coefs1[3] * data$x2) +
                (coefs1[4] * data$x3) + (coefs1[5] * data$x4) + 
                (coefs1[6] * data$x5) + (coefs1[7] * data$x6) +
                (coefs1[8] * data$x7) + (coefs1[9] * data$x8) +
                (coefs1[10] * data$x9) + e
        
        # make new data frame
        sim_data <- bind_cols(data[,1:15], y_sim)
        # make sure last column is called y_sim
        colnames(sim_data)[16] <- "y_sim"
        
        
        # fit models to simulated data.
        
        # true model
        true_mod1 <- lm(y_sim~x1+ x2 + x3 +x4 +x5 +x6 +x7 +x8 +x9,
                        data = sim_data)
        
        # parsimonious model
        parsim_mod2 <- lm(y_sim~ x1 +x2 +x3 + x4 + x5 + x6, data = sim_data)
        
        # under-fit model
        under_mod3 <- lm(y_sim ~ x1 + x2 +x3, data = sim_data)
        
        # false model
        false_mod4 <- lm(y_sim ~ x1 + x3 + x5 + x7 + x10 + x11 + x12 + x14,
                         data = sim_data)
        
        # saturated/over-fit model
        over_mod5 <- lm(y_sim~., data = sim_data)
        
        # calculate model selection criteria
        r_sqr <- c(summary(true_mod1)$r.squared, summary(parsim_mod2)$r.squared,
                   summary(under_mod3)$r.squared, summary(false_mod4)$r.squared,
                   summary(over_mod5)$r.squared)
        
        adj_r_sqr <- c(summary(true_mod1)$adj.r.squared, 
                       summary(parsim_mod2)$adj.r.squared,
                       summary(under_mod3)$adj.r.squared, 
                       summary(false_mod4)$adj.r.squared,
                       summary(over_mod5)$adj.r.squared)
        
        AIC <- c(AIC(true_mod1), AIC(parsim_mod2), AIC(under_mod3), 
                 AIC(false_mod4), AIC(over_mod5))
        
        BIC <- c(BIC(true_mod1), BIC(parsim_mod2), BIC(under_mod3), 
                 BIC(false_mod4), BIC(over_mod5))
        
        # r-squared criteria indicate model with largest value
        r_sqr_select <- which.max(r_sqr)
        adj_r_sqr_select <- which.max(adj_r_sqr)
        
        # AIC and BIC indicate the model with smallest value
        AIC_select <- which.min(AIC)
        BIC_select <- which.min(BIC)
        
        # make selections vector
        selections <- c(r_sqr_select, adj_r_sqr_select, 
                        AIC_select, BIC_select)
        names(selections) <- c("r_sqr_select", "adj_r_sqr_select",
                               "AIC_select", "BIC_select")
        
        return(selections)
}


# experiment2 function
experiment2 <- function(sd, data){
        
        # set error
        e <- rnorm(length(data$x1), mean = 0, sd = sd)
        
        # generate dependent variable from regression model
        y_sim <- coefs1[1] + (coefs1[2] * data$x1)  + (coefs1[3] * data$x2) +
                (coefs1[4] * data$x3) + (coefs1[5] * data$x4) + 
                (coefs1[6] * data$x5) + (coefs1[7] * data$x6) +
                (coefs1[8] * data$x7) + (coefs1[9] * data$x8) +
                (coefs1[10] * data$x9) + e
        
        # make new data frame
        sim_data <- bind_cols(data[,1:15], y_sim)
        # make sure last column is called y_sim
        colnames(sim_data)[16] <- "y_sim"
        
        # parsimonious model
        parsim_mod1 <- lm(y_sim~ x1 +x2 +x3 + x4 + x5 + x6,
                          data = sim_data)
        
        # under-fit model
        under_mod2 <- lm(y_sim ~ x1 + x2 +x3, data = sim_data)
        
        # false model
        false_mod3 <- lm(y_sim ~ x1 + x3 + x5 + x7 + x10 + x11 + x12 + x14,
                         data = sim_data)
        
        # saturated/over-fit model
        over_mod4 <- lm(y_sim~., data = sim_data)
        
        # calculate model selection criteria
        r_sqr <- c(summary(parsim_mod1)$r.squared,
                   summary(under_mod2)$r.squared, summary(false_mod3)$r.squared,
                   summary(over_mod4)$r.squared)
        
        adj_r_sqr <- c(summary(parsim_mod1)$adj.r.squared,
                       summary(under_mod2)$adj.r.squared, 
                       summary(false_mod3)$adj.r.squared,
                       summary(over_mod4)$adj.r.squared)
        
        AIC <- c(AIC(parsim_mod1), AIC(under_mod2), 
                 AIC(false_mod3), AIC(over_mod4))
        
        BIC <- c(BIC(parsim_mod1), BIC(under_mod2), 
                 BIC(false_mod3), BIC(over_mod4))
        
        
        
        # r-squared criteria indicate model with largest value
        r_sqr_select <- which.max(r_sqr)
        adj_r_sqr_select <- which.max(adj_r_sqr)
        
        # AIC and BIC indicate the model with smallest value
        AIC_select <- which.min(AIC)
        BIC_select <- which.min(BIC)
        
        # make selections vector
        selections <- c(r_sqr_select, adj_r_sqr_select, 
                        AIC_select, BIC_select)
        names(selections) <- c("r_sqr_select", "adj_r_sqr_select",
                               "AIC_select", "BIC_select")
        
        return(selections)
}

# experiment3 function
experiment3 <- function(sd, data){
        
        # set error
        e <- rnorm(length(data$x1), mean = 0, sd = sd)
        
        # generate dependent variable from regression model
        y_sim <- coefs1[1] + (coefs1[2] * data$x1)  + (coefs1[3] * data$x2) +
                (coefs1[4] * data$x3) + (coefs1[5] * data$x4) + 
                (coefs1[6] * data$x5) + (coefs1[7] * data$x6) +
                (coefs1[8] * data$x7) + (coefs1[9] * data$x8) +
                (coefs1[10] * data$x9) + e
        
        # make new data frame
        sim_data <- bind_cols(data[,1:15], y_sim)
        # make sure last column is called y_sim
        colnames(sim_data)[16] <- "y_sim"
        
        # parsimonious model
        # Here is what is different from experiment 2!
        # We added x7 and x8 to the model
        parsim_mod1 <- lm(y_sim~ x1 +x2 +x3 + x4 + x5 + x6 +x7 +x8,
                          data = sim_data)
        
        # under-fit model
        under_mod2 <- lm(y_sim ~ x1 + x2 +x3, data = sim_data)
        
        # false model
        false_mod3 <- lm(y_sim ~ x1 + x3 + x5 + x7 + x10 + x11 + x12 + x14,
                         data = sim_data)
        
        # saturated/over-fit model
        over_mod4 <- lm(y_sim~., data = sim_data)
        
        # calculate model selection criteria
        r_sqr <- c(summary(parsim_mod1)$r.squared,
                   summary(under_mod2)$r.squared, summary(false_mod3)$r.squared,
                   summary(over_mod4)$r.squared)
        
        adj_r_sqr <- c(summary(parsim_mod1)$adj.r.squared,
                       summary(under_mod2)$adj.r.squared, 
                       summary(false_mod3)$adj.r.squared,
                       summary(over_mod4)$adj.r.squared)
        
        AIC <- c(AIC(parsim_mod1), AIC(under_mod2), 
                 AIC(false_mod3), AIC(over_mod4))
        
        BIC <- c(BIC(parsim_mod1), BIC(under_mod2), 
                 BIC(false_mod3), BIC(over_mod4))
        
        
        
        # r-squared criteria indicate model with largest value
        r_sqr_select <- which.max(r_sqr)
        adj_r_sqr_select <- which.max(adj_r_sqr)
        
        # AIC and BIC indicate the model with smallest value
        AIC_select <- which.min(AIC)
        BIC_select <- which.min(BIC)
        
        # make selections vector
        selections <- c(r_sqr_select, adj_r_sqr_select, 
                        AIC_select, BIC_select)
        names(selections) <- c("r_sqr_select", "adj_r_sqr_select",
                               "AIC_select", "BIC_select")
        
        return(selections)
}

# experiment4 function
experiment4 <- function(sd, data){
        
        # set error
        e <- rnorm(length(data$x1), mean = 0, sd = sd)
        
        # generate dependent variable from regression model
        # here we include x10
        y_sim <- coefs1[1] + (coefs1[2] * data$x1)  + (coefs1[3] * data$x2) +
                (coefs1[4] * data$x3) + (coefs1[5] * data$x4) + 
                (coefs1[6] * data$x5) + (coefs1[7] * data$x6) +
                (coefs1[8] * data$x7) + (coefs1[9] * data$x8) +
                (coefs1[10] * data$x9) + (coefs1[11] * data$x10) + e
        
        # make new data frame
        sim_data <- bind_cols(data[,1:15], y_sim)
        # make sure last column is called y_sim
        colnames(sim_data)[16] <- "y_sim"
        
        # parsimonious model
        # Here is what is different from experiment 2!
        # We added x7 and x8 to the model
        parsim_mod1 <- lm(y_sim~ x1 +x2 +x3 + x4 + x5 + x6 +x7 +x8 +x9,
                          data = sim_data)
        
        # under-fit model
        under_mod2 <- lm(y_sim ~ x1 + x2 +x3, data = sim_data)
        
        # false model
        false_mod3 <- lm(y_sim ~ x1 + x3 + x5 + x7 + x10 + x11 + x12 + x14,
                         data = sim_data)
        
        # saturated/over-fit model
        over_mod4 <- lm(y_sim~., data = sim_data)
        
        # calculate model selection criteria
        r_sqr <- c(summary(parsim_mod1)$r.squared,
                   summary(under_mod2)$r.squared, summary(false_mod3)$r.squared,
                   summary(over_mod4)$r.squared)
        
        adj_r_sqr <- c(summary(parsim_mod1)$adj.r.squared,
                       summary(under_mod2)$adj.r.squared, 
                       summary(false_mod3)$adj.r.squared,
                       summary(over_mod4)$adj.r.squared)
        
        AIC <- c(AIC(parsim_mod1), AIC(under_mod2), 
                 AIC(false_mod3), AIC(over_mod4))
        
        BIC <- c(BIC(parsim_mod1), BIC(under_mod2), 
                 BIC(false_mod3), BIC(over_mod4))
        
        
        
        # r-squared criteria indicate model with largest value
        r_sqr_select <- which.max(r_sqr)
        adj_r_sqr_select <- which.max(adj_r_sqr)
        
        # AIC and BIC indicate the model with smallest value
        AIC_select <- which.min(AIC)
        BIC_select <- which.min(BIC)
        
        # make selections vector
        selections <- c(r_sqr_select, adj_r_sqr_select, 
                        AIC_select, BIC_select)
        names(selections) <- c("r_sqr_select", "adj_r_sqr_select",
                               "AIC_select", "BIC_select")
        
        return(selections)
}


# experiment5 function 
experiment5 <- function(sd, data){
        
        # set error
        e <- rnorm(length(data$x1), mean = 0, sd = sd)
        
        # generate dependent variable from regression model
        # here we go use x10, different than previous functions!
        y_sim <- coefs1[1] + (coefs1[2] * data$x1)  + (coefs1[3] * data$x2) +
                (coefs1[4] * data$x3) + (coefs1[5] * data$x4) + 
                (coefs1[6] * data$x5) + (coefs1[7] * data$x6) +
                (coefs1[8] * data$x7) + (coefs1[9] * data$x8) +
                (coefs1[10] * data$x9)+ (coefs1[11] * data$x10) + e
        
        # make new data frame
        sim_data <- bind_cols(data[,1:15], y_sim)
        # make sure last column is called y_sim
        colnames(sim_data)[16] <- "y_sim"
        
        
        # fit models to simulated data.
        
        # true model
        true_mod1 <- lm(y_sim~x1+ x2 + x3 +x4 +x5 +x6 +x7 +x8 +x9 +x10,
                        data = sim_data)
        
        # parsimonious model
        # goes up to x9
        parsim_mod2 <- lm(y_sim~ x1 +x2 +x3 + x4 + x5 + x6 +
                                  x7 +x8 +x9, data = sim_data)
        
        # under-fit model
        under_mod3 <- lm(y_sim ~ x1 + x2 +x3, data = sim_data)
        
        # false model
        false_mod4 <- lm(y_sim ~ x1 + x3 + x5 + x7 + x10 + x11 + x12 + x14,
                         data = sim_data)
        
        # saturated/over-fit model
        over_mod5 <- lm(y_sim~., data = sim_data)
        
        # calculate model selection criteria
        r_sqr <- c(summary(true_mod1)$r.squared, summary(parsim_mod2)$r.squared,
                   summary(under_mod3)$r.squared, summary(false_mod4)$r.squared,
                   summary(over_mod5)$r.squared)
        
        adj_r_sqr <- c(summary(true_mod1)$adj.r.squared, 
                       summary(parsim_mod2)$adj.r.squared,
                       summary(under_mod3)$adj.r.squared, 
                       summary(false_mod4)$adj.r.squared,
                       summary(over_mod5)$adj.r.squared)
        
        AIC <- c(AIC(true_mod1), AIC(parsim_mod2), AIC(under_mod3), 
                 AIC(false_mod4), AIC(over_mod5))
        
        BIC <- c(BIC(true_mod1), BIC(parsim_mod2), BIC(under_mod3), 
                 BIC(false_mod4), BIC(over_mod5))
        
        # r-squared criteria indicate model with largest value
        r_sqr_select <- which.max(r_sqr)
        adj_r_sqr_select <- which.max(adj_r_sqr)
        
        # AIC and BIC indicate the model with smallest value
        AIC_select <- which.min(AIC)
        BIC_select <- which.min(BIC)
        
        # make selections vector
        selections <- c(r_sqr_select, adj_r_sqr_select, 
                        AIC_select, BIC_select)
        names(selections) <- c("r_sqr_select", "adj_r_sqr_select",
                               "AIC_select", "BIC_select")
        
        return(selections)
}


# bar plot function
histo <- function(MC, title = "plots"){
        par(mfrow = c(2,2))
        breaks = seq(from = 0.5,to = 5.5,by =  1)
        hist(MC$r_sqr_select, breaks = breaks)
        hist(MC$adj_r_sqr_select, breaks = breaks)
        hist(MC$AIC_select, breaks = breaks)
        hist(MC$BIC_select, breaks = breaks)
        mtext(title, side = 1, line = -1, outer = TRUE)
}

# selection proportions function
props <- function(MC){
        # calculate proportions
        r_sqr_prop <- c(sum(MC$r_sqr_select == 1), 
                        sum(MC$r_sqr_select == 2),
                        sum(MC$r_sqr_select == 3), 
                        sum(MC$r_sqr_select == 4),
                        sum(MC$r_sqr_select == 5)) / dim(MC)[1]
        
        adj_r_sqr_prop <- c(sum(MC$adj_r_sqr_select == 1), 
                            sum(MC$adj_r_sqr_select == 2),
                            sum(MC$adj_r_sqr_select == 3), 
                            sum(MC$adj_r_sqr_select == 4),
                            sum(MC$adj_r_sqr_select == 5)) / dim(MC)[1]
        
        AIC_prop <- c(sum(MC$AIC_select == 1), 
                      sum(MC$AIC_select == 2),
                      sum(MC$AIC_select == 3), 
                      sum(MC$AIC_select == 4),
                      sum(MC$AIC_select == 5)) / dim(MC)[1]
        
        BIC_prop <- c(sum(MC$BIC_select == 1), 
                      sum(MC$BIC_select == 2),
                      sum(MC$BIC_select == 3), 
                      sum(MC$BIC_select == 4),
                      sum(MC$BIC_select == 5)) / dim(MC)[1]
        
        
        
        # make data frame of results
        models <- 1:5
        props <- data.frame(models,r_sqr_prop, adj_r_sqr_prop, AIC_prop, BIC_prop)
        
        # display results
        props
}