library(tidyverse)
library(dplyr)
library(readxl)
library(binsreg)
library(ggplot2)
library(wCorr)
library(mgcv)
library(psycho)
library(corrplot)
library(xtable)
library(Hmisc)
library(rmarkdown)
library(gt)
library(extraDistr)
setwd("")


county_outcomes <- read.delim("county_outcomes.csv", sep = ",")
cty_covariates <- read.delim("cty_covariates.csv", sep = ",")
inventor_rate_by_college <- read.delim("inventor_rate_by_college.csv", sep = ",")



df2 <- merge(x=county_outcomes,y=cty_covariates,
             by.x=c("state", "county"),
             by.y=c("state", "county"),
             all.y = TRUE)


# /////////////PART |//////////// 

#Remove rows with no data about count of children

df2 <- df2[!is.na(df2$kid_pooled_pooled_n),]

df2 <- df2[!is.na(df2$kfr_pooled_pooled_p25),]

df2 <- df2[!is.na(df2$kfr_pooled_pooled_p75),]

df2 <- df2[!is.na(df2$singleparent_share2000),]

df2$w <- df2$kid_pooled_pooled_n


df3 <- df2 %>% dplyr::select(kfr_pooled_pooled_p75, singleparent_share2000, w) 
df4 <- df2 %>% dplyr::select(kfr_pooled_pooled_p25, singleparent_share2000, w) 

df3$p75 <- "75th percentile"
df4$p25 <- "25th percentile"  

colnames(df3) <- c("avg_future_family_income", "singleparent_share", "w", "id")
colnames(df4) <- c("avg_future_family_income", "singleparent_share", "w", "id")

df5 <- rbind(df3, df4)


binsreg(data = df5, x = singleparent_share, y = avg_future_family_income,  
        by = df5$id, bycolors = c("red", "blue"), polyreg = 1, weights = df5$w, legendTitle= "Percentile")


# /////////////PART 2//////////// 

#a)

df2 <- df2[!is.na(df2$popdensity2000),]
df2 <- df2[!is.na(df2$poor_share2000),]

df_cor <- df2 %>% dplyr::select(kfr_pooled_pooled_p75, kfr_pooled_pooled_p25, singleparent_share2000, popdensity2000, poor_share2000, emp2000, hhinc_mean2000) %>% as.data.frame() 

colnames(df_cor) <- c("familiy_income_p75", "family_income_p25", "singleparent_share", "population_denisty", "poverty_share", "employment_share", "house_hold_income")

df_cor <- cor(df_cor)

corrplot(df_cor, method="number", type="lower")

#b) 

df_se <- df2 %>% dplyr::select(kfr_asian_pooled_p75_se, kfr_asian_pooled_p25_se, kfr_black_pooled_p75_se, kfr_black_pooled_p25_se, kfr_white_pooled_p75_se, kfr_white_pooled_p25_se) %>% as.data.frame()

df_se <- df_se[!is.na(df_se$kfr_asian_pooled_p75_se),]
df_se <- df_se[!is.na(df_se$kfr_asian_pooled_p25_se),]
df_se <- df_se[!is.na(df_se$kfr_black_pooled_p75_se),]
df_se <- df_se[!is.na(df_se$kfr_black_pooled_p25_se),]
df_se <- df_se[!is.na(df_se$kfr_white_pooled_p75_se),]
df_se <- df_se[!is.na(df_se$kfr_white_pooled_p25_se),]


square <- function(x) {
  sse <- as.numeric(x)^2
  return(sse)
}

df_sse <- lapply(df_se, square)
df_sse <- as.data.frame(df_sse)

rel <- data.frame(matrix(NA,    # Create empty data frame
                          nrow = nrow(df_se),
                          ncol = 0))

rel$kfr_asian_pooled_p75_noise <-  mean(df_sse$kfr_asian_pooled_p75_se) 
rel$kfr_asian_pooled_p25_noise <-  mean(df_sse$kfr_asian_pooled_p25_se) 
rel$kfr_black_pooled_p75_noise <-  mean(df_sse$kfr_black_pooled_p75_se) 
rel$kfr_black_pooled_p25_noise <-  mean(df_sse$kfr_black_pooled_p25_se) 
rel$kfr_white_pooled_p75_noise <-  mean(df_sse$kfr_white_pooled_p75_se) 
rel$kfr_white_pooled_p25_noise <-  mean(df_sse$kfr_white_pooled_p25_se)


df_reliability <- (1  - (rel/df_se))

df_avg_reliability <- data.frame(kfr_asian_pooled_p75_rel =  mean(df_reliability$kfr_asian_pooled_p75_noise), 
                         kfr_asian_pooled_p25_rel =  mean(df_reliability$kfr_asian_pooled_p25_noise), 
                         kfr_black_pooled_p75_rel = mean(df_reliability$kfr_black_pooled_p75_noise), 
                         kfr_black_pooled_p25_rel = mean(df_reliability$kfr_black_pooled_p25_noise), 
                         kfr_white_pooled_p75_rel = mean(df_reliability$kfr_white_pooled_p75_noise), 
                         kfr_white_pooled_p25_rel = mean(df_reliability$kfr_white_pooled_p25_noise))
gt_tbl <-
   gt(df_avg_reliability) %>%
  tab_header(
    title = "Reliability",
    subtitle = "Variables"
  ) 

print(gt_tbl)


#c)

df_aux <- df2 %>% dplyr::select(kfr_asian_pooled_p75, kfr_asian_pooled_p25, kfr_black_pooled_p75, kfr_black_pooled_p25, kfr_white_pooled_p75, kfr_white_pooled_p25) %>% as.data.frame()

df_aux <- df_aux[!is.na(df_aux$kfr_asian_pooled_p75),]
df_aux <- df_aux[!is.na(df_aux$kfr_asian_pooled_p25),]
df_aux <- df_aux[!is.na(df_aux$kfr_black_pooled_p75),]
df_aux <- df_aux[!is.na(df_aux$kfr_black_pooled_p25),]
df_aux <- df_aux[!is.na(df_aux$kfr_white_pooled_p75),]
df_aux <- df_aux[!is.na(df_aux$kfr_white_pooled_p25),]

cor_df_aux <- cor(df_aux) %>% as.data.frame()

rel_root_prod <- function(x, y=df_avg_reliability) {
  result <- sqrt(as.numeric(x))*sqrt(as.numeric(y))
  return(result)
}

rel_root_prod <- lapply(df_avg_reliability, rel_root_prod)
rel_root_prod <- as.data.frame(rel_root_prod)

signal_correlation <- cor_df_aux/rel_root_prod

gt_tbl <-
  gt(signal_correlation, rownames_to_stub = TRUE) %>%
  tab_header(
    title = "Signal Correlations",
    subtitle = "Variables"
  )

print(gt_tbl)

# /////////////PART 3//////////// 

#d) 

global_sensitivity <- 100/(as.numeric(nrow(inventor_rate_by_college)))

epsilon <- 1

noise <- rlaplace(n = as.numeric(nrow(inventor_rate_by_college)), mu = 0, sigma = global_sensitivity / epsilon )

inventor_rate_by_college$inventor_rate_w_noise <- inventor_rate_by_college$inventor + noise
 

#e) 


global_sensitivity <- 100/(as.numeric(nrow(inventor_rate_by_college)))

epsilon_range <- seq(0.1, 1, by = 0.1)

threshold <- quantile(inventor_rate_by_college$inventor, 0.9)

original_ids <- inventor_rate_by_college[inventor_rate_by_college$inventor >= as.numeric(threshold), ] %>% select(super_opeid)

aux_df <- data.frame(matrix(NA,    # Create empty data frame
                                                    nrow = nrow(inventor_rate_by_college),
                                                    ncol = length(epsilon_range)))

for (i in epsilon_range) {

  aux_df <- inventor_rate_by_college$inventor + 
    rlaplace(n = as.numeric(nrow(inventor_rate_by_college)), mu = 0, sigma = global_sensitivity / i)
  
  
  top_tail <- inventor_rate_by_college[inventor_rate_by_college$inventor_w_noise >= quantile(inventor_rate_by_college$inventor_w_noise, 0.9), ]
  top_tail_accuracy <- as.numeric(count( top_tail[ top_tail$super_opeid %in% original_ids, ]) / as.numeric(nrow(original_ids)))

}

plot(epsilon_values, top_tail_accuracy, type = "b", xlab = "Epsilon", ylab = "Top-Tail Accuracy", main = "Tradeoff Between Epsilon and Top-Tail Accuracy")


result_matrix <- matrix(nrow = nrow(inventor_rate_by_college), ncol = nrow(as.data.frame(epsilon_range)))

for (i in 1:length(epsilon_range)) {
  epsilon <- epsilon_range[i]
  noisy_data <- inventor_rate_by_college$inventor + 
    rlaplace(n = as.numeric(nrow(inventor_rate_by_college)), mu = 0, sigma = global_sensitivity / epsilon)
  
  # Store the noisy data in the matrix
  result_matrix[, i] <- noisy_data
}

result_matrix <- t(result_matrix)


