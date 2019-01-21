library(gridExtra)
library(Information)
library(ROSE)
setwd("C:/Users/dongdai/Desktop/kaggle/titanic")
df = read.csv("train.csv")
df = df[, -which(names(df) %in% c("PassengerId"))]

## ranking variables using penalized IV
IV <- create_infotables(data = df, y = "Survived")
grid.table(head(IV$Summary), rows = NULL)

## bin "Age"
IV_Age <- create_infotables(data = df[,c("Age","Survived")], y = "Survived", bins = 10)
plot_infotables(IV_Age,"Age")
print(IV_Age$Tables$Age[,c(1,2,4)], row.names=FALSE)
df$Age[is.na(df$Age)] <- -1
df$Age_bin <- "Special"
df$Age_bin[0 <= df$Age & df$Age < 13] <- "[0,13)"
df$Age_bin[13 <= df$Age & df$Age < 19] <- "[13,19)"
df$Age_bin[19 <= df$Age & df$Age < 22] <- "[19,22)"
df$Age_bin[22 <= df$Age & df$Age < 31] <- "[22,31)"
df$Age_bin[31 <= df$Age & df$Age < 36] <- "[31,36)"
df$Age_bin[36 <= df$Age & df$Age < 99] <- "[36,99)"
df$Age_bin <- as.factor(df$Age_bin)
IV_Age_Bin <- create_infotables(data = df[,c("Age_bin","Survived")], y = "Survived")
plot_infotables(IV_Age_Bin, "Age_bin")
print(IV_Age_Bin$Tables$Age_bin[,c(1,2,4)], row.names = FALSE)
df = df[, -which(names(df) %in% c("Age"))]

## bin "SibSp"
IV_SibSp <- create_infotables(data = df[,c("SibSp","Survived")], y = "Survived", bins = 100)
plot_infotables(IV_SibSp,"SibSp")
print(IV_SibSp$Tables$SibSp[,c(1,2,4)], row.names=FALSE)
df$SibSp_bin[0 <= df$SibSp & df$SibSp < 1] <- "[0,1)"
df$SibSp_bin[1 <= df$SibSp & df$SibSp < 3] <- "[1,3)"
df$SibSp_bin[3 <= df$SibSp & df$SibSp < 5] <- "[3,5)"
df$SibSp_bin[5 <= df$SibSp & df$SibSp < 10] <- "[5,10)"
df$SibSp_bin <- as.factor(df$SibSp_bin)
IV_SibSp_Bin <- create_infotables(data = df[,c("SibSp_bin","Survived")], y = "Survived")
plot_infotables(IV_SibSp_Bin, "SibSp_bin")
print(IV_SibSp_Bin$Tables$SibSp_bin[,c(1,2,4)], row.names = FALSE)
df = df[, -which(names(df) %in% c("SibSp"))]

## bin Fare
IV_Fare <- create_infotables(data = df[,c("Fare","Survived")], y = "Survived", bins = 8)
plot_infotables(IV_Fare,"Fare")
print(IV_Fare$Tables$Fare[,c(1,2,4)], row.names=FALSE)
df$Fare_bin[0 <= df$Fare & df$Fare < 7.75] <- "[0,7.75)"
df$Fare_bin[7.75 <= df$Fare & df$Fare < 7.9] <- "[7.75,7.9)"
df$Fare_bin[7.9 <= df$Fare & df$Fare < 9.83] <- "[7.9,9.83)"
df$Fare_bin[9.83 <= df$Fare & df$Fare < 69.4] <- "[9.83,69.4)"
df$Fare_bin[69.4 <= df$Fare & df$Fare < 999] <- "[69.4,+inf)"
df$Fare_bin <- as.factor(df$Fare_bin)
IV_Fare_Bin <- create_infotables(data = df[,c("Fare_bin","Survived")], y = "Survived")
plot_infotables(IV_Fare_Bin, "Fare_bin")
print(IV_Fare_Bin$Tables$Fare_bin[,c(1,2,4)], row.names = FALSE)
df = df[, -which(names(df) %in% c("Fare"))]


plot_infotables(IV, "Sex")
print(IV$Tables$Sex, row.names = FALSE)

## change age to different bins
IV_Age <- create_infotables(data = df[,c("Age", "S")])

X_RAW = df[, -which(names(df) %in% c("Survived"))]

## change variables to WOE
X = as.data.frame(matrix(0, nrow = dim(X_RAW)[1], ncol = dim(X_RAW)[2]))
colnames(X) <- colnames(X_RAW)

for(i in colnames(X_RAW)){
  IV_table <- IV$Tables[[i]]
  for(j in 1:dim(IV_table)[1]){
    X[X_RAW[,i] == IV_table[j,1],1] <- IV_table[j,4]
  }
}
new_df <- cbind(X, df[,"Survived"])
colnames(new_df) <- names(df)
write.csv(new_df, "new_data_WOE.csv")


## visualization  of target variable
library(ggplot2)
library(tidyverse)
# install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
library(dplyr)
library(stringr)
setwd("C:/Users/dongdai/Desktop/kaggle/titanic")
df = read.csv("train.csv")
df$Survived <- as.factor(df$Survived)
# discrete variable #
ds_plot <- function(data = df, columns = c("Pclass","Sex")){
  for(i in columns){
    plot_table <- as.data.frame(df %>% group_by(df[,i], df[,"Survived"]) %>%
                                  summarise(n = n()) %>% mutate(freq = n/sum(n)))
    colnames(plot_table)[1:2] <- c(i, "Survived")
    #print(plot_table)
    freq_plot <- ggplot(plot_table, aes_string(x = i, y = 'freq', fill = 'Survived')) +
      geom_bar(position = 'fill', stat = 'identity', color = 'black', width = 0.9) +
      scale_y_continuous(label = scales::percent) +
      geom_text(aes(label = paste0(round(plot_table$freq*10000)/100,"%")),
                position = position_stack(vjust = 0.5), size = 2.4) +
      theme_bw()
    print(freq_plot)
  }
}

# continuous variable
sc_plot <- function(df = df, columns = c("Age","Fare")){
  df[,"Survived"] <- as.factor(df[,"Survived"])
  for(i in columns){
    if(length(table(df[,i])) > 30){
      df_plot <- df[!is.na(df[,i]), c('Survived',i)]
      df_plot <- df_plot[df_plot[,2] > 0,]
      qt <- as.numeric(quantile(df_plot[,2], c(0.05,0.95)))
      max_value <- qt[2]
      min_value <- qt[1]
      df_plot <- df_plot[min_value < df_plot[,2] & df_plot[,2] < max_value,]
      df_plot$bin <- cut_interval(df_plot[,i], length = round((max_value - min_value)/20), dig.lab = 10)
      plot_table <- as.data.frame(df_plot %>% na.omit() %>% group_by(bin,Survived) %>%
                                    summarise(n = n()) %>% mutate(freq = n/sum(n)))
      plot_table$bin <- format(plot_table$bin, scientific = FALSE)
      plot_table$bin <- as.numeric(str_extract(plot_table$bin, "[[:digit:]]+")) + 
        round((max_value - min_value)/20)
      sur_plot <- ggplot(plot_table, aes(x=bin, y=freq, fill=Survived)) +
        geom_line(stat = 'identity', aes(color = Survived)) +
        geom_text(aes(label = paste0(round(freq*10000)/100,"%")),size = 2) +
        xlab(i) + ylim(0,1) +
        theme_few()
      print(sur_plot)
    } else {
      df_plot <- df[!is.na(df[,i]), c('Survived',i)]
      df_plot <- df_plot[df_plot[,2] > 0,]
      max_value <- max(df_plot[,2])
      min_value <- min(df_plot[,2])
      df_plot <- df_plot[min_value < df_plot[,2] & df_plot[,2] < max_value,]
      df_plot$bin <- cut_interval(df_plot[,i], length = round((max_value - min_value)/20), dig.lab = 10)
      plot_table <- as.data.frame(df_plot %>% na.omit() %>% group_by(bin,Survived) %>%
                                    summarise(n = n()) %>% mutate(freq = n/sum(n)))
      plot_table$bin <- format(plot_table$bin, scientific = FALSE)
      plot_table$bin <- as.numeric(str_extract(plot_table$bin, "[[:digit:]]+")) + 
        round((max_value - min_value)/20)
      sur_plot <- ggplot(plot_table, aes(x=bin, y=freq, fill=Survived)) +
        geom_line(stat = 'identity', aes(color = Survived)) +
        geom_text(aes(label = paste0(round(freq*10000)/100,"%")),size = 2) +
        xlab(i) + ylim(0,1) +
        theme_few()
      print(sur_plot)
    }
  }
}
