startlibrary(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ChannelAttribution)
library(markovchain)
library(dplyr)
library(stringr)

##### simple example #####
# creating a data sample
# df1 <- data.frame(path = c('c1 > c2 > c3', 'c1', 'c2 > c3'),
#                   conv = c(2, 0,1),
#                   conv_null = c(0, 1, 0))

df1 <- read.csv("inputFile.csv", stringsAsFactors = FALSE, header = TRUE)
df1 <- aggregate(x = df1[2:3], by = list(df1$path), FUN = sum)
names(df1) <- c("path", "conv", "conv_null")
df1$path <- gsub(" > ",">",df1$path)
df1$path <- gsub(" ","_",df1$path)
df1$path <- gsub(">"," > ",df1$path)


# calculating the model
mod1 <- markov_model(df1,
                     var_path = 'path',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE)

# extracting the results of attribution
df_res1 <- mod1$result
df_res1$total_conversions <- df_res1$total_conversions/sum(df_res1$total_conversions)

# extracting a transition matrix
df_trans1 <- mod1$transition_matrix
df_trans1 <- dcast(df_trans1, channel_from ~ channel_to,
                   value.var = 'transition_probability')

### plotting the Markov graph ###
df_trans <- mod1$transition_matrix

# adding dummies in order to plot the graph
df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       transition_probability = c(0, 1, 1))
df_trans <- rbind(df_trans, df_dummy)

# ordering channels
df_trans$channel_from <- factor(df_trans$channel_from,
                                levels = c('(start)', '(conversion)', '(null)',
                                           unique(unlist(strsplit(x = df1$path, split = " > "))))
                                )
df_trans$channel_to <- factor(df_trans$channel_to,
                              levels = c('(start)', '(conversion)', '(null)',
                                         unique(unlist(strsplit(x = df1$path, split = " > "))))
                                         )
df_trans <- dcast(df_trans, channel_from ~ channel_to, value.var = 'transition_probability')

# creating the markovchain object
trans_matrix <- matrix(data = as.matrix(df_trans[, -1]),
                       nrow = nrow(df_trans[, -1]), ncol = ncol(df_trans[, -1]),
                       dimnames = list(c(as.character(df_trans[, 1])), c(colnames(df_trans[, -1]))))
trans_matrix[is.na(trans_matrix)] <- 0
trans_matrix1 <- new("markovchain", transitionMatrix = trans_matrix)

# plotting the graph
plot(trans_matrix1, edge.arrow.size = 0.35)
