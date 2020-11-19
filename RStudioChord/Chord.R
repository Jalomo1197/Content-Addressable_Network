# Title     : TODO
# Objective : TODO
# Created by: david
# Created on: 11/17/2020
# R Version : R-4.0.3 : win

# https://cran.r-project.org/web/packages/config/vignettes/introduction.html

install.packages("circlize")
install.packages("digest")
install.packages("strtoi")
library(circlize)
library(digest)
library(strtoi)

set.seed(999)

mat = matrix(sample(18, 18), 3, 6)
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)
mat

df = data.frame(from = rep(rownames(mat), times = ncol(mat)),
                to = rep(colnames(mat), each = nrow(mat)),
                value = as.vector(mat),
                stringsAsFactors = FALSE)
df
chordDiagram(mat)

sapply("MovieName", digest, algo = "sha1")
sapply("MovieName", sha1 , digits = 5)
x = sapply("MovieName", sha1 , digits = 5)

strtoi(x, base = 0L)
