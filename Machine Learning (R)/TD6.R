#TD6 ALOUGES Damien

#1)
iris = read.csv("C:/Users/Damien/Downloads/iris (2).data")

#2)
par(mfrow=c(2,2))
boxplot(iris$sepal_length~iris$class , ylab="sepal_length")
boxplot(iris$sepal_width~iris$class , ylab="sepal_width")
boxplot(iris$petal_length~iris$class , ylab="petal_length")
boxplot(iris$petal_width~iris$class , ylab="petal_width")

#3)
par(mfrow=c(1,1))
# Let's use the ggplot2 library
# ggplot2 is the most advanced package for data visualization
# gg corresponds to The Grammar of Graphics.
library(ggplot2) #of course you must install it first if you don't have it already

# histogram of sepal_length
ggplot(iris, aes(x=sepal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of sepal_width
ggplot(iris, aes(x=sepal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_length
ggplot(iris, aes(x=petal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_width
ggplot(iris, aes(x=petal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)

#4)
pcairis=princomp(iris[,-5], cor=T) 
# Note that we take only the numerical columns to apply PCA.
# now pcairis is a R object of type princomp

# To display the internal structure of pcairis
str(pcairis)
summary(pcairis) 
plot(pcairis)

#interpertation: the comp 1 and the comp 2 are the  more significant

# To plot together the scores for PC1 and PC2 and the 
# variables expressed in terms of PC1 and PC2.
biplot(pcairis) 

#the petal width and petal length arevery related 
#but sepal width andsepal length are not really related

#5)
require(factoextra)

fviz_eig(pcairis, addlabels = TRUE)

fviz_pca_ind(pcairis, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_pca_var(pcairis, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_biplot(pcairis,repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969")

#contribution of the variables on the components 1
fviz_contrib(pcairis, choice = "var", axes = 1, top = 10)

#contribution of the variables on the components 2
fviz_contrib(pcairis, choice = "var", axes = 2, top = 10)

#6)
x= iris[,-5]
y= iris[,5]

#7)
X_scaled = scale(x)
cov_mat=cov(X_scaled)
cov_mat

#9)
eigendecomp=eigen(cov_mat)
eigendecomp
#

#10)
eigendecompcor= eigen(cor(X_scaled))
eigendecompcor

#11)
eigendecompraw= eigen(cor(x))
eigendecompraw

#the results obtained are the same as the results of question 10)

#12)
ind_ex_var = eigendecomp$values/sum(eigendecomp$values)
ind_ex_var

cum_ex_var = cumsum(ind_ex_var)
cum_ex_var

#13)
plot(ind_ex_var,type="b")

#14)
proj_mat = eigendecomp$vectors[1:4,1:2]
proj_mat

#15)
Y=X_scaled %*% proj_mat
head(Y)

#16)
plot(Y, xlab="PC1",ylab="PC2")

#17)
plot(Y, xlab="PC1",ylab="PC2", col=y)