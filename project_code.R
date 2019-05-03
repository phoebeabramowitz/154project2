#Load all libraries for the project
library(dplyr)
library(ggplot2)
library(GGally)
library(reshape2)
library(plotly)
library(reshape2)
library(caret)
library(MASS)
library(class)
library(gbm)
library(DAAG)
library(ROCR)

#Upload dataframes from image data within the
#Make data frames from the txt files
images_columns <- c("y", "x","cloud_label","NDAI","SD","CORR","rad_DF","rad_CF",
                    "rad_BF","rad_AF","rad_AN" )
image1 <- read.delim("./image_data/image1.txt", sep="", col.names = images_columns)
image2 <- read.delim("./image_data/image2.txt", sep="", col.names = images_columns)
image3 <- read.delim("./image_data/image3.txt", sep="", col.names = images_columns)

#function to calculate percentage of pixels in each class
pixel_perc <- function(image){
  n <- nrow(image)
  cloud <- nrow(filter(image, cloud_label==1))
  nocloud <- nrow(filter(image, cloud_label==-1))
  nolabel <- nrow(filter(image, cloud_label==0))
  return(list("cloud"=round(cloud/n,2), "no cloud"=round(nocloud/n, 2), 
              "unlabelled"=round(nolabel/n,2)))
}

#Show the above percentage for each image
i1_proportions <- pixel_perc(image1)
i2_proportions <- pixel_perc(image2)
i3_proportions <- pixel_perc(image3)
pixels <- data.frame(Image1=unlist(pixel_perc(image1)), Image2=unlist(pixel_perc(image2)), Image3=unlist(pixel_perc(image3)))
pixels

#Create Beautiful maps of the Expert Labels for each image
colors <- c("#56B4E9", "#999999", "orange")
ggplot(image1,aes(x=x, y=y, fill=factor(cloud_label)))+
  geom_tile()+
  labs(fill= "Cloud?",
       title="Expert Labels")+
  scale_fill_manual(values=colors,
                    labels=c("No Clouds", "Unlabelled", "Clouds")) +
  theme_classic()

ggplot(image2)+
  geom_tile(aes(x=x, y=y, fill=factor(cloud_label)))+
  labs(fill= "Cloud?",
       title="Expert Labels")+
  scale_fill_manual(values=colors,
                    labels=c("No Clouds", "Unlabelled", "Clouds")) +
  theme_classic()

ggplot(image3)+
  geom_tile(aes(x=x, y=y, fill=factor(cloud_label)))+
  labs(fill= "Cloud?",
       title="Expert Labels")+
  scale_fill_manual(values=colors,
                    labels=c("No Clouds", "Unlabelled", "Clouds")) +
  theme_classic()

# quantitative pairwise relationship between features for each image
cormat1 <- round(cor(image1),3)
cormat2 <- round(cor(image2),3)
cormat3 <- round(cor(image3),3)

# Create Heatmaps of the above correlation matrices
melted_cormat1 <- melt(cormat1)
ggplot(data = melted_cormat1, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  ggtitle("Variable Correlation: Image 1")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation Value")

melted_cormat2 <- melt(cormat2)
ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  ggtitle("Variable Correlation: Image 2")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation Value")

melted_cormat3 <- melt(cormat3)
ggplot(data = melted_cormat3, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  ggtitle("Variable Correlation: Image 3")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation Value")


# Visualizations of relationship between NDAI and cloud labels
ggplot(image1, aes(x=cloud_label, y=NDAI)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("NDAI and Cloud Label on image1")

ggplot(image2, aes(x=cloud_label, y=NDAI)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("NDAI and Cloud Label on image2")

ggplot(image3, aes(x=cloud_label, y=NDAI)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("NDAI and Cloud Label on image3")


# relationship between CORR and cloud labels
ggplot(image1, aes(x=cloud_label, y=CORR)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("CORR and Cloud Label on image1")

ggplot(image2, aes(x=cloud_label, y=CORR)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("CORR and Cloud Label on image2")

ggplot(image3, aes(x=cloud_label, y=CORR)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("CORR and Cloud Label on image3")

# relationship between SD and cloud labels
ggplot(image1, aes(x=cloud_label, y=SD)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("SD and Cloud Label on image1")

ggplot(image2, aes(x=cloud_label, y=SD)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("SD and Cloud Label on image2")

ggplot(image1, aes(x=cloud_label, y=rad_DF)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("rad_DF and Cloud Label on image1")

# relationship between one angle and cloud labels
ggplot(image2, aes(x=cloud_label, y=rad_DF)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("rad_DF and Cloud Label on image2")

ggplot(image3, aes(x=cloud_label, y=rad_DF)) + 
  geom_violin() + 
  theme_minimal() +
  ggtitle("rad_DF and Cloud Label on image3")


#Problem 2

# filter unlabelled points
image1 <- filter(image1,cloud_label != 0)
image2 <- filter(image2,cloud_label != 0)
image3 <- filter(image3,cloud_label != 0)

# function to randomly split data
#' Split1
#' @param images_list list of all labelled image dataframes
#' @return images a list of three dataframes: train, test, and split
split1 <- function(images_list, spec = c(train = .8, test = .1, val = .1)){
  set.seed(1997)
  images_comb <- data.frame()
  for(i in 1:length(images_list)){
    images_comb <- rbind(images_comb,images_list[[i]])
  }
  g <-  sample(cut(seq(nrow(images_comb)), nrow(images_comb)*cumsum(c(0,spec)),
                   labels = names(spec)))
  res <-  split(images_comb,g)
  return(res)
}

#run first split method on our project data, define combined, training, validation, and test data
images <- split1(list(image1,image2,image3))
imagestrain <- images$train
imagesval <- images$val
imagestest <- images$test


#cloud_avg function takes in vector and returns -1, 0, 1 depending on avg, for classifying mixed superpixels
cloud_avg <- function(vector){
  avg <- mean(vector)
  if(avg<0){
    return(-1)}
  else if(avg>0){
    return(1)}
  else{
    return(0)}
}


#Function to Split data into 3pixel x 3pixel blocks
#' Super Pixelize
#'
#' @param images list of dataframes with correct column names
#'
#' @return timages, a data frame in the global environment with 1/9th the values, averaged by 3x3 blocks,no x,y cols
super_pixelize <- function(images){
  newimages <-data.frame(matrix(ncol = 9, nrow = 0))
  for(k in 1:length(images)){
    image <- images[[k]]
    xs <- seq(from=min(image$x), to=max(image$x), by=3)
    ys <- seq(from=min(image$y), to=max(image$y), by=3)
    ktrans <-data.frame(matrix(ncol = 9, nrow = 0))
    for(i in xs){
      for(j in ys){
        pts <- image %>%filter((x==i | x==i+1 | x== i+2)&(y==j | y==j+1 | y== j+2))
        if (nrow(pts)>0){
          new_row <- c(cloud_avg(pts$cloud_label),mean(pts$NDAI),
                       mean(pts$SD),mean(pts$CORR),mean(pts$rad_DF),mean(pts$rad_CF),
                       mean(pts$rad_BF),mean(pts$rad_AF),mean(pts$rad_AN))
          ktrans <- rbind(ktrans, new_row)
        }
      }
    }
    images_columns <- c("cloud_label","NDAI","SD","CORR","rad_DF","rad_CF",
                        "rad_BF","rad_AF","rad_AN" )
    colnames(newimages) <- images_columns
    colnames(ktrans) <- images_columns 
    newimages <- rbind(newimages,ktrans)
  }
  return(newimages)
}

#Another Split FUnction to Randomly Dvidide superpixelized data into training, test, and val

#' Split2
#' @param images_comb A list of all image dataframes
#' @param spec vector of sets to create in the split
#' @return transformed images a list of three dataframes: train, test, and split
split2 <- function(images_comb, spec = c(train = .8, test = .1, val = .1)){
  transformed <- super_pixelize(images_comb)
  transformed_list <- list(transformed[1:floor(0.5*nrow(transformed)),],
                           transformed[floor(0.5*nrow(transformed)):nrow(transformed),])
  return(split1(transformed_list, spec))
}

#Creating transformed training, validation, and test datasets
image_list <- list(image1,image2,image3)
timages <- split2(image_list)
timagestrain <- timages$train
timagesval <- timages$val
timagestest <- timages$test

#Accuracy of trivial classifier
trivial <- rep(-1, nrow(imagesval))
ttrivial1 <- rep(-1, nrow(timagestest))
ttrivial2 <- rep(-1, nrow(timagesval))
data.frame(
  data_set = c("first_split_val", "first_split_test", "second_split_val", "second_split_test"),
  trivial_classifier_accuracy = c(mean(imagesval$cloud_label == trivial),
                                  mean(imagestest$cloud_label == trivial),
                                  mean(timagesval$cloud_label == ttrivial2),  
                                  mean(timagestest$cloud_label == ttrivial1)))

#create cloud column of strings not factors, for use in histograms
imagestrain <- mutate(imagestrain, cloud = ifelse(cloud_label==1, "Cloud", "No Cloud"))
timagestrain <- mutate(timagestrain, cloud = ifelse(cloud_label==1, "Cloud", "No Cloud"))
#make histograms for non-automated feature selection
#NDAI
ggplot(imagestrain,aes(x=NDAI, fill=cloud)) + 
  geom_histogram(alpha=0.3, position="identity", bins=50) + 
  theme_classic() + 
  ggtitle("Histogram of NDAI based on cloud label: first split method") 
ggplot(timagestrain,aes(x=NDAI, fill=cloud)) + 
  geom_histogram(alpha=0.3, position="identity", bins=50) + 
  theme_classic() + 
  ggtitle("Histogram of NDAI based on cloud label: second split method") 
#CORR
gplot(imagestrain,aes(x=CORR, fill=cloud)) + 
  geom_histogram(alpha=0.3, position="identity", bins=50) + 
  theme_classic() +
  ggtitle("Histogram of CORR based on cloud label: first split")
ggplot(timagestrain,aes(x=CORR, fill=cloud)) + 
  geom_histogram(alpha=0.3, position="identity", bins=50) + 
  theme_classic() +
  ggtitle("Histogram of CORR based on cloud label: second split")
#SD
ggplot(imagestrain,aes(x=SD, fill=cloud)) + 
  geom_histogram(alpha=0.3, position="identity", bins=50) + 
  theme_classic() +
  ggtitle("Histogram of SD based on cloud label: first split method")
ggplot(timagestrain,aes(x=SD, fill=cloud)) + 
  geom_histogram(alpha=0.3, position="identity", bins=50) + 
  theme_classic() +
  ggtitle("Histogram of SD based on cloud label: second split method")

# run PCA on the angles, and see how PC1 performs when predicting cloud label
pca_angles <- prcomp(~ rad_DF + rad_CF + rad_BF + rad_AF + rad_AN, data=imagestrain,
                     scale. = TRUE)
loadings <- pca_angles$rotation
scores <- pca_angles$x
eigenvalues <- pca_angles$sdev^2
eigs_cum = cumsum(eigenvalues) / sum(eigenvalues)
scree_plot <- ggplot() + 
  geom_point(aes(x = 1:length(eigenvalues), y=eigs_cum)) +
  labs(x = "Principal Component", y = "Fraction of Total Variance Explained") +
  ggtitle("Screeplot") + 
  theme_minimal()
scree_plot
PC1 <- scores[,1]
#Add to dataframe
train_pca <- imagestrain
train_pca$PC1 <- PC1
ggplot(train_pca, aes(x=PC1, fill=cloud)) + 
  geom_histogram(alpha=0.3, position="identity", bins=50) + 
  theme_classic() + ggtitle("Histogram of PC1 based on cloud label: first split method")
ggplot(train_pca, aes(x=cloud_label, y=PC1)) +
  geom_violin() +
  theme_minimal() + 
  ggtitle("Radiance angle PC1 and cloud label")

#Write zero on loss function
zero_one_loss <- function(predicted, expert){
  return(mean(predicted == expert))
}  


#' CVgeneric
#'
#' @param classifier string name of function
#' add clauses
#' @param data list of image dataframes or combined image df, with all features
#' @param k number of folds
#' @param splitter function to split data
#' @param loss  any function that takes true labels and predicted label/predicted class
#' @param featstrain vector of string names of feature columns
#' @param labeltrain string name of label column
#'
#' @return k-fold cv loss on training set(split via second, better method)
CVgeneric <- function(classifier, data, K, loss, splitter=split1, featstrain, labeltrain){
  set.seed(12345678)
  split_data <- splitter(data, spec=c(train=0.9,test=0.1))
  datatrain <- split_data$train
  folds <- createFolds(datatrain$cloud_label, k = K)
  fold_loss <- rep(0,K)
  
  for (i in 1:K){
    val_dat <- datatrain[folds[[i]],]
    train_dat <- datatrain[-folds[[i]],]
    
    #knn
    if (classifier=="knn"){
      datclassed <- knn(train_dat[,4:6], val_dat[,4:6],train_dat$cloud_label,k=10)
    }
    
    #logistic
    else if (classifier=="glm"){
      formula <- as.formula(paste(labeltrain, "~",  paste(featstrain, collapse = "+")))
      train_dat$cloud_label <- replace(train_dat$cloud_label,
                                       train_dat$cloud_label == -1, 0)
      val_dat$cloud_label <- replace(val_dat$cloud_label,
                                     val_dat$cloud_label == -1, 0)
      model <- glm(formula, data=train_dat, family="binomial")
      model.pred <- predict(model, val_dat, type="response")
      datclassed <- rep(0, length(model.pred))
      datclassed[model.pred >= 0.5] = 1
    }  
    
    #lda
    else if(classifier=="lda"){
      formula <- as.formula(paste(labeltrain, "~",  paste(featstrain, collapse = "+")))
      model <- lda(formula, train_dat)
      model.pred <- predict(model, val_dat)
      datclassed <- model.pred$class
    }
    
    #qda
    else if(classifier=="qda"){
      formula <- as.formula(paste(labeltrain, "~",  paste(featstrain, collapse = "+")))
      model <- qda(formula, train_dat)
      model.pred <- predict(model, val_dat)
      datclassed <- model.pred$class
    }
    
    #boosting
    else if(classifier=="gbm"){
      formula <- as.formula(paste(labeltrain, "~",  paste(featstrain, collapse = "+")))
      train_dat$cloud_label <- replace(train_dat$cloud_label,
                                       train_dat$cloud_label == -1, 0)
      val_dat$cloud_label <- replace(val_dat$cloud_label,
                                     val_dat$cloud_label == -1, 0)
      model <- gbm(formula, data = train_dat, distribution = "gaussian")
      model.pred <- predict(model, val_dat, n.trees = 100)
      datclassed <- rep(0, length(model.pred))
      datclassed[model.pred >= 0.5] = 1
    }
  
    fold_loss[i] <- loss(datclassed, val_dat$cloud_label) 
  }  
  return(fold_loss)
}

#Function to Run Cross Validation without splitting the data, which is computationally expensive and only needs to be performed once
CVmodel_accuracy <- function(classifier, data, K, loss, featstrain, labeltrain){
  
  folds <- createFolds(data$cloud_label, k = K)
  fold_loss <- rep(0,K)
  
  for (i in 1:K){
    val_dat <- data[folds[[i]],]
    train_dat <- data[-folds[[i]],]
    
    #knn
    if (classifier=="knn"){
      datclassed <- knn(train_dat[,4:6], val_dat[,4:6],train_dat$cloud_label,k=10)
    }
    
    #logistic
    else if (classifier=="glm"){
      formula <- as.formula(paste(labeltrain, "~",  paste(featstrain, collapse = "+")))
      train_dat$cloud_label <- replace(train_dat$cloud_label,
                                       train_dat$cloud_label == -1, 0)
      val_dat$cloud_label <- replace(val_dat$cloud_label,
                                     val_dat$cloud_label == -1, 0)
      model <- glm(formula, data=train_dat, family="binomial")
      model.pred <- predict(model, val_dat, type="response")
      datclassed <- rep(0, length(model.pred))
      datclassed[model.pred >= 0.5] = 1
    }  
    
    #lda
    else if(classifier=="lda"){
      formula <- as.formula(paste(labeltrain, "~",  paste(featstrain, collapse = "+")))
      model <- lda(formula, train_dat)
      model.pred <- predict(model, val_dat)
      datclassed <- model.pred$class
    }
    
    #qda
    else if(classifier=="qda"){
      formula <- as.formula(paste(labeltrain, "~",  paste(featstrain, collapse = "+")))
      model <- qda(formula, train_dat)
      model.pred <- predict(model, val_dat)
      datclassed <- model.pred$class
    }
    
    #boosting
    else if(classifier=="gbm"){
      formula <- as.formula(paste(labeltrain, "~",  paste(featstrain, collapse = "+")))
      train_dat$cloud_label <- replace(train_dat$cloud_label,
                                       train_dat$cloud_label == -1, 0)
      val_dat$cloud_label <- replace(val_dat$cloud_label,
                                     val_dat$cloud_label == -1, 0)
      model <- gbm(formula, data = train_dat, distribution = "adaboost") 
      model.pred <- predict(model, val_dat, n.trees = 100)
      datclassed <- rep(0, length(model.pred))
      datclassed[model.pred >= 0.5] = 1
    }
    fold_loss[i] <- loss(datclassed, val_dat$cloud_label) 
  }  
  return(fold_loss)
}



#Dataframe Setup for all model methods with cross validation
rn <- c("Fold 1", "Fold 2", "Fold 3", "Fold 4", "Fold 5")
# Add validation set to test set, since it's unnecessary on it's own
timagestest <- rbind(timagestest, timagesval)
imagestest <- rbind(imagestest, imagesval)

#Add cl column for models that want binary 0,1 inputs
timagestrain <- mutate(timagestrain, cl=ifelse(cloud_label==1, 1, 0))
timagestest <- mutate(timagestest, cl=ifelse(cloud_label==1, 1, 0))
imagestrain <- mutate(imagestrain, cl=ifelse(cloud_label==1, 1, 0))
imagestest <- mutate(imagestest, cl=ifelse(cloud_label==1, 1, 0))


#CV for every Model

#### Logistic
```{r} 
#CV for both fold methods, across folds
a1 <- CVmodel_accuracy("glm",timagestrain,5,loss=zero_one_loss,
                       c("NDAI","CORR","SD"),"cloud_label")#First fold method
a2 <- CVmodel_accuracy("glm",imagestrain,5,loss=zero_one_loss,
                       c("NDAI","CORR","SD"),"cloud_label")
data.frame("transformed"=round(a1,3),"untransformed"=round(a2,3), row.names=rn)
#Test Accuracy- Transformed images have better CV accuracy, so use second split on test data
model.pred <- glm("cl ~ NDAI + CORR +SD", data=timagestrain, family="binomial")
glm.pred <- predict(model.pred, timagestest, type="response")
datclassedglm <- rep(0, length(glm.pred))
datclassedglm[glm.pred >= 0.5] = 1
data.frame("logistic_test_accuracy"=zero_one_loss(datclassedglm, timagestest$cl))

```

#### K Nearest Neighbors
#CV for both fold methods, across folds
a1 <- CVmodel_accuracy("knn",timagestrain,5,loss=zero_one_loss,
                       c("NDAI","CORR","SD"),"cloud_label")
a2 <- CVmodel_accuracy("knn",imagestrain,5,loss=zero_one_loss,
                       c("NDAI","CORR","SD"),"cloud_label")
data.frame("transformed"=round(a1,3),"untransformed"=round(a2,3), row.names=rn)
#Test Accuracy- Untransformed images have slightly better CV accuracy but lower test accuracy
datclassedknn <- knn(imagestrain[,4:6], imagestest[,4:6],imagestrain$cloud_label,k=10, prob=TRUE)
data.frame("KNN_test_accuracy"=zero_one_loss(datclassedknn, imagestest$cloud_label))
#### LDA 
a1 <- CVmodel_accuracy("lda",timagestrain,5,loss=zero_one_loss,
                       c("NDAI","CORR","SD"),"cloud_label")
a2 <- CVmodel_accuracy("lda",imagestrain,5,loss=zero_one_loss,
                       c("NDAI","CORR","SD"),"cloud_label")
data.frame("transformed"=round(a1,3),"untransformed"=round(a2,3), row.names=rn)
#Test Accuracy- Untransformed images have better slightly better CV and test accuracy, 
#so use second split on test data
model <- lda(cloud_label ~ NDAI + CORR +SD,timagestrain)
lda.pred <- predict(model, timagestest)
datclassedlda <- lda.pred$class
data.frame("LDA_test_accuracy"=zero_one_loss(datclassedlda, timagestest$cloud_label))

#### QDA
a1 <- CVmodel_accuracy("qda",timagestrain,5,loss=zero_one_loss,
                       c("NDAI","CORR","SD"),"cloud_label")
a2 <- CVmodel_accuracy("qda",imagestrain,5,loss=zero_one_loss,
                       c("NDAI","CORR","SD"),"cloud_label")
data.frame("transformed"=round(a1,3),"untransformed"=round(a2,3), row.names=rn)y 
model <- qda(cloud_label ~ NDAI + CORR +SD,timagestrain)
qda.pred <- predict(model, timagestest)
datclassedqda <- qda.pred$class
data.frame("QDA_test_accuracy"=zero_one_loss(datclassedqda, timagestest$cloud_label))

#Combined ROC curves for all methods
plot( lda_perf, col="purple",main="ROC Curves for All Methods")
plot(qda_perf, add = TRUE, col="red")
plot(log_perf,add = TRUE,col = "blue")
plot(knn_perf, avg= "threshold", col= "orange", add=TRUE)
legend("bottomright", legend=c("lda", "qda","logistic","knn"),
       col=c("purple", "red","blue","orange"),lty=1)
points(x=0.13,y=0.96582,pch=16,cex=1.5)
