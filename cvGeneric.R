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
