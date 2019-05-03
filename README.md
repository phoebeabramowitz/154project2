# 154project2
Title: Statistical Methods of Classification on Arctic Cloud Data
Team name: The Meme Team  
  Omri Newman (3032273024)
  Phoebe Abramowitz (26386343)
  
README file describing, in detail, how to reproduce your paper from scratch (assume researcher has access to the images).

# 1. Data Collection and Exploration

  The first step to reproducing this report, is to read the image data into R, and change the column names within the read.delim function. All the code in this report assumes the names are
c("y", "x","cloud_label","NDAI","SD","CORR","rad_DF","rad_CF", "rad_BF","rad_AF","rad_AN"). 
  
  To explore the percent of pixels for each class within an image, write a function which counts the number of times there is a 0, 1, or -1, and divide by the total number of rows in the image. This will give you a rough sense of how cloudy or clear a given image is without visualizations. To plot the images use geom_tile() in ggplot2 with the corresponding aesthetics. Be sure to coerce cloud_label as a factor in order for the plot function to work. Change colors and labels as desired. 
  
  For visual and quantitative exploratory data analysis, create a heatmap for each image depicting the pairwise relationship between the features by calling the correlation function cor(). This will make it easier to identify highly correlated features with cloud_label, to then use while training your classifier. Use violin plots to visualize the relationship between each feature and the cloud_label. Pay attention to patterns and trends amongst the plots. 
  
# 2. Preparation

  In this section you will split the data in preparation for training of various statistical models. Before you start the split, remove unlabeled points from each image.  
  
  Write a split function which takes in a list of images, and returns a list of three dataframes; a training set, validation set, and test set. Run this function on the images, and create three dataframes using the outputted list from the function. 80% of the original data should be in the training set, 10% in the validation set, and the last 10% should be in the test set. These dataframes will serve as your untransformed set, and you will run your models on these along with a transformed set to compare dependence relations amongst the data.
  
  Write a second function to transform the data, which takes in a list of images and returns a dataframe with 1/9th the original amount of data. This transformation should occur by taking 3pixel X 3pixel blocks and turn them into super pixels. A cloud_average function will be useful in this process, so write one which takes in a vector of values and returns -1, 0, or 1 depending on the average of values within each super pixel. Then write a new split function which randomly divides the superpixelized intro three sets. You can incorporate your first split function for ease.
  
  Now that you have a training, validation, and test set for both untransformed and transformed data, establish a baseline for this classification problem by reporting the accuracy of a trivial classifier which sets all labels to -1 (cloud-free) on the validation set and on the test set. Simply create a vector of -1's and compare the accuracy of this vector to the cloud_label in the validation and test set for both split methods. 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  