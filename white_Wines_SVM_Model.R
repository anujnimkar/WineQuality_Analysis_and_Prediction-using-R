# Rattle is Copyright (c) 2006-2014 Togaware Pty Ltd.

#============================================================

# Rattle version 3.4.1 user 'Anuj'

# Export this log textview to a file using the Export button or the Tools 
# menu to save a log of all activity. This facilitates repeatability. Exporting 
# to file 'myrf01.R', for example, allows us to the type in the R Console 
# the command source('myrf01.R') to repeat the process automatically. 
# Generally, we may want to edit the file to suit our needs. We can also directly 
# edit this current log textview to record additional information before exporting. 

# Saving and loading projects also retains this log.

library(rattle)

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building

# The colorspace package is used to generate the colours used in plots, if available.

library(colorspace)

# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================

# Load the data.

crs$dataset <- read.csv("file:///F:/Anuj/Study & Work/Sem 2 Courses/INST 737/Final Project/wineWhites_trainData.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================


# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 3848 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 2693 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 577 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 578 observations

# The following variable selections have been noted.

crs$input <- c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
               "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density",
               "pH", "sulphates", "alcohol", "quality")

crs$numeric <- c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
                 "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density",
                 "pH", "sulphates", "alcohol", "quality")

crs$categoric <- NULL

crs$target  <- "Acceptability"
crs$risk    <- NULL
crs$ident   <- "X"
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

require(kernlab, quietly=TRUE) 

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(Acceptability) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="rbfdot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm
