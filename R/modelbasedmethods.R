#Model based outlier detection methods
#==============
#' @title Identify outliers using Isolation forest mode (..).
#'
#' @param data Dataframe of environmental variables extracted from where the species was recorded present or absent.
#' @param size Proportion of data to be used in training isolation forest n´model. It ranges form 0.1 (fewer data  selected ) to 1 to all data used in
#' traing isolation model.
#' @param cutoff Cut to select where the record was an outlier or not.
#' @param output Either clean: for a data set with no outliers or outlier: to output a dataframe with outliers. Default is 0.5.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#'
#' @importFrom isotree isolation.forest
#' @importFrom stats predict
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#'
#'
isoforest <- function(data, size, cutoff =0.5, output, exclude){

  if(cutoff<0 |cutoff>1)stop('cutoff should range from 0 to 1')

  if(size<0 | size>1)stop('size should range from 0 to 1')

  df<- data[!colnames(data)%in%exclude]

  isomodel <- isolation.forest(data= df, sample_size = size, ntrees = 100, 1)

  isopred <- predict(isomodel,df)

  datIn <- which(isopred< cutoff)

  switch (output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}


#===========================================
#One class support vector machine
#=========================================

#' @title Identify outliers using One Class Support Vector Machines
#'
#' @param data Dataframe of environmental variables extracted from where the species was recorded present or absent.
#' @param kernel Either radial, linear
#' @param tune To performed a tuned version of one-class svm. High computation requirements needed.
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesnot want to consider.
#' @param output Either clean: for a dataset with no outliers or outlier: to output a dataframe with outliers.
#' @param tpar A list of parameters to be varied during tunning from the normal model.
#'
#' @importFrom  e1071 svm tune.svm
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#'
onesvm <- function(data, kernel='radial', tune=NULL, exclude, output,
                   tpar = list(gamma = 1^(-1:1), epislon =seq(0, 1, 0.1),
                               cost =2^2:4, nu = seq(0.05, 1, 0.1))){

  match.arg(kernel, choices = c('radial', 'linear')) #radial set to defualt, works well for high dimensional data

  df<- data[!colnames(data)%in%exclude]


  if(is.null(tune)){

    svm_model <- svm(df, type = 'one-classification', nu=0.1, scale = T, kernel = kernel)

    prediction <- predict(svm_model, df)


  }else if(tune==TRUE){

    tunemodel =  tune.svm(x=df, y=rep(T,nrow(df)), type='one-classification', kernel= kernel,
                          nu=tpar$nu, gamma = tpar$gamma)#create TRUE class to validate the model

    modnew <- tunemodel$best.model

    prediction <- predict(modnew, df)

  }else{
    message('Default untuned model executed')
  }
  datIn <- which(prediction==TRUE)

  switch (output, clean=return(data[datIn,]), outlier= return(data[-datIn,]))
}
