
#' @title Flags outliers using kmeans clustering method
#'
#' @param data Dataframe to check for outliers
#' @param k The number of clusters to be used for optimization. It should be greater than 1. For many species k should be be greater 10
#' to ably cater for each species search for optimal k using the different optimization methods in kmethod
#' @param exclude Exclude variables that should not be considered in the fitting the one class model, for example x and y columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param output Either clean: for a data set with no outliers or outlier: to output a data frame with outliers.
#' @param mode Either robust, if a robust mode is used which uses median instead of mean and median absolute deviation from median.
#' @param verbose To indicate messages and the default is FALSE.
#' @param method The method to be used for the kmeans clustering. Default is \code{silhouette}.
#' \code{Elbow method} can be used but user input is required, and therefore multiple outlier detection method
#' is not possible.
#'
#' @importFrom stats kmeans sd dist
#' @importFrom cluster silhouette
#'
#' @return Dataframe with or with no outliers.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(terra)
#'
#' #species data from online databases
#'
#' gbdata <- df_retronline(data='Gymnocephalus baloni', gbiflim = 100, inatlim = 100, vertlim = 100)
#'
#' gbfinal <- merge_all(online = gbdata)
#'
#' gbchecked <- check_names(data = gbfinal, colsp='species', pct=90, merge=TRUE)
#'
#' #preclean and extract
#'
#' danube <- system.file('extdata/danube/basinfinal.shp', quiet=TRUE, package='spcleanr')
#'
#' danubebasin <- sf::st_read(danube)
#'
#' #Get environmental data
#'
#' #worldclim_bio <- env_download(var='bio', resolution = 10, basin = danube, folder='worlclimddata')
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', quiet=TRUE, package='spcleanr'))
#'
#' precleaned <- pre_cleaner(data = gbchecked,
#'                           raster= worldclim ,
#'                           colsp = 'speciescheck',
#'                           basin = danubebasin,
#'                           multiple = FALSE,
#'                           maxpts = 10)
#'
#' #outliers
#' kmeanoutliers <- xkmeans(data = precleaned,
#'                          k=6, exclude = c('x','y'),
#'                          kmethod ='silhouette',
#'                          output='outlier')
#'
#' #clean data
#' kmeanclean <- xkmeans(data = precleaned,
#'                          k=6, exclude = c('x','y'),
#'                          kmethod ='silhouette',
#'                          output='clean')
#' }
#'
#'
xkmeans <- function(data, k, exclude, output, mode, method=NULL, verbose=FALSE){

  match.arg(method, choices = c('silhouette','elbow'))

  match.arg(mode, choices = c('soft', 'robust'))

  df2 <- data[,!colnames(data) %in% exclude]

  df_scaled <- scale(df2)
  if(k<1 || k==1) stop('Increase k to atleast 2 to form clusters in the data.')

  wcss <- c(); kcenters <- c(); sil_val <- list()

  if(method=='elbow' && !is.null(method)){

    if(nrow(df_scaled)<10) k = 3 else k = k # to avoid errors for few data.

    for (ik in 2:k) {

      km <- stats::kmeans(df_scaled, centers = ik, iter.max = 100, nstart = 4)

      wcss[ik-1] <- km$tot.withinss

      kcenters[ik-1] <- ik

      plot(x=kcenters, y=wcss, xlab='Number of clusters', ylab='Total within sum of squares',
           type = "b", main='Choose cluster number where the line forms an bend')

    }
    opt_k <- as.integer(readline(prompt = 'Enter the value of k with a bend: '))

    if(opt_k>length(kcenters)) stop('Optimal cluster centers should be within the
                                   number of cluster iteration number: ', k, call. = FALSE)
  }
  else if(method=='silhouette' && !is.null(method)){

    if(nrow(df_scaled)<10) k = 3 else k = k # to avoid errors for few data.

    for (ik in 1:k) {

      km <- stats::kmeans(df_scaled, centers = ik, iter.max = 100, nstart = 4)

      sil <- cluster::silhouette(km$cluster, dist(df_scaled))

      if(length(sil)>1){

        sil_val[[ik]] <- sil[,3]

      }else{
        if(isTRUE(verbose)) message('The k centers should be greater than 1 to compute silhouette neighbour clusters')

      }
    }

    meanlst <- sapply(sil_val[-which(sapply(sil_val, is.null))], mean)

    opt_k <- which(meanlst==max(meanlst))+1 #to cater for 1-k means

    if(isTRUE(verbose))message('The optimal k value of ', opt_k, ' has been selected')

  }else{
    if(isTRUE(verbose)) message('Optimal k optimal method not selected and a default value of 3 will be used')
    opt_k = 3
  }

  km_new <- stats::kmeans(df_scaled, centers = opt_k, iter.max = 100, nstart = 4)

  eu_dist<- sqrt(rowSums(df_scaled - km_new$centers[km_new$cluster,])**2)#euclidean distances

  meaneu <- base::mean(eu_dist)
  sdeu <- stats::sd(eu_dist)

  medianeu <- stats::median(eu_dist)

  madeu <- stats::mad(eu_dist)

  zscores <- c()

  for(iik in 1:length(eu_dist)){

    zscores[iik] <- switch(mode, soft=(eu_dist[iik]-meaneu)/sdeu, robust=(eu_dist[iik]-medianeu)/madeu)
  }

  datIn <- which(zscores<2 & zscores> (-2))

  switch(output, clean= return(data[datIn,]), outlier=return(data[-datIn,]))
}



#' @title Using kmedian outlier detection methods
#'
#' @param data Input data for for checking outliers
#' @param k Number of clusters
#'
#' @return clean data set after kmedian outlier cleaning.
#'
#' @export
#'
#' @examples
#'
xkmedian <- function(data, k=3){

  set.seed(123)

  #select random starting rows
  set.seed(123)
  rstart <- sample(x=nrow(data), size = k)
  dfx <- data[rstart,]
  cl <- matrix(NA, nrow = nrow(data), ncol = nrow(dfx))
  for (i in 1:nrow(data)) {
    dr <- as.vector(t(data)[, i])
    for (ii in 1:nrow(dfx)) {
      cc <- as.vector(t(dfx[ii,]))
      cl[i,ii] <- sqrt(Reduce(abs(dr-cc)^2, f='+'))
    }
  }
  #get cluster labels
  label <- apply(cl, 1, which.min)
  #recalculate the centroids
  crcal <- list()
  for (iii in seq_along(unique(label))) {
    clust <- data[which(label==iii),]
    crcal[[iii]] <- apply(clust, 2, median)
  }
  #recompute distances
  cshf <- matrix(NA, nrow = nrow(data), ncol = length(crcal))

  for (iv in 1:nrow(data)) {
    dr2 <- as.vector(t(data)[, iv])
    for (v in seq_along(crcal)) {
      c2 <- as.vector(t(crcal[[v]]))
      cshf[iv,v] <- sqrt(Reduce(abs(dr2-c2)^2, f='+'))
    }
  }
  label2 <- apply(cshf, 1, which.min)


  if(all(label==label2)==FALSE){
    #get the centroids
    crcal2 <- list()
    for (vi in seq_along(unique(label2))) {
      clust2 <- data[which(label2==vi),]
      crcal2[[vi]] <- apply(clust2, 2, median)
    }

    #recompute distances
    cshf2 <- matrix(NA, nrow = nrow(data), ncol = length(crcal2))

    for (vii in 1:nrow(data)) {
      dr3 <- as.vector(t(data)[, vii])
      for (viii in seq_along(crcal2)) {
        c3 <- as.vector(t(crcal2[[viii]]))
        cshf2[vii,viii] <- sqrt(Reduce(abs(dr3-c3)^2, f='+'))
      }
    }
    label3 <- apply(cshf2, 1, which.min)
  }else{
    message('converged')
  }

  return(list(label, label2, label3))
}

#' Title
#'
#' @param data kk
#' @param k kk
#' @param metric kk
#' @param output kk
#' @param exclude kk
#' @param x a constant to detemrine outliers.
#'
#' @return
#' @export
#'
#' @examples
xkmedoid <- function(data, k = 2, metric = 'manhattan', output='clean', exclude, x=1.5){

  if(missing(data)) stop('Data missing')

  if(k==1) warning('All data willl put in one cluster with one medoid.')

  match.arg(metric, choices = c('euclidean', 'manhattan'))

  match.arg(output, choices = c('clean', 'outlier'))

  #run the partitioning around the medoid (pam function)

  dfd <- scale(data[,!colnames(data) %in% exclude])

  p = switch(metric, euclidean = cluster::pam(x= dfd, k= k, metric = 'euclidean'),
             manhattan = cluster::pam(x= dfd, k= k, metric = 'manhattan'))

  admp <- c()
  fd <- list()

  for(i in seq_along(unique(p$clustering))){

    df <- dfd[which(p$clustering==i),]

    for (ii in 1:nrow(df) ) {

      rd <- as.vector(t(df)[, ii])

      md <- as.vector(t(p$medoids)[, i])

      if(metric=='euclidean'){
        admp[ii] <- sqrt(Reduce(abs(rd-md)^2, f='+')) #compute the absolute distance between medoids
      }else{
        admp[ii] <- Reduce(abs(rd-md), f='+') #compute the absolute distance between medoids
      }

    }

    tr <- mean(admp)*x #compute threshold for determining outliers

    datIn <- which(admp<tr) #identify outliers

    fd[[i]] <- switch(output, clean= df[datIn,], outlier=df[-datIn,])

    out <- do.call(rbind, fd)
  }
  return(out)
}

