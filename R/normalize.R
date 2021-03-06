#' Normalize data with different method
#'
#' @param data_matrix The data matrix with column in sample and row in feature
#' @param method different methods to normalize data.
#' \describe{
#'    \item{\code{median-MAD}}{median-centering followed by median absolute deviation (MAD) scaling}
#'    \item{\code{zscore}}{mean-centering followed by standard deviation scaling}
#'    \item{\code{VSN}}{use VSN method normalize data}
#'    \item{\code{quantile}}{use quantile normalization method normalize data}
#'    \item{\code{2-component}}{use 2-component gaussian mixture model normalize data}
#' }
#'
#' @seealso \code{\link[vsn]{justvsn}}
#'
#' @export
normalize_data <- function(data_matrix, method = c('median-MAD', 'zscore', 'VSN', 'quantile', '2-component')) {

  cat('\n\n-- normalize data --\n\n')

  method = match.arg(method)

  cat('   normalization method: ', method, '\n')
  if (method == 'median-MAD') {
    data_norm <- scale_normalize(data_matrix, method = 'median')
  } else if (method == 'zscore') {
    data_norm <- scale_normalize(data_matrix, method = 'mean')
  } else if (method == 'quantile') {
    data_norm <- quantile_normalize(data_matrix)
  } else if (method == 'VSN') {
    data_norm <- variance_stabilizing_normalize(data_matrix)
  } else if (method == '2-component') {

    data.norm.list <- vector('list', ncol(data_matrix))
    names(data.norm.list) <- colnames(data_matrix)

    for(x in colnames(data_matrix)){
      res <- try(two_comp_normalize(data_matrix[, x], type="unimodal"))
      data.norm.list[[x]] <- res
      if(class(res) == 'try-error') break;
    }

    ## if 2-comp was successful on all data column convert list to matrix
    data_norm <-  matrix( unlist(lapply(data.norm.list, function(x)x$norm.sample)), ncol=length(data.norm.list), dimnames=list(rownames(data_matrix), names(data.norm.list)) )
  } else {
    stop('We can\'t get the method to nromalize data')
  }
  cat('\n\n-- normalize data done--\n\n')
  return(data_norm)
}



#' Normalize data by median value or mean value
#'
#' @param data_matrix The data matrix with column in sample and row in feature
#' @param method The normalization method:
#' \describe{
#'    \item{\code{median-MAD}}{median-centering followed by median absolute deviation (MAD) scaling}
#'    \item{\code{zscore}}{mean-centering followed by standard deviation scaling}
#' }
#' @importFrom stats median mad sd
#'
#' @return return data matrix after normalize
#' @export
#'
scale_normalize <- function(data_matrix, method = c('median-MAD', 'zscore')) {
  ## Reference: the function `scale()`
  method <- match.arg(method)
  if (method == 'median') {
    norm.params <- t(apply(data_matrix, 2, function (x) c(center = median(x, na.rm = T), scale = mad(x, na.rm = T))))
    data.norm <- scale(data_matrix, center = norm.params[,'center'], scale = norm.params[,'scale'])
  } else {
    norm.params <- t(apply(data_matrix, 2, function (x) c(center = mean(x, na.rm = T), scale = mean(x, na.rm = T))))
    data.norm <- scale(data_matrix, center = norm.params[,'center'], scale = norm.params[,'scale'])
  }
  return(data.norm)
}


#' Quantile normalization of data
#'
#' @param data_matrix The data matrix with column in sample and row in feature
#' @importFrom preprocessCore normalize.quantiles
#'
#' @return return data matrix after normalize
#' @export
#'
quantile_normalize <- function(data_matrix) {
  data.norm <- normalize.quantiles(data_matrix)
  return(data.norm)
}

# Normalize data use upper quantile
#
# @param data_matrix The data matrix with column in sample and row in feature
# @importFrom stats quantile
#
# @return return data matrix after normalize
#
# upper_quantile_normalize <- function(data_matrix) {
#   data.norm <- apply(data, 2, function(x) x - quantile(x, c(0.75),na.rm=T))
#   return(data.norm)
# }


#' VSN normalization of data
#'
#' @param data_matrix The data matrix with column in sample and row in feature
#' @importFrom vsn justvsn
#'
#' @return return data matrix after normalize
#' @export
#'
vsn_normalize <- function(data_matrix) {
  data.norm <- justvsn(data_matrix)
  return(data.norm)
}

#' Two component normalize of data
#' @param sample the value of one sample
#' @param type types
#' @param mode.lower.bound mode.lower.bound
#'
#' @importFrom stats density
#' @importFrom mixtools normalmixEM
#' @importFrom mclust Mclust
#' @export
two_comp_normalize <- function (sample, type='default', mode.lower.bound=-3) {
  #   1. For all sample types, fit a 2-component gaussian mixture model using normalmixEM.
  #   2. For the bimodal samples, find the major mode M1 by kernel density estimation
  #     2a. Fit the model with one component mean constrained to equal M1
  #     2b. Normalize (standardize) samples using mean (M1) and resulting std. dev.
  #   3. For unimodal (default) samples, find the mode M using kernel density estimation
  #     3a. Fit the model with mean for both components constrained to be equal to M
  #     3b. Normalize (standardize) samples using mean M and smaller std. dev. from model fit
  #
  #  the major mode should be located at a value larger than mode.lower.bound

  # WARNING:
  # This code has a lot of hacks to fix the flakiness of normalmixEM, and the idiosyncracies
  # of the actual data. Carefully re-examine code for new or altered input data
  # Currently works for log-ratio data approximately centered around 0
  cat('\n-- two.comp.normalize --\n')
  is.error <- function(x) inherits(x, "try-error")             # function to check for error

  dat <- sample [ !is.na (sample) ]
  data.range <- diff (range (dat))
  dens <- try (density (dat, kernel='gaussian', bw='SJ'))     # gaussian kernel with S-J bandwidth
  if (is.error (dens))                                         # sometimes, SJ bw estimation fails
    dens <- density (dat, kernel='gaussian', bw='ucv')        # in such cases, use unbiased CV
  # (see Venalbles & Ripley, 2002, pg, 129
  #  and Density Estimation, S.J.Sheather, Stat. Sci. 2004)
  # find major (highest) mode > -3 (to avoid problems with lower mode having higher density than higher mode)
  x.range <- dens$x > mode.lower.bound
  dens.x <- dens$x [x.range];  dens.y <- dens$y [x.range]
  mode <- dens.x[which.max(dens.y)]
  if (type=='bimodal') mean.constr <- c (NA, mode) else mean.constr <- c (mode, mode)
  model <- normalmixEM (dat, k=2, mean.constr=mean.constr, maxit=10000)
  model.rep <- normalmixEM (dat, k=2, mean.constr=mean.constr, maxit=10000)
  model.alt <- Mclust (dat, G=2, modelNames=c ("V","E"))
  # V results is separate SDs for each cluster; E fits a single SD for both clusters
  if (length (model.alt$parameters$variance$sigmasq)==1)  # latter code expects two SD values
    model.alt$parameters$variance$sigmasq <- rep (model.alt$parameters$variance$sigmasq, 2)
  alt.mu <- model.alt$parameters$mean
  alt.sd <- sqrt (model.alt$parameters$variance$sigmasq)
  # find reproducible model fit that is close to Mclust fit
  # if not, re-fit model -- without this condition
  # normalmixEM produces one-off model fits
  n.try <- 1
  if (type=='bimodal') model.mode <- which(model$mu==mode)
  else model.mode <- which(model$mu==mode)[which.min (model$sigma)]
  model.other <- model.mode %% 2 + 1
  alt.mode <- ifelse (diff (alt.mu) < data.range*0.05,          # means are close --
                      which.min (alt.sd),                       # use min sd to pick alt.mode
                      which.min(abs(model.alt$par$mean-mode)))  #  else use alt.mu closest to mode
  # always using latter can result in consistently picking the wrong alt.mode => no convergence
  alt.other <- alt.mode %% 2 + 1
  while ( abs (model$mu[model.mode] - alt.mu[alt.mode]) > data.range*0.05 ||
          abs (model$sigma[model.mode]-alt.sd[alt.mode]) > data.range*0.05 ||
          model$sigma[model.mode] < 0.1 ||
          (type=='bimodal' && (abs (model$mu[model.other] - alt.mu[alt.other]) > data.range*0.25)) ||
          abs (sum (c (model$mu, model$sigma) - c (model.rep$mu, model.rep$sigma))) > 1e-3 ) {
    # if major mode (and SD of mode) is not within 5% of data range, or if the other mean (for bimodals only)
    # is not within 25% of the Mclust result, try again
    model <- normalmixEM (dat, k=2, mean.constr=mean.constr, maxit=10000)
    model.rep <- normalmixEM (dat, k=2, mean.constr=mean.constr, maxit=10000)

    ###############################
    ## error handling
    if (n.try > 1e3){
      stop("No_success_2comp")
    }
    n.try <- n.try + 1
  }


  if (type=='bimodal') {
    # sometimes (esp. in phosphoproteome) the minor (lower) mode can be larger than the major (higher) mode
    # this situation is not possible in the unimodal samples
    corrected.mode <- model$mu [which.max(model$mu)]
    if (corrected.mode != mode) {
      cat ('  Lower mode larger than higher mode\n')
      mode <- corrected.mode
    }
  }
  norm.mean <- mode
  norm.sd <- ifelse (type=='bimodal', model$sigma[which(model$mu==mode)], min (model$sigma))

  # normalize by standardizing
  dat <- dat - norm.mean
  dat <- dat / norm.sd

  # return normalized data reorganized to original order
  sample [ !is.na (sample) ] <- dat
  cat('\n-- two.comp.normalize exit --\n')
  return ( list (norm.sample=sample, norm.mean=norm.mean, norm.sd=norm.sd, fit=unlist (c(model$mu, model$sigma))) )
}
