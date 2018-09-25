# Copyright (C) 2014 - 2017  James Balamuta, Stephane Guerrier, Roberto Molinari
#
# This file is part of GMWM R Methods Package
#
# The `gmwm` R package is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# The `gmwm` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Wavelet Variance
#'
#' Calculates the (MODWT) wavelet variance
#' @param x         A \code{vector} with dimensions N x 1, or a \code{lts} object, or a \code{gts} object, or a \code{imu} object.
#' @param decomp    A \code{string} that indicates whether to use the "dwt" or "modwt" decomposition.
#' @param filter    A \code{string} that specifies what wavelet filter to use.
#' @param nlevels   An \code{integer} that indicates the level of decomposition. It must be less than or equal to floor(log2(length(x))).
#' @param robust    A \code{boolean} that triggers the use of the robust estimate.
#' @param eff       A \code{double} that indicates the efficiency as it relates to an MLE.
#' @param alpha     A \code{double} that indicates the \eqn{\left(1-p\right)*\alpha}{(1-p)*alpha} confidence level
#' @param freq      A \code{numeric} that provides the rate of samples.
#' @param from.unit A \code{string} indicating the unit which the data is converted from.
#' @param to.unit   A \code{string} indicating the unit which the data is converted to.
#' @param ... Further arguments passed to or from other methods.
#' @return A \code{list} with the structure:
#' \describe{
#'   \item{"variance"}{Wavelet Variance}
#'   \item{"ci_low"}{Lower CI}
#'   \item{"ci_high"}{Upper CI}
#'   \item{"robust"}{Robust active}
#'   \item{"eff"}{Efficiency level for Robust}
#'   \item{"alpha"}{p value used for CI}
#'   \item{"unit"}{String representation of the unit}
#' }
#' @details
#' If \code{nlevels} is not specified, it is set to \eqn{\left\lfloor {{{\log }_2}\left( {length\left( x \right)} \right)} \right\rfloor}{floor(log2(length(x)))}
#' @author JJB
#' @rdname wvar
#' @examples
#' set.seed(999)
#' x = rnorm(100)
#' # Default
#' wvar(x)
#' # Robust
#' wvar(x, robust = TRUE, eff=0.3)
#' # 90% confidence interval
#' wvar(x, alpha = 0.10)
#'
#' # IMU Object
#' \dontrun{
#' if(!require("imudata")){
#'    install_imudata()
#'    library("imudata")
#' }
#'
#' data(imu6)
#' test = imu(imu6, gyros = 1:3, accels = 4:6, freq = 100)
#' df = wvar.imu(test)
#' }
#' @export
wvar = function(x, ...) {
  UseMethod("wvar")
}

#' @rdname wvar
#' @export
wvar.lts = function(x, decomp = "modwt", filter = "haar", nlevels = NULL, alpha = 0.05, robust = FALSE, eff = 0.6, to.unit = NULL, ...){
  warning('`lts` object is detected. This function can only operate on the combined process.')
  freq = attr(x, 'freq')
  unit = attr(x, 'unit')
  x = x[,ncol(x)]

  wvar.default(x, decomp, filter, nlevels, alpha, robust, eff, freq = freq, from.unit = unit, to.unit = to.unit)
}

#' @rdname wvar
#' @export
wvar.gts = function(x, decomp="modwt", filter = "haar", nlevels = NULL, alpha = 0.05, robust = FALSE, eff = 0.6, to.unit = NULL, ...){
  freq = attr(x, 'freq')
  unit = attr(x, 'unit')
  x = x[,1]

  wvar.default(x, decomp, filter, nlevels, alpha, robust, eff, freq = freq, from.unit = unit, to.unit = to.unit)
}

#' @rdname wvar
#' @export
wvar.ts = function(x, decomp="modwt", filter = "haar", nlevels = NULL, alpha = 0.05, robust = FALSE, eff = 0.6, to.unit = NULL, ...){
  freq = attr(x, 'tsp')[3]
  unit = NULL

  wvar.default(x, decomp, filter, nlevels, alpha, robust, eff, freq = freq, from.unit = unit, to.unit = to.unit)
}

#' @rdname wvar
#' @export
wvar.default = function(x, decomp = "modwt", filter = "haar", nlevels = NULL, alpha = 0.05, robust = FALSE, eff = 0.6, freq = 1, from.unit = NULL, to.unit = NULL, ...){
  if(is.null(x)){
    stop("`x` must contain a value")
  }else if((is.data.frame(x) || is.matrix(x))){
    if(ncol(x) > 1) stop("There must be only one column of data supplied.")
  }

  if(is.null(nlevels)){
    nlevels = floor(log2(length(x)))
  }

  # check freq
  if(!is(freq,"numeric") || length(freq) != 1){ stop("'freq' must be one numeric number.") }
  if(freq <= 0) { stop("'freq' must be larger than 0.") }

  # check unit
  all.units = c('ns', 'ms', 'sec', 'second', 'min', 'minute', 'hour', 'day', 'mon', 'month', 'year')
  if( (!is.null(from.unit) && !from.unit %in% all.units) || (!is.null(to.unit) && !to.unit %in% all.units) ){
      stop('The supported units are "ns", "ms", "sec", "min", "hour", "day", "month", "year". ')
  }

  if(robust) {
    if(eff > 0.99) {
      stop("The efficiency specified is too close to the classical case. Use `robust = FALSE`")
    }
  }

  obj =  .Call('_gmwm2_modwt_wvar_cpp', PACKAGE = 'gmwm2',
               signal=x, nlevels=nlevels, robust=robust, eff=eff, alpha=alpha,
               ci_type="eta3", strWavelet=filter, decomp = decomp)

  scales = .Call('_gmwm2_scales_cpp', PACKAGE = 'gmwm2', nlevels)/freq

  # NO unit conversion
  if( is.null(from.unit) && is.null(to.unit)==F ){
    warning("'from.unit' is NULL. Unit conversion was not done.")
  }

  # unit conversion
  if (!is.null(from.unit)){
    if (!is.null(to.unit)){
      convert.obj = unitConversion(scales, from.unit = from.unit, to.unit = to.unit)

      if (convert.obj$converted) {
        # YES unit conversion
        scales = convert.obj$x
        message(paste0('Unit of object is converted from ', from.unit, ' to ', to.unit), appendLF = T)
      }
    }
  }

  if(!is.null(from.unit) && !is.null(to.unit)){
    unit = to.unit
  }else{
    unit = from.unit}

  create_wvar(obj, decomp, filter, robust, eff, alpha, scales, unit)
}

#' @rdname wvar
#' @export
wvar.imu = function(x, decomp = "modwt", filter = "haar", nlevels = NULL, alpha = 0.05, robust = F, eff = 0.6, to.unit = NULL, ...){

  if(!is.imu(x)){
    stop("`wvar.imu()` requires an IMU Object")
  }

  mlevels = floor(log2(nrow(x)))

  if(is.null(nlevels)){
    nlevels = mlevels
  }
  if(nlevels > mlevels){
    stop("`nlevels` must be less than ", mlevels,", which is the max number of levels.")
  }

  if(robust) {
    if(eff > 0.99) {
      stop("The efficiency specified is too close to the classical case. Use `robust = FALSE`")
    }
  }

  # freq conversion
  x.freq = attr(x, 'freq')
  scales = .Call('_gmwm2_scales_cpp', PACKAGE = 'gmwm2', nlevels)/x.freq

  # NO unit conversion
  from.unit = attr(x, 'unit')
  if( is.null(from.unit) && is.null(to.unit)==F ){
    warning("The unit of the object is NULL. Unit conversion was not done.")
  }

  # unit conversion
  if (!is.null(from.unit)){
    if (!is.null(to.unit)){
      # start_end = c(start, end)
      obj = unitConversion(scales, from.unit = from.unit, to.unit = to.unit)

      if (obj$converted) {
        # YES unit conversion
        scales = obj$x

        message(paste0('Unit of object is converted from ', from.unit, ' to ', to.unit), appendLF = T)
      }
    }
  }

  obj.mat = .Call('_gmwm2_batch_modwt_wvar_cpp', PACKAGE = 'gmwm2',
                   x, nlevels, robust, eff, alpha, ci_type="eta3", strWavelet="haar", decomp)

  # The correct unit
  if(!is.null(from.unit) && !is.null(to.unit)){
    unit = to.unit
  }else{
    unit = from.unit}

  # put data into the expected format
  obj.list = lapply(obj.mat, FUN = create_wvar,
                    decomp = decomp, filter = filter,
                    robust = robust, eff = eff,
                    alpha = alpha, scales = scales, unit = unit)

  sensor = attr(x, 'sensor')
  x.axis = attr(x, 'axis')
  stype = attr(x, 'stype')

  out = structure(list(dataobj = obj.list, #most info is stored in obj.list
                       axis = x.axis,
                       sensor = sensor,
                       stype = stype,
                       freq = x.freq), class="wvar.imu")

  out
}
