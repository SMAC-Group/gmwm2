
#' @title Place Legend
#' @description This function decides where the legend should be put (top left or bottom left)
#' @param wv_1 A \code{double} that indicates the first value of \code{wv.empir}
#' @param low_n A \code{doble} that indicates the last value of \code{ci.low}
#' @param high_n A \code{dobule} that indicates the last value of \code{ci.high}
#' @return A numeric vector containing 4 elements. The first two elements indicate legend.justification, the last two elements indicate legend.position (see \code{?theme}).
#' @keywords internal
placeLegend = function(wv_1, low_n, high_n){
  if(log10(wv_1) > ( log10(low_n) + log10(high_n) )/2 ){
    # legend should be placed in bottom left
    legend.justification = c(0,0)
    legend.position = c(0,0)
    #x = wv_1[1]/xlim_length
    #y = high_n/ylim_length
  }
  else{
    # legend should be placed in top left
    legend.justification = c(0,1)
    legend.position = c(0,1)
    #x = wv_1[1]/xlim_length
    #y = low_n/ylim_length
  }
  return( c(legend.justification, legend.position) )

}


#' @title Check the Parameters
#' @description Check the user supplied parameters and assign them to the default if they are wrong.
#' @param params A \code{character vector} that specifies the user supplied parameters.
#' @param require.len An \code{integer vector} that specifies the required length of each parameter.
#' @param default A \code{list} that specifies the default of each parameter.
#' @param null.is.fine A \code{boolean vector} to indicate whether \code{NULL} is fine for parameters.
#' @param env An \code{environment} to use.
#' @keywords internal
#' @details
#'
#' The user supplied parameters are usually \code{line.color}, \code{line.type}, \code{point.size},
#' \code{point.shape}, \code{CI.color} and \code{legend.label}.
#'
#' This function will check whether the required length of the parameter is met. If not, it will assign the
#' default value to that parameter.
#'
checkParams = function(params, require.len, default, null.is.fine, env = parent.frame()){

  for (i in 1:length(params)){

    one_param = params[i]
    value = get(one_param, envir = env)

    if( length(value)!=require.len[i]){

      isNull = is.null(value)

      if( (!isNull) || (!null.is.fine[i]) ){

        warning(paste('Parameter', one_param, 'requires', require.len[i],'elements,','but', length(value),
                      'is supplied.','Default setting is used.'))
      }

      assign(one_param, default[[i]], envir = env)
    }
  }

}
