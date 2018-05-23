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
