#' @importFrom stats model.offset
#' @export
fitted.geese <- function(object, ...) {

    # Get the linear predictor
    glmcall <- object$call

    drop <- c("id", "jack", "control", "corstr", "waves", "zcor",
          "std.err", "scale.fix", "scale.value", "z", "family")
    glmcall[drop] <- NULL

    glmcall[[1]] <- as.name("model.frame")

    mf <- eval(glmcall, parent.frame())

    X <- model.matrix(formula(object), data=mf)
    N <- nrow(X)


    offset <- model.offset(mf)
    if (is.null(offset))
        offset <- rep(0, N)

  value <- object


    value$offset <- offset
    if (is.null(value$offset))
        value$linear.predictors <- X %*% object$beta
    else value$linear.predictors <- value$offset + X %*%
        object$beta


    fitted.values <- family(value)$linkinv(value$linear.predictors)

    fitted.values

}

#' @export
family.geese <- function(object, ...) {
    family <- object$call$family
    if (is.null(family))
        tryCatch({
            family_defaults <- formals(eval(object$call[[1]]))$family # attempt to find defaults of call
            if (is.language(family_defaults)) {
                eval(family_defaults)
            } else {
                family_defaults
            }
        }, error = function(e) {
            stats::gaussian()
        })
    else {
        eval(family)
    }

}
