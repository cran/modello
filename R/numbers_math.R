##' @rdname dfun
##' @export
dfun.numeric <- function (x, fn) {
    dx = (fn(x + 1e-6) - fn(x - 1e-6)) / 2e-6
    if (length(x) == 1) {
        sum(dx)
    } else {
        dx
    }
}
##' @rdname dfun
##' @export
dfun.array <- function (x, fn) {
    h = x * 0
    dx = sapply(1:length(x), function (i) {
        h[i] = 1e-6
        sum((fn(x + h) - fn(x - h)) / 2e-6) 
    })
    dim(dx) = dim(x)
    return(dx)
}
##' Estimates the derivate (or gradient) of the
##' function \code{fn} with respect to the argument
##' \code{x}
##'
##' @title Finite Differences 
##' @param x input \code{numeric}
##' @param fn function 
##' @return Returns the gradient of \code{x}
##' @author Filippo Monari
##' @examples
##' fn = function(x)x**2
##' ## On scalar arguments
##' dx_scalar = dfun(3, fn)
##' print(dx_scalar)
##' ## On matrix (or arrays)
##' dx_array = dfun(matrix(c(3, 3, 3, 3), 2, 2), fn)
##' print(dx_array)
##' @export
dfun <- function (x, fn) {
    UseMethod('dfun', x)
}
##' Add two \code{numbers}.
##'
##' @title Number Addition 
##' @param e1 a reference object of call 'number'
##' @param e2 a reference object of call 'number'
##' @return Returns a referecen object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## Scalar + scalar
##' x1 = number(1, scalar=TRUE)
##' x2 = number(2, scalar=TRUE)
##' x3 = x1 + x2
##' print(x3)
##' print(x3$v)
##' ## Array + scalar
##' x4 = number(matrix(1, 2, 2))
##' x5 = x1 + x4
##' print(x5)
##' print(x5$v)
##' ##Array + array
##' x6 = number(c(1, 1))
##' x7 = x4 + x6
##' print(x7)
##' print(x7$v)
##' modello.close()
##' @export
`+.modello_number` <- function (e1, e2) {
    stopifnot(is.number(e2))
    .modello$apply.math_op(.intrf.number__add, e1, e2)
}
##' Subtract two \code{numbers}
##'.
##' @title Number Subtraction 
##' @param e1 a reference object of class 'number'
##' @param e2 a reference objcet of class 'number
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## Scalar - scalar
##' x1 = number(2, scalar=TRUE)
##' x2 = number(1, scalar=TRUE)
##' x3 = x1 - x2
##' print(x3)
##' print(x3$v)
##' ## Array - scalar
##' x4 = number(matrix(3, 2, 2))
##' x5 = x4 - x1
##' print(x5)
##' print(x5$v)
##' ##Array - array
##' x6 = number(c(1, 1))
##' x7 = x4 - x6
##' print(x7)
##' print(x7$v)
##' modello.close()
##' @export
`-.modello_number` <- function (e1, e2) {
    stopifnot(is.number(e2))
    .modello$apply.math_op(.intrf.number__sub, e1, e2)
}
##' Multiplicates two \code{number}
##'
##' @title Number Multiplication 
##' @param e1 a reference object of class 'number'
##' @param e2 a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## Scalar * scalar
##' x1 = number(2, scalar=TRUE)
##' x2 = number(2, scalar=TRUE)
##' x3 = x1 * x2
##' print(x3)
##' print(x3$v)
##' ## Array * scalar
##' x4 = number(matrix(3, 2, 2))
##' x5 = x1 * x4
##' print(x5)
##' print(x5$v)
##' ##Array * array
##' x6 = number(c(2, 2))
##' x7 = x4 * x6
##' print(x7)
##' print(x7$v)
##' modello.close()
##' @export
`*.modello_number` <- function (e1, e2) {
    stopifnot(is.number(e2))
    .modello$apply.math_op(.intrf.number__mult, e1, e2)
}
##' Calculates the power of number by another.
##'
##' @title Number Power 
##' @param e1 a reference object of class 'number'
##' @param e2 a reference object of class 'number
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## Scalar ** scalar
##' x1 = number(2, scalar=TRUE)
##' x2 = number(2, scalar=TRUE)
##' x3 = x1 ** x2
##' print(x3)
##' print(x3$v)
##' ## Array ** scalar
##' x4 = number(matrix(2, 2, 2))
##' x5 = x1 ** x4
##' print(x5)
##' print(x5$v)
##' ##Array ** array
##' x6 = number(c(3, 3))
##' x7 = x4 ** x6
##' print(x7)
##' print(x7$v)
##' modello.close()
##' @export
'^.modello_number' <- function (e1, e2) {
    stopifnot(is.number(e2))
    .modello$apply.math_op(.intrf.number__pow, e1, e2)
}
##' @rdname pow-.modello_number
##' @export
`**.modello_number` <- function (e1, e2) {
    e1 ^ e2
}
##' Divides a number by another.
##'
##' @title Number Division 
##' @param e1 a reference object of class 'number'
##' @param e2 a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## Scalar / scalar
##' x1 = number(1, scalar=TRUE)
##' x2 = number(2, scalar=TRUE)
##' x3 = x1 / x2
##' print(x3)
##' print(x3$v)
##' ## Array / scalar
##' x4 = number(matrix(2, 2, 2))
##' x5 = x1 / x4
##' print(x5)
##' print(x5$v)
##' ##Array / array
##' x6 = number(c(2, 1))
##' x7 = x4 / x6
##' print(x7)
##' print(x7$v)
##' modello.close()
##' @export
`/.modello_number` <- function (e1, e2) {
    stopifnot(is.number(e2))
    .modello$apply.math_op(.intrf.number__div, e1, e2)
}
##' @rdname bin.entropy
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers
##' y = number(sample(c(1, 0), 10, replace=TRUE), dx=FALSE)
##' yh = number(runif(10))
##' h = bin.entropy(y, yh)
##' print(h)
##' print(h$v)
##' modello.close()
##' @export
bin.entropy.modello_number <- function (y, yh) {
    stopifnot(is.number(yh))
    .modello$apply.math_op(.intrf.number__bin_entropy, y, yh)
}
##' @rdname bin.entropy
##' @examples
##' ## For numerics
##' y = sample(c(1, 0), 10, replace=TRUE)
##' yh = runif(10)
##' h = bin.entropy(y, yh)
##' print(h)
##' @export
bin.entropy.default <- function (y, yh) {
    sum(-y * log(yh) - (1 - y) * log(1 - yh))
}
##' Entropy for binary target variables (Negative Bernoulli Log-Likelihood)
##' \deqn{h = \sum_i{-y_i * log(\hat y_i) - (1- y_i) * log(1 - \hat y_i)}}
##' 
##'
##' @title Binary Entropy 
##' @param yh target classes (0 or 1)
##' @param y predicted probability
##' @return Returns the entropy 
##' @author Filippo Monari
##' @export
bin.entropy <- function (y, yh) {
    UseMethod('bin.entropy', y)
}
##' @rdname cross.entropy
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers
##' y = number(matrix({x=sample(c(1, 0), 5, replace=TRUE); c(x, 1-x)}, 5, 2), dx=FALSE)
##' yh = number(t(apply(matrix(runif(10), 5, 2), 1, function(x)x/sum(x))))
##' h = cross.entropy(y, yh)
##' print(h)
##' print(h$v)
##' modello.close()
##' @export
cross.entropy.modello_number <- function (y, yh) {
    .modello$apply.math_op(.intrf.number__cross_entropy, y, yh)
}
##' @rdname cross.entropy
##' @examples
##' ## For numerics
##' y = matrix({x=sample(c(1, 0), 5, replace=TRUE); c(x, 1-x)}, 5, 2)
##' yh = t(apply(matrix(runif(10), 5, 2), 1, function(x)x/sum(x)))
##' h = cross.entropy(y, yh)
##' print(h)
##' @export
cross.entropy.default <- function (y, yh) {
    -sum(y * log(yh))
}
##' Cross-entropy (Negaitve Multinomial log-likelihood)
##' \deqn{\sum_i{y_i * log(\hat y_i)}}
##'
##' @title Cross-entropy 
##' @param yh target classes (0 or 1)
##' @param y predicted probability
##' @return Returns the entropy 
##' @author Filippo Monari
##' @export
cross.entropy <- function (y, yh) {
    UseMethod('cross.entropy', y)
}
##' @rdname mse
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers
##' y = number(rnorm(10), dx=FALSE)
##' yh = number(rnorm(10))
##' h = mse(y, yh)
##' print(h)
##' print(h$v)
##' modello.close()
##' @export
mse.modello_number <- function (y, yh) {
    stopifnot(is.number(yh))
    .modello$apply.math_op(.intrf.number__mse, y, yh)
}
##' @rdname mse
##' @examples
##' ## For numerics
##' y = rnorm(10)
##' yh = rnorm(10)
##' h = mse(y, yh)
##' print(h)
##' @export
mse.default <- function (y, yh) {
    mean((y - yh)**2)
}
##' Mean Squared Error between target values and predictions
##' \deqn{mse = \sum_i^N{(y_i - \hat y_i)^2} / N}
##'
##' @title Mean Squared Error 
##' @param y target values
##' @param yh predictions
##' @return Returns the mse
##' @author Filippo Monari
##' @export
mse <- function (y, yh) {
    UseMethod('mse', y)
}
##' @rdname mae
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers
##' y = number(rnorm(10), dx=FALSE)
##' yh = number(rnorm(10), dx=FALSE)
##' h = mae(y, yh)
##' print(h)
##' print(h$v)
##' modello.close()
##' @export
mae.modello_number <- function (y, yh) {
    stopifnot(is.number(yh))
    .modello$apply.math_op(.intrf.number__mae, y, yh)
}
##' @rdname mae
##' @export
##' @examples
##' ## For numerics
##' y = rnorm(10)
##' yh = rnorm(10)
##' h = mae(y, yh)
##' print(h)
mae.default <- function (y, yh) {
    mean(abs(y - yh))
}
##' Calculates the mean absolute error between target values and predictions
##' \deqn{mae = \sum_i^N{|y_i - \hat y_i|} / N}
##'
##' @title Mean Absolute Error 
##' @param y target values
##' @param yh predictions
##' @return Returns the mse
##' @author Filippo Monari
##' @export
mae <- function (y, yh) {
    UseMethod('mae', y)
}
##' Calculates the absolute value of a \code{number}
##'
##' @title Number Abs 
##' @param x a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For sclars
##' x1 = number(-3, scalar=TRUE)
##' y1 = abs(x1)
##' print(y1)
##' print(y1$v)
##' ## For arrays
##' x2 = number(matrix(-3, 3, 3))
##' y2 = abs(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
abs.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__abs, x)
}
##' Calculates the exponential of a \code{number}
##'
##' @title Number Exponential 
##' @param x a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For sclars
##' x1 = number(-3, scalar=TRUE)
##' y1 = exp(x1)
##' print(y1)
##' print(y1$v)
##' ## For arrays
##' x2 = number(matrix(-3, 3, 3))
##' y2 = exp(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
exp.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__exp, x)
}
##' Calculates the logarithm of a \code{number}
##'
##' @title Number Log 
##' @param x a reference object of class 'number'
##' @param base ignored, for compatibility with the generic function
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For sclars
##' x1 = number(3, scalar=TRUE)
##' y1 = log(x1)
##' print(y1)
##' print(y1$v)
##' ## For arrays
##' x2 = number(matrix(3, 3, 3))
##' y2 = log(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
log.modello_number <- function (x, base=NULL) {
    .modello$apply.math_op(.intrf.number__log, x)
}
##' Calculates the sine of a \code{number}
##'
##' @title Number Sin
##' @param x a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For sclars
##' x1 = number(pi, scalar=TRUE)
##' y1 = sin(x1)
##' print(y1)
##' print(y1$v)
##' ## For arrays
##' x2 = number(matrix(pi, 3, 3))
##' y2 = sin(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
sin.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__sin, x)
}
##' Calculates the cosine of a \code{number}
##'
##' @title Number Cosine
##' @param x a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For sclars
##' x1 = number(pi, scalar=TRUE)
##' y1 = cos(x1)
##' print(y1)
##' print(y1$v)
##' ## For arrays
##' x2 = number(matrix(pi, 3, 3))
##' y2 = cos(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
cos.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__cos, x)
}
##' Calculates the tangent of a \code{number}
##'
##' @title Number Tan
##' @param x a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For sclars
##' x1 = number(pi, scalar=TRUE)
##' y1 = tan(x1)
##' print(y1)
##' print(y1$v)
##' ## For arrays
##' x2 = number(matrix(pi, 3, 3))
##' y2 = tan(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
tan.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__tan, x)
}
##' Calculates the hyperbolic sine of a \code{number}
##'
##' @title Number SinH
##' @param x a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For sclars
##' x1 = number(pi, scalar=TRUE)
##' y1 = sinh(x1)
##' print(y1)
##' print(y1$v)
##' ## For arrays
##' x2 = number(matrix(pi, 3, 3))
##' y2 = sinh(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
sinh.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__sinh, x)
}
##' Calculates the hyperbolic cosine of a \code{number}
##'
##' @title Number CosH
##' @param x a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For sclars
##' x1 = number(pi, scalar=TRUE)
##' y1 = cosh(x1)
##' print(y1)
##' print(y1$v)
##' ## For arrays
##' x2 = number(matrix(pi, 3, 3))
##' y2 = cosh(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
cosh.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__cosh, x)
}
##' Calculates the hyperbolic tangent of a \code{number}
##'
##' @title Number TanH
##' @param x a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For sclars
##' x1 = number(pi, scalar=TRUE)
##' y1 = tanh(x1)
##' print(y1)
##' print(y1$v)
##' ## For arrays
##' x2 = number(matrix(pi, 3, 3))
##' y2 = tanh(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
tanh.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__tanh, x)
}
##' @rdname sigmoid
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers sclars
##' x1 = number(rnorm(1), scalar=TRUE)
##' y1 = sigmoid(x1)
##' print(y1)
##' print(y1$v)
##' ## For modello_number arrays 
##' x2 = number(matrix(rnorm(9), 3, 3))
##' y2 = sigmoid(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
sigmoid.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__sigmoid, x)
}
##' @rdname sigmoid
##' @examples
##' ## For numeric sclars
##' x1 = rnorm(1)
##' y1 = sigmoid(x1)
##' print(y1)
##' ## For numeric arrays
##' x2 = matrix(rnorm(9), 3, 3)
##' y2 = sigmoid(x2)
##' print(y2)
##' @export 
sigmoid.default <-  function (x) {
    1 / (1 + base::exp(-x))
}
##' Calculates the sigmoid
##' \deqn{sigmoid = 1 / (1 + exp(-x))}
##'
##' @title Sigmoid
##' @param x a 'numeric' or a reference object of class 'number'
##' @return Returns a 'numeric' or reference object of class 'number'
##' @author Filippo Monari
##' @export
sigmoid <- function (x) {
    UseMethod('sigmoid', x)
}
##' @rdname relu
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers sclars
##' x1 = number(rnorm(1), scalar=TRUE)
##' y1 = relu(x1)
##' print(y1)
##' print(y1$v)
##' ## For modello_number arrays 
##' x2 = number(matrix(rnorm(9), 3, 3))
##' y2 = relu(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
relu.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__relu, x)
}
##' @rdname relu
##' @examples
##' ## For numeric sclars
##' x1 = rnorm(1)
##' y1 = relu(x1)
##' print(y1)
##' ## For numeric arrays
##' x2 = matrix(rnorm(9), 3, 3)
##' y2 = relu(x2)
##' print(y2)
##' @export
relu.default <- function (x) {
    x[x < 0] = 0
    return(x)
}
##' Calculates the relu fucntion
##' \deqn{relu = max(0, x)}
##'
##' @title ReLU
##' @param x a 'numeric' or a reference object of class 'number'
##' @return Returns a 'numeric' or reference object of class 'number'
##' @author Filippo Monari
##' @export
relu <- function (x) {
    UseMethod('relu', x)
}
##' @rdname swish
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers sclars
##' x1 = number(rnorm(1), scalar=TRUE)
##' y1 = swish(x1)
##' print(y1)
##' print(y1$v)
##' ## For modello_number arrays 
##' x2 = number(matrix(rnorm(9), 3, 3))
##' y2 = swish(x2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
swish.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__swish, x)
}
##' @rdname swish
##' @examples
##' ## For numeric sclars
##' x1 = rnorm(1)
##' y1 = swish(x1)
##' print(y1)
##' ## For numeric arrays
##' x2 = matrix(rnorm(9), 3, 3)
##' y2 = swish(x2)
##' print(y2)
##' @export
swish.default <- function (x) {
    x * sigmoid(x)
}
##' Calculates the Swish function
##' \deqn{swish = x * sigmoid(x)}
##'
##' @title Number Swish
##' @param x a 'numeric' or a reference object of class 'number'
##' @return Returns a 'numeric' or reference object of class 'number'
##' @author Filippo Monari
##' @export
swish <- function(x) {
    UseMethod('swish', x)
}
##' @rdname softmax
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_number matrices
##' x = number(matrix(rnorm(9), 3, 3))
##' y1 = softmax(x)
##' print(y1)
##' print(y1$v)
##' y2 = softmax(x, 1)
##' print(y2)
##' print(y2$v)
##' y3 = softmax(x, 2)
##' print(y3)
##' print(y3$v)
##' modello.close()
##' @export
softmax.modello_number <- function (x, k=0) {
    .modello$apply.math_op(.intrf.number__softmax, x, k)
}
##' @rdname softmax
##' @examples
##' ## For numerics
##' x = rnorm(5)
##' y = softmax(x)
##' print(y)
##' @export
softmax.numeric <- function (x, k=0) {
    x = x - base::max(x) + .Machine$double.eps**0.5
    x = base::exp(x)
    x / base::sum(x)
}
##' @rdname softmax
##' @examples
##' ## For matrices
##' x = matrix(rnorm(9), 3, 3)
##' y1 = softmax(x)
##' print(y1)
##' y2 = softmax(x, 1)
##' print(y2)
##' y3 = softmax(x, 2)
##' print(y3)
##' @export
softmax.matrix <- function (x, k=0) {
    if (k == 0) {
        softmax.numeric(x)
    } else {
        ans = apply(x, k, softmax)
        if (k == 1) {
            t(ans)
        } else {
            ans
        }
    }
}
##' Calculates the Softmax function
##' \deqn{sofmax = \frac{exp(\bf x)}{\sum{exp(\bf x)}}}
##'
##' @title Number Softmax
##' @param x a 'numeric' or a reference object of class 'number'
##' @param k dimension index. Currently constained to be <= 2
##' @return Returns a 'numeric' or reference object of class 'number'
##' @author Filippo Monari
##' @export
softmax <- function (x, k) {
    UseMethod('softmax', x)
}
##' @rdname invMat
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_number matrices
##' x = number(diag(0.1, 3))
##' ix = invMat(x)
##' print(ix)
##' print(ix$v)
##' modello.close()
##' @export
invMat.modello_number <- function (A) {
    .modello$apply.math_op(.intrf.number__invMat, A)
    
}
##' @rdname invMat
##' @examples
##' ## For matrices
##' x = diag(0.1, 3)
##' ix = invMat(x)
##' print(ix)
##' @export
invMat.matrix <- function (A) {
    base::solve(A)
}
##' Calculates the inverse matrix
##'
##' @title Number Inverse Matrix
##' @param A a 'numeric' or a reference object of class 'number'
##' @return Returns a 'numeric' or reference object of class 'number'
##' @author Filippo Monari
##' @export
invMat <- function (A) {
    UseMethod('invMat', A)
}
##' Sum function
##'
##' @title Sum
##' @param x a reference object of class 'number'
##' @param k integer, dimension along wihich do the reduction. Currently constrained to be <= 2
##' @param ... numerics to sum. Ignored for \code{modello_numbers}
##' @return Returns a 'numeric' or reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' x = number(rep(1, 20))
##' y = sum(x)
##' print(y)
##' print(y$v)
##' modello.close()
##' @export
sum.modello_number <- function (x, k=0, ...) {
    .modello$apply.math_op(.intrf.number__sum, x, k)     
}
##' @rdname ssq
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers
##' x = number(rnorm(10))
##' y = ssq(x)
##' print(y)
##' print(y$v)
##' modello.close()
##' @export
ssq.modello_number <- function (x) {
    .modello$apply.math_op(.intrf.number__ssq, x)
}
##' @rdname ssq
##' @export
##' @examples
##' ## For numerics
##' x = rnorm(10)
##' y = ssq(x)
##' print(y)
ssq.default <- function (x) {
    base::sum(x**2)
}
##' Sum of square function
##' \deqn{ssq = \sum_i x_i^2}
##'
##' @title Sum of Squares
##' @param x a 'numeric' or a reference object of class 'number'
##' @return Returns a 'numeric' or reference object of class 'number'
##' @author Filippo Monari
##' @export
ssq <- function (x) {
    UseMethod('ssq', x)
}
##' @rdname ldexp
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers
##' y = number(rexp(10))
##' ld = ldexp(y, .k(1))
##' print(ld)
##' print(ld$v)
##' modello.close()
##' @export
ldexp.modello_number <- function (y, lam) {
    stopifnot(is.number(lam))
    .modello$apply.math_op(.intrf.number__ldexp, y, lam)
}
##' @rdname ldexp
##' @examples
##' ## For numerics
##' y = rexp(10)
##' ld = ldexp(y, 1)
##' print(ld)
##' @export
ldexp.default <- function (y, lam) {
    stats::dexp(y, lam, log=TRUE)
}
##' Considering an Exponential Distribution
##' calculates the log-density
##' \deqn{ld = log(\lambda) - \lambda * y; y \ge 0}
##'
##' @title Exponential Distribution: log-density 
##' @param y observations, numeric or reference object of class 'number'
##' @param lam rate parameter, numeric of reference object of class 'number'
##' @return Returns a 'numeric' or a reference object of class 'number'
##' @author Filippo Monari
##' @export
ldexp <- function (y, lam) {
    UseMethod('ldexp', y)
}
##' @rdname ldlaplace
##' @examples
##' modello.init(10, 10, 10, 10)
##' y = number(rnorm(10))
##' ld = ldlaplace(y, .k(0), .k(1))
##' print(ld)
##' print(ld$v)
##' modello.close()
##' @export
ldlaplace.modello_number <- function (y, mu, lam) {
    stopifnot(is.number(mu) && is.number(lam))
    .modello$apply.math_op(.intrf.number__ldlaplace, y, mu, lam)
}
##' Considering a Laplace Distribution
##' calculates the log-density
##' \deqn{ld = log(0.5) + log(\lambda) - \lambda * |y - \mu|}
##'
##' @title Laplace Distribution: log-density 
##' @param y observations, numeric or reference object of class 'number'
##' @param mu location parameter, numeric or reference object of class 'number'
##' @param lam rate parameter, numeric of reference object of class 'number'
##' @return Returns a 'numeric' or a reference object of class 'number'
##' @author Filippo Monari
##' @export
ldlaplace <- function (y, mu, lam) {
    UseMethod('ldlaplace', y)
}
##' @rdname ldbeta
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers
##' y = number(rbeta(10, 1, 2))
##' ld = ldbeta(y, .k(1), .k(2))
##' print(ld)
##' print(ld$v)
##' modello.close()
##' @export
ldbeta.modello_number <- function (y, a1, a2) {
    stopifnot(is.number(a1) && is.number(a2))
    .modello$apply.math_op(.intrf.number__ldbeta, y, a1, a2)
}
##' @rdname ldbeta
##' @examples
##' ## For numerics
##' y = rbeta(10, 1, 2)
##' ld = ldbeta(y, 1, 2)
##' print(ld) 
##' @export
ldbeta.default <-  function (y, a1, a2) {
    stats::dbeta(y, a1, a2, log=TRUE)
}
##' Considering a Beta Distribution calculates the log-density
##' \deqn{ld = (\alpha_1 - 1) * log(y) + (\alpha_2 - 1) * log(1 - y) - logBeta(\alpha_1, \alpha_2); y \in (0, 1)}
##'
##' @title Beta Distribution: log-density 
##' @param y observations, numeric or reference object of class 'number'
##' @param a1 shape parameter, numeric of reference object of class 'number'
##' @param a2 shape parameter, numeric of reference object of class 'number'
##' @return Returns a 'numeric' or a reference object of class 'number'
##' @author Filippo Monari
##' @export
ldbeta <- function (y, a1, a2) {
    UseMethod('ldbeta', y)
}
##' @rdname ldgamma
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers
##' y = number(rgamma(10, 5, 4))
##' ld = ldgamma(y, .k(5), .k(4))
##' print(ld)
##' print(ld$v)
##' modello.close()
##' @export
ldgamma.modello_number <- function (y, a, b) {
    stopifnot(is.number(a) && is.number(b))
    .modello$apply.math_op(.intrf.number__ldgamma, y, a, b)
}
##' @rdname ldgamma
##' @examples
##' ## For numerics
##' y = rgamma(10, 5, 4)
##' ld = ldgamma(y, 5, 4)
##' print(ld) 
##' @export
ldgamma.default <- function (y, a, b) {
    stats::dgamma(y, a, b, log=TRUE)
}
##' Considering a Gamma Distribution calculates the log-density.
##' \deqn{\alpha * log(\beta) - logGamma(\alpha) + (\alpha - 1) log(y) - \beta * y; y > 0}
##' 
##' @title Gamma Distribution: log-density 
##' @param y observations, numeric or reference object of class 'number'
##' @param a shape parameter, numeric of reference object of class 'number'
##' @param b rate parameter, numeric of reference object of class 'number'
##' @return Returns a 'numeric' or a reference object of class 'number'
##' @author Filippo Monari
##' @export
ldgamma <- function (y, a, b) {
    UseMethod('ldgamma', y)
}
##' @rdname ldnorm
##' @examples
##' modello.init(10, 10, 10, 10)
##' ## For modello_numbers
##' y = number(rnorm(10))
##' ld = ldnorm(y, .k(0), .k(1))
##' print(ld)
##' print(ld$v)
##' modello.close()
##' @export
ldnorm.modello_number <- function (y, mu, s) {
    stopifnot(is.number(mu) && is.number(s))
    .modello$apply.math_op(.intrf.number__ldnorm, y, mu, s)
}
##' @rdname ldnorm
##' @examples
##' ## For numerics
##' y = rnorm(10)
##' ld = ldnorm(y, 0, 1)
##' print(ld) 
##' @export
ldnorm.default <- function (y, mu, s) {
    stats::dnorm(y, mu, s, log=TRUE)
}
##' Considering a Normal Distribution calculates the log-density
##' \deqn{ld = -log(\sigma) - 0.5 * log(2 * \pi) - 0.5 * ((y - \mu) / \sigma)^2}
##'
##' @title Normal Distribution: log-density 
##' @param y observations, numeric or reference object of class 'number'
##' @param mu mean, numeric of reference object of class 'number'
##' @param s standard deviation, numeric of reference object of class 'number'
##' @return Returns a 'numeric' or a reference object of class 'number'
##' @author Filippo Monari
##' @export
ldnorm <- function (y, mu, s) {
    UseMethod('ldnorm', y)
}
##' Considering a Multivariate Normal Distribution calculates the log-density
##' \deqn{ld = -0.5 * (n * log(2 * \pi) * log(|\Sigma|) + y^T \Sigma y)}
##'
##' @title Multivariante Normal: log-density 
##' @param y observations, numeric or reference object of class 'number'
##' @param mu mean vector, numeric of reference object of class 'number'
##' @param E covariance matrix, numeric of reference object of class 'number'
##' @return Returns a 'numeric' or a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' y = number(rnorm(10))
##' mu = number(rep(0, 10))
##' E = number(diag(1, 10))
##' ld = ldmvnorm(y, mu, E)
##' print(ld)
##' print(ld$v)
##' modello.close()
##' @export
ldmvnorm <- function (y, mu, E) {
    stopifnot(is.number(mu) && is.number(E))
    .modello$apply.math_op(.intrf.number__ldmvnorm__1, y, mu, E)
}
##' Calculates the weighted Normal Likelihood for independent variables
##' \deqn{lkh = \sum_i{w_i * ldnorm(y_i, \mu_i, \sigma_i)}}
##'
##' @title Weighted Normal Likelihood for Independent Variables 
##' @param y observations, a reference object of class 'number'
##' @param mu mean, a refernce object of class 'number'
##' @param s standard deviation, a reference object of class 'number'
##' @param w weights, a reference object of class 'number'
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' y = number(rnorm(10), dx=FALSE)
##' mu = number(rep(0, 10))
##' s = number(rep(1, 10))
##' w = number(runif(10))
##' lkh = lkh.norm(y, mu, s, w)
##' print(lkh)
##' print(lkh$v)
##' modello.close()
##' @export
lkh.norm <- function (y, mu, s, w=NULL) {
    stopifnot(is.number(mu) && is.number(s))
    if (is.null(w)) {w = -1} else {stopifnot(is.number(w))}
    .modello$apply.math_op(.intrf.number__lkh_norm, y, mu, s, w)
}
##' Calculates:
##' ans = alpha * op(A).modello$apply.math_op(B) + beta * C
##' where alpha and beta are scalars and A, B, and C are matrices.
##'
##' @title Genral Matrix Multiplication
##' @param ta if > 1 or TRUE op(A) = A**T else op(A) = A
##' @param tb if > 1 or TRUE op(B) = A**T else op(B) = A
##' @param alpha a reference object of class 'number' with rank 0
##' @param A a reference object of class 'number' with rank 2
##' @param B a reference object of class 'number' with rank 2
##' @param beta a reference object of class 'number' with rank 0
##' @param C a reference number of class 'number' with rank 2
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' alpha = number(1, scalar=TRUE)
##' beta = number(1, scalar=TRUE)
##' A = number(matrix(rnorm(9), 3, 3))
##' B = number(matrix(rnorm(9), 3, 3))
##' C = number(matrix(rnorm(9), 3, 3))
##' ans1 = gemm(0, 0, alpha, A, B, beta, C)
##' ans2 = gemm(0, 0, alpha, A, B)
##' ans3 = gemm(0, 0, NULL, A, B, NULL, C)
##' ans4 = gemm(0, 0, NULL, A, B, NULL, NULL)
##' print(ans1)
##' print(ans1$v)
##' print(ans2)
##' print(ans2$v)
##' print(ans3)
##' print(ans3$v)
##' print(ans4)
##' print(ans4$v)
##' modello.close()
##' @export
gemm <- function (ta, tb, alpha=NULL, A, B, beta=NULL, C=NULL) {
    stopifnot(is.null(alpha) || is.number(alpha))
    stopifnot(is.number(A) && is.number(B))
    stopifnot(is.null(beta) || is.number(beta))
    stopifnot(is.null(C) || is.number(C))
    if (is.null(alpha) && is.null(beta) && is.null(C)) {
         .modello$apply.math_op(.intrf.number__dgemm2, ta, tb, A$id(), B$id())
     } else if (is.null(alpha) && is.null(beta)) {
         .modello$apply.math_op(.intrf.number__dgemm15, ta, tb, A$id(), B$id(), C$id())
     } else if (is.null(beta) && is.null(C)) {
         .modello$apply.math_op(.intrf.number__dgemm1, ta, tb, alpha$id(), A$id(), B$id())
     } else {
         .modello$apply.math_op(.intrf.number__dgemm0, ta, tb, alpha$id(), A$id(), B$id(), beta$id(), C$id())
     }
}
##' Calculates:
##' ans = alpha * op(A).B + beta * C
##' where alpha and beta are scalars, A is a matrix, and B and C are vectors.
##'
##' @title Genral Matrix-Vector Multiplication
##' @param ta if > 1 or TRUE op(A) = A**T else op(A) = A
##' @param alpha a reference object of class 'number' with rank 0
##' @param A a reference object of class 'number' with rank 2
##' @param B a reference object of class 'number' with rank 1
##' @param beta a reference object of class 'number' with rank 0
##' @param C a reference number of class 'number' with rank 1
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' alpha = number(1, scalar=TRUE)
##' beta = number(1, scalar=TRUE)
##' A = number(matrix(rnorm(9), 3, 3))
##' B = number(rnorm(3))
##' C = number(rnorm(3))
##' ans1 = gemv(0, alpha, A, B, beta, C)
##' ans2 = gemv(0, alpha, A, B)
##' ans3 = gemv(0, NULL, A, B, NULL, C)
##' ans4 = gemv(0, NULL, A, B, NULL, NULL)
##' print(ans1)
##' print(ans1$v)
##' print(ans2)
##' print(ans2$v)
##' print(ans3)
##' print(ans3$v)
##' print(ans4)
##' print(ans4$v)
##' modello.close()
##' @export
gemv <-  function (ta,  alpha=NULL, A, B, beta=NULL, C=NULL) {
    stopifnot(is.null(alpha) || is.number(alpha))
    stopifnot(is.number(A) && is.number(B))
    stopifnot(is.null(beta) || is.number(beta))
    stopifnot(is.null(C) || is.number(C))
    if (is.null(alpha) && is.null(beta) && is.null(C)) {
         .modello$apply.math_op(.intrf.number__dp_gemv__4, ta, A$id(), B$id())
     } else if (is.null(alpha) && is.null(beta)) {
         .modello$apply.math_op(.intrf.number__dp_gemv__3, ta, A$id(), B$id(), C$id())
     } else if (is.null(beta) && is.null(C)) {
         .modello$apply.math_op(.intrf.number__dp_gemv__2, ta, alpha$id(), A$id(), B$id())
     } else {
         .modello$apply.math_op(.intrf.number__dp_gemv__1, ta, alpha$id(), A$id(), B$id(), beta$id(), C$id())
     }
}
##' Calculates:
##' ans = alpha * A.B**T + C
##' where alpha is a scalar and A, B, are vectors and C is a matrix.
##'
##' @title Genral Vector Outer Product
##' @param alpha a reference object of class 'number' with rank 0
##' @param A a reference object of class 'number' with rank 1
##' @param B a reference object of class 'number' with rank 1
##' @param C a reference number of class 'number' with rank 2
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' alpha = number(1, scalar=TRUE)
##' A = number(rnorm(3))
##' B = number(rnorm(3))
##' C = number(matrix(rnorm(9), 3, 3))
##' ans1 = ger(alpha, A, B, C)
##' ans2 = ger(NULL, A, B, C)
##' ans3 = ger(NULL, A, B, NULL)
##' print(ans1)
##' print(ans1$v)
##' print(ans2)
##' print(ans2$v)
##' print(ans3)
##' print(ans3$v)
##' modello.close()
##' @export
ger <- function (alpha=NULL, A, B, C=NULL) {
    stopifnot(is.null(alpha) || is.number(alpha))
    stopifnot(is.number(A) && is.number(B))
    stopifnot(is.null(C) || is.number(C))
    if (is.null(alpha) && is.null(C)) {
        .modello$apply.math_op(.intrf.number__dp_ger__3, A$id(), B$id())
    } else if (is.null(alpha)) {
        .modello$apply.math_op(.intrf.number__dp_ger__2, A$id(), B$id(), C$id())
    } else {
        .modello$apply.math_op(.intrf.number__dp_ger__1, alpha$id(), A$id(), B$id(), C$id())
    }
}
##' Calculates:
##' ans = A.B 
##' where A and B are matrices
##'
##' @title Matrix - Matrix Multiplication
##' @param A a reference object of class 'number' with rank 2
##' @param B a reference object of class 'number' with rank 2
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' A = number(matrix(rnorm(9), 3, 3))
##' B = number(matrix(rnorm(9), 3, 3))
##' b = number(rnorm(3))
##' ans1 = A %.% B
##' ans2 = A %.% b
##' print(ans1)
##' print(ans1$v)
##' print(ans2)
##' print(ans2$v)
##' modello.close()
##' @export
`%.%` <- function (A, B) {
    if (B$rank() == 1) {
        .modello$apply.math_op(.intrf.number__dp_gemv__4, 0, A$id(), B$id())
    } else {
        .modello$apply.math_op(.intrf.number__dgemm2, 0, 0, A$id(), B$id())
    }
}
##' Calculates:
##' ans = A**T.B 
##' where A and B are matrices
##'
##' @title Matrix Transpose - Matrix Multiplication
##' @param A a reference object of class 'number' with rank 2
##' @param B a reference object of class 'number' with rank 2
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' A = number(matrix(rnorm(9), 3, 3))
##' B = number(matrix(rnorm(9), 3, 3))
##' b = number(rnorm(3))
##' ans1 = A %T.% B
##' ans2 = b %T.% A
##' print(ans1)
##' print(ans1$v)
##' print(ans2)
##' print(ans2$v)
##' modello.close()
##' @export
`%T.%` <- function (A, B) {
    if (A$rank() == 1 && B$rank() == 1) {
        .modello$apply.math_op(.intrf.number__dp_dot, A, B)
    } else if (A$rank() == 1 && B$rank() == 2) {
        .modello$apply.math_op(.intrf.number__dp_gemv__4, 1, B$id(), A$id())#gemv(ta=1, A=B, B=A)
    } else if (A$rank() == 2 && B$rank() == 1) {
        .modello$apply.math_op(.intrf.number__dp_gemv__4, 1, A$id(), B$id())#gemv(ta=1, A=A, B=B)
    } else {
        .modello$apply.math_op(.intrf.number__dgemm2, 1, 0, A$id(), B$id())#gemm(ta=1, tb=0, A=A, B=B)
    }
}
##' Calculates:
##' ans = A.B**T 
##' where A and B are matrices
##'
##' @title Matrix - Matrix Traspose Multiplication
##' @param A a reference object of class 'number' with rank 2
##' @param B a reference object of class 'number' with rank 2
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' A = number(matrix(rnorm(9), 3, 3))
##' B = number(matrix(rnorm(9), 3, 3))
##' b = number(matrix(rnorm(3), 1, 3))
##' ans1 = A %.T% B
##' ans2 = A %.T% b
##' print(ans1)
##' print(ans1$v)
##' print(ans2)
##' print(ans2$v)
##' modello.close()
##' @export
`%.T%` <- function (A, B) {
    if (A$rank() == 1 && B$rank() == 1) {
        .modello$apply.math_op(.intrf.number__dp_ger__3, A$id(), B$id())#ger(A=A, B=B)
    } else {
        .modello$apply.math_op(.intrf.number__dgemm2, 0, 1, A$id(), B$id())#gemm(ta=0, tb=1, A=A, B=B)
    }
}
##' Calculates:
##' ans = A**T.B**T 
##' where A and B are matrices
##'
##' @title Matrix Traspose - Matrix Traspose Multiplication
##' @param A a reference object of class 'number' with rank 2
##' @param B a reference object of class 'number' with rank 2
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' A = number(matrix(rnorm(9), 3, 3))
##' B = number(matrix(rnorm(9), 3, 3))
##' ans = A %T.T% B
##' print(ans)
##' print(ans$v)
##' modello.close()
##' @export
`%T.T%` <- function (A, B) {
    .modello$apply.math_op(.intrf.number__dgemm2, 1, 1, A$id(), B$id())#gemm(ta=1, tb=1, A=A, B=B)
}
