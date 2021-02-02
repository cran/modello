##' R6 class representing a module
##'
##' A module is a  set of graph operation combined together.
##' @author Filippo Monari
##' @export
module = R6Class(
    'module',
    public = list(
        ##' @description
        ##' It just sets the name of the module
        ##'
        ##' @param ... initialisation arguments
        initialize = function (...) {

        },
        ##' @description
        ##' Returns the name of the module
        ##'
        ##' @return Returns the name of the module
        name = function () {
            private$.name
        },
        ##' @description
        ##' Saves the module in RDS format
        ##'
        ##' @param of out file name
        ##' @param ... additional arguments to pass to \code{saveRDS}
        save = function (of, ...) {
            pars = lapply(self$pars(), function(x)x$collect())
            obj = list(
                'module' = self,
                'pars' = pars
            )
            saveRDS(obj, file=of, ...)
        },
        ##' @description
        ##' The operator method implements the calculations happeing in the module.
        ##' This method must be implemented for each object inheriting the class 'module'.
        ##'
        ##' @param ... arguments for the calculations
        ##' @return Returns a reference object of class 'number'
        op = function (...) {
            stop("to implement - has to contain the operations in the module.")
        },
        ##' @description
        ##' In the case the module represents a full model, the objective method must be
        ##' implemented.
        ##' This method method implements the calculations returning the value of the
        ##' objective function that has to be optimised during the model training.
        ##' Its first argument must the targets, and its second argument must be
        ##' the input matrix.
        ##'
        ##' @param ... arguments for the calculations
        ##' @return Returns a reference object of class 'number' of rank 0
        obj = function (...) {
            stop("to implement - has to contain the objective that the module is optimising.")
        },
        ##' @description
        ##' The pars method has to return a flat list containing all the parameters of
        ##' the module (reference object of class 'number').
        ##' This method must be implemented for each object inheriting the class 'module'.
        ##'
        ##' @return Returns a flat list containing the module parameters 
        ##' (reference object of class 'number')
        pars = function () {
            stop("to implement - has to return the parameters of the module.")
        },
        ##' @description
        ##' This method return the total size of the module parameters,
        ##' that is the sum of the sizes of the individual \code{numbers}
        ##' parameters.
        ##'
        ##' @return Returns the total number of parameters
        npars = function () {
            sum(sapply(self$pars(), length))
        },
        ##' @description
        ##' Returns the reference object of class 'number' containing the output of the
        ##' operator method.
        y = function () {
            private$.y
        },
        ##' @description
        ##' Returns the reference object of class 'number' containing the output of the
        ##' objective method.
        j = function () {
            private$.j
        }
    ),
    private = list(
        .y = NULL,
        .j = NULL
    )
)
##' R6 class representing a linear model.
##'
##' @author Filippo Monari
##' @export
module.lm = R6Class(
    'module.lm',
    inherit = module,
    public = list(
        ##' @description
        ##' The initialisation method sets the weights (W), the bais (B) matrices,
        ##' the objective function (obj), as well as the name of the module.
        ##' The calculation performed is the following
        ##' ans = W.op(X) + B
        ##'
        ##' @param tx if TRUE it traspose the input matrix
        ##' @param nin number of column of the input matrix
        ##' @param nout number of outputs
        ##' @param obj objective function to adopt
        ##' @param b if TRUE the intercept term is included
        ##' @return Returns the total number of parameters
        initialize = function (tx, nin, nout, obj = mse, b=TRUE) {
            private$.tx = tx
            private$.W = number(matrix(rnorm(nin*nout, 0, 0.01), nout, nin))
            if (b) private$.B = number(rnorm(nout, 0, 0.01))
            private$.obj = obj
        },
        ##' @description
        ##' Performs: ans = W.op(X) + B
        ##'
        ##' @param X input matrix, reference object of class 'number'
        ##' @return Returns a reference object of class 'number'
        ##' @examples
        ##' \donttest{
        ##' modello.init(10, 10, 10, 10)
        ##' X = number(as.matrix(rnorm(10)), dx=FALSE)
        ##' mdl = module.lm$new(1, 1, 1, b=FALSE)
        ##' print(X$v)
        ##' Yh = mdl$op(X)
        ##' print(Yh)
        ##' print(Yh$v)
        ##' modello.close()
        ##' }
        op = function (X) {
            if (is.null(private$.B)) {
                private$.y = gemm(ta=0, tb=private$.tx, A=private$.W, B=X)
            } else {
                private$.y = gemm(ta=0, tb=private$.tx, A=private$.W, B=X) + private$.B
            }
            invisible(private$.y)
        },
        ##' @description
        ##' Calculates the objective function.
        ##'
        ##' @param X input matrix, reference object of class 'number'
        ##' @param y target values, reference object of class 'number'
        ##' @return Returns a reference object of class 'number'
        ##' @examples
        ##' \donttest{
        ##' modello.init(10, 10, 10, 10)
        ##' X = number(as.matrix(rnorm(10)), dx=FALSE)
        ##' y = number(as.matrix(rnorm(10)), dx=FALSE)
        ##' mdl = module.lm$new(1, 1, 1, b=FALSE)
        ##' g = graph.open()
        ##' J = mdl$obj(y, X)
        ##' graph.close()
        ##' J$dv = 1
        ##' g$bw()
        ##' print(lapply(mdl$pars(), function(x)x$dv))
        ##' modello.close()
        ##' }
        obj = function (y, X) {
            private$.j = private$.obj(y, self$op(X))
            invisible(private$.j)
        },
        ##' @description
        ##' Returns the parameters of the module as list with entries:
        ##' W = weight matrix, and B = bias matrix
        ##'
        ##' @return Returns a flat list with the parametrs of the module
        ##' @examples
        ##' \donttest{
        ##' modello.init(10, 10, 10, 10)
        ##' mdl = module.lm$new(1, 1, 1, b=FALSE)
        ##' print(mdl$pars())
        ##' print(lapply(mdl$pars(), function(x)x$v))
        ##' print(lapply(mdl$pars(), function(x)x$dv))
        ##' modello.close()
        ##' }
        pars = function () {
            ANS = list('W'=private$.W)
            if (!is.null(private$.B)) ANS[['B']] = private$.B
            return(ANS)
        }        
    ),
    private = list(
        .W = NULL,
        .B = NULL,
        .obj = NULL,
        .tx = NULL
    )
)
##' R6 class representing a logistic regression model.
##'
##' @author Filippo Monari
##' @export
module.logistic = R6Class(
    'module.logistic',
    inherit = module.lm,
    public = list(
        ##' @description
        ##' The initialisation method sets the weights (W), the bais (B) matrices,
        ##' the objective function (obj), as well as the name of the module.
        ##' The calculation performed is the following
        ##' ans = sigmoid(W.op(X) + B)
        ##'
        ##' @param tx if TRUE it traspose the input matrix
        ##' @param nin number of column of the input matrix
        ##' @param b if TRUE the intercept term is included
        ##' @return Returns the total number of parameters
        initialize = function (tx, nin, b=TRUE) {
            super$initialize(tx, nin, 1, bin.entropy, b)
        },
        ##' @description
        ##' Performs: ans = sigmoid(W.op(X) + B)
        ##'
        ##' @param X input matrix, reference object of class 'number'
        ##' @return Returns a reference object of class 'number'
        ##' @examples
        ##' \donttest{
        ##' modello.init(10, 10, 10, 10)
        ##' X = number(as.matrix(rnorm(10)), dx=FALSE)
        ##' mdl = module.logistic$new(1, 1, b=FALSE)
        ##' print(X$v)
        ##' Yh = mdl$op(X)
        ##' print(Yh)
        ##' print(Yh$v)
        ##' modello.close()
        ##' }
        op = function (X) {
            private$.y = sigmoid(super$op(X))
            invisible(private$.y)
        }
    ),
    private = list(
        .W = NULL,
        .B = NULL,
        .obj = NULL,
        .tx = NULL
    )
)
##' R6 class representing a softmax regression model.
##'
##' @author Filippo Monari
##' @export
module.softmax = R6Class(
    'module.softmax',
    inherit = module.lm,
    public = list(
        ##' @description
        ##' The initialisation method sets the weights (W), the bais (B) matrices,
        ##' the objective function (obj), as well as the name of the module.
        ##' The calculation performed is the following
        ##' ans = softmax(W.op(X) + B)
        ##'
        ##' @param tx if TRUE it traspose the input matrix
        ##' @param nin number of column of the input matrix
        ##' @param nout number of outputs
        ##' @param b if TRUE the intercept term is included
        ##' @return Returns the total number of parameters
        initialize = function (tx, nin, nout, b=TRUE) {
            stopifnot(nout > 1)
            super$initialize(tx, nin, nout, cross.entropy, b)
        },
        ##' @description
        ##' Performs: ans = sigmoid(W.op(X) + B)
        ##'
        ##' @param X input matrix, reference object of class 'number'
        ##' @return Returns a reference object of class 'number'
        ##' @examples
        ##' \donttest{
        ##' modello.init(10, 10, 10, 10)
        ##' X = number(matrix(rnorm(12), 6), dx=FALSE)
        ##' mdl = module.softmax$new(1, 2, 3, b=FALSE)
        ##' print(X$v)
        ##' Yh = mdl$op(X)
        ##' print(Yh)
        ##' print(Yh$v)
        ##' modello.close()
        ##' }
        op = function (X) {
            private$.y = softmax(super$op(X))
            invisible(private$.y)
        }
    ),
    private = list(
        .W = NULL,
        .B = NULL,
        .obj = NULL,
        .tx = NULL
    )
)
##' R6 class representing a fully connected layer.
##'
##' @author Filippo Monari
##' @export
module.fc = R6Class(
    'module.fc',
    inherit = module,
    public = list(
        ##' @description
        ##' The initialisation method sets the weights (W), the bais (B) matrices,
        ##' the activation function (act), as well as the name of the module.
        ##' The calculation performed is the following
        ##' ans = act(W.op(X) + B)
        ##'
        ##' @param tx if TRUE it traspose the input matrix
        ##' @param nin number of column of the input matrix
        ##' @param nout number of hidden units
        ##' @param act activation function to adopt
        ##' @return Returns the total number of parameters
        initialize = function (tx, nin, nout, act) {
            private$.tx = tx
            l = sqrt(nin**-1)
            private$.W = number(matrix(runif(nin*nout, -l, l), nout, nin))
            private$.B = number(runif(nout, -l, l))
            private$.act = act
        },
        ##' @description
        ##' Performs: ans = act(W.op(X) + B)
        ##'
        ##' @param X input matrix, reference object of class 'number'
        ##' @return Returns a reference object of class 'number'
        op = function (X) {
            if (is.null(private$.act)) {
                private$.y = gemm(ta=0, tb=private$.tx, A=private$.W, B=X) + private$.B
            } else {
                private$.y = private$.act(gemm(ta=0, tb=private$.tx, A=private$.W, B=X) + private$.B)
            }                
            invisible(private$.y)
        },
        ##' @description
        ##' Returns the parameters of the module as list with entries:
        ##' W = weight matrix, and B = bias matrix
        ##'
        ##' @return Returns a flat list with the parametrs of the module
        pars = function () list('W'=private$.W, 'B'=private$.B) 
    ),
    private = list(
        .W = NULL,
        .B = NULL,
        .act = NULL,
        .tx = NULL
    )
)

##' R6 class representing the recurrent layer of a Elmam or Jordan
##' recurrent neural network
##'
##' @author Filippo Monari
##' @export
module.RecUnit = R6Class(
    'module.RecUnit',
    inherit = module,
    public = list(
        ##' @description
        ##' Initialisation method
        ##'
        ##' @param tx transposition flag. If > 0 op(x) = t(x) 
        ##' @param nh number of input from the previous time steps
        ##' @param nx number of input form the current time step
        ##' @param act activation function
        initialize = function (tx, nh, nx, act) {
            private$.tx = tx
            lh = sqrt(nh**-1)
            private$.Wh = number(matrix(runif(nh*nh, -lh, lh), nh, nh))
            private$.Wx = number(matrix(runif(nh*nx, -lh, lh), nh, nx))
            private$.B = number(runif(nh, -lh, lh))
            private$.act = act
        },
        ##' @description
        ##' Performs:
        ##' act(Wx . op(x) + Wh . h + B)
        ##'
        ##' @param h \code{number} input from previous time steps
        ##' @param x \code{number} input from the current timestep
        op = function (h, x) {
            fc = gemm(ta=0, tb=private$.tx, A=private$.Wx, B=x)
            rec = gemm(ta=0, tb=0, A=private$.Wh, B=h)
            private$.act(fc + rec + private$.B)
        },
        ##' @description
        ##' Returns the parameters of the module as list with entries:
        ##' W0 = weight matrix for past inputs,
        ##' W = weight matrix for current input,
        ##' and B = bias matrix
        ##'
        ##' @return Returns a flat list with the parametrs of the module
        pars = function () list('Wh'=private$.Wh, 'Wx'=private$.Wx,
                                'B'=private$.B)
    ),
    private = list(
        .tx = NULL,
        .Wh = NULL,
        .Wx = NULL,
        .B = NULL,
        .act = NULL
    )
)

##' R6 class representing the recurrent layer of a
##' LSTM network
##'
##' @author Filippo Monari
##' @export
module.LstmUnit = R6Class(
    "module.LstmUnit",
    inherit = module,
    public = list(
        ##' @description
        ##' Initialisation method
        ##'
        ##' @param tx transposition flag. If > 0 op(x) = t(x) 
        ##' @param nh number of input from the previous time steps
        ##' @param nx number of input form the current time step
        ##' @param act activation function
        initialize = function (tx, nh, nx, act) {
            private$.tx = tx
            lh = sqrt(nh**-1)
            private$.Wh.i = number(matrix(runif(nh*nh, -lh, lh), nh, nh))
            private$.Wx.i = number(matrix(runif(nh*nx, -lh, lh), nh, nx))
            private$.B.i = number(runif(nh, -lh, lh))

            private$.Wh.f = number(matrix(runif(nh*nh, -lh, lh), nh, nh))
            private$.Wx.f = number(matrix(runif(nh*nx, -lh, lh), nh, nx))
            private$.B.f = number(runif(nh, -lh, lh))

            private$.Wh.g = number(matrix(runif(nh*nh, -lh, lh), nh, nh))
            private$.Wx.g = number(matrix(runif(nh*nx, -lh, lh), nh, nx))
            private$.B.g = number(runif(nh, -lh, lh))

            private$.Wh.o = number(matrix(runif(nh*nh, -lh, lh), nh, nh))
            private$.Wx.o = number(matrix(runif(nh*nx, -lh, lh), nh, nx))
            private$.B.o = number(runif(nh, -lh, lh))
            
            private$.act = act
        },
        ##' @description
        ##' Performs:
        ##' input_gate = Wxi . op(x) + Whi . h + Bi
        ##' forget_gate = Wxf . op(x) + Whf . h + Bf
        ##' output_gate = Wxo . op(x) + Who . h + Bo
        ##' current_cell = act(Wxg . op(x) + Whg . h + Bg)
        ##' cell_sate = forget_gate * cell_previous + input_gate * cell_current
        ##' hidden_state = output_gate * act(cell_state)
        ##' Returns a list composed by hidde_state and cell_state
        ##'
        ##' @param h past hidden state 
        ##' @param g past cell state
        ##' @param x current input
        op = function (h, g, x) {
            i = sigmoid(gemm(ta=0, tb=private$.tx, A=private$.Wx.i, B=x, C=(private$.Wh.i %.% h)) + private$.B.i)
            f = sigmoid(gemm(ta=0, tb=private$.tx, A=private$.Wx.f, B=x, C=(private$.Wh.f %.% h)) + private$.B.f)
            o = sigmoid(gemm(ta=0, tb=private$.tx, A=private$.Wx.o, B=x, C=(private$.Wh.o %.% h)) + private$.B.o)
            g1 = private$.act(gemm(ta=0, tb=private$.tx, A=private$.Wx.g, B=x, C=(private$.Wh.g %.% h)) + private$.B.g)
            g2 = f * g + i * g1
            h1 = o * private$.act(g2)
            list(h1, g2)
        },
        ##' @description
        ##' Returns a list with the model parameters
        pars = function () {
            list(private$.Wh.i,
                 private$.Wx.i,
                 private$.B.i,
                 private$.Wh.f,
                 private$.Wx.f,
                 private$.B.f,
                 private$.Wh.g,
                 private$.Wx.g,
                 private$.B.g,
                 private$.Wh.o,
                 private$.Wx.o,
                 private$.B.o)
        }
    ),
    private = list(
        .tx = NULL,
        .act = NULL,
        .Wh.i = NULL,
        .Wx.i = NULL,
        .B.i = NULL,
        .Wh.f = NULL,
        .Wx.f = NULL,
        .B.f = NULL,
        .Wh.g = NULL,
        .Wx.g = NULL,
        .B.g = NULL,
        .Wh.o = NULL,
        .Wx.o = NULL,
        .B.o = NULL
    )
)
        

##' R6 class representing an RNN.
##'
##' @author Filippo Monari
##' @export
module.RNN = R6Class(
    "module.RNN",
    inherit = module,
    public = list(
        ##' @description
        ##' Initialisation method
        ##'
        ##' @param tx transposition flag. If > 0 op(x) = t(x) 
        ##' @param nh integer vector of length equal to the hidden layes indicating the number of
        ##' hidden units for each layer
        ##' @param nx integer, number if inputs
        ##' @param acth activation function for the recurrent units
        ##' @param ny integer, number of outputs of the last fully connected layer
        ##' @param acty activation function for the last fully connected layer
        ##' @param par.h logical, true => the initial hidden state is treated as a paramter of the optimisation
        initialize = function (tx, nh, nx, acth, ny, acty, par.h=FALSE) {
            nx = c(nx, nh[-length(nh)])
            tx = c(tx, rep(0, length(nh)-1))
            private$.rus = lapply(1:length(nh), function (i) {
                module.RecUnit$new(tx[i], nh[i], nx[i], acth)
            })
            private$.fc = module.fc$new(0, nh[length(nh)], ny, acty)
            private$.H = lapply(nh, function(n)number(matrix(0, n, 1), dx=par.h))
            private$.par.h = par.h
        },
        ##' @description
        ##' Save the module to an RDS file
        ##'
        ##' @param of output file name or path
        ##' @param ... additional arguments to be passed to the function \code{base::saveRDS} 
        save = function (of, ...) {
            if (private$.par.h) {
                super$save(of, ...)
            } else {
                pars = lapply(c(H=private$.H, self$pars()), function(x)x$collect())
                obj = list(
                    'module' = self,
                    'pars' = pars
                )
                base::saveRDS(obj, file=of, ...)
            }
        },
        ##' @description
        ##' Runs the calculation stored in the module
        ##'
        ##' @param X 'number', module inputs
        op = function (X) {
            h = private$.recur.outer(private$.H, X, private$.rus)
            private$.fc$op(h[[length(h)]])
        },
        ##' @description
        ##' Returns a list with the model parameters
        pars = function () {
            ANS = c(
                unlist(lapply(private$.rus, function(x)x$pars())),
                private$.fc$pars()
            )
            if (private$.par.h) {
                return(c(private$.H, ANS))
            } else {
                return(ANS)
            }
        },
        ##' @description
        ##' Returns a list with the recurrent units
        rus = function() private$.rus,
        ##' @description
        ##' Returns the initial hidden state
        H = function() private$.H,
        ##' @description
        ##' Returns the final fully connected layer
        fc = function() private$.fc
    ),
    private = list(
        .H = NULL,
        .rus = NULL,
        .fc = NULL,
        .par.h = NULL,
        .recur.inner = function (h, x, rus, hnew) {
            if (length(rus) == 0) {
                hnew
            } else {
                x = rus[[1]]$op(h[[1]], x)
                hnew = c(hnew, x)
                private$.recur.inner(h[-1], x, rus[-1], hnew)
            }
        },
        .recur.outer = function (h, X, rus) {
            if (length(X) == 0) {
                h
            } else {
                h = private$.recur.inner(h, X[[1]], rus, list())
                private$.recur.outer(h, X[-1], rus)
            }
        }
    )
)


##' R6 class representing an LSTM.
##'
##' @author Filippo Monari
##' @export
module.LSTM = R6Class(
    "module.LSTM",
    inherit = module,
    public = list(
        ##' @description
        ##' Initialisation method
        ##'
        ##' @param tx transposition flag. If > 0 op(x) = t(x) 
        ##' @param nh integer vector of length equal to the hidden layes indicating the number of
        ##' hidden units for each layer
        ##' @param nx integer, number if inputs
        ##' @param acth activation function for the recurrent units
        ##' @param ny integer, number of outputs of the last fully connected layer
        ##' @param acty activation function for the last fully connected layer
        ##' @param par.h logical, true => the initial hidden state is treated as a paramter of the optimisation
        ##' @param par.g logical, true => the initial cell state is treated as a paramter of the optimisation
        initialize = function (tx, nh, nx, acth, ny, acty, par.h=FALSE, par.g=FALSE) {
            nx = c(nx, nh[-length(nh)])
            tx = c(tx, rep(0, length(nh) - 1))
            private$.rus = lapply(1:length(nh), function(i) {
                module.LstmUnit$new(tx[i], nh[i], nx[i], acth)
            })
            private$.fc = module.fc$new(0, nh[length(nh)], ny, acty)
            H = lapply(nh, function(n)number(matrix(0, n, 1), dx=par.h))
            private$.par.h = par.h
            G = lapply(nh, function(n)number(matrix(0, n, 1), dx=par.h))
            private$.par.g = par.g
            private$.HG = lapply(1:length(nh), function(i)list(H[[i]], G[[i]]))
        },
        ##' @description
        ##' Save the module to an RDS file
        ##'
        ##' @param of output file name or path
        ##' @param ... additional arguments to be passed to the function \code{base::saveRDS} 
        save = function (of, ...) {
            pars = c(
                unlist(private$.HG),
                unlist(lapply(private$.rus, function(x)x$pars())),
                private$.fc$pars()
            )
            pars = lapply(pars, function(x)x$collect())
            obj = list(
                module = self,
                pars = pars
            )
            base::saveRDS(obj, file=of, ...)
        },
        ##' @description
        ##' Runs the calculation stored in the module
        ##'
        ##' @param X 'number', module inputs
        op = function (X) {
            hg = private$.recur.outer(private$.HG, X, private$.rus)
            private$.fc$op(hg[[length(hg)]][[1]])
        },
        ##' @description
        ##' Returns a list with the model parameters
        pars = function () {
            ANS = c(
                unlist(lapply(private$.rus, function(x)x$pars())),
                private$.fc$pars()
            )
            if (private$.par.g) ANS = c(private$.G, ANS)
            if (private$.par.h) ANS = c(private$.H, ANS)
            ANS
        },
        ##' @description
        ##' Returns a list with the recurrent units
        rus = function() private$.rus,
        ##' @description
        ##' Returns the initial hidden state
        H = function() private$.H,
        ##' @description
        ##' Returns the final fully connected layer
        fc = function() private$.fc
    ),
    private = list(
        .HG = NULL,
        .rus = NULL,
        .fc = NULL,
        .par.h = NULL,
        .par.g = NULL,
        .recur.inner = function (hg, x, rus, hgnew) {
            if (length(rus) == 0) {
                hgnew
            } else {
                x  = rus[[1]]$op(hg[[1]][[1]], hg[[1]][[2]], x)
                hgnew[[length(hgnew) + 1]] = x
                private$.recur.inner(hg[-1], x[[1]], rus[-1], hgnew)
            }
        },
        .recur.outer = function (hg, X, rus) {
            if (length(X) == 0) {
                hg
            } else {
                hg = private$.recur.inner(hg, X[[1]], rus, list())
                private$.recur.outer(hg, X[-1], rus)
            }
        }
    )
)
        

##' Load a module from and RDS file.
##'
##' @title Load Module from RDS 
##' @param inf input file name
##' @param ... additional arguments to be passed to \code{readRDS}
##' @return Returns the saved module
##' @author Filippo Monari
##' @export
module_from_RDS <- function (inf, ...) {
    obj = readRDS(inf, ...)
    mdl = obj$module
    pars = lapply(obj$pars, number_from_list)
    return(mdl)
}



    


    


        
        
        
            
