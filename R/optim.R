##' Root R6 class representing a generic \code{optmiser}
##'
##' Object of this class are created by the '.modello'
##' session object that linkes them with the corresponding
##' \code{optimiser} in the FORTRAN environment.
##' @author Filippo Monari
.opt = R6Class(
    '.opt',
    public = list(
        ##' @description
        ##' Initialise the reference object of class 'opt'
        ##'
        ##' @param name \code{number} name
        initialize = function (name) {
            private$.name = name
        },
        ##' @description
        ##' Awares of a reference object associated
        ##' to an existing \code{number} is removed 
        finalize = function () {
            if (self$is.linked()) warning("the optmiser is linked")
        },
        ##' @description
        ##' Returns the name of the \code{optimiser}.
        ##'
        ##' @return Returns the name of the \code{optmiser}
        name = function () {
            private$.name
        },
        ##' @description
        ##' Returns the id of the \code{optmiser}
        ##' (i.e. its position index in the \code{OPTS_} array).
        ##'
        ##' @return Returns the id of the optmiser
        id = function () {
            .modello$opt.id(self)
        },
        ##' @description
        ##' Pop (removes) the \code{optmiser} from the
        ##' \code{OPTS_} array.
        ##'
        ##' @return Returns invisible self
        pop = function () {
            .modello$pop.opt(self)
        },
        ##' @description
        ##' Checks that the reference object is linked to
        ##' a \code{optmiser}
        ##'
        ##' @return Retursn TRUE if is linked, FALSE otherwise
        is.linked = function () {
            .modello$opt.exists(self)
        },
        ##' @description
        ##' Prints a representation of the \code{optmiser}
        print = function () {
            if (self$is.linked()) {
                repr = paste0(self$name(), ' => <OPTS_(', self$id(), ')>')
            } else {
                repr = paste0(self$name(), ' => <NULL>')
            }
            cat(repr, sep='\n')
        }
    ),
    private = list(
        .name = NULL
    )
)

##' R6 class representing a SGD \code{optmiser}
##'
##' @rdname dot-opt
.sgd.opt = R6Class(
    'sgd.opt',
    inherit = .opt,
    public = list(
        ##' @description
        ##' Performs \code{niter} SGD steps 
        ##'
        ##' @param g reference object of class 'graph'
        ##' containing the computational graph of the objective function
        ##' @param lr learning rate
        ##' @param j refernence object of class 'number'
        ##' represeting the output of the objective function
        ##' @param niter number of steps
        step = function (g, j, lr, niter) {
            .intrf.sgd__step(self$id(), g$id(), j$id(), lr, niter)
        }
    )
)

##' R6 class representing a SGDWM \code{optmiser}
##'
##' @rdname dot-opt
.sgdwm.opt = R6Class(
    'sgdwm.opt',
    inherit = .opt,
    public = list(
        ##' @description
        ##' Performs \code{niter} SGDWM steps 
        ##'
        ##' @param g reference object of class 'graph'
        ##' containing the computational graph of the objective function
        ##' @param lr learning rate
        ##' @param alpha momentum parameter
        ##' @param j refernence object of class 'number'
        ##' represeting the output of the objective function
        ##' @param niter number of steps
        step = function (g, j,  lr, alpha, niter) {
            .intrf.sgdwm__step(self$id(), g$id(), j$id(), lr, alpha, niter)
        }
    )
)

##' R6 class representing an Adam \code{optmiser}
##'
##' @rdname dot-opt
.adam.opt = R6Class(
    'adam.opt',
    inherit = .opt,
    public = list(
        ##' @description
        ##' Performs \code{niter} Adam steps 
        ##'
        ##' @param g reference object of class 'graph'
        ##' containing the computational graph of the objective function
        ##' @param lr learning rate
        ##' @param beta1 first order momentum parameter
        ##' @param beta2 second order momentum parameter
        ##' @param j refernence object of class 'number'
        ##' represeting the output of the objective function
        ##' @param niter number of steps
        step = function (g, j, lr, beta1, beta2, niter) {
            .intrf.adam__step(self$id(), g$id(), j$id(), lr, beta1, beta2, niter)
        }
    )
)




    
