##' Allocates all the arrays necessary to initialise
##' the modello session.
##' 
##' @title Initialise Modello Session
##' @param n.numbers number of \code{numbers} (size of \code{NUMBERS_})
##' @param n.nodes  number of \code{nodes} (size of \code{NODES_})
##' @param n.graphs number of \code{graphs} (size of \code{GRAPHS_})
##' @param n.opts number of \code{optimisers} (size of \code{OPTS_})
##' @return Return invisible NULL
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10) # Initilaises the modello session with arrays of the given size
##' modello.is.init() # TRUE
##' modello.reset()   # restart the modello session with initial array sizes 
##' modello.is.init() # TRUE
##' modello.close()   # Closes modello
##' modello.is.init() # FALSE
##' @export
modello.init <- function (n.numbers=1000000, n.nodes=1000000,
                          n.graphs=1000000,  n.opts=10000000) {
    .modello$init(n.numbers, n.nodes, n.graphs, n.opts)
}
##' Checks if the modello sesssion is initialised
##'
##' @title Modello Is Initialised
##' @return Returns TRUE/FALSE 
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10) # Initilaises the modello session with arrays of the given size
##' modello.is.init() # TRUE
##' modello.reset()   # restart the modello session with initial array sizes 
##' modello.is.init() # TRUE
##' modello.close()   # Closes modello
##' modello.is.init() # FALSE
##' @export
modello.is.init <- function () {
    .modello$.init
}
##' Rest the modello session, that is
##' deallocates and reallocates all the arrays.
##' All the data stored in them is lost.
##'
##' @title Reset Modello Session 
##' @return Return invisible NULL
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10) # Initilaises the modello session with arrays of the given size
##' modello.is.init() # TRUE
##' modello.reset()   # restart the modello session with initial array sizes 
##' modello.is.init() # TRUE
##' modello.close()   # Closes modello
##' modello.is.init() # FALSE
##' @export
modello.reset <- function () {
    .modello$reset()
    modello.gc()
}
##' Deallocates all the arrays.
##'
##' @title Close Modello Session 
##' @return Returns invsible NULL
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10) # Initilaises the modello session with arrays of the given size
##' modello.is.init() # TRUE
##' modello.reset()   # restart the modello session with initial array sizes 
##' modello.is.init() # TRUE
##' modello.close()   # Closes modello
##' modello.is.init() # FALSE
##' @export
modello.close <- function () {
    modello.gc()
    .modello$close()
}
##' Garbage collector for the current modello session.
##'
##' @title Modello Session Garbage Collector 
##' @param env environmet wherein collect the garbage
##' @author Filippo MOnari
##' @export
modello.gc <- function (env=.GlobalEnv) {
    .modello$number.gc()
    .modello$graph.gc()
    nms = ls(envir=env)
    for (nm in nms) {
        x = get(nm, envir=env)
        if (is.number(x) || is.graph(x)) {
            if (!x$is.linked()) {
                rm(list=nm, envir=env)
            }
        }
    }
}
##' Checks if a object is of class 'number'
##'
##' @title Is Number 
##' @param x an object
##' @return Returns TRUE/FALSE
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' x = number(1)
##' is.number(x) # TRUE
##' modello.close()
##' @export
is.number <- function (x) {
    methods::is(x, 'modello_number')
}

##' Checks if an object os of class 'graph'
##'
##' @title Is Graph 
##' @param g object
##' @return Returns TRUE/FALSE
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' g = graph.open()
##' is.graph(g) # TRUE
##' modello.close()
##' @export
is.graph <- function (g) {
    methods::is(g, 'graph')
}
##' Check if an object is an optimiser.
##'
##' @title Is Optimiser
##' @param x objcet
##' @return Return TRUE/FALSE
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' mdl = module.lm$new(1, 3, 1)
##' optimizer = adam.opt(mdl$pars())
##' is.opt(optimizer) # TRUE
##' modello.close()
##' @export
is.opt <- function (x) {
    is(x, '.opt')
}
##' @rdname number
##' @examples
##' modello.init(n.numbers=10, n.nodes=10, n.graphs=10, n.opts=10)
##' x = number(integer(3))
##' print(x)
##' print(x$v)
##' modello.close()
##' @export
number.integer <- function (x, dx=TRUE, ...) {
    ans = .modello$append.number(x, dx)
    return(ans)
}
##' @rdname number
##' @param scalar if TRUE the \code{number} is implemented as scalar (rank = 0)
##' @examples
##' modello.init(n.numbers=10, n.nodes=10, n.graphs=10, n.opts=10)
##' x1 = number(1, scalar=TRUE)
##' print(x1)
##' print(x1$v)
##' x2 = number(1)
##' print(x2)
##' print(x2$v)
##' x3 = number(c(1, 2, 3))
##' print(x3)
##' print(x3$v)
##' modello.close()
##' @export
number.numeric <-  function (x, dx=TRUE, scalar=FALSE, ...) {
    if (scalar && length(x) > 1) stop("scalar cannot have length > 1")
    if (scalar) shp = integer(0) else shp=length(x)
    .modello$append.number(shp, dx)$set.v(x)
}
##' @rdname number
##' @examples
##' modello.init(n.numbers=10, n.nodes=10, n.graphs=10, n.opts=10)
##' x = number(matrix(rnorm(9), 3, 3))
##' print(x)
##' print(x$v)
##' modello.close()
##' @export
number.array <- function (x, dx=TRUE, ...) {
    .modello$append.number(dim(x), dx)$set.v(x)
}
##' S3 method for creating new \code{numbers}.
##'
##' @title Create New Number 
##' @param x integer, numeric, or array
##' @param dx if TRUE to the \code{number} is attributed a derivative
##' @param ... further arguments, actually ignored
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @export
number <- function (x, ...) {
    UseMethod('number', x)
}
##' Create a constant scalar \code{number}
##'
##' @title Create constnat scalar number
##' @param x value of the constant
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' x = .k(1)
##' print(x)
##' print(x$v)
##' modello.close()
##' @export
.k <- function (x) {
    number(x, dx=FALSE, scalar=T) 
}
##' S3 method to get the total size of a \code{number}
##'
##' @title Number Length 
##' @param x a referecen object of class 'number'
##' @return Returns the size of the \code{number}
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' x = number(rnorm(3))
##' length(x) #3
##' modello.close()
##' @export
length.modello_number <- function (x) {
    x$length()
}
##' S3 method to get the shape of a \code{number}
##'
##' @title Get Number Shape 
##' @param x a reference object of class 'number'
##' @return Returns the shape vector of the \code{number}
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' x = number(matrix(rnorm(9), 3, 3))
##' dim(x) # c(3, 3)
##' modello.close()
##' @export
dim.modello_number <- function (x) {
    x$dim()
}
##' Open a \code{graph}
##'
##' If \code{g} is NULL a new \code{graph} is open. Otrewise
##' the \code{graph} liked to \code{g} is open.
##' @title Open Graph 
##' @param g a reference object of class 'graph'
##' @return Returns a reference object of class 'graph' linked to the open \code{graph}
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' x1 = number(1)
##' x2 = number(3)
##' x3 = number(2)
##' g = graph.open()
##' y = log((x1 + x3)**x2)
##' graph.close()
##' print(y$v)
##' x1$v = 2
##' g$op()
##' print(y$v)
##' y$dv = 1
##' g$bw()
##' print(x1$dv)
##' print(x2$dv)
##' print(x3$dv)
##' modello.close()
##' @export
graph.open <- function (g=NULL) {
    .modello$graph.open(g)
}
##' Closes the last open \code{graph}
##'
##' Only one open \code{graph} at a time is allowed.
##' @title Close Graph
##' @return Returns invisible NULL
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' x1 = number(1)
##' x2 = number(3)
##' x3 = number(2)
##' g = graph.open()
##' y = log((x1 + x3)**x2)
##' graph.close()
##' print(y$v)
##' x1$v = 2
##' g$op()
##' print(y$v)
##' y$dv = 1
##' g$bw()
##' print(x1$dv)
##' print(x2$dv)
##' print(x3$dv)
##' modello.close()
##' @export
graph.close <- function () {
    ans = .intrf.graph__close()
    invisible()
}
##' S3 method to pop (remove) \code{numbers} or
##' \code{graphs} from their respective arrays
##' (\code{NUMBERS_} or \code{GRAPHS_}).
##'
##' @title Pop Modello Objects 
##' @param x a reference object of call 'number' or 'graph'
##' @return Returns invisible x
##' @author Filippo Monari
##' @export
pop <- function(x) {
    UseMethod('pop', x)
}
#' @rdname pop
#' @examples
#' modello.init(10, 10, 10, 10)
#' x = number(1)
#' x$is.linked() # TRUE
#' pop(x)
#' x$is.linked() # FALSE
#' modello.close()
#' @export
pop.modello_number <- function (x) {
    x$pop()
}
#' @rdname pop
#' @examples
#' modello.init(10, 10, 10, 10)
#' g = graph.open()
#' graph.close()
#' g$is.linked() # TRUE
#' pop(g)
#' g$is.linked() # FALSE
#' modello.close()
#' @export
pop.graph <- function (x) {
    x$pop()
}
##' Slices \code{numbers}
##'
##' @title Silce Numbers 
##' @param x reference object of class 'number'
##' @param ... additional parametrers to pass to the slice methods
##' @param contiguous if TRUE it tryes to create a contiguous slice
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @examples
##' modello.init(10, 10, 10, 10)
##' x = number(matrix(rnorm(9), 3, 3))
##' print(x$v)
##' s1 = x[1, 2]
##' print(s1)
##' print(s1$v)
##' s2 = x[1,2, contiguous=FALSE]
##' print(s2)
##' print(s2$v)
##' modello.close()
##' @export
`[.modello_number` <- function (x, ..., contiguous=TRUE) {
    slice = list(...)
    if (contiguous) {
        stopifnot(length(slice) <= 2)
        do.call(x$contiguous.slice, slice)
    } else {
        if (length(slice) == 1) {
            do.call(x$flat_slice, slice)
        } else {
            do.call(x$slice, slice)
        }
    }
}
##' Sets the value of a slice in a \code{number}
##'
##' @param x \code{number}
##' @param ... indexes defining the slice
##' @param dx if TRUE the slice is taken from the gradient
##' @param value value to set
##' @examples
##' modello.init(10, 10, 10, 10)
##' x = number(matrix(rnorm(9), 3, 3))
##' print(x$v)
##' x[1,1] = -100
##' print(x$v)
##' x[1:2,1:2] = 100
##' print(x$v)
##' modello.close()
##' @export
`[<-.modello_number` <- function (x, ..., dx=FALSE, value) {
    slice = list(...)
    if (length(slice) == 1) {
        do.call(x$set.flat_slice, c(v=value, slice, dx=dx))
    } else {
        do.call(x$set.slice, c(v=value, slice, dx=dx))
    }
}
##' @rdname bind
##' @param ... \code{numbers} to bind together
##' @examples
##' modello.init(10, 10, 10, 10)
##' x1 = number(c(1, 2, 3))
##' y1 = bind(x1, x1, k=1)
##' print(y1)
##' print(y1$v)
##' x2 = number(matrix(c(1, 2, 3), 3, 1))
##' y2 = bind(x2, x2, k=2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
bind.modello_number <- function (..., k) {
    bind.list(list(...), k)
}
##' @rdname bind
##' @param x list of \code{numbers} to bind together
##' @examples
##' modello.init(10, 10, 10, 10)
##' x1 = number(c(1, 2, 3))
##' y1 = bind(list(x1, x1), k=1)
##' print(y1)
##' print(y1$v)
##' x2 = number(matrix(c(1, 2, 3), 3, 1))
##' y2 = bind(list(x2, x2), k=2)
##' print(y2)
##' print(y2$v)
##' modello.close()
##' @export
bind.list <- function (x, k, ...) {
    go <- function (xi, ll) {
        if (length(ll) == 0) return(xi)
        xi = xi$bind(ll[[1]], k)
        go(xi, ll[-1])
    }
    stopifnot(all(sapply(x, is.number)))
    go(x[[1]], x[-1])
}
##' S3 method to bind \code{numbers} together
##' along a certain dimension.
##'
##' @title Bind  
##' @param k dimesion along to perform the bind
##' @return Returns a \code{number} 
##' @author Filippo Monari
##' @export
bind <- function (x, ...) {
    UseMethod('bind', x)
}
##' Embeddings
##'
##' @title Embeddings
##' @param xf a reference object of class 'number' referring to the factors 
##' @param xx a reference object of class 'number' referring to the embedding vectors
##' @param n number of embedding vector to bind together
##' @return Returns a reference object of class 'number'
##' @author Filippo Monari
##' @export
embeddings <- function (xf, xx, n) {
    .modello$apply.math_op(.intrf.number__embeddings, xf, xx, n)
}
##' Helper function to get the ids from a list of
##' reference object 
##'
##' @title Get IDs 
##' @param x list of reference objects
##' @return Return a vector with the ids
##' @author Filippo Monari
.ids <- function (x) {
    sapply(x, function(x)x$id())
}
##' Creates an stochastic gradient descent optimiser.
##'
##' @title Create SGD optimiser
##' @param pars list of reference object of class 'number' representing
##' the parameters to optmise.
##' @return a reference object of class 'sgd.opt'
##' @examples
##' modello.init(10, 10, 10, 10)
##' mdl = module.lm$new(1, 3, 1)
##' optimizer = sgd.opt(mdl$pars())
##' is.opt(optimizer) # TRUE
##' modello.close()
##' @export
sgd.opt <- function (pars) {
    .modello$append.opt(.intrf.sgd__append, .sgd.opt, .ids(pars))
}
##' Creates a stochastic gradient descent with momentum optimiser.
##'
##' @title Create SGDWM optmiser
##' @param pars list of reference object of class 'number' representing
##' the parameters to optmise.
##' @return a reference object of class 'sgdwm.opt'
##' @examples
##' modello.init(10, 10, 10, 10)
##' mdl = module.lm$new(1, 3, 1)
##' optimizer = sgdwm.opt(mdl$pars())
##' is.opt(optimizer) # TRUE
##' modello.close()
##' @export
sgdwm.opt <- function (pars) {
    .modello$append.opt(.intrf.sgdwm__append, .sgdwm.opt, .ids(pars))
}
##' Creates an Adam optimiser.
##'
##' @title Create Adam optmiser
##' @param pars list of reference objects of class 'number' representing
##' the parameters to optmise.
##' @return a reference object of class 'adam.opt'
##' @examples
##' modello.init(10, 10, 10, 10)
##' mdl = module.lm$new(1, 3, 1)
##' optimizer = adam.opt(mdl$pars())
##' is.opt(optimizer) # TRUE
##' modello.close()
##' @export
adam.opt <- function (pars) {
    .modello$append.opt(.intrf.adam__append, .adam.opt, .ids(pars))
}
    
