##' R6 class representing a \code{number}
##'
##' Object of this class are created by the '.modello'
##' session object that linkes them with the corresponding
##' \code{numbers} in the FORTRAN environment.
##' @author Filippo Monari
.number = R6Class(
    'modello_number',
    public = list(
        ##' @description
        ##' Initialise the reference object of class 'number'
        ##'
        ##' @param name \code{number} name
        initialize = function (name=NULL) {
            private$.name = name                 
        },
        ##' @description
        ##' Awares of a reference object associated
        ##' to an existing \code{number} is removed 
        finalize = function () {
            if (self$is.linked()) warning('deleted reference to existing number.')
        },
        ##' @description
        ##' Returns the name of the \code{number}.
        ##'
        ##' @return Returns the name of the \code{number}
        name = function () {
            private$.name
        },
        ##' @description
        ##' Returns the id of the \code{number}
        ##' (i.e. its position index in the \code{NUMBERS_} array).
        ##'
        ##' @return Returns the id of the number
        id = function () {
            .modello$number.id(self)
        },
        ##' @description
        ##' Pop (removes) the \code{number} from the
        ##' \code{NUMBERS_} array.
        ##'
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(1)
        ##' x$is.linked() # TRUE
        ##' x$pop()
        ##' x$is.linked() # FALSE
        ##' modello.close()
        ##' }
        pop = function () {
            ANS = .modello$pop.number(self)
            invisible(self)
        },
        ##' @description
        ##' Checks that the reference object is linked to
        ##' a \code{number}
        ##'
        ##' @return Retursn TRUE if is linked, FALSE otherwise
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(1)
        ##' x$is.linked() # TRUE
        ##' x$pop()
        ##' x$is.linked() # FALSE
        ##' modello.close()
        ##' }
        is.linked = function () {
            .modello$number.exists(self)
        },
        ##' @description
        ##' Returns the rank of the \code{number}.
        ##'
        ##' @return Returns the rank of the \code{number}
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(matrix(rnorm(9), 3, 3))
        ##' x$rank() # 2
        ##' modello.close()
        ##' }
        rank = function () {
            .intrf.number__rank(self$id())
        },
        ##' @description
        ##' Returns TRUE if the \code{number} has derivative,
        ##' FALSE otherwise
        ##' @return TRUE/FALSE
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x1 = number(1)
        ##' x1$has.dx() # TRUE
        ##' x2 = number(1, dx=FALSE)
        ##' x2$has.dx() # FALSE
        ##' modello.close()
        ##' }
        has.dx = function () {
            .intrf.number__has_dx(self$id())
        },
        ##' @description
        ##' Returns a list representation of the \code{number}
        collect = function () {
            ANS = list(
                'name' = self$name(),
                'v' = self$get.v(),
                'dx' = self$has.dx(),
                'scalar' = length(self$dim()) == 0
            )
            if (ANS$dx) ANS$dv = self$get.dv()
            return(ANS)
        },
        ##' @description
        ##' Saves the \code{number} in RDS format
        ##'
        ##' @param file filename, if null the \code{number} name is used
        ##' @param ... additional arguments for the function \code{saveRDS}
        save = function (file=NULL, ...) {
            x = self$collect()
            if (is.null(file)) file = paste(self$name, 'rds', sep='.')
            saveRDS(x, file)
        },
        ##' @description
        ##' Returns the size of the \code{number}
        ##'
        ##' @title Number Legnth 
        ##' @return Returns the size of the \code{number}
        ##' @author Filippo Monari
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(c(1, 2, 3))
        ##' x$length() # 3
        ##' modello.close()
        ##' }
        length = function () {
            .intrf.numbers__size(self$id())
        },
        ##' @description
        ##' Returns the shape of the \code{number}
        ##'
        ##' @return Returns the \code{number} shape
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(matrix(rnorm(9, 3, 3)))
        ##' x$dim() # c(3, 3)
        ##' modello.close()
        ##' }
        dim = function () {
            .intrf.numbers__shape(self$id())
        },
        ##' @description
        ##' Sets the value of the \code{number}
        ##'
        ##' @param x \code{number} value
        ##' @return Returns invisible self
        ##' @examples
        ##' modello.init()
        ##' x = number(1)
        ##  x$get.v()
        ##' x$set.v(2)
        ##' x$get.v()
        ##' modello.close()
        set.v = function (x) {
            ans = .intrf.number__set_v(self$id(), x)
            invisible(self)
        },
        ##' @description
        ##' Returns the value of a \code{number}
        ##' 
        ##' @return Returns the \code{number} value
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(1)
        ##  x$get.v()
        ##' x$set.v(2)
        ##' x$get.v()
        ##' modello.close()
        ##' }
        get.v = function () {
            .intrf.number__get_v(self$id())
        },
        ##' @description
        ##' Sets the derivative value of the \code{number}
        ##'
        ##' @param x \code{number} derivative value
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(1)
        ##  x$get.dv()
        ##' x$set.dv(1)
        ##' x$get.dv()
        ##' modello.close()
        ##' }
        set.dv = function (x) {
            ans = .intrf.number__set_dv(self$id(), x)
            invisible(self)
        },
        ##' @description
        ##' Sets the values of a slice in a \code{number}
        ##'
        ##' @param v value to be set
        ##' @param ... indexes along the number dimensions defining the slice
        ##' @param dx if TRUE the slice is taken from the gradient
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(matrix(0, 3, 3))
        ##' print(x$v)
        ##' print(x$dv)
        ##' x$set.slice(1, 1:3, 1:3)
        ##' x$set.slice(2, 1:3, 1:3, dx=TRUE)
        ##' print(x$v)
        ##' print(x$dv)
        ##' modello.close()
        ##' }
        set.slice = function (v, ..., dx=FALSE) {
            slice = t(as.matrix(expand.grid(...)))
            if (dx) {
                .intrf.number__set_slice_dv(self$id(), v, slice)
            } else {
                .intrf.number__set_slice_v(self$id(), v, slice)
            }
            invisible(self)
        },
        ##' @description
        ##' Sets the values of a slice in a \code{number}
        ##' considering the number flat
        ##'
        ##' @param v value to be set
        ##' @param s indexes defining the slice
        ##' @param dx if TRUE the slice is taken from the gradient
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(matrix(0, 3, 3))
        ##' print(x$v)
        ##' print(x$dv)
        ##' x$set.flat_slice(1, 1:9)
        ##' x$set.flat_slice(2, 1:9, dx=TRUE)
        ##' print(x$v)
        ##' print(x$dv)
        ##' modello.close()
        ##' }
        set.flat_slice = function (v, s, dx=FALSE) {
            if (dx) {
                .intrf.number__set_flat_slice_dv(self$id(), v, s)
            } else {
                .intrf.number__set_flat_slice_v(self$id(), v, s)
            }
            invisible(self)
        },
        ##' @description
        ##' Returns the derivative value of a \code{number}
        ##'
        ##' @return Returns the \code{number} value
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(1)
        ##  x$get.dv()
        ##' x$set.dv(1)
        ##' x$get.dv()
        ##' modello.close()
        ##' }
        get.dv = function () {
            .intrf.number__get_dv(self$id())
        },
    
        ## get.node = function () {
        ##     ##  .intrf.numbers_nds__get(private$.id, private$.typ)
        ## },
        
        ##' @description
        ##' Create a slice of the \code{number}
        ##'
        ##' @param ... slice indexes
        ##' @param name outout \code{number} name
        ##' @return Returns a reference object of class 'number' with the slice
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(matrix(rnorm(9), 3, 3))
        ##' print(x$v)
        ##' y = x$slice(1:2, 1:3)
        ##' print(y$v)
        ##' modello.close()
        ##' }
        slice = function (...) {
            slice = t(as.matrix(expand.grid(...)))
            .modello$apply.math_op(.intrf.number__slice, self, slice)
        },
        ##' @description
        ##' Create a flat slice of the \code{number}
        ##'
        ##' @param s flat slice indexes
        ##' @param name outout \code{number} name
        ##' @return Returns a reference object of class 'number' with the flat slice
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(matrix(rnorm(9), 3, 3))
        ##' print(x$v)
        ##' y = x$flat_slice(1:6)
        ##' print(y$v)
        ##' modello.close()
        ##' }
        flat_slice = function (s) {
            .modello$apply.math_op(.intrf.number__flat_slice, self, s)
        },
        ##' @description
        ##' Create a contiguous slice of the \code{number}.
        ##' Slice along the leading order (columns). 
        ##'
        ##' The \code{number} values are not copied but only referred through pointers.
        ##' @param s1 intial index of the contiguous slice
        ##' @param s2 final index of the contiguous slice
        ##' @param name outout \code{number} name
        ##' @return Returns a reference object of class 'number' with the contiguous slice
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(matrix(rnorm(9), 3, 3))
        ##' print(x$v)
        ##' y = x$slice(1, 2)
        ##' print(y$v)
        ##' modello.close()
        ##' }
        contiguous.slice = function (s1, s2=NULL) {
            if (is.null(s2)) s2 = s1
            .modello$apply.math_op(.intrf.number__contiguous_slice, self, s1, s2)
        },
        ##' @description
        ##' Reshape the \code{number} according the the given shape vector.
        ##'
        ##' The \code{number} is not copied but reshaped through pointers.
        ##' @param shp shape vector
        ##' @param name output \code{number} name
        ##' @return Returns a reference object of class 'number' with the reshape
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(c(1, 2, 3, 4, 5, 6))
        ##' y = x$reshape(c(3, 2))
        ##' print(x)
        ##' print(x$v)
        ##' print(y)
        ##' print(y$v)
        ##' modello.close()
        ##' }
        reshape = function (shp, name=NULL) {
            .modello$apply.math_op(.intrf.number__reshape, self, shp)
        },
        ##' @description
        ##' Reshape the \code{number} by dropping the collapsed dimensions.
        ##'
        ##' The \code{number} is not copied but reshaped through pointers.
        ##' @param name output \code{number} name
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(as.matrix(c(1, 2, 3)))
        ##' print(x)
        ##' print(x$v)
        ##' y = x$drop.dim()
        ##' print(y)
        ##' print(y$v)
        ##' modello.close()
        ##' }
        drop.dim = function () {
            .modello$apply.math_op(.intrf.number__drop_shape, self)
        },
        ##' @description
        ##' Binds the \code{number} to another along the given dimension
        ##'
        ##' @param x \code{number} to bind
        ##' @param k dimension index
        ##' @param name name of the output \code{number}
        ##' @return Returns areference object of class 'number'
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x = number(as.matrix(c(1, 2, 3)))
        ##' y = number(as.matrix(c(4, 5, 6)))
        ##' z = x$bind(y, 2)
        ##' print(z)
        ##' print(z$v)
        ##' modello.close()
        ##' }
        bind = function (x, k) {
            .modello$apply.math_op(.intrf.number__bind, self, x, k)
        },
        ##' @description
        ##' Runs the node operator that generated the \code{number}
        ##'
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' g = graph.open()
        ##' x1 = number(1)
        ##' x2 = number(2)
        ##' x3 = x1 + x2
        ##' graph.close()
        ##' print(x3$v)
        ##' x1$v = 2
        ##' x3$op()
        ##' print(x3$v)
        ##' modello.close()
        ##' }
        op = function () {
            ANS = .intrf.number__op(self$id())
            invisible(self)
        },
        ##' @description
        ##' Resest the derivative values for the \code{number} node
        ##' accoriding to the backward differentiation schema.
        ##'
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' g = graph.open()
        ##' x1 = number(4)
        ##' x2 = number(2)
        ##' x3 = x1 ** x2
        ##' g = graph.close()
        ##' print(x1$dv)
        ##' print(x2$dv)
        ##' x3$dv = 1
        ##' x3$bw()
        ##' print(x1$dv)
        ##' print(x2$dv)
        ##' x3$bw.zero()
        ##' print(x1$dv)
        ##' print(x2$dv)
        ##' modello.close()
        ##' }
        bw.zero = function () {
            ANS = .intrf.number__bw_zero(self$id())
            invisible(self)
        },
        ##' @description
        ##' Applies bakward differentiation to the \code{number} node
        ##'
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' x1 = number(4)
        ##' x2 = number(2)
        ##' g = graph.open()
        ##' x3 = x1 ** x2
        ##' graph.close()
        ##' print(x1$dv)
        ##' print(x2$dv)
        ##' x3$dv = 1
        ##' x3$bw()
        ##' print(x1$dv)
        ##' print(x2$dv)
        ##' x3$bw.zero()
        ##' print(x1$dv)
        ##' print(x2$dv)
        ##' modello.close()
        ##' }
        bw = function () {
            ANS = .intrf.number__bw(self$id())
            invisible(self)
        },
        ##' @description
        ##' Prints a representation of the \code{number}
        print = function () {
            if (self$is.linked()) {
                shp = paste(self$dim(), collapse=',')
                repr = paste0(self$name(), ' => <NUMBERS_(', self$id(), '), (', shp, ')>')
            } else {
                repr = paste0(sef$name(), ' => <NULL>')
            }
            cat(repr, sep='\n')
        }
    ),
    active = list(
        ##' @field v  Sets or gets the \code{number} value
        v = function (x) {
            if (missing(x)) {
                .intrf.number__get_v(self$id())
            } else {
                ans = .intrf.number__set_v(self$id(), x)
            }
        },
        ##' @field dv Sets or gets the \code{number} derivative value
        dv = function (x) {
            if (missing(x)) {
                .intrf.number__get_dv(self$id())
            } else {
                ans = .intrf.number__set_dv(self$id(), x)
            }
        }
    ),
    private = list(
        .name = NULL
    )
)


##' Given a list created with a 'number' collection method,
##' it recreates that 'number'
##'
##' @title Number from Collected List 
##' @param args list of collected arguments
##' @return Returns the created number invisibly.
##' @author Filippo Monari
number_from_list <- function (args) {
     if (args$scalar) {
        shp = integer(0)
     } else {
        shp = dim(args$v)
     }
     x = .modello$append.number(shp, args$dx, args$name)
     if (args$dx) {
         x$set.v(args$v)$set.dv(args$dv)
     } else {
         x$set.v(args$v)
     }
     invisible(x)
}

##' Load a number form an RDS file created with the save method.
##'
##' @title Load Number from RDS
##' @param inf rds file name/path
##' @param ... additional arguments for the readRDS function
##' @return Returns a number
##' @author Filippo
##' @export
number_from_RDS <- function (inf, ...) {
    args = base::readRDS(inf, ...)
    number_from_list(args)
}



    
    










        
            
            


