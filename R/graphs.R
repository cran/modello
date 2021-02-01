##' R6 class representing a computational graph
##'
##' This objects are created by the '.modello' session
##' object that links them to the corresponding \code{graph}
##' in the FORTRAN environment.
.graph = R6Class(
    'graph',
    public = list(
        ##' @description
        ##' Initialisation method for reference object of class 'graph'
        ##'
        ##' @param name \code{graph} name
        initialize = function (name) {
            private$.name = name
        },
        ##' @description
        ##' Awares if a reference object is remove when
        ##' when stil referring to an existing \code{graph}
        finalize = function () {
            if (self$is.linked()) warning("the graph is linked")
        },
        ##' @description
        ##' Returns the \code{graph} name
        ##'
        ##' @return Returns the \code{graph} name
        name = function () {
            private$.name
        },
        ##' @description
        ##' Returns the \code{graph} id
        ##' (i.e. position within the \code{GRAPHS_} array)
        ##'
        ##' @return Returns the \code{graph} id
        id = function () {
            .modello$graph.id(self)
        },
        ##' @description
        ##' Pop (removes) the \code{graph} from the
        ##' \code{GRAPHS_} array.
        ##'
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' g = graph.open()
        ##' graph.close()
        ##' g$is.linked() # TRUE
        ##' g$pop()
        ##' g$is.linked() # FALSE
        ##' modello.close()
        ##' }
        pop = function () {
            ANS = .modello$pop.graph(self)
        },
        ##' @description
        ##' Checks that the reference object is linked to
        ##' a \code{graph}
        ##'
        ##' @return Retursn TRUE if is linked, FALSE otherwise
        ##' @examples
        ##' \donttest{
        ##' modello.init()
        ##' g = graph.open()
        ##' graph.close()
        ##' g$is.linked() # TRUE
        ##' g$pop()
        ##' g$is.linked() # FALSE
        ##' modello.close()
        ##' }
        is.linked = function () {
            .modello$graph.exists(self)
        },
        ##' @description
        ##' Applies all the operators for the nodes in the
        ##' \code{graph}
        ##'
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
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
        ##' }
        op = function () {
            ANS = .intrf.graph__op(self$id())
            invisible(self)
        },
        ##' @description
        ##' Resest the derivative values for the nodes in the \code{graph}
        ##' accoriding to the backward differentiation schema.
        ##'
        ##' @return Returns invisible self
        bw.zero = function () {
            ANS = .intrf.graph__bw_zero(self$id())
            invisible(self)
        },
        ##' @description
        ##' Applies all the backward differentiation operators for
        ##' the nodes in the \code{graph}
        ##'
        ##' @return Returns invisible self
        ##' @examples
        ##' \donttest{
        ##' modello.init()
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
        ##' }
        bw = function () {
            ANS = .intrf.graph__bw(self$id())
            invisible(self)
        },
        ##' @description
        ##' Prints a representation of the \code{graph}
        print = function () {
            if (self$is.linked()) {
                #shp = paste(self$dim(), collapse=',')
                repr = paste0(self$name(), ' => <GRAPHS_(', self$id(), ')>')
            } else {
                repr = paste0(sef$name(), ' => <NULL>')
            }
            cat(repr, sep='\n')
        }
        ## get = function () {
        ##     .intrf.ggg(self$id())
        ## }
    ),
    private = list(
        .name = NULL
    )
)


        
