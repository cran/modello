##' R6 class representing the current modello session
##'
##' An object of this class called '.modello' is created when
##' the package is loaded and will do the booking keeping of
##' what happens during the session.
##' @author Filippo Monari
.modello.session = R6Class(
    'modello_session',
    public = list(
        ##' @field .init stores the init status of the session (TRUE/FALSE)
        .init = NULL,
        ##' @description
        ##' modello session object initialisation method.
        ##' It does nothgin, it is just use on package load
        ##' to create the .modello object containing the session.
        initialize = function () {
            self$.init = FALSE
            self$.numbers = new.env()
            self$.graphs = new.env()
            self$.opts = new.env()
        },
        ##' @description
        ##' Allocates all the arrays necessary to the session
        ##'
        ##' @param n.numbers number of \code{numbers}
        ##' @param n.nodes number of \code{nodes}
        ##' @param n.graphs number of \code{graphs}
        ##' @param n.opts number of \code{optimisers}
        ##' @return Returns invisible NULL.
        init  = function (n.numbers, n.nodes, n.graphs, n.opts) {
            .intrf.allocate_numbers(n.numbers)
            .intrf.nodes__allocate(n.nodes)
            .intrf.graphs__allocate(n.graphs)
            .intrf.allocate_gopts(n.opts)
            self$.n.numbers = n.numbers
            self$.n.nodes = n.nodes
            self$.n.graphs = n.graphs
            self$.n.opts = n.opts
            self$.init = TRUE
            invisible()
        },
        ##' @description
        ##' Deallocates all the session arrays
        ##'
        ##' @return Returns invisible NULL.
        close = function () {
            .intrf.deallocate_numbers()
            .intrf.deallocate_nodes()
            .intrf.deallocate_graphs()
            .intrf.deallocate_gopts()
            self$.init = FALSE
            invisible()
        },
        ##' @description
        ##' Reset the session by deallocating and reallocating all the arrays.
        ##' The data in the arrays is lost.
        ##'
        ##' @return Returns invisible NULL.
        reset = function () {
            self$close()
            self$init(self$.n.numbers, self$.n.nodes,
                      self$.n.graphs, self$.n.opts)
        },
        ##' @description
        ##' Closes the session if its object is destroyed.
        finalize = function () {
            if (self$.init) self$close()
        },
        ##' @description
        ##' build unique random number ideintifying a \code{number},
        ##' \code{graph} or \code{optimizer}.
        ##' @param typ charcter indicating the type of object.
        next.name = function (typ) {
            tempfile('x', as.character(as.integer(Sys.time())), paste0('.', typ))
        },
        
        ##=======#
        ##NUMBERS#
        ##=======#

        ##' @field .n.numbers number of allocate slot for storing \code{number}
        .n.numbers = NULL,
        ##' @field .numbers environment keeping track of the created \code{numbers}
        .numbers = NULL,
        ##' @description
        ##' Creates a new \code{number} and return the corresponding
        ##' reference object of class 'number'
        ##'
        ##' @param id \code{number} id 
        ##' @param name \code{number} name
        new.number = function (id, name=NULL) {
            if (is.null(name)) {
                name = self$next.name('number')
            } else {
                stopifnot(!self$number.name.exists(name))
            }
            self$.numbers[[name]] = id
            .number$new(name=name)
        },
        ##' @description
        ##' Checks if a \code{number} name exists.
        ##'
        ##' @param name \code{number} name
        ##' @return Returns TRUE if the name exists, FALSE otherwise
        number.name.exists = function (name) {
            stopifnot(is.character(name))
            exists(name, envir=self$.numbers)
        },
        ##' @description
        ##' Append a \code{number} of the given shape to the
        ##' \code{NUMBER_} array. A link to the \code{number}
        ##' is create the \code{numbers} environment within the
        ##' session object. This method is called each time
        ##' a new \code{number} is created.
        ##'
        ##' @param shp \code{number} shape
        ##' @param dx if TRUE allocate a derivative array for the \code{number}
        ##' @param name character, \code{number} name. Only for loading saved \code{numbers}
        ##' @return Returns an object of class 'number' referring
        ##' to the appended \code{number}
        append.number = function (shp, dx, name=NULL) {
            id = .intrf.number__append(shp, dx)
            self$new.number(id, name)
        },
        ##' @description
        ##' Given a name (character identifier) for a \code{number}
        ##' creates and returns a reference objects of class 'number'.
        ##'
        ##' @param name \code{number} name
        ##' @return Returns a object of class 'number'
        get.number = function (name) {
            stopifnot(self$number.name.exists(name))
            self$new.number(self$.numbers[[name]], name)
        },
        ##' @description
        ##' Pops (removes) a \code{number} fro the \code{NUMBERS_} array
        ##' according to the provided reference object.
        ##'
        ##' @param x reference object of class 'number'
        ##' @return Returns invisible x.
        pop.number = function (x) {
            stopifnot(is.number(x) && x$is.linked())
            .intrf.number__pop(x$id())
            rm(list=x$name(), envir=self$.numbers)
            invisible(x)
        },
        ##' @description
        ##' Given a reference object of class 'number', retrives and
        ##' returs the id of the associated \code{nummber}
        ##' (i.e. its position in the \code{NUMBERS_} array.
        ##'
        ##' @param x reference object of class 'number'
        ##' @return Returns the \code{number} id
        number.id = function (x) {
            stopifnot(is.number(x))
            self$.numbers[[x$name()]]
        },
        ##' @description
        ##' Checks that the \code{number} associated to a referece
        ##' object of class 'number' exists.
        ##' 
        ##' @param x reference object of class 'number'
        ##' @return Returns TRUE if the \code{number} exists, FALSE otherwise.
        number.exists = function (x) {
            stopifnot(is.number(x))
            self$number.name.exists(x$name())
        },
        ##' @description
        ##' Calls the \code{number} garbage collector.
        ##'
        ##' @return Returns invisible NULL
        number.gc = function () {
            .intrf.number__gc()
            nms = names(self$.numbers)
            for (nm in nms) {
                if (!.intrf.number__is_allocated(self$.numbers[[nm]])) {
                    rm(list=nm, envir=self$.numbers)
                }
            }
            invisible()
        },
        ##' @description
        ##' Applies a mathematical operator to its arguments.
        ##'
        ##' @param op operator name
        ##' @param ... operator parameters
        ##' @param name name of the output \code{number}. If NULL is automatically generated
        ##' @return Returns a \code{number}
        apply.math_op = function (op, ...) {
            op.args = lapply(list(...), function (x) {
                if (is.number(x)) {
                    stopifnot(x$is.linked())
                    x$id()
                } else {
                    x
                }
            })
            id = do.call(op, op.args)
            self$new.number(id)
        },

        ##======##
        ##GRAPHS##
        ##======##

        ##' @field .n.nodes number of slot allocated for storing \code{nodes}
        .n.nodes = NULL,
        ##' @field .n.graphs number of slots allocated for storing \code{graphs}
        .n.graphs = NULL,
        ##' @field .graphs environement keeping track of the created \code{graphs}
        .graphs = NULL,
        ##' @description
        ##' Creates a new \code{graph} and return the corresponding
        ##' reference object of class 'graph'
        ##'
        ##' @param id \code{graph} id 
        ##' @return Returns a reference object of class 'graph'
        new.graph = function (id) {
            name = self$next.name('graph')
            stopifnot(!self$graph.name.exists(name))
            self$.graphs[[name]] = id
            .graph$new(name)
        },
        ##' @description
        ##' Checks if a \code{graph} name exists.
        ##'
        ##' @param name \code{graph} name
        ##' @return Returns TRUE if the name exists, FALSE otherwise
        graph.name.exists = function (name) {
            stopifnot(is.character(name))
            exists(name, envir=self$.graphs)
        },
        ##' @description
        ##' Opens a \code{graph}. 
        ##'
        ##' if \code{g} is NULL a new graph is open  and appended to the
        ##' \code{graph} array. If a reference object
        ##' of class 'graph' is porvided the corresponding \code{graph}
        ##' is open.
        ##' @param g reference object of class 'graph'
        ##' @return Returns an reference object of class 'graph'
        graph.open = function (g=NULL) {
            if (is(g, 'graph')) {
                .intrf.graph__open(g$id())
                invisible(g)
            } else {
                .intrf.graph__open(0)
                self$new.graph(.intrf.graphi__get())
            }
        },
        ##' @description
        ##' Given a \code{graph} name (character identifier) creates
        ##' and returns a reference object of class 'graph'.
        ##'
        ##' @param name \code{graph} name
        ##' @return Returns a reference object of class 'graph'.
        get.graph = function (name) {
            stopifnot(self$graph.name.exixsts)
            self$new.graph(self$.graphs[[name]])
        },
        ##' @description
        ##' Pops (removes) a \code{graph} from the \code{GRAPHS_} array.
        ##'
        ##' @param x reference object of class 'graph'
        ##' @return Returns invisible x.
        pop.graph = function (x) {
            stopifnot(is.graph(x) && x$is.linked())
            .intrf.graph__pop(x$id())
            rm(list=x$name(), envir=self$.graphs)
            invisible(x)
        },
        ##' @description
        ##' Given a reference object of class 'graph', retunrs
        ##' the id (i.e. position in the \code{GRAPHS_} array)
        ##' of the associated \code{graph}.
        ##'
        ##' @param x reference object of class 'graph'
        ##' @return Returns the \code{graph} id
        graph.id = function (x) {
            self$.graphs[[x$name()]]
        },
        ##' @description
        ##' Checks that the \code{graph} associated to a referece
        ##' object of class 'graph' exists.
        ##' 
        ##' @param x reference object of class 'graph'
        ##' @return Returns TRUE if the \code{graph} exists, FALSE otherwise.
        graph.exists = function (x) {
            self$graph.name.exists(x$name())
        },
        ##' @description
        ##' Calls the \code{graph} garbage collector.
        ##'
        ##' @return Returns invisible NULL
        graph.gc = function () {
            .intrf.graph__gc()
            nms = names(self$.graph)
            for (nm in nms) {
                if (!.intrf.graph__is_allocated(self$.graph[[nm]])) {
                    rm(list=nm, envir=self$.graph)
                }
            }
        },

        ##====##
        ##OPTS##
        ##====##

        ##' @field .n.opts number of slots allocated for storing \code{optimisers}
        .n.opts = NULL,
        ##' @field .opts environment keeping track of the created \code{optimisers}
        .opts = NULL,
        ##' @description
        ##' Creates a new \code{optimiser} and return the corresponding
        ##' reference object of class 'opt'
        ##'
        ##' @param id \code{optimiser} id
        ##' @param opt R6 class indentifying the kind of optimiser
        ##' @return Returns a reference object of class 'opt'
        new.opt = function (id, opt) {
            name = self$next.name('opt')
            stopifnot(!self$opt.name.exists(name))
            self$.opts[[name]] = id
            opt$new(name)
        },
        ##' @description
        ##' Checks if a \code{optimiser} name exists.
        ##'
        ##' @param name \code{optimiser} name
        ##' @return Returns TRUE if the name exists, FALSE otherwise
        opt.name.exists = function (name) {
            stopifnot(is.character(name))
            exists(name, envir=self$.opts)
        },
        ##' @description
        ##' Append an \code{optimiser} with the given parameters to
        ##' the \code{OPTS_} array. A link to the \code{optimiser}
        ##' is created in \code{.opt} environment within the
        ##' session object. This method is called each time
        ##' a new \code{optimiser} is created.
        ##'
        ##' @param append interface function for appending the optimiser to \code{GOPTS_} 
        ##' @param opt character dincating the kind of optimiser
        ##' @param ... optmiser parameters
        ##' @return Returns an object of class 'opt' referring
        ##' to the appended \code{optmiser}
        append.opt = function (append, opt, ...) {
            id = append(...)
            self$new.opt(id, opt)
        },
        ##' @description
        ##' Given a name (character identifier) for an \code{optimiser}
        ##' creates and returns a reference objects of class 'opt'.
        ##'
        ##' @param name \code{optimiser} name
        ##' @return Returns a object of class 'opt'
        ## :todo: correct!
        get.opt = function (name) {
            stopifnot(self$opt.name.exists(name))
            self$opt.number(self$.opts[[name]], name)
        },
        ##' @description
        ##' Pops (removes) an \code{optmiser} from the \code{OPTS_} array
        ##' according to the provided reference object.
        ##'
        ##' @param x reference object of class 'opt'
        ##' @return Returns invisible x.
        pop.opt = function (x) {
            stopifnot(is.opt(x))
            stopifnot(x$is.linked())
            .intrf.gopt__pop(x$id())
            rm(list=x$name(), envir=self$.opts)
            invisible(x)
        },
        ##' @description
        ##' Given a reference object of class 'opt', retrives and
        ##' returs the id of the associated \code{optmiser}
        ##' (i.e. its position in the \code{OPTS_} array.
        ##'
        ##' @param x reference object of class 'opt'
        ##' @return Returns the \code{optmiser} id
        opt.id = function (x) {
            stopifnot(is.opt(x))
            self$.opts[[x$name()]]
        },
        ##' @description
        ##' Checks that the \code{optmiser} associated to a referece
        ##' object of class 'opt' exists.
        ##' 
        ##' @param x reference object of class 'opt'
        ##' @return Returns TRUE if the \code{optmiser} exists, FALSE otherwise.
        opt.exists = function (x) {
            stopifnot(is.opt(x))
            self$opt.name.exists(x$name())
        }#,

        ## ##=======##
        ## ##INUNITS##
        ## ##=======##

        ## ##' @field .n.inunits number of slots allocated for storing \code{inunits}
        ## .n.inunits = NULL,
        ## ##' @field .inunits environment keeping track of the created \code{inunits}
        ## .inunits = NULL,
        ## ##' @description
        ## ##' Creates a new \code{inunit} and return the corresponding
        ## ##' reference object of class 'inunit'
        ## ##'
        ## ##' @param id \code{inunit} id 
        ## ##' @return Returns a reference object of class 'inunit'
        ## new.inunit = function (id) {
        ##     name = self$next.name('inunit')
        ##     stopifnot(!self$inunit.name.exists(name))
        ##     self$.inunits[[name]] = id
        ##     .inunit$new(name)
        ## },
        ## ##' @description
        ## ##' Checks if a \code{inunit} name exists.
        ## ##'
        ## ##' @param name \code{inunit} name
        ## ##' @return Returns TRUE if the name exists, FALSE otherwise
        ## inunit.name.exists = function (name) {
        ##     stopifnot(is.character(name))
        ##     exists(name, envir=self$.inunits)
        ## },
        ## ##' @description
        ## ##' Append a new \code{inunit} to the \code{INUNITS_} array.
        ## ##'
        ## ##' @param infs list of csv file containind the data
        ## ##' @return Returns a refrence object of class 'inunit'.
        ## append.inunit = function (infs) {
        ##     write(c(length(infs), infs), file='data.modello', ncolumns=1)
        ##     id = .intrf.inunit__append()
        ##     file.remove('data.modello')
        ##     self$new.inunit(id)
        ## },
        ## ##' @description
        ## ##' Given a name (character identifier) for an \code{inunit}
        ## ##' creates and returns a reference objects of class 'inunit'.
        ## ##'
        ## ##' @param name \code{inunit} name
        ## ##' @return Returns a object of class 'inunit'
        ## get.inunit = function (name) {
        ##     stopifnot(self$inunit.name.exists(name))
        ##     self$inew.nunit(self$.inunits[[name]])
        ## },
        ## ##' @description
        ## ##' Pops (removes) an \code{inunit} from the \code{INUNITS_} array
        ## ##' according to the provided reference object.
        ## ##'
        ## ##' @param x reference object of class 'inunit'
        ## ##' @return Returns invisible x.
        ## pop.inunit = function (x) {
        ##     stopifnot(is.inunit(x))
        ##     stopifnot(x$is.linked())
        ##     .intrf.inunit__pop(x$id())
        ##     rm(list=x$name(), envir=self$.inunits)
        ##     invisible(x)
        ## },
        ## ##' @description
        ## ##' Given a reference object of class 'inunit', retrives and
        ## ##' returs the id of the associated \code{inunit}
        ## ##' (i.e. its position in the \code{INUINTS_} array.
        ## ##'
        ## ##' @param x reference object of class 'inunit'
        ## ##' @return Returns the \code{inunit} id
        ## inunit.id = function (x) {
        ##     stopifnot(is.inunit(x))
        ##     self$.inunits[[x$name()]]
        ## },
        ## ##' @description
        ## ##' Checks that the \code{inunit} associated to a referece
        ## ##' object of class 'inunit' exists.
        ## ##' 
        ## ##' @param x reference object of class 'inunit'
        ## ##' @return Returns TRUE if the \code{inunit} exists, FALSE otherwise.
        ## inunit.exists = function (x) {
        ##     stopifnot(is.inunit(x))
        ##     self$inunit.name.exists(x$name())
        ## }
        
    )
)

##' Object containing the corrent session.
.modello = .modello.session$new()
