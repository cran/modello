##' R6 class representing an array dataset
##'
##' It loads the data from an R array
##' @author Filippo Monari
##' @export
DataSet.Array = R6Class(
    "DataSet.Array",
    public = list(
        ##' @description
        ##' Initialisation method.
        ##'
        ##' @param X array
        ##' @param indexes subset of indexes along the sliceing dimension
        ##' @param yi subset of indexes to return as targets
        ##' @param si index of slicing dimension
        initialize = function (X, indexes, yi, si) {
            private$.X = X
            private$.indexes0 = indexes
            private$.yi = yi
            private$.si = si
            private$.data.dim = dim(X)[-si]
            private$.slices = slice.index(X, si)
            self$reset()
        },
        ##' @description
        ##' Returns the length of the dataset
        ##' @examples
        ##' X = matrix(rnorm(100), 20)
        ##' ds = DataSet.Array$new(X, 1:20, 1, 1)
        ##' ds$length() # 20
        length = function () {
            if (is.null(private$.indexes0)) {
                nrow(private$.X)
            } else {
                length(private$.indexes0)
            }
        },
        ##' @description
        ##' Returns TRUE if the dataset has still data
        ##' @examples
        ##' X = matrix(rnorm(100), 20)
        ##' ds = DataSet.Array$new(X, 1:20, 1, 1)
        ##' ds$has.next() # TRUE
        has.next = function () {
            length(private$.indexes) > 0
        },
        ##' @description
        ##' Reset the dataset to its initial state
        ##' @examples
        ##' X = matrix(rnorm(100), 20)
        ##' ds = DataSet.Array$new(X, 1:20, 1, 1)
        ##' ds$..get.data..()
        ##' ds$..get.data..()
        ##' ds$reset()
        ##' ds$..get.data..()
        reset = function () {
            if (is.null(private$.indexes0)) {
                private$.indexes = 1:self$length()
            } else {
                private$.indexes = private$.indexes0
            }
        },
        ##' @description
        ##' Auxiliary method not to be called directly.
        ##' It pops an index from the dataset according to the given method
        ##'
        ##' @param mode popping mode
        ..pop.index.. = function (mode="head") {
            stopifnot(self$has.next())
            if (mode == "head") {
                i = 1
            } else if (mode == "tail") {
                i = length(private$.indexes)
            } else if (mode == "random") {
                i = sample(1:length(private$.indexes), 1)
            } else {
                stop("Unkwown 'mode'.")
            }
            index = private$.indexes[i]
            private$.indexes = private$.indexes[-i]
            return(index)
        },
        ##' @description
        ##' Auxiliary method not to be called directly.
        ##'
        ##' @param mode popping mode
        ..get.data.. = function (mode="head") {
            iii = which(private$.slices == self$..pop.index..(mode))
            ANS = private$.X[iii]
            dim(ANS) = private$.data.dim
            return(ANS)
        },
        ##' @description
        ##' Method to feed the bathces of data to 'numbers'.
        ##'
        ##' @param x number
        ##' @param mode popping mode
        ##' @examples
        ##' modello.init(100, 100, 100, 100)
        ##' X = matrix(rnorm(100), 20)
        ##' ds = DataSet.Array$new(X, 1:20, 1, 1)
        ##' x = number(rnorm(5))
        ##' print(x$v)
        ##' ds$feed(x)
        ##' print(x$v)
        ##' modello.close()
        feed = function (x, mode="head") {
            stopifnot(all(dim(x) == private$.data.dim))
            x$set.v(self$..get.data..(mode))
        }
    ),
    private = list(
        .X = NULL,
        .indexes0 = NULL,
        .indexes = c(),
        .yi = NULL,
        .si = NULL,
        .data.dim = NULL,
        .slices = NULL
    )
)
        
##' R6 class representing a LaF csv data loader
##'
##' It uses the capability of the LaF package to load
##' sequentially batch of data from a csv file.
##' @author Filippo Monari
##' @export
DataSet.CSV = R6Class(
    'DataSet.CSV',
    public = list(
        ##' @field df stores the batch of data read from the csv file 
        df = NULL,
        ##' @description
        ##' Initialisation method.
        ##'
        ##' @param inf path to file or a LaF csv data model.
        ##' @param n number of rows to read ecah time from the csv file
        ##' @param yi indexes of the output variables
        ##' @param si index of slicing dimension after feature engineering
        ##' @param ... additional arguments for the
        ##' \code{LaF::detect_dm_csv} function 
        initialize = function (inf, n, yi, ...) {
            if (is.character(inf)) {
                private$.dm = detect_dm_csv(inf, ...)
            } else if (islist(inf)) {
                private$.dm = inf
            } else {
                stop("'inf' must be the path to the csv file or a LaF csv data model.")
            }    
            private$.nrows = n
            private$.yi = yi
        },
        ##' @description
        ##' Opens the connection with the csv file.
        ##'
        ##' @param ... additional arguments for the
        ##' \code{LaF::laf_open} function
        ..connect.. = function (...) {
            private$.conn = laf_open(private$.dm, ...)
        },
        ##' @description
        ##' Return a batch of data of the wanted size.
        ##'
        ##' @param n size of the batch of data
        ##' @param ... additional arguments for the process fucntion
        ..get.data.. = function () {
            if (is.null(self$df) || nrow(self$df) < private$.nrows) {
                private$.load()
            }
            df = self$df[1:private$.nrows,]
            self$df = self$df[-(1:private$.nrows),]
            df
        },
        ##' @description
        ##' Feed a number with the data from the CSV file
        ##' Returns X invisibly.
        ##'
        ##' @param data list composed by X (containing the features) and Y (containing the targets).
        ##' If NULL a the list is created. If provided the numbers in ti will be fed in place. 
        ##' @param ... additional arguemnts to be passed to 
        feed = function (data=NULL, ...) {
            DATA = as.matrix(self$..get.data..())
            if (is.null(X)) {
                data = list(X=number(DATA[,-yi], dx=FALSE), Y=number(DATA[,yi], dx=FALSE))
            } else {
                data$X$set.v(DATA[,-yi])
                data$X$set.v(DATA[,yi])
            }
            invisible(data)
        },
        ##' @description
        ##' Returns the data model.
        dm = function () {
            private$.dm
        }
    ),
    private = list(
        .dm = NULL,
        .conn = NULL,
        .nrows = NULL,
        .load = function () {
            df = next_block(private$.conn, nrows=private$.nrows)
            if (nrow(df) < private$.nrows) {
                begin(private$.conn)
                df = rbind(df, next_block(private$.conn, nrows=private$.nrows))
            }
            self$df = rbind(self$df, df)
        }
    )
)
##' R6 class representing a cluster LaF csv data loaders
##'
##' It load batch of data from a set of csv files and
##' binds them together.
##' @author Filippo Monari
##' @export
DataLoader.CSV = R6Class(
    'DataLoader.CSV',
    public = list(
        ##' @description
        ##' Initialisation method
        ##'
        ##' @param loaders a list containing the 'CSVLoader.LaF' objects. 
        initialize = function (loaders) {
            private$.loaders = loaders
        },
        ##' @description
        ##' Calls the connect methods of the loadres in order to
        ##' open the connections with the csv files
        ##'
        ##' @param ... additional argument for the \code{connect}
        ##' methods of each loader.
        ..connect.. = function (...) {
            for (ld in private$.loaders) ld$connect(...)
        },
        ##' @description
        ##' Returns the binded batch of data after applying the
        ##' \code{self$process} function.
        ##'
        ##' @param n number of rows to draw from each csv file
        ##' @param ... additional arguments for the \code{self$process} function
        ..get.data.. = function (n, ...) {
            foreach(ld = private$.loaders, .combine=rbind) %dopar% ld$..get.data..() 
        },
        ##' @description
        ##' Feed a number with the data from the CSV file
        ##' Returns X invisibly.
        ##'
        ##' @param data list composed by X (containing the features) and Y (containing the targets).
        ##' If NULL a the list is created. If provided the numbers in ti will be fed in place. 
        ##' @param ... additional arguemnts to be passed to 
        feed = function (data=NULL, ...) {
            DATA = as.matrix(self$..get.data..())
            if (is.null(X)) {
                data = list(X=number(DATA[,-yi], dx=FALSE), Y=number(DATA[,yi], dx=FALSE))
            } else {
                data$X$set.v(DATA[,-yi])
                data$X$set.v(DATA[,yi])
            }
            invisible(data)
        },
        ##' @description
        ##' Returns a list with the loaders.
        loaders = function () {
            private$.loaders
        }
    ),
    private = list(
        .loaders = NULL
    )
)
    
##' R6 class representign a dataset of sequences to feed RNN
##'
##' It load the data from a R array
##' @author Filippo Monari
##' @export
DataSet.Seq = R6Class(
    inherit = DataSet.Array,
    public = list(
        ##' @description
        ##' Initialisation method
        ##'
        ##' @param X matrix/array
        ##' @param ls integer, sequence length
        ##' @param yi integer vector, subset of indices to return a targets
        ##' @param indexes integer vector, a subset of indexes
        ##' indicating the starting point fir the sequences
        initialize = function (X, ls, yi, indexes=NULL) {
            private$.X = X
            private$.ls = ls
            private$.yi = yi
            private$.indexes0 = indexes
            self$reset()
        },
        ##' @description
        ##' Returns the length of the dataset
        ##' @examples
        ##' ds = DataSet.Seq$new(matrix(rnorm(100), 20), 5, 1)
        ##' ds$length() # 20
        length = function () {
            if (is.null(private$.indexes0)) {
                nrow(private$.X) - private$.ls
            } else {
                length(private$.indexes0)
            }
        },
        ##' @description
        ##' Feeds the data into an existing seqience or creating a new one
        ##'
        ##' @param x existing sequence, if NULL it creates a new one
        ##' @param mode chectacter indicating the way to build the sequences
        ##' @examples
        ##' modello.init(100, 100, 100, 100)
        ##' ds = DataSet.Seq$new(matrix(rnorm(100), 20), 5, 1)
        ##' SEQ = ds$feed(NULL)
        ##' print(names(SEQ))
        ##' print(SEQ$x[[1]]$v)
        ##' print(SEQ$y$v)
        ##' ds$feed(SEQ)
        ##' print(SEQ$x[[1]]$v)
        ##' print(SEQ$y$v)
        ##' modello.close()
        feed = function (x, mode="head") {
            if (is.null(x)) {
                ANS = self$..get.data..(mode)
                x = lapply(ANS$x, function(x) number(as.matrix(x), dx=FALSE))
                y = number(as.matrix(ANS$y), dx=FALSE)                           
                return(list(x=x, y=y))
            }
            x$y$set.v(self$..feed..(x$x, mode))
            return(invisible(x))
        },
        ##' @description
        ##' Sets the data loader back of n previous states.
        ##' Only available for "head" and "tail" modes.
        ##'
        ##' @param n number of step to rewind
        ##' @param mode rewind mode. Possibly the same as the feed mode.
        ##' @examples
        ##' ds = DataSet.Seq$new(matrix(rnorm(100), 20), 5, 1)
        ##' ds$..get.data..("head")
        ##' ds$..get.data..("head")
        ##' ds$rewind(2)
        ##' ds$..get.data..("head")
        ##' ds$..get.data..("head")
        rewind = function (n, mode="head") {
            stopifnot(length(private$.tape) >= n)
            for (i in 1:n) {
                if (mode == "head") {
                    private$.indexes = c(private$.tape[1], private$.indexes)
                    private$.tape = private$.tape[-1]
                } else if (mode == "tail") {
                    private$.indexes = c(private$.indexes, private$.tape[1])
                    private$.tape = private$.tape[-1]
                } else {
                    stop("Unkown mode. Rewind is only allowed for 'head' or 'tail'")
                }
            }
        },           
        ##' @description
        ##' Auxiliary method not to be called directly.
        ##' It pops an index from the dataset according to the given method
        ##'
        ##' @param mode popping mode
        ..pop.index.. = function (mode="head") {
            stopifnot(self$has.next())
            if (mode == "head") {
                i = 1
                private$.tape = c(private$.tape, private$.indexes[i])
            } else if (mode == "tail") {
                i = length(private$.indexes)
                private$.tape = c(private$.tape, private$.indexes[i])
            } else if (mode == "random") {
                i = sample(1:length(private$.indexes), 1)
            } else {
                stop("Unkwown 'mode'.")
            }
            index = private$.indexes[i]
            private$.indexes = private$.indexes[-i]
            
            return(index)
        },
        ##' @description
        ##' Auxiliary method. Not to be called directly.
        ##'
        ##' @param x existing sequence, if NULL it creates a new one
        ##' @param mode chectacter indicating the way to build the sequences
        ..feed.. = function (x, mode) {
            stopifnot(length(x) == private$.ls)
            n = private$.ls - 1
            a = self$..pop.index..(mode)
            b = a + n
            for (i in a:b) {
                x[[1]]$set.v(private$.X[i,])
                x = x[-1]
            }
            return(private$.X[b+1,private$.yi])
        },
        ##' @description
        ##' Auxiliary method. Not to be called directly.
        ##'
        ##' @param mode chectacter indicating the way to build the sequences
        ..get.data.. = function (mode) {
            n = private$.ls - 1
            a = self$..pop.index..(mode)
            b = a + n
            list(x=lapply(a:b, function(i)private$.X[i,]),
                 y=private$.X[b+1,private$.yi])
        }
    ),
    private = list(
        .X = NULL,
        .ls = NULL,
        .yi = NULL,
        .indexes0 = NULL, 
        .indexes = c(),
        .tape = c()
    )
)

##' R6 class representign a data loader for sequence datasets.
##'
##' It loads the data from a R array
##' @author Filippo Monari
##' @export
DataLoader.Seq = R6Class(
    public = list(
        ##' @description
        ##' Initialisation method
        ##'
        ##' @param ds sequence datast
        ##' @param bz integer, batch size
        initialize = function (ds, bz) {
            private$.ds = ds
            private$.bz = bz
            self$reset()
        },
        ##' @description
        ##' Returns the length of teh data loader object
        ##' @examples
        ##' ds = DataSet.Seq$new(matrix(rnorm(100), 20), 5, 1)
        ##' dl = DataLoader.Seq$new(ds, 5)
        ##' ds$length()
        ##' dl$length()
        length = function () {
            floor(private$.ds$length() / private$.bz)
        },
        ##' @description
        ##' Returns TRUE if the data loader has still batches available
        ##' @examples
        ##' ds = DataSet.Seq$new(matrix(rnorm(100), 20), 5, 1)
        ##' dl = DataLoader.Seq$new(ds, 5)
        ##' ds$has.next() # TRUE
        has.next = function () {
            private$.batchi < self$length()
        },
        ##' @description
        ##' Resets the data loader to its initial state
        reset = function () {
            private$.ds$reset()
            private$.batchi = 0
        },
        ##' @description
        ##' Feeds the data into a sequence of  batches of sequences or it creates a new one
        ##'
        ##' @param SEQ batch of sequences. If NULL it creates a new one
        ##' @param mode feeding mode
        ##' @examples
        ##' modello.init(100, 100, 100, 100)
        ##' ds = DataSet.Seq$new(matrix(rnorm(100), 20), 5, 1)
        ##' dl = DataLoader.Seq$new(ds, 5)
        ##' SEQ = dl$feed(NULL)
        ##' print(SEQ)
        ##' print(SEQ$X[[1]][[1]]$v)
        ##' dl$feed(SEQ)
        ##' print(SEQ$X[[1]][[1]]$v)
        ##' modello.close()
        feed = function (SEQ, mode="head") {
            stopifnot(self$has.next())
            if (is.null(SEQ)) {
                ANS = private$.feed0(mode)
            } else {
                stopifnot(length(SEQ$X) == private$.bz)
                SEQ$Y$set.v(sapply(SEQ$X, function(x)private$.ds$..feed..(x, mode)))
                ANS = SEQ
            }
            private$.batchi = private$.batchi + 1
            invisible(ANS)
        }
    ),
    private = list(
        .ds = NULL,
        .bz = NULL,
        .batchi = NULL,
        .feed0 = function (mode="head") {
            ANS = lapply(1:private$.bz, function(i) private$.ds$..get.data..(mode))
            X = lapply(ANS, function (x) {
                lapply(x$x, function (xx) {
                    number(as.matrix(xx), dx=FALSE)
                })
            })
            Y = number(sapply(ANS, function(x)x$y), dx=FALSE)
            list("X"=X, "Y"=Y)
        }
    )
)

