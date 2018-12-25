#' The constructor for qfifo
#' @return An S3 object of class fifoq
#' @examples
#' q <- qfifo()
#' @export
qfifo <- function(){
  structure(list(data=list()), class ="qfifo")
}


#' Add a value to the queue
#'
#' @param q is the current fifo queue object
#' @param val is the value to be added to the queue
#'
#' @return The updated queue object
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
add <- function(q, val) {
  UseMethod("add")
}

#' Gets the length of the queue
#'
#' @param q is the current fifo queue object
#'
#' @return The length of the queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
#' q <- get_length(q)
get_length <- function(q){
  UseMethod("get_length")
}

#' @export
get_length.qfifo <- function(q) {
  # Used to get the length of the queue
  length(q$data)
}

#' @export
add.qfifo <- function(q, val) {
  #add the new value at the end of qfifo
  q$data[get_length(q)+1] <- val
  q
}

#' Return the top value in the queue
#'
#' @param q is the current fifo queue object
#'
#' @return The top of the queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
#' v <- top(q)
top <- function(q) {
  UseMethod("top")
}

#' @export
top.qfifo <- function(q) {
  if(get_length(q) == 0) #check if there are no elements in the queue
    stop("No elements on the queue")
  vec <-  q$data
  q$data[[1]]
}

#' Delete the top element from teh queue
#'
#' @param q is the current fifo queue object
#'
#' @return The modified queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
#' q <- add(q,5678)
#' q <- process(q)
process <- function(q) {
  UseMethod("process")
}

#' @export
process.qfifo <- function(q) {
  if (get_length(q) == 0) #check before deleting any elements in the queue
    stop("No elements on the queue to delete")
  q$data[1] <- NULL # setting the first element in the queue to null
  q
}

