ifElseNull <- function(bool, value1, value2=NULL) {
    if (bool) {
        return(value1)
    } else {
        return(value2)
    }
}
