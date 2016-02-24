#Test if num is not numeric  or integer
.testNumeric <- function (num, arg = NULL)
{
  if (is.null(arg)) {
    arg <- substitute(num)
  } else {
    stopifnot(is.character(arg) && length(arg) == 1)
  }
  
  if (!class(num) %in% c("numeric","integer")) {
    stop(paste0(arg," must be of class 'numeric' instead of '", class(num), "'"))
  } else {}
  
  TRUE
}

#Test if num is not logical
.testLogical <- function (logi, arg = NULL)
{
  if (is.null(arg)) {
    arg <- substitute(logi)
  } else {
    stopifnot(is.character(arg) && length(arg) == 1)
  }
  
  if (!class(logi) %in% c("logical")) {
    stop(paste0(arg," must be of class 'logical' instead of '", class(logi), "'"))
  } else {}
  
  TRUE
}

.testCharacter <- function(char, arg = NULL)
{
  if (is.null(arg)) {
    arg <- substitute(char)
  } else {
    stopifnot(is.character(arg) && length(arg) == 1)
  }
  
  if (!class(char) %in% c("character","factor")) {
    stop(paste0(arg," must be of class 'character', instead of '", class(char), "'"))
  } else {}
  
  TRUE
}


#Test if num<binf or/and num>bsup
.testInterval <- function(num, binf, bsup, arg = NULL)
{
  .testNumeric(num = num, arg = arg)
  
  if (is.null(arg)) {
    arg <- substitute(num)
  } else {
    stopifnot(is.character(arg) && length(arg) == 1)
  }
  
  if (!missing(binf)) {
    stopifnot(is.numeric(binf) && length(binf) == 1)
    if (any(num < binf)) {
      stop(paste0(arg, " must be greater than ", binf))
    } else {}
  } else {}
  
  if (!missing(bsup)) {
    stopifnot(is.numeric(bsup) && length(bsup) == 1)
    if (any(num > bsup)) {
      stop(paste0(arg, " must be lower than ", bsup))
    } else {}
  } else {}
  
  TRUE
}


#Test if length(param)%in%len
.testLength <- function (param, len, arg = NULL)
{
  if (is.null(arg)) {
    arg <- substitute(param)
  } else {
    stopifnot(is.character(arg) && length(arg) == 1)
  }
  
  stopifnot(is.numeric(len))
  
  if (length(param) < min(len) || length(param) > max(len)) {
    stop(paste0("length of ", arg, " must be ",
                ifelse(length(len) == 1, "", "in ["),
                paste(len, collapse = ", "),
                ifelse(length(len) == 1, "", "]"),
                ", currently ", length(param)))
  } else {}
  
  TRUE
}

##Test if all elemnt of vect are in control
.testIn <- function (vect, control, arg = NULL)
{
  if (is.null(arg)) {
    arg <- substitute(vect)
  } else {
    stopifnot(is.character(arg) && length(arg) == 1)
  }
  
  if (!all(vect %in% control)) {
    stop(paste0("Element(s) of ", arg," are not in [",paste(control, collapse = ", "), "]"))
  } else {}
  
  TRUE
}

#Test numeric and length 1
.testNumericLength1 <- function (num)
{
  arg <- as.character(substitute(num))
  .testLength(param = num, len = 1, arg = arg)
  .testNumeric(num = num, arg = arg)
  
  TRUE
}

#Test character and length 1
.testCharacterLength1 <- function (char)
{
  arg <- as.character(substitute(char))
  .testLength(param = char, len = 1, arg = arg)
  .testCharacter(char = char, arg = arg)
  
  TRUE
}


#Test logical and length 1
.testLogicalLength1 <- function (logi)
{
  arg <- as.character(substitute(logi))
  .testLength(param = logi, len = 1, arg = arg)
  .testLogical(logi = logi, arg = arg)
  
  TRUE
}
