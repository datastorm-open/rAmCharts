#Test if num is not numeric  or integer
.testNumeric <- function (num, arg = NULL)
{
  if (is.null(arg)) {
    arg <- substitute(num)
  } else {}
  
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
  } else {}
  
  if (!class(logi) %in% c("logical")) {
    stop(paste0(arg," must be of class 'logical' instead of '", class(logi), "'"))
  } else {}
  
  TRUE
}



.testCharacter <- function(char, arg = NULL)
{
  if(is.null(arg)) {
    arg <- substitute(char)
  } else {}
  
  if (!class(char) %in% c("character","factor")) {
    stop(paste0(arg," must be of class 'character', instead of '", class(char), "'"))
  } else {}
  
  TRUE
}


#Test if num<binf or/and num>bsup
.testInterval <- function(num, binf = NULL, bsup = NULL, arg = NULL)
{
  if (is.null(arg)) {
    arg <- substitute(num)
  } else {}
  
  if (!is.null(binf)) {
    if (num < binf) {
      stop(paste0(arg, " < ",binf, " must be upper"))
    } else {}
  } else {}
  
  if (!is.null(bsup)) {
    if (num > bsup) {
      stop(paste0(arg, " > ", bsup, " must be lower"))
    } else {}
  } else {}
  
  TRUE
}


#Test if length(param)%in%len
.testLength <- function (param, len = NULL, arg = NULL)
{
  if (is.null(arg)) {
    arg <- substitute(param)
  } else {}
  
  if (!is.null(len)) {
    if (!length(param) %in% len) {
      stop(paste0("length of ", arg, " must be", ifelse(length(len)==1, " ", " in "),
                  paste(len, collapse = ", "), " currently ", length(param)))
    } else {}
  } else {}
  
  TRUE
}

#Test numeric and length 1
.testNumericLength1 <- function (num)
{
  arg <- substitute(num)
  .testLength(param = num, len = 1, arg = arg)
  .testNumeric(num = num, arg = arg)
  
  TRUE
}

#Test character and length 1
.testCharacterLength1 <- function (char)
{
  arg <- substitute(char)
  .testLength(param = char, len = 1, arg = arg)
  .testCharacter(char = char, arg = arg)
  
  TRUE
}


#Test logical and length 1
.testLogicalLength1 <- function (logi)
{
  arg <- substitute(logi)
  .testLength(param = logi, len = 1, arg = arg)
  .testLogical(logi = logi, arg = arg)
  
  TRUE
}


##Test if all elemnt of vect are in control
.testIn <- function (vect,control = NULL)
{
  if (!is.null(control)) {
    arg <- substitute(vect)
    
    if (length(which(vect %in% control)) != length(vect)) {
      stop(paste0("Element(s) of ", arg," are not in ",paste(control, collapse = ", ")))
    } else {}
  } else {}
  
  TRUE
}

