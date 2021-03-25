getSnareBeats = function(timeSig) {
  if (timeSig == "4/4") {
    return(c(2, 4))
  }
  else if (timeSig == "3/4") {
    return(3)
  }
  else if (timeSig == "6/8") {
    return(4)
  }
  else if (timeSig == "9/4") {
    return(c(3, 6, 9))
  }
  else if (timeSig == "5/4") {
    return(c(2, 4))
  }
  else if (timeSig == "6/4") {
    return(c(3, 6))
  }
  else {
    return(2)
  }
}
