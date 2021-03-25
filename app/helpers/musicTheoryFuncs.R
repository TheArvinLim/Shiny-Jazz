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

convertToKeyEnharmonic = function(note, key) {
  if (note =="X" || length(note)==0) {
    return(note)
  }

  octave = substr(note, nchar(note), nchar(note))
  note = substr(note, 1, nchar(note)-1)

  enharmonic = enharmonicNotes %>%
    filter(Note %in% scaleDegrees[[key]]) %>%
    filter(Enharmonic==note) %>%
    pull(Note)

  if (length(enharmonic)==0) {
    return(note)
  }
  return(paste0(enharmonic, octave))
}
