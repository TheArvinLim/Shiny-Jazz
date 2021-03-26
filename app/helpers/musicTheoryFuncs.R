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
  if (is.na(key) || key == "") {
    key = "C"
  }

  if (note =="X" || length(note)==0) {
    return(note)
  }

  octave = substr(note, nchar(note), nchar(note))

  if (suppressWarnings(is.na(as.numeric(octave)))) {
    octave = ""
  }
  else {
    note = substr(note, 1, max(1, nchar(note)-1))
  }

  enharmonic = enharmonicNotes %>%
    filter(Note %in% scaleDegrees[[key]]) %>%
    filter(Enharmonic==note) %>%
    pull(Note)

  if (length(enharmonic)==0) {
    return(note)
  }
  return(paste0(enharmonic, octave))
}

getChordRoot = function(chordName) {
  root = substr(chordName, 1, 2)
  if (!(root %in% enharmonicNotes$Note)) {
    root = substr(chordName, 1, 1)
  }

  return(root)
}

getInterval = function(note1, note2) {
  degree = scaleDegrees[c("Degree", note1)] %>%
    filter(get(note1) == note2) %>%
    pull(Degree)

  return(degree)
}
