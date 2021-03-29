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

getSemitoneDiff = function(note1, note2) {
  noteSemitones = list(
    "A" = 1,
    "A#" = 2,
    "B" = 3,
    "C" = 4,
    "C#" = 5,
    "D" = 6,
    "D#" = 7,
    "E" = 8,
    "F" = 9,
    "F#" = 10,
    "G" = 11,
    "G#" = 12
  )

  note1 = enharmonicNotes %>%
    filter(Note==note1) %>%
    pull(Enharmonic)

  note2 = enharmonicNotes %>%
    filter(Note==note2) %>%
    pull(Enharmonic)

  semitone1 = noteSemitones[[note1]]
  semitone2 = noteSemitones[[note2]]

  semitoneDiff = semitone2 - semitone1

  if (semitoneDiff < 0) {
    semitoneDiff = semitoneDiff + 12
  }

  return (semitoneDiff)
}

getInterval = function(semitoneDiff) {
  interval = list(
    "0" = "U",
    "1" = "m2",
    "2" = "M2",
    "3" = "m3",
    "4" = "M3",
    "5" = "P4",
    "6" = "A4",
    "7" = "P5",
    "8" = "m6",
    "9" = "M6",
    "10" = "m7",
    "11" = "M7"
  )

  return(interval[[as.character(semitoneDiff)]])
}

getSpiciness = function(chord, note) {
  if (chord=="NC" || chord=="") {
    return(NA)
  }
  consonances = getConsonanceLevels(chord)
  root = getChordRoot(chord)
  semitoneDiff = getSemitoneDiff(root, note)

  return(consonances[[as.character(semitoneDiff)]])
}

getSpicinessLevels = function(chord) {
  if (grepl("-", chord)) {
    quality = "min"
  }
  else if (grepl("o", chord)) {
    quality = "dim"
  }
  else if (grepl("[+]", chord)) {
    quality = "aug"
  }
  else
  {
    quality = "maj"
  }

  if (grepl("j7", chord)) {
    seventh="maj"
  }
  else if (grepl("7", chord)) {
    seventh="min"
  }
  else if (quality == "dim") {
    seventh="min"
  }
  else if (quality == "min") {
    seventh="min"
  }
  else {
    seventh="maj"
  }

  consonances = list(
    "0" = 0,
    "1" = 0,
    "2" = 0,
    "3" = 0,
    "4" = 0,
    "5" = 0,
    "6" = 0,
    "7" = 0,
    "8" = 0,
    "9" = 0,
    "10" = 0,
    "11" = 0
  )
  if (quality=="maj") {
    consonances[["1"]] = 3  # m2 chunky boi
    consonances[["2"]] = 1  # just part of the scale but has some tension
    consonances[["3"]] = 2  # blue note - chunky, but common
    consonances[["5"]] = 2  # sus 4, easy resolution
    consonances[["6"]] = 2  # aug 4, generally quite spicy, maj is common tho
    consonances[["8"]] = 3  # m6, over a maj chord, this is a spicy one
    consonances[["9"]] = 1  # M6, same as M2 ay
  }
  else if (quality=="min") {
    consonances[["1"]] = 3  # m2 chunky boi
    consonances[["2"]] = 1  # just part of the scale but has some tension
    consonances[["4"]] = 3  # M3 over the m3 is quite the yuck
    consonances[["5"]] = 1  # sus 4, much less spicy as the 11th
    consonances[["6"]] = 2  # aug 4, generally quite spicy
    consonances[["8"]] = 2  # m6
    consonances[["9"]] = 2  # M6, spooky
  }
  else if (quality=="aug") {
    consonances[["1"]] = 3
    consonances[["2"]] = 2  #
    consonances[["3"]] = 2
    consonances[["4"]] = 1  # part of the chord
    consonances[["5"]] = 2  #
    consonances[["6"]] = 3
    consonances[["7"]] = 3
    consonances[["8"]] = 1  # part of the chord
    consonances[["9"]] = 3
  }
  else if (quality=="dim") {
    consonances[["1"]] = 2  # not as spicy over the diminished chord
    consonances[["2"]] = 3  #
    consonances[["3"]] = 1  # part of the chord
    consonances[["4"]] = 2  #
    consonances[["5"]] = 3  #
    consonances[["6"]] = 1  # part of the chord
    consonances[["7"]] = 2
    consonances[["8"]] = 3  #
    consonances[["9"]] = 1  # part of the chord
  }

  if (seventh=="min") {
    consonances[["10"]] = 1
    consonances[["11"]] = 3
  }
  else if (seventh=="maj") {
    consonances[["11"]] = 1
    consonances[["10"]] = 3
  }

  return(consonances)
}
