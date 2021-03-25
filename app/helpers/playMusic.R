createWavFile = function(melid, outDir) {
  soloMetadata = getSoloMetaData(melid)
  soloMelodyNotes = generateMelodyNotes(melid)
  bassNotes = generateBassNotes(melid)
  snareBeats = getSnareBeats(soloMetadata$signature)
  snareNotes = generateSnareNotes(melid, snareBeats=snareBeats)

  bpm = soloMetadata$avgtempo

  melody = getWave(notes_pitch=soloMelodyNotes$Note, tempo=bpm, notes_duration=soloMelodyNotes$duration*bpm/60, 200)
  bass = getWave(notes_pitch=bassNotes$Note, tempo=bpm, notes_duration=bassNotes$duration*bpm/60, 200)
  snare = getWave(notes_pitch=snareNotes$Note, tempo=bpm, notes_duration=snareNotes$duration*bpm/60, 200, wave_func=whiteNoise)

  addToBass = rep(0, max(0, length(melody)-length(bass), length(snare)-length(bass)))
  addToMelody = rep(0, max(0, length(bass)-length(melody), length(snare)-length(melody)))
  addToSnare = rep(0, max(0, length(bass)-length(snare), length(melody)-length(snare)))

  combined = c(melody, addToMelody) + c(bass, addToBass) + c(snare, addToSnare)
  combined = combined * (1 / max(combined, na.rm = T))  # prevent distortion
  # trim zeros
  combined = combined[ min( which ( combined != 0 )) : max( which( combined != 0 )) ]

  save.wave(combined, file.path(outDir, paste0(melid, ".wav")))



  return (file.path(outDir, paste0(melid, ".wav")))
}


playWavFile = function(wavFile) {
  wav = load.wave(wavFile)
  audioInstance = play(wav)

  return (audioInstance)
}
