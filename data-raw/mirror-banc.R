# construct a mirroring registration from landmarks
mirror_banc_tps <- function(anns) {
  if(is.character(anns)) {
    anns=ngl_annotations(anns)
  }
  AB=rbind(banc_raw2nm(anns$pointA), banc_raw2nm(anns$pointB))
  BA=rbind(banc_raw2nm(anns$pointB), banc_raw2nm(anns$pointA))
  # have decided to use this rather than "official" space
  BANCmesh=nat.templatebrains::templatebrain("BANC",
    BoundingBox = structure(c(79392.9, 966179.9, 35524.5, 1131169.6, -62.8, 315466.7), dim = 2:3, class = "boundingbox"))
  ABf=nat.templatebrains::mirror_brain(AB, brain = BANCmesh, mirrorAxis = 'X', transform = 'flip')
  delta=BA-ABf
  tpsreg(sample = ABf, reference = BA)
}
mirror_banc_lm=mirror_banc_tps('https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/5486159028289536')

usethis::use_data(mirror_banc_lm, overwrite = TRUE)
