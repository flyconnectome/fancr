## code to prepare `BANC` dataset goes here
banc.mesh.nl=fafbseg::read_cloudvolume_meshes(1,
  cloudvolume.url = 'precomputed://gs://lee-lab_brain-and-nerve-cord-fly-connectome/volume_meshes')
banc.mesh.nl.orig=banc.mesh.nl
banc.mesh.nl[[1]]=Rvcg::vcgUpdateNormals(banc.mesh.nl[[1]])
nat::write.neurons(banc.mesh.nl, format = 'ply', dir='.', Force = T)


# the processing in meshlab ...
# Opened mesh /Users/jefferis/dev/R/fancr/1.ply in 205 msec
# Applied filter HC Laplacian Smooth in 154 msec
# PostSimplification Cleaning: Removed 135 unreferenced vertices
# Applied filter Simplification: Quadric Edge Collapse Decimation in 3720 msec
# Started Mode Select Connected Components in a region
# Applied filter Invert Selection in 7 msec
# Applied filter Delete Selected Vertices in 12 msec
# Deleted 2506 faces, 1319 vertices.
# Saved Mesh /Users/jefferis/dev/R/fancr/1_100k.ply in 71 msec


ply=read.neurons('1_100k.ply')
ply.i=read.neurons('1_100k_inv.ply')
nopen3d()
nclear3d()
shade3d(ply[[1]], col='grey')

# eventually chose the one with inverted normals (pointing out)
BANC.surf <- as.hxsurf(ply.i[[1]])
usethis::use_data(BANC.surf, overwrite = TRUE)


nclear3d()


ply2=as.hxsurf(ply[[1]])
nclear3d()
plot3d(ply2)
