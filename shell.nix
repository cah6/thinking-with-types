let 
  projectDrv = (import ./release.nix { withHoogle = true; } ).project1;
in 
  projectDrv.env