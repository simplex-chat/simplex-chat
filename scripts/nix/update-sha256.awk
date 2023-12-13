BEGIN {
  print "{"
  loc=""
  ref=""
  isGit=false
}
/source-repository-package/ { loc=""; ref=""; isGit=false; }

/type: git/ { isGit=true; }
/location/ && isGit == true { loc=$2 }
/tag/ && isGit == true { ref=$2 }

isGit == true && loc != "" && ref != "" {
  cmd = "nix-prefetch-git --fetch-submodules --quiet "loc" "ref" | jq -r .sha256"
  cmd | getline sha256
  close(cmd)
  print "  \""loc"\".\""ref"\" = \""sha256"\";";
  isGit=false; loc=""; ref="";
}

END {
  print "}"
}
