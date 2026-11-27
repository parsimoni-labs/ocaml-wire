prove.exe prune keeps the cache entries a run listed and removes the rest:
another F* version, the 3d.exe output of a .3d that changed, a module no
codec has any more.

  $ mkdir -p cache/v1/3d/Tiny-1-10 cache/v1/3d/Tiny-2-10 cache/v0/3d/Tiny-1-10
  $ touch cache/v1/Tiny.Scan.fst.checked cache/v1/Gone.Scan.fst.checked
  $ touch cache/v1/3d/Tiny-1-10/Tiny.fst cache/v1/3d/Tiny-2-10/Tiny.fst
  $ touch cache/v0/Tiny.Scan.fst.checked
  $ cat > used.Tiny <<'EOF2'
  > v1/3d/Tiny-2-10
  > v1/Tiny.Scan.fst.checked
  > EOF2
  $ ./prove.exe prune cache used.Tiny
  $ find cache | sort
  cache
  cache/v1
  cache/v1/3d
  cache/v1/3d/Tiny-2-10
  cache/v1/3d/Tiny-2-10/Tiny.fst
  cache/v1/Tiny.Scan.fst.checked

A cache that does not exist yet has nothing to prune.

  $ ./prove.exe prune absent used.Tiny
