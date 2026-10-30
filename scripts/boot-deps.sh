#!/bin/sh
# Prints make rules for the boot files: the fasl of a boot file depends on the fasls
# of the boot files its `|#%require|` header imports with `(boot X)` or `(only (boot X) ...)`.
#
#   scripts/boot-deps.sh lib/boot/*.scm > boot-deps.mk
for f in "$@"; do
  file=${f#lib/boot/}; file=${file%.scm}
  deps=$(awk '/^\(\|#%require\|/ {on=1} /^\(\|#%provide\|/ {on=0} on' "$f" \
    | sed -e 's/;.*$//' \
    | tr '\n' ' ' \
    | sed -e 's/(late (boot [^) ]*)//g' \
    | grep -o '(boot [^) ]*)' | sed -e 's/(boot \(.*\))/\1/' | sort -u)
  for d in $deps; do
    [ "$d" != "$file" ] && [ -f "lib/boot/$d.scm" ] && echo "\$(OUT)/boot/$file.fasl: \$(OUT)/boot/$d.fasl"
  done
done
exit 0
