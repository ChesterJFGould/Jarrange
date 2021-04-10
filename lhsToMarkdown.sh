#! /bin/sh

cat $1 \
| sed '/{-/,/-}/s/^/-- /g' \
| sed 's/^-- {-//g;s/^-- -}//g' \
| sed -E 's/^([^-])/> \1/g' \
| sed 's/-- //g' \
| pandoc -f markdown+lhs -t markdown \
| sed 's/``` {.haskell .literate}/```haskell/g' > $2
