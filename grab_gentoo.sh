#!bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# The tricky way to get find's output into an array
unset files i
while IFS= read -r -d $'\0' file; do
  files[i++]="$file"
  REF_FILE="${file//$DIR\/gentoo\/}"
  echo "/$REF_FILE" "$DIR/gentoo/$REF_FILE"
done < <(find $DIR/gentoo -type f -print0)
