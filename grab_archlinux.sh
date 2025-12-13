#!bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# The tricky way to get find's output into an array
unset files i
while IFS= read -r -d $'\0' file; do
  files[i++]="$file"
  REF_FILE="${file//$DIR\/archlinux\/}"
  echo "/$REF_FILE" -> "$DIR/archlinux/$REF_FILE"
  cp "/$REF_FILE" "$DIR/archlinux/$REF_FILE"
done < <(find $DIR/archlinux -type f -print0)
