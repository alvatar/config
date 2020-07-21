#!bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# The tricky way to get find's output into an array
unset files i
while IFS= read -r -d $'\0' file; do
  files[i++]="$file"
  REF_FILE="${file//$DIR\/nixos\/}"
  echo "copy: /$REF_FILE -> $DIR/nixos/$REF_FILE"
  cp "/$REF_FILE" "$DIR/nixos/$REF_FILE"
done < <(find $DIR/nixos -type f -print0)
