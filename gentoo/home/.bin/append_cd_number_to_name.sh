#!/bin/bash
echo "Write cd number: "
read prefix

for file in *.{mp3,flac,ape}
	do newfile=`echo $file | awk -F" " '{print $0}'`
	newfile=$prefix" - "$newfile
	echo $file "->" $newfile
	eval mv \"$file\" \"$newfile\"
done
