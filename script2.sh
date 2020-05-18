mkdir raw
for file in /home/toni/Desktop/FAKS/wav/*.wav
   do
      if [ -f $file ]
      then
        tail +45c $file > ${file%.*}.raw
      fi
    done
