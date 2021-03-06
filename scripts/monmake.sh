#!/bin/sh
while file=$(inotifywait -r -q -e modify src); do
  ext=${file##*.}
  # only recompile if some ocaml source file changes
  if [ $ext != "ml" ] && [ $ext != "mli" ] && [ $ext != "mll" ] && [ $ext != "mly" ]; then
    continue
  fi
  clear
  make
  if [ $? -eq 0 ]; then
    clear
    notify-send "Build ok!"
    #./test.sh file
    ./scripts/regression.sh ${1-"file"}
    if [ $? -eq 0 ]; then
      paplay /usr/share/sounds/freedesktop/stereo/complete.oga
    else
      : # paplay /usr/share/sounds/freedesktop/stereo/bell.oga
    fi
  else
    # TODO find a way to save stdout to var and still print it to avoid calling make again just for the error message
    msg=$(make)
    # send error message to sublime plugin using netcat
    echo -e "`pwd`\n$msg" | nc localhost 9999
    # notify-send -i stop Build failed!
    paplay /usr/share/sounds/freedesktop/stereo/bell.oga
  fi
done
