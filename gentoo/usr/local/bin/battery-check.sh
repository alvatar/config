#!/bin/sh

while true; do

  RAWLINE=$(acpitool | head -n 1)
  BATTERY_STATUS=$(echo $RAWLINE | cut -d',' -f1  | cut -d' ' -f4 )
  PERCENTAGE=$(echo $RAWLINE | cut -d',' -f2 | cut -f1 -d".")

  echo Battery: $BATTERY_STATUS \(~$PERCENTAGE% \)

  if [ "$BATTERY_STATUS" == "Discharging" ]; then
    if (($PERCENTAGE < 15)); then
      if (($PERCENTAGE < 5)); then
        notify-send "BATTERY ALERT" "Battery is below 5%"
        sleep 60 # 1 min
      else
        notify-send "BATTERY ALERT" "Battery is below 15%"
        sleep 360 # 6 mins
      fi
    else
      sleep 720 # 12 mins
    fi
  else
    sleep 1200 # 20 mins
  fi
done
