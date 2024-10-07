#!/usr/bin/bash

if [[ $(pactl get-sink-mute @DEFAULT_SINK@) =~ "yes" ]]; then
	printf "MUTE"
else
	pactl get-sink-volume @DEFAULT_SINK@ | awk 'NR==1{printf $5}'
fi
