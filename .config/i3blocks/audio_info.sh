#!/usr/bin/env bash

if [[ $(pactl get-sink-mute @DEFAULT_SINK@) =~ "yes" ]]; then
	printf "MUTE\n"
else
	pactl get-sink-volume @DEFAULT_SINK@ | awk 'NR==1{printf $5"\n"}'
fi
