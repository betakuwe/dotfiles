#!/usr/bin/bash

if [[ $(pactl get-source-mute @DEFAULT_SOURCE@) =~ "yes" ]]; then
	printf "MUTE"
else
	pactl get-source-volume @DEFAULT_SOURCE@ | awk 'NR==1{printf $5}'
fi
