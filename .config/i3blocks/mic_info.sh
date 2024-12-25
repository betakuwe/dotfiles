#!/usr/bin/env bash

if [[ $(pactl get-source-mute @DEFAULT_SOURCE@) =~ "yes" ]]; then
	printf "MUTE\n"
else
	pactl get-source-volume @DEFAULT_SOURCE@ | awk 'NR==1{printf $5"\n"}'
fi
