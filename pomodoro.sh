#!/bin/bash

WORK=25
BREAK=3
LONGBREAK=15
BEEPS=7

function alarm {
    beep -r ${BEEPS} -f 1000 -d 750
}

function deadline {
    date -d "+ ${1} minutes" "+%H:%M"
}

function notification {
    if [ -x $(command -v notify-send) ]; then
        notify-send "${1}"
    fi
}

function one_pomodoro {
    echo -n "${1} until "; deadline ${!1}
    sleep ${!1}m
    echo "${1} is over!"
    notification "${1} is over!"
    alarm
}


clear
while true
do
    one_pomodoro "WORK"
    one_pomodoro "BREAK"
done
