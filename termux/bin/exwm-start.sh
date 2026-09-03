#!/bin/bash

EXWM_START=1 termux-x11 :0 -dpi 96 -xstartup "dbus-launch --exit-with-session emacs"