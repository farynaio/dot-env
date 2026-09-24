#!/bin/bash

termux-x11 :1 -dpi 96 -xstartup "dbus-launch --exit-with-session icewm-session"

# termux-x11 :1 -dpi 96 -xstartup "dbus-launch --exit-with-session xfce4-session"

# EXWM_START=1 termux-x11 :1 -dpi 96 -xstartup "dbus-launch --exit-with-session emacs"