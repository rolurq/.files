#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Launch top bar
echo "---" | tee -a /tmp/polybar-top.log
polybar top >>/tmp/polybar-top.log 2>&1 &

echo "Bars launched..."
