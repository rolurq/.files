#!/bin/bash

cp $HOME/{.bashrc,.gitconfig,.vimrc} .
cp -r $HOME/{.i3,.xmonad} .
cp -r $HOME/.config/{dunst,rofi,polybar} .config

# Check git status
gs="$(git status | grep -i "modified")"

# If there is a new change
if [[ $gs == *"modified"* ]]; then
  # push to Github
  git add -u;
  git commit -m "New backup `date +'%Y-%m-%d %H:%M:%S'`";
  git push origin master echo "push"
fi


