#!/bin/bash

branch=$( git branch | grep '^*' | sed 's/^* //' )
echo "Current branch : $branch"

stashed=$(git stash)

error=""

if git checkout master && git pull ; then
  echo "==== master update successfull"
  git mergetool && git commit -uno

  if git checkout $branch && git rebase master; then
    git mergetool && git commit -uno
    echo "==== rebase successfull"
  else
    error="$branch rebase"
  fi

else
  error="master checkout"
fi

if [ "$stashed" != "No local changes to save" ]; then
  if ! [ -n "$error" ]; then
    git stash pop -q
    echo 'Merging your uncommited changes (="remote"). Indexed files are "local".'
    git mergetool
    git commit -uno
  else
    echo 'WARNING: You have to use git stash pop -q'
  fi
fi

[ -n "$error" ] && echo "==== ERROR : >$error<"
