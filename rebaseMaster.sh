#!/bin/bash

branch=$( git branch | grep '^*' | sed 's/^* //' )
echo "Current branch : $branch"

stashed=$(git stash)

error=""

if git checkout master && git pull ; then
  echo "==== master update successfull"
  git mergetool && git commit -uno

  if git checkout $branch then
    git rebase master; done=$?
    while [ "$done" != "0" ]; do
      if git mergetool; then
        git rebase --continue; done=$?
      else
        error="$branch rebase"
        echo "==== rebase aborted due to unsuccessful merge"
        done=0
      fi
    done
    [ -n "$error" ] || echo "==== rebase successfull"
  else
    error="$branch checkout"
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
