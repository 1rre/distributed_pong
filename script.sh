#!/bin/bash
erl -noshell -eval "halt(0)"
if [[ $? -ne 0 ]]; then
  sudo apt-get -y update && sudo apt-get install erlang
fi
git clone git@github.com:tjm1518/Info_Proc_Labs.git && cd Info_Proc_Labs
escript server.erl

