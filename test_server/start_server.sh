#!/bin/sh
erlc pong_app.erl pong_sup.erl pong_game.erl pong_server.erl &&\
  erl -pa .build -name pong@$(dig +short myip.opendns.com @resolver1.opendns.com) -setcookie "let's play pong" -eval "application:start(pong)"

