#!/bin/sh
# Compile the files then boot the server, starting the application as part of the boot scirpt
# and setting the cookie (needed to connect to the server) to "let's play pong"
erlc pong_app.erl pong_sup.erl pong_game.erl pong_server.erl &&\
  screen erl -pa .build -name pong@$(dig +short myip.opendns.com @resolver1.opendns.com) -setcookie "let's play pong" -eval "application:start(pong)"

