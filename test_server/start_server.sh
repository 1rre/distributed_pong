#!/bin/sh
#screen erl -name remnd@$(dig +short myip.opendns.com @resolver1.opendns.com) -setcookie infoproc -eval "compile:file(rm_node),application:start(rm_node)."
erlc -o .build pong_app.erl pong_sup.erl pong_game.erl pong_server.erl && \
erl -pa .build -sname pong -setcookie "let's play pong" -eval "application:start(pong)"

