#!/bin/sh
screen erl -name remnd@$(dig +short myip.opendns.com @resolver1.opendns.com) -setcookie infoproc -eval "compile:file(rm_node),application:start(rm_node)."