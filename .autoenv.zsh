#!/bin/zsh
# Common aliases
#


alias backend_name="docker-compose ps | grep backend | awk '{ print \$1 }'"

export BACKEND_IMAGE=$(backend_name)
export BACKEND_CMD="docker exec -it \$(backend_name)"
alias shell="$BACKEND_CMD bash"
