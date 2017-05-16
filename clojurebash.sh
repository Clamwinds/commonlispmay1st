8#!/bin/bash
# File: clj
# Version: 1.0
# Date: 2010OCT31
#
# Usage:
#
# clj                           # Starts REPL
# clj my_script.clj             # Runs an external script
# clj my_script.clj arg1 arg2   # Runs the script passing it arguments
CWD="/opt/Learning Clojure"
if [ $# -eq 0 ]; then 
    java -server -cp ${CWD}/clojure.jar clojure.main
else
    java -server -cp ${CWD}/clojure.jar clojure.main $1 -- "$@"
fi
