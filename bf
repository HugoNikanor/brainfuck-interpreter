#!/bin/bash
tr -d '\n' | ./main.scm $* < /dev/stdin
