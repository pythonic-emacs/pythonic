#!/bin/bash

while true
do
    echo -n .
    sleep 1m
done &

ansible-playbook -i inventories/travis playbook.yml
