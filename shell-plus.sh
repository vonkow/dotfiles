#!/bin/sh
eval $(docker-machine env default)
cd ~/projects/web
source ~/projects/web/profile
docker-compose -f ~/projects/web/docker-compose.yml run --rm web sh /usr/bin/python -i /web/src/aplaceforrover/manage.py
#(cd ~/projects/web source ./profile && docker-compose -f ~/projects/web/docker-compose.yml run --rm web /usr/bin/python -i /web/src/aplaceforrover/manage.py shell_plus)
#docker-compose -f ~/projects/web/docker-compose.yml run --rm web ./manage.py shell_plus
