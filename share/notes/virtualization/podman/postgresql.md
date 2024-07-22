To create and run
=================

    podman run --name apostgre -e POSTGRES_USER=andrei -e POSTGRES_PASSWORD=mypassword -e POSTGRES_HOST_AUTH_METHOD=trust -p 127.0.0.1:15432:5432/tcp   -v /mnt/vault/Postgre:/var/lib/postgresql/data  docker.io/postgres:15-bookworm

Install plpython
================

In [container shell](get-shell.md) :

    apt install postgresql-plpython3-15 




To connect to it
================

    psql -h localhost -p 15432
