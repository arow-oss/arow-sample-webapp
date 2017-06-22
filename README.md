# arow-sample-webapp
Sample Servant Web Application and JSON API

### Steps to install postgres ###

Use the following steps to install postgres and set it up to be used with the app.

```sh
# install postgres

# become postgres user
$ sudo -i -u postgres

# as postgres user, initialize the database
# (maybe only needed on arch linux?)
$ initdb --locale en_US.UTF-8 -E UTF8 -D '/var/lib/postgres/data'

# As the postgres user, comment out all lines in the following file and add the
# following line.  This disables local trust-based authentication (any local
# user can login as any database user without a password), and enables
# password-based authentication (but only from localhost).
$ echo 'host all all 127.0.0.1/32 md5' >> /var/lib/postgres/data/pg_hba.conf

# create the appuser for developement and testing
$ sudo -u postgres -- psql --command "CREATE ROLE appuser NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN ENCRYPTED PASSWORD 'ahah8eh9ahf23hr9aaw3r'"

# create the appdb for developement
$ sudo -u postgres -- createdb appdb

# grant access to dev db for appuser
$ sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE appdb TO appuser"

# restart postgres service
$ sudo systemctl restart postgresql
($ sudo service postgresql restart # Ubuntu)

# as normal user, try accessing database
$ psql -U appuser -d appdb -h 127.0.0.1
```
