# create kb
make-db:
	createdb -h localhost -p 5432 -U anparisi kb

# create test db
make-test-db:
	createdb -h localhost -p 5432 -U anparisi kb_test

# tear down db
drop-db:
	dropdb kb

# tear down test db
drop-test-db:
	dropdb kb_test

# reset db
reset-db: drop-db make-db

# reset-test-db
reset-test-db: drop-test-db make-test-db

# test
test-all:
	nix-shell --command "clj -M:dev/test -m kaocha.runner"

# start a repl
run:
	nix-shell --command "clj -M:dev/repl"
