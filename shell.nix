let
  pkgs = import <nixpkgs> {};

  ## Useful commands

  db-start-cmd = ''pg_ctl -o "-F -p $DB__PORT" -D $DB_LOC -U postgres -l logfile start ;'';


  ## Functions
  make-body = {commands}: builtins.foldl'
    (acc: str: acc + "  " + str + "\n")
    ""
    commands ;

  shell-fn = {name, body} : "function " + name + " () {\n"
                            + make-body { commands = body ;}
                            + "\n}; \n" ;

  db-reset = shell-fn {
    name = "db-reset" ;
    body = [
      ''PGPASSWORD="postgres" psql -h "localhost" -U "postgres" -p $DB__PORT -c "DROP DATABASE kb WITH(FORCE)"''
      ''PGPASSWORD="postgres" psql -h "localhost" -U "postgres" -p $DB__PORT -c "CREATE DATABASE kb"''
    ] ;
  } ;

  db-test-reset = shell-fn {
    name = "db-test-reset" ;
    body = [
      ''PGPASSWORD="postgres" psql -h "localhost" -U "postgres" -p $DB__PORT -c "DROP DATABASE kb_test WITH(FORCE)"''
      ''PGPASSWORD="postgres" psql -h "localhost" -U "postgres" -p $DB__PORT -c "CREATE DATABASE kb_test"''
    ] ;
  } ;

  db-start = shell-fn {

    name = "db-start" ;
    body = [db-start-cmd] ;
  } ;

  run = shell-fn {
    name = "run" ;
    body = [db-start-cmd "clj -M:dev/repl"] ;
  };

  stop = shell-fn {
    name = "stop" ;
    body = [
      ''while true ; do''
      ''  read -p  "tear down scotus services? (y/n) " reply''
      ''  case $reply in''
      ''    [yY] )  echo "stopping scotus services"''
      ''           pg_ctl -D $DB_LOC -U postgres -l logfile stop''
      ''           echo "good bye"'' ''           exit ;;''
      ''    * ) echo " good bye"''
      ''         exit ;;''
      ''  esac''
      ''done''] ;
  } ;

  test-all = shell-fn {
    name = "test-all" ;
    body = [
      db-start-cmd
      "time clj -M:dev/test -m kaocha.runner;"
    ] ;
  };

  test-integration = shell-fn {
    name = "test-integration" ;
    body = [
      db-start-cmd
      "time clj -M:dev/test -m kaocha.runner --focus-meta :integration ;"
    ] ;
  };

  functions =
    db-reset
    + db-test-reset
    + db-start
    + run
    + stop
    + test-all
    + test-integration ;

  ## Aliases

  alias = {name, command}: "alias " + name + ''="'' + command + ''" ;'' ;

  psql-local = alias {
    name = "psql-local";
    command = "psql postgresql://postgres:postgres@localhost:$DB__PORT/kb" ;
  } ;

  db-create = alias {
    name = "db-create" ;
    command = "createdb -p $DB__PORT -U postgres kb";
  } ;

  db-create-test = alias {
    name = "db-test-create" ;
    command = "createdb -p $DB__PORT -U postgres kb_test";
  } ;

  db-init = alias {
    name = "db-init" ;
    command = "initdb -E UTF8 -D $DB_LOC -U postgres" ;
  };

  db-status = alias {
    name = "db-status" ;
    command = "pg_ctl -D $DB_LOC status" ;
  };

  db-stop = alias {
    name = "db-stop" ;
    command = "pg_ctl -D $DB_LOC -U postgres -l logfile stop" ;
  };


  pgcli-local = alias {
    name = "pgcli-local";
    command = "pgcli postgresql://postgres:postgres@localhost:$DB__PORT/kb" ;
  } ;

  test = alias {
    name = "test" ;
    command = "time clj -M:dev/test -m kaocha.runner --skip-meta :integration --skip scotus.database" ;
  } ;




  aliases =
    pgcli-local
    + psql-local
    + test
    + db-init
    + db-create
    + db-create-test
    + db-stop
    + db-status ;
in
pkgs.mkShell {
  packages = with pkgs; [
    clojure
    postgresql
    less
    pgcli
    # add this to a dev packages somehow
    tmux
  ] ;

  DB_LOC = ".tmp/kb" ;
  DB__NAME="kb";
  DB__HOST="localhost";
  DB__USER="postgres";
  DB__PORT="5431";
  shellHook =
    functions
    + aliases
    + ''trap stop EXIT '' ;
}
