let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-23.11" ;
  pkgs = import nixpkgs { config= {} ; overlays = [ ]; } ;

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
    db-start
    + run
    + stop
    + test-all
    + test-integration ;

  ## Aliases

  alias = {name, command}: "alias " + name + ''="'' + command + ''" ;'' ;

  pgcli-local = alias {
    name = "pgcli-local";
    command = "pgcli postgresql://postgres:postgres@localhost:$DB__PORT/kb" ;
  } ;

  psql-local = alias {
    name = "psql-local";
    command = "psql postgresql://postgres:postgres@localhost:$DB__PORT/kb" ;
  } ;

  test = alias {
    name = "test" ;
    command = "time clj -M:dev/test -m kaocha.runner --skip-meta :integration" ;
  } ;

  db-init = alias {
    name = "db-init" ;
    command = "initdb -D $DB_LOC -U postgres" ;
  };

  db-create = alias {
    name = "db-create" ;
    command = "createdb -p $DB__PORT -U postgres kb";
  } ;

  db-test-create = alias {
    name = "db-test-create" ;
    command = "createdb -p $DB__PORT -U postgres kb-test";
  } ;

  db-stop = alias {
    name = "db-stop" ;
    command = "pg_ctl -D $DB_LOC -U postgres -l logfile stop" ;
  };

  db-status = alias {
    name = "db-status" ;
    command = "pg_ctl -D $DB_LOC status" ;
  };

  aliases =
    pgcli-local
    + psql-local
    + test
    + db-init
    + db-create
    + db-test-create
    + db-stop
    + db-status ;
in
pkgs.mkShell {
  packages = with pkgs; [
    clojure
    postgresql
    less
    pgcli
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
