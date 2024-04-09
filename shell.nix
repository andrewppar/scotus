let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-23.11" ;
  pkgs = import nixpkgs { config= {} ; overlays = [ ]; } ;

  test-all = ''
   function test-all () {
     pg_ctl -D $DB_LOC -U postgres -l logfile start ;
     time clj -M:dev/test -m kaocha.runner ;
   }
  '' ;

  test-integration = ''
    function test-integration () {
      pg_ctl -D $DB_LOC -U postgres -l logfile start ;
      time clj -M:dev/test -m kaocha.runner --focus-meta :integration ;
    }
   '' ;

  run = ''
   function run () {
     pg_ctl -D $DB_LOC -U postgres -l logfile start
     clj -M:dev/repl
   }
  '' ;

  stop = ''
    function stop () {
     while true ; do
       read -p  "tear down scotus services? (y/n) " reply
       case $reply in
         [yY] )  echo "stopping scotus services"
                pg_ctl -D $DB_LOC -U postgres -l logfile stop
                echo "good bye"
                exit ;;
         * ) echo " good bye"
              exit ;;
       esac
     done
    }
  '' ;

  functions =
    test-all
    + test-integration
    + run
    + stop ;

  alias = {name, command}: "alias " + name + ''="'' + command + ''" ;'' ;

  pgcli-local = alias {
    name = "pgcli-local";
    command = "pgcli postgresql://postgres:postgres@localhost:5432/kb" ;
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
    command = "createdb -U postgres kb";
  } ;

  db-start = alias {
    name = "db-start" ;
    command = "pg_ctl -D $DB_LOC -U postgres -l logfile start" ;
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
    + test
    + db-init
    + db-create
    + db-start
    + db-stop
    + db-status ;
in
pkgs.mkShell {
  packages = with pkgs; [
    clojure
    pgcli
    postgresql
    less
  ] ;

  DB_LOC = ".tmp/kb" ;
  DB__NAME="kb";
  DB__HOST="localhost";
  DB__USER="postgres";
  DB__PORT="5432";
  shellHook =
    functions
    + aliases
    + ''trap stop EXIT '' ;
}
