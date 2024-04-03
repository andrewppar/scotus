let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-23.11" ;
  pkgs = import nixpkgs { config= {} ; overlays = [ ]; };
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
  shellHook = ''
    function run () {
       pg_ctl -D $DB_LOC -U postgres -l logfile start
       clj -M:dev/repl
    }
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
    alias pgcli_local="pgcli postgresql://postgres:postgres@localhost:5432/kb"
    alias test="time clj -M:dev/test -m kaocha.runner --skip-meta :integration"
    alias test_all="time clj -M:dev/test -m kaocha.runner"
    alias test_integration="time clj -M:dev/test -m kaocha.runner --focus-meta :integration"
    alias db_init="initdb -D $DB_LOC -U postgres"
    alias db_create="createdb -U postgres kb"
    alias db_start="pg_ctl -D $DB_LOC -U postgres -l logfile start"
    alias db_stop="pg_ctl -D $DB_LOC -U postgres -l logfile stop"
    alias db_status="pg_ctl -D $DB_LOC status"
    trap stop EXIT
'' ;
}
