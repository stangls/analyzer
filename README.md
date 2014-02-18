analyzer
========

A fork of the goblint static analysis framework with an interface to external information.

installation under debian-like linux distros
--------------------------------------------

This installation routine was successfully tested on
* Ubuntu (LUbuntu 12.10) with an additional repository for OCaml 4
* Debian wheezy with testing - repositories for access to the OCaml 4 package

Install Ocaml 4.…
Get access to a repository
* testing on debian wheezy
* use debian jessie ( https://packages.debian.org/jessie/ocaml ) 
* on Ubuntu : https://launchpad.net/~avsm/+archive/ocaml41+opam11

Install required packages:
```
apt-get install ocaml git
```

Next install OPAM:
```
wget http://www.ocamlpro.com/pub/opam_installer.sh
sh ./opam_installer.sh /usr/local/bin
```

And required ocaml packages:
```
opam update
opam install ocamlfind camomile batteries xml-light cil
```


Then clone the repository from this url and compile:
```
git clone $thisurl$
cd analyzer
./make.sh
```

Run goblint
-----------
```
./goblint
```
or run the full testing suite via:
```
scripts/update_suite.rb
```
