## Synopsis

This generates `iptables` shell scripts from a domain specific language
which

  - provides variable definitions and control structures at the
    outer level while staying close to the `iptables` command in the
    details.
  - supports IP numbers, networks, and unions of networks as first-class objects
    which can be manipulated by set-operators.
  - allows literal host names which will be resolved at
    compile-time, so that the final script is independent of DNS lookups.

Please note that the project is at an early stage.  It is wise to inspect
the generated scripts before using them.

## Installation

This package can be installed with [opam](http://opam.ocaml.org/),

    opam repo add paurkedal https://github.com/paurkedal/opam-repo-paurkedal.git
    opam install iplogic

It provides two programs `iplogic`, which compiles scripts into shell code,
and `iplogic-depend`, which generates dependencies to include in makefiles.

## Example

    # Define some commonly used conditions for convenince.  These are better
    # put in a separate file and included.
    con tcp is -p "tcp"
    con udp is -p "udp"

    # Some IP numbers and network ranges.
    val my_work_computers is 192.0.2.42, 192.0.2.100
    val my_isp_range is 203.0.113.0/24
    val ssh_clients is my_work_computers, my_isp_range

    # Accept incoming http and selected ssh connections, as well as
    # connections from eth1.
    chain INPUT policy drop
        if -m "state" --state "RELATED,ESTABLISHED" accept
        if -p "icmp" accept
        if -i "lo" accept
        if -i "eth0"
            if -s ssh_clients tcp --dport 22 accept
            if --dport 80 accept
            continue
        if -i "eth1" -s 192.168.10.0/24 accept
        continue
