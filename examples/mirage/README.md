## A MirageOS proxy to a SOCKS4 server

This unikernel provide an HTTP(S) proxy which transmits any requests a SOCKS4
server. For instance, we will use Tor which provides such service for us. We
need to install Tor:
```sh
$ sudo apt update && sudo apt upgrade
$ sudo apt install tor
```

By default, Tor initiates a server on \*:9050. You can check it via:
```sh
$ ss -nlt
State    Recv-Q   Send-Q     Local Address:Port     Peer Address:Port  Process
LISTEN   0        4096           127.0.0.1:9050          0.0.0.0:*
```

Then, you need to compile and run the unikernel:
```sh
$ cd examples/mirage
$ opam install mirage
$ mirage configure -t unix
$ make depends
$ mirage build
```

Finally, you just need to launch the executable:
```sh
$ ./example --socks4-hostname localhost --socks4-port 9050
```

By default, the MirageOS runs on \*:8080 but you can change it via the `--port`
option. Finally, you can configure your favorite webbrowser to use as the
HTTP and HTTPS proxy: 127.0.0.1:8080. You can directly check the result if you
compare these commands:
```sh
$ curl -x localhost:8080 https://api.ipify.org ; echo
a.b.c.d
$ curl https://api.ipify.org ; echo
e.f.g.h
```
