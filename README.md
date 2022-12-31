cl-unix-cybernetics 0.4.0
=========================

UNIX cybernetics in Common Lisp.

You describe your systems (hosts) using resources having properties.

The properties are then probed and synchronized using only
`/bin/sh` on the remote host, and `/usr/bin/ssh` on the control host.


Current status
--------------

There is support for local shell and connection to remote hosts via ssh.

Using only `/bin/sh` commands makes `ksh` and `bash` suitable shells as
they are compatible with `/bin/sh`.

Supported resource types :
 - Host (hostname)
 - User (useradd, usermod, userdel)
 - Group (groupadd, groupmod, groupdel)
 - File (owner, group, permissions, content)
 - Directory (owner, group, permissions)
 - Package (Debian, OpenBSD)


Security design
---------------

You should only allow what you would allow your system operators :
  - a shell accessible through SSH using a public key
  - apropriate sudo permissions

All commands issued to the remote hosts can be logged.

Does not grant the hosts access to its workstation while it works.
Does not grant access to data belonging to any host.
Does not send any data that is not of direct concern to the host.
In short, all UNIX permissions are respected.


Usage
-----


### 1. Install [repo](https://github.com/common-lisp-repo/repo).


### 2. Fetch sources.

``` shell
  $ sbcl --eval '(repo:install :cl-unix-cybernetics)'
```


### 3. Build and install the `cl-unix-cybernetics` binary

``` shell
  $ cd ~/common-lisp/thodg/cl-unix-cybernetics
  $ make
  $ sudo cp build/cl-unix-cybernetics /usr/local/bin/
```


### 4. Write some resources in a `.lisp` script

In the `tutorial.lisp` file :
``` common-lisp
  #!/usr/local/bin/cl-unix-cybernetics --script

  (resource 'host "example.kmx.io"
            :user "sysadm"
            (resource 'user "sysadm"
                      :shell "/bin/sh"
                      :ensure :present))

  (with-host "example.kmx.io"
    (sync *host*))
```


### 6. Profit.

``` shell
  $ chmod 755 tutorial.lisp
  $ ./tutorial.lisp
```

The `tutorial.lisp` script will synchronize the host "example.kmx.io"
according to the resource specifications given in the file.


### 7. DRY up your scripts using `#.(include "file")`

In the `user/dx.lisp` file :
``` common-lisp
  ;; Thomas de Grivel (kmx.io)
  (resource 'group "dx"
            :gid 19256
            :ensure :present)
  (resource 'user "dx"
            :uid 19256
            :gid 19256
            :home "/home/dx"
            :ensure :present)
```

In your main script :
``` common-lisp
  #!/usr/local/bin/cl-unix-cybernetics --script

  (resource 'host "example.kmx.io"
            :user "admin"
            (resource 'user "admin"
                      :shell "/bin/sh"
                      :ensure :present)
            #.(include "user/dx"))

  (with-host "example.kmx.io"
    (sync *host*))
```


History
-------

This project used to be named "Adams".
It was renamed in 2022 to "cl-unix-cybernetics".


[License](LICENSE.md)
---------------------


Authors
-------

Thomas de Grivel <thodg@kmx.io>
