Adams 0.1
=========

Adams is a UNIX system administration tool.

You describe your systems (hosts) using resources having properties.

The properties are then probed and synchronized by Adams using only
`/bin/sh` on the remote host, and `/usr/bin/ssh` on the control host.

More viable alternatives include
[Ansible](https://www.ansible.com/),
[Chef](https://www.chef.io/)
and
[Puppet](https://puppet.com/).


Current status
--------------

Adams is currently able to use a local shell or connect to remote hosts
via ssh.

Adams is this hardcore hacker using only `/bin/sh` commands.
This makes `ksh` and `bash` suitable shells for adams as they are
compatible with `/bin/sh`.

Supported resource types :
 - Host (hostname)
 - User (useradd, usermod, userdel)
 - Group (groupadd, groupmod, groupdel)
 - File (owner, group, permissions, content)
 - Directory (owner, group, permissions)
 - Package (Debian, OpenBSD)


Security design
---------------

You should only allow Adams what you would allow your system operators :
  - a shell accessible through SSH using a public key
  - apropriate sudo permissions

All commands issued to the remote hosts can be logged.

Adams does not grant the hosts access to its workstation while it works.
Adams does not grant access to data belonging to any host.
Adams does not send any data that is not of direct concern to the host.
In short, all UNIX permissions are respected, Adams is a regular UNIX user.


Usage
-----


### 1. Install [repo](https://github.com/common-lisp-repo/repo).


### 2. Fetch adams sources.

``` shell
  $ sbcl --eval '(repo:install :adams)'
```


### 3. Build and install the `adams` binary

``` shell
  $ cd ~/common-lisp/cl-adams/adams
  $ make
  $ sudo cp build/adams /usr/local/bin/adams
```


### 4. Configure emacs (optional)

In your `~/.emacs` file :
``` emacs-lisp
  ;;  Adams
  (add-to-list 'auto-mode-alist '("\\.adams\\'" . lisp-mode))
```


### 5. Write some resources in a `.adams` script

In the `tutorial.adams` file :
``` common-lisp
  #!/usr/local/bin/adams --script

  (resource 'host "adams.kmx.io"
            :user "adams"
            (resource 'user "adams"
                      :shell "/bin/sh"
                      :ensure :present))

  (with-host "adams.kmx.io"
    (sync *host*))
```


### 6. Profit.

``` shell
  $ chmod 755 tutorial.adams
  $ ./tutorial.adams
```

The `tutorial.adams` script will synchronize the host "adams.kmx.io"
according to the resource specifications given in the file.


### 7. DRY up your scripts using `#.(include "file")`

In the `user/dx.adams` file :
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
  #!/usr/local/bin/adams --script

  (resource 'host "adams.kmx.io"
            :user "adams"
            (resource 'user "adams"
                      :shell "/bin/sh"
                      :ensure :present)
            #.(include "user/dx"))

  (with-host "adams.kmx.io"
    (sync *host*))
```


ISC License
-----------
```
;;  Copyright YEAR AUTHOR <EMAIL>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;
```


Contribute
----------

To contribute, fork this repository and send us a pull request.

Please publish under the
[ISC License](https://en.wikipedia.org/wiki/ISC_license)
terms.


Authors
-------

Thomas de Grivel <thoxdg@gmail.com>
