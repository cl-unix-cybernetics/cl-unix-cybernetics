Adams
=====

Adams is our new cybernetic DevOps. Please welcome him and make him feel
at home, I hope he will find a nice place to work amongst us. So far he's
been a brilliant student though a bit dumb and formal, I hope he will find
a warm and welcoming place in our hearts.
For the next months he will remain in formation so if you would please
consider handing him any rookie task you might have he shall gladly take
them upon him and will probably crash the system and need your help to fix it
but, hey, that's what unpaid interns are for, right ?


Current status
--------------

Adams is currently able to use a local shell or connect to remote hosts via
ssh.
He is quite the hardcore hacker wannabe using only /bin/sh though ksh and
bash suit him fine too.
He's still green but he can already gather basic information about users,
groups and files.

We are currently teaching him about new kinds of resources and how to read
resource specification manifests.


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
Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
```


Contribute
----------

To contribute, fork this repository and send us a pull request.

Your code must be published under the
[ISC License](https://en.wikipedia.org/wiki/ISC_license)
terms.


Authors
-------

Thomas de Grivel <thoxdg@gmail.com>
