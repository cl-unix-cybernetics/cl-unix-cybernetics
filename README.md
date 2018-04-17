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
