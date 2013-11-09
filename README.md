Adams
=====

Adams is our new cybernetic DevOps. Please welcome him and make him feel
at home, I hope he will find a nice place to work amongst us. So far he's
been a brilliant student though a bit dumb and formal... err..
For the next months he will remain in formation so if you would please
consider handing him any rookie task you might have he shall gladly take
them upon him and probably fail but that's what unpaid interns are for, right ?

Current status
--------------

Adams is currently able to write shell scripts to gather information
about current system status.

For now he can handle Unix hosts through shell commands.

Roadmap
-------

Our goal is to have him connect using SSH and once connected check if the
system is in good shape and otherwise bring it in line with the
requirements, using shell commands just like your regular sysadmin.

Security design
---------------

You should only allow Adams what you would allow your system operators :
  - a shell accessible through SSH using a public key
  - sudo permissions

All commands issued to the remote hosts can be logged.

Adams does not grant the hosts access to its workstation while it works.
Adams does not grant access to data belonging to any host.
Adams does not send any data that is not of direct concern to the host.
