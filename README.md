dropbox-like-client-erlang
==========================

Client for the dropboxlike application developed in erlang and corba


For the server (written in java and corba) and another client (in java) look for the project dropbox-like-corba 
at https://github.com/m4mbr3/dropbox-like-corba

How Install it?
--------------------------

###Requirements:

1. [A running server] (https://github.com/m4mbr3/dropbox-like-corba)
2. [erlang] (http://www.erlang.org/download.html)
3. [make] (https://www.gnu.org/software/make/)

###Steps:

1. After the installation of the server you should install the interpreter erlang for your system
2. Clone the repository using
    `git clone https://github.com/m4mbr3/dropbox-like-client-erlang.git`
wherever you want on the filesystem (to decide the name of the folder add at the end of the command above your preference).
NB:  this point should be considered only if you are installing a stand-alone client.
3. Run the command `make` inside the main directory of the project

Configuration
--------------------------

1. If you have cloned the stand-alone client you shuold set both i`DROPBOXLIKECLIENT_HOME` and `DROPBOXLIKESERVERIP` environment variable.  
To set them run for instance:  
`export DROPBOXLIKECLIENT_HOME=$HOME` or `export DROPBOXLIKECLIENT_HOME=/path/you/want`  
You have to be able to write there. The program automagically appends `dropboxlikeclient` to the path specified.  
Then with the same command you have to specify the address of the server.  
For instance:
`export DROPBOXLIKESERVERIP="127.0.0.1"` if your server is running on the same machine of the client.

2. If you have cloned it as a submodule of the server consider to launch the file config.sh in the terminal where you are going to start  the client.  After have edited it of course.  
To do that run from the src folder of the server the command `./config.sh`

How run it?
-----------------------------

### Run the orbd daemon and the server
Before start the client be sure you have correctly launch the orbd daemon and the server as described [here] (https://github.com/m4mbr3/dropbox-like-corba).  

### Run the erlang client

To run the dropboxlike client from the main folder of the client run the command:  

erl -pa Dropboxlike/

from the erlang command line run  
`> c(main).`
`> main:init().`
`> main:dropboxlike_client().`
