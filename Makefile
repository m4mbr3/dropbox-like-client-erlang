all:
	erlc -o Dropboxlike ../dropbox-like.idl
	erlc main.erl
idl: 
	erlc -o Dropboxlike ../dropbox-like.idl 
	
