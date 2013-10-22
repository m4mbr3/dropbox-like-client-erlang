all:
	erlc -o Dropboxlike ../dropbox-like.idl
	erlc +debug_info -o Dropboxlike Dropboxlike/*.erl
	erlc +debug_info -I Dropboxlike *.erl
idl: 
	erlc -o Dropboxlike ../dropbox-like.idl 
	
