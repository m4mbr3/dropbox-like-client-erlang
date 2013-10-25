all:
	erlc -o Dropboxlike ../dropbox-like.idl
	erlc -o Dropboxlike Dropboxlike/*.erl
	erlc -I Dropboxlike *.erl
idl: 
	erlc -o Dropboxlike ../dropbox-like.idl
	
client:
	erlc -o Dropboxlike Dropboxlike/*.erl
	erlc -I Dropboxlike *.erl

clean:
	rm -rf Dropboxlike
	rm *.beam

