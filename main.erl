-module(main).
-export([init/0, menu/0]).


init() -> 
    mnesia:start(),
    corba:orb_init([{domain, "ramnode"}]),
    orber:install([node()], [{ifr_storage_type, ram_copies}]),
    orber:start(),
    'oe_Account':'oe_register'(),
    dropboxlike_client().

menu() ->   %io:clear(),
            io:fwrite("/*************************  DropboxLike   ***************************/\n"),
            
            io:fwrite("/***********  Author : Andrea Mambretti   Version 1.0   *************/ \n"),
            io:fwrite("/********************************************************************/ \n"),
            io:fwrite ("Select an operation:\n"),
            io:fwrite ("\n"),
            io:fwrite ("    subscribe\n"),
            io:fwrite ("    remove_account\n"),
            io:fwrite ("    login\n"),
            io:fwrite ("    logout\n"),
            io:fwrite ("    send_file\n"),
            io:fwrite ("    remove_file\n"),
            io:fwrite ("    clear\n"),
            io:fwrite ("    dir\n"),
            io:fwrite ("    ls\n"),
            io:fwrite ("    help\n"),
            io:fwrite ("    exit\n"),
            io:fwrite ("\n").

dropboxlike_client() ->
    NS = corba:string_to_object("corbaloc:iiop:localhost:1050/NameService"),
    Dropboximpl = 'CosNaming_NamingContextExt':'resolve_str' (NS, "DBServer"),
    Res = "Dropboxlike_Repository":isLogged(Dropboximpl,"m4mbr3", "kasjd").
