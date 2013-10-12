-module(main).
-export([init/0, dropboxlike_client/0, start/2]).
init() -> 
    mnesia:start(),
    corba:orb_init([{domain, "ramnode"}]),
    orber:install([node()], [{ifr_storage_type, ram_copies}]),
    orber:start(),
    'oe_dropbox-like':'oe_register'().

menu() ->   io:fwrite("/*************************  DropboxLike   ***************************/\n"),
            
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
    Home = os:getenv("DROPBOXLIKECLIENT_HOME"),
    case Home of 
        false -> 
            Home_env = ".";
        _ ->
            Home_env = Home
    end,
    NS = corba:string_to_object("corbaloc:iiop:localhost:1050/NameService"),
    Dropboximpl = 'CosNaming_NamingContextExt':'resolve_str' (NS, "DBServer"),
    start(Dropboximpl, Home_env).

start(Dropboximpl, Home_env) -> menu(),
               {ok,[Choice]} = io:fread("dropboxlike $ ", "~s"),
               check_operation(Dropboximpl,Choice),
               if
                Choice =:= "exit" ->
                    halt();
                true ->
                    start(Dropboximpl, Home_env)
               end.
               
check_operation(Dropboximpl, Exp) -> case Exp of 
                            "subscribe"->
                                subscribe(Dropboximpl);
                            "remove_account" ->
                                remove_account();
                            "login" ->
                                login();
                            "logout" ->
                                logout();
                            "send_file" ->
                                send_file();
                            "remove_file" ->
                                remove_file();
                            "clear" ->
                                clear();
                            "dir" ->
                                dir();
                            "ls" ->
                                ls();
                            "help" ->
                                help();
                            "exit" ->
                                go_out()
                        end.


subscribe(Dropboximpl) -> Name = string:strip(io:get_line("Insert your name: "),both, $\n),
                Surname = string:strip(io:get_line("Insert your surname: "), both, $\n),
                Username = string:strip(insert_username(Dropboximpl, "Insert your username: "),both, $\n),
                Password = string:strip(insert_password(),both, $\n),
                case 'Dropboxlike_Repository':subscribe(Dropboximpl, Name, Surname, Username, Password) of 
                    true  -> 
                        io:fwrite("Registered user \n");
                    false ->
                        io:fwrite("Error during the registration \n")
                end.


remove_account() -> ciao.
login() -> ciao.
logout() -> ciao.
send_file() -> ciao.
remove_file() -> ciao.
clear() -> ciao.
dir() -> ciao.
ls() -> ciao.
help() -> ciao.
go_out() -> halt().


insert_username(Dropboximpl, ToBeShown) -> El = io:get_line(ToBeShown),
                              Res = 'Dropboxlike_Repository':check_username(Dropboximpl, El),
                              case Res of
                                false -> 
                                    io:fwrite("Username not available. Try again...\n"),
                                    insert_username(Dropboximpl, ToBeShown);
                                true -> El
                              end.

insert_password() -> Password1 = io:get_line("Enter your new password: "),
                                Password2 = io:get_line("Enter your new password again: "),
                                case string:equal(Password1, Password2) of
                                    true ->
                                        Password1;
                                    false ->
                                        io:fwrite("Passwords don't match. Try again... \n"),
                                        insert_password()
                                end.



