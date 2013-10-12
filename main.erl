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
            Home_env = Home++"/Dropboxlikeclient",
            file:make_dir(Home_env)
    end,
    NS = corba:string_to_object("corbaloc:iiop:localhost:1050/NameService"),
    Dropboximpl = 'CosNaming_NamingContextExt':'resolve_str' (NS, "DBServer"),
    start(Dropboximpl, Home_env).

start(Dropboximpl, Home_env) -> menu(),
               {ok,[Choice]} = io:fread("dropboxlike $ ", "~s"),
               check_operation(Dropboximpl,Choice, Home_env),
               if
                Choice =:= "exit" ->
                    halt();
                true ->
                    start(Dropboximpl, Home_env)
               end.


check_operation(Dropboximpl, Exp, Home_env) -> case Exp of 
                            "subscribe"->
                                subscribe(Dropboximpl,Home_env);
                            "remove_account" ->
                                remove_account(Dropboximpl, Home_env);
                            "login" ->
                                login(Dropboximpl);
                            "logout" ->
                                logout(Dropboximpl);
                            "send_file" ->
                                send_file(Dropboximpl);
                            "remove_file" ->
                                remove_file(Dropboximpl);
                            "clear" ->
                                clear(Dropboximpl);
                            "dir" ->
                                dir(Dropboximpl);
                            "ls" ->
                                ls(Dropboximpl);
                            "help" ->
                                help(Dropboximpl);
                            "exit" ->
                                go_out(Dropboximpl)
                        end.


subscribe(Dropboximpl,Home_env) -> Name = string:strip(io:get_line("Insert your name: "),both, $\n),
                Surname = string:strip(io:get_line("Insert your surname: "), both, $\n),
                Username = string:strip(insert_username(Dropboximpl, "Insert your username: "),both, $\n),
                Password = string:strip(insert_password(),both, $\n),
                case 'Dropboxlike_Repository':subscribe(Dropboximpl, Name, Surname, Username, Password) of 
                    true ->
                        io:fwrite("Registered user \n"),
                        file:make_dir(Home_env++"/"++Username),
                        io:fwrite("Home directory for user "++ Username ++ " removed\n");
                    false ->
                        io:fwrite("Error during the registration \n")
                end.


remove_account(Dropboximpl, Home_env) -> Username = string:strip(io:get_line("Insert your username: "), both, $\n),
                    Password = string:strip(io:get_line("Insert the password for "++ Username ++": "), both, $\n),
                    case 'Dropboxlike_Repository':remove(Dropboximpl, Username,Password) of
                        true ->
                            io:fwrite("Removed user \n"),
                            file:del_dir(Home_env++"/"++Username),
                            io:fwrite("Home directory for user "++ Username ++" removed\n");
                        false -> 
                            io:fwrite("Error: maybe the user or/and the password are wrong\n")
                    end.


login(Dropboximpl) -> ciao.
logout(Dropboximpl) -> ciao.
send_file(Dropboximpl) -> ciao.
remove_file(Dropboximpl) -> ciao.
clear(Dropboximpl) -> ciao.
dir(Dropboximpl) -> ciao.
ls(Dropboximpl) -> ciao.
help(Dropboximpl) -> ciao.
go_out(Dropboximpl) -> halt().


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



