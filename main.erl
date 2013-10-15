-module(main).
-export([init/0, dropboxlike_client/0, start/3, print_file_list/1]).
%-record(login,{username="", token="", dev_id=""}).
%-record(smallL,{name, md5}).
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
    Login = dict:new(),
    Login1 = dict:store(username, "", Login),
    Login2 = dict:store(dev_id, "", Login1),
    Login3 = dict:store(token, "", Login2),
    clear(Dropboximpl, Login3),
    Login4 = dict:store(files, [], Login3),
    menu(),
    start(Dropboximpl, Home_env, Login4).

start(Dropboximpl, Home_env, Data) -> 
                case  string:equal(dict:fetch(username, Data),"") of
                true ->
                    {ok,[Choice]} = io:fread("dropboxlike $ ", "~s");
                false ->
                    {ok, [Choice]} = io:fread(dict:fetch(username,Data)++"@dropboxlike $ ", "~s")
                end,
               New_Data = check_operation(Dropboximpl,Choice, Home_env,Data),
               if
                Choice =:= "exit" ->
                    halt();
                true ->
                    start(Dropboximpl, Home_env, New_Data)
               end.


check_operation(Dropboximpl, Exp, Home_env, Data) -> 
                    New_Data =
                        case Exp of
                            "subscribe"->
                                subscribe(Dropboximpl,Home_env,Data);
                            "remove_account" ->
                                remove_account(Dropboximpl, Home_env, Data);
                            "login" ->
                                login(Dropboximpl, Data);
                            "logout" ->
                                logout(Dropboximpl, Data);
                            "send_file" ->
                                send_file(Dropboximpl, Data);
                            "remove_file" ->
                                remove_file(Dropboximpl, Data);
                            "clear" ->
                                clear(Dropboximpl, Data);
                            "dir" ->
                                ls(Dropboximpl, Data);
                            "ls" ->
                                ls(Dropboximpl, Data);
                            "help" ->
                                help(Dropboximpl, Data);
                            "exit" ->
                                go_out(Dropboximpl, Data);
                            NoCommand -> 
                                io:fwrite("dropboxlike: "++NoCommand++": command not found\n"),
                                Data
                        end,
                    New_Data.


subscribe(Dropboximpl,Home_env,Data) -> Name = string:strip(io:get_line("Insert your name: "),both, $\n),
                Surname = string:strip(io:get_line("Insert your surname: "), both, $\n),
                Username = string:strip(insert_username(Dropboximpl, "Insert your username: "),both, $\n),
                Password = string:strip(insert_password(),both, $\n),
                case 'Dropboxlike_Repository':subscribe(Dropboximpl, Name, Surname, Username, Password) of 
                    true ->
                        io:fwrite("Registered user \n"),
                        file:make_dir(Home_env++"/"++Username),
                        io:fwrite("Home directory for user "++ Username ++ " created\n");
                    false ->
                        io:fwrite("Error during the registration \n")
                end,
                Data.


remove_account(Dropboximpl, Home_env,Data) -> Username = string:strip(io:get_line("Insert your username: "), both, $\n),
                    Password = string:strip(io:get_line("Insert the password for "++ Username ++": "), both, $\n),
                    case 'Dropboxlike_Repository':remove(Dropboximpl, Username,Password) of
                        true ->
                            io:fwrite("Removed user \n"),
                            file:del_dir(Home_env++"/"++Username),
                            io:fwrite("Home directory for user "++ Username ++" removed\n");
                        false ->
                            io:fwrite("Error: maybe the user or/and the password are wrong\n")
                    end,
                    Data.


login(Dropboximpl, L) ->
        case 'Dropboxlike_Repository':isLogged(Dropboximpl, dict:fetch(username,L), dict:fetch(token,L)) of
        true ->
            io:fwrite("You are already logged!!!\n"), 
            io:fwrite("If you want to log in with a different user, please log out first\n");
        false ->
            Dev_id = string:strip(io:get_line("Insert your hostname: "),both, $\n),
            Username = string:strip(io:get_line("Insert your username: "),both, $\n),
            Password = string:strip(io:get_line("Insert the password for "++ Username++ ": "), both, $\n),
            case 'Dropboxlike_Repository':login(Dropboximpl, Username, Password, Dev_id) of
                "INVALID_USER" ->
                    io:fwrite("Error: maybe the user or/and the password are wrong\n");
                Token ->
                    L1 = dict:store(username,Username,L),
                    L2 = dict:store(dev_id, Dev_id, L1),
                    L3 = dict:store(token, Token, L2),
                    L3
            end
        end.

logout(Dropboximpl, L) -> 
    case 'Dropboxlike_Repository':isLogged(Dropboximpl, dict:fetch(username, L), dict:fetch(token,L)) of
        false ->
            io:fwrite("You are already logged!!!\n");
        true ->
            'Dropboxlike_Repository':logout(Dropboximpl, dict:fetch(username, L), dict:fetch(dev_id, L), dict:fetch(token, L)),
            L1 = dict:store(username, "", L),
            L2 = dict:store(dev_id, "", L1),
            L3 = dict:store(token, "", L2),
            L3
    end.

send_file(Dropboximpl, L) -> L.
remove_file(Dropboximpl, L) -> L.
clear(Dropboximpl, L) ->    io:fwrite("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"),
                            L.
ls(Dropboximpl, L) -> 
    case 'Dropboxlike_Repository':isLogged(Dropboximpl, dict:fetch(username, L), dict:fetch(token, L)) of
        false ->
            io:fwrite("Error: you are not logged\n"),
            L;
        true ->
            io:fwrite("Your repository contains: \n"), 
            case dict:fetch(files,L) of
                [] -> io:fwrite("No file found in your repository\n");
                Files -> print_file_list(Files)
            end,
            L
    end.

print_file_list([X|XS]) -> io:fwrite(X),
                           print_file_list(XS);
print_file_list([])-> finish.


help(Dropboximpl, L) -> menu(),
                        L.
go_out(Dropboximpl, L) -> halt().


insert_username(Dropboximpl, ToBeShown) -> El = io:get_line(ToBeShown),
                              Res = 'Dropboxlike_Repository':check_username(Dropboximpl, El),
                              case Res of
                                false -> 
                                    io:fwrite("Username not available. Try again...\n"),
                                    insert_username(Dropboximpl, ToBeShown);
                                true -> El
                              end.

insert_password() -> Password1 = string:strip(io:get_line("Enter your new password: "),both, $\n),
                                Password2 = string:strip(io:get_line("Enter your new password again: "),both, $\n),
                                case string:equal(Password1, Password2) of
                                    true ->
                                        Password1;
                                    false ->
                                        io:fwrite("Passwords don't match. Try again... \n"),
                                        insert_password()
                                end.



