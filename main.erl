-module(main).
-author('m4mbr3@gmail.com').
-export([init/0, dropboxlike_client/0]).
-include_lib("Dropboxlike/Dropboxlike.hrl").

init() ->
    mnesia:start(),
    corba:orb_init([{domain, "ramnode"}]),
    orber:install([node()], [{ifr_storage_type, ram_copies}]),
    orber:start(),
    'oe_dropbox-like':'oe_register'().

menu() ->
    io:fwrite("/*************************  DropboxLike   ***************************/\n"),
    io:fwrite("/***********  Author : Andrea Mambretti   Version 1.1   *************/ \n"),
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
            Home_env = "./Dropboxlikeclient";
        _ ->
            Home_env = Home++"/Dropboxlikeclient",
            file:make_dir(Home_env)
    end,
    Address = os:getenv("DROPBOXLIKESERVERIP"),
    case Address of
        false ->
            NS = corba:string_to_object("corbaloc:iiop:localhost:1050/NameService");
        _ ->
            NS = corba:string_to_object("corbaloc:iiop:"++Address++":1050/NameService")
    end,
    Dropboximpl = 'CosNaming_NamingContextExt':'resolve_str' (NS, "DBServer"),
    Dict = dict:new(),
    Login = dict:store(home, Home_env, Dict),
    Login1 = dict:store(username, "", Login),
    Login2 = dict:store(dev_id, "", Login1),
    Login3 = dict:store(token, "", Login2),
    clear(Login3),
    Login4 = dict:store(files, [], Login3),
    menu(),
    ReadCommandPid = spawn(fun read_command_loop/0),
    ExecuteCommandPid = spawn(fun execute_command_loop/0),
    UpdatePid = spawn(fun update_loop/0),
    ReadCommandPid!{main_loop, Login4, self()},
    loop_main(ReadCommandPid, ExecuteCommandPid, UpdatePid, Login4, Dropboximpl).

loop_main(ReadCommandPid, ExecuteCommandPid, UpdatePid, Dict, Dropboxlike) ->
    receive 
        {execute_loop, DictEx} ->
            ReadCommandPid!{main_loop, DictEx, self()},
            loop_main(ReadCommandPid, ExecuteCommandPid, UpdatePid, DictEx, Dropboxlike);
        {read_loop, Choice} ->
            if
                Choice =:= "exit" ->
                    ExecuteCommandPid!{main_loop, Dropboxlike, Dict, Choice, self()},
                    halt();
                true ->
                    ExecuteCommandPid!{main_loop, Dropboxlike, Dict, Choice, self()},
                    loop_main(ReadCommandPid, ExecuteCommandPid, UpdatePid, Dict, Dropboxlike)
            end
    after
        3000 ->
            UpdatePid!{main_loop,self(),Dict, Dropboxlike},
            receive_from_update(ReadCommandPid, ExecuteCommandPid, UpdatePid, Dict, Dropboxlike)

    end.

receive_from_update(ReadCommandPid, ExecuteCommandPid, UpdatePid, Dict, Dropboxlike) ->
    receive
        {update_loop, DictUp} ->
            loop_main(ReadCommandPid, ExecuteCommandPid, UpdatePid, DictUp, Dropboxlike);
        {none} ->
            loop_main(ReadCommandPid, ExecuteCommandPid, UpdatePid, Dict, Dropboxlike);
        {execute_loop, DictEx} ->
            self()!{execute_loop, DictEx},
            receive_from_update(ReadCommandPid, ExecuteCommandPid, UpdatePid, Dict, Dropboxlike);
        {read_loop, Choice} ->
            self()!{read_loop, Choice},
            receive_from_update(ReadCommandPid, ExecuteCommandPid, UpdatePid, Dict, Dropboxlike)
    end.

update_loop() ->
    receive
        {main_loop,MainPID,Dict,Dropboximpl} ->
            case string:equal(dict:fetch(username, Dict),"") and string:equal(dict:fetch(token, Dict),"") of
                true ->
                    MainPID!{none},
                    update_loop();
                false ->
                    Files = dict:fetch(files, Dict),
                    FilesFromServer = 'Dropboxlike_Repository':askListUser(Dropboximpl,dict:fetch(username, Dict),dict:fetch(token, Dict)),
                    NewFilesList = downloadNewFile(FilesFromServer, Files, Dict, Dropboximpl),
                    Dict_up = dict:append_list(files, NewFilesList, dict:erase(files,Dict)),
                    ListFile = record_to_list(FilesFromServer),
                    NewDict = removeFile(NewFilesList, ListFile, Dict_up),
                    NewDictUp = updateFileModified(dict:fetch(files, NewDict), NewDict, Dropboximpl),
                    MainPID!{update_loop,NewDictUp},
                    update_loop()
            end
    end.

record_to_list ([X|XS]) ->
    FileName = X#'Dropboxlike_SmallL'.'name',
    SHA512 = X#'Dropboxlike_SmallL'.'md5',
    lists:append([[FileName,SHA512]],record_to_list(XS));
record_to_list ([]) -> [].

updateFileModified([X|XS], Dict, Dropboxlike) ->
    try
        [Name|Sha512_list] = X,
        OldPath = file:get_cwd(),
        file:set_cwd(dict:fetch(home, Dict)++"/"++dict:fetch(username,Dict)),
        {ok, Binary} = file:read_file(Name),
        ContextSha512 = crypto:hash_init(sha512),
        PartialSha512 = crypto:hash_update(ContextSha512, Binary),
        Sha512 = hexstring(crypto:hash_final(PartialSha512)),
        case string:equal(Sha512, Sha512_list) of
            true ->
                file:set_cwd(OldPath),
                updateFileModified(XS, Dict, Dropboxlike);
            false ->
                Entity = #'Dropboxlike_FileAtRepository' {'ownerUserName'=dict:fetch(username,Dict), 'md5'=Sha512, 'cont'=Binary, 'name'=Name},
                'Dropboxlike_Repository':updateFile(Dropboxlike, Entity, dict:fetch(username,Dict), dict:fetch(token, Dict)),
                ToInsert = lists:append([[Name, Sha512]], remove_from_list(Name, dict:fetch(files,Dict))),	
                DictWithoutList = dict:erase(files, Dict),
                DictUp = dict:append_list(files, ToInsert, DictWithoutList),
                updateFileModified(XS,DictUp, Dropboxlike)
        end
    catch
        throw:_ -> erlang:display(erlang:get_stacktrace());
        exit:_ -> erlang:display(erlang:get_stacktrace());
        error:_ -> erlang:display(erlang:get_stacktrace())
    end;
updateFileModified([], Dict, _) -> Dict.


removeFile([X|XS], FilesFromServer, Dict) ->
    try
        [Head|_] = FilesFromServer,
        [FName,_] = Head,
        case string :equal(FName, "NULL") of
            true ->
                [FileName, _] = X,
                {ok, OldPath} =  file:get_cwd(),
                file:set_cwd(dict:fetch(home,Dict)++"/"++dict:fetch(username, Dict)),
                case file:delete(FileName) of
                    ok ->
                        Files = dict:fetch(files,Dict),
                        NewFiles = remove_from_list(FileName, Files),
                        NewDict = dict:erase(files,Dict),
                        ToReturn = dict:append_list(files, NewFiles, NewDict),
                        {ok, File} = file:open(".data", [write]),
                        write_on_file(NewFiles, File, dict:fetch(username, ToReturn)),
                        file:set_cwd(OldPath),
                        removeFile(XS,FilesFromServer, ToReturn);
                    {error,_} ->
                        file:set_cwd(OldPath),
                        io:fwrite("Error: Impossible to remove a file during the update \n"),
                        removeFile(XS,FilesFromServer, Dict)
                end;
            false ->
                [FileName, _] = X,
                case already_present_name(FileName, FilesFromServer) of
                    true ->
                        removeFile(XS, FilesFromServer, Dict);
                    false ->
                        {ok, OldPath} = file:get_cwd(),
                        file:set_cwd(dict:fetch(home,Dict)++"/"++dict:fetch(username, Dict)),
                        case file:delete(FileName) of
                            ok ->
                                Files = dict:fetch(files, Dict),
                                NewFiles = remove_from_list(FileName, Files),
                                NewDict = dict:erase(files,Dict),
                                ToReturn = dict:append_list(files, NewFiles, NewDict),
                                {ok, File} = file:open(".data",[write]),
                                write_on_file(NewFiles, File, dict:fetch(username,ToReturn)),
                                file:set_cwd(OldPath),
                                removeFile(XS,FilesFromServer, ToReturn);
                            {error,_} ->
                                file:set_cwd(OldPath),
                                io:fwrite("Error: Impossible to remove a file during the update\n"),
                                removeFile(XS,FilesFromServer, Dict)
                        end
                end
        end
    catch
        throw:_ -> erlang:display(erlang:get_stacktrace());
        exit:_ -> erlang:display(erlang:get_stacktrace());
        error:_ -> erlang:display(erlang:get_stacktrace())
    end;

removeFile([], _, Dict )  -> Dict.

downloadNewFile([X|XS], FilesList, Dict, Dropboxlike) ->
    try
        FileName = X#'Dropboxlike_SmallL'.'name',
        Sha512 = X#'Dropboxlike_SmallL'.'md5',
        case string:equal(FileName, "NULL") of
            true-> FilesList;
            false ->
                case already_present_name(FileName, FilesList) of
                    true ->
                        case already_present(FileName, Sha512, FilesList) of
                            true ->
                                downloadNewFile(XS, FilesList, Dict, Dropboxlike);
                            false ->
                                DownloadedFile = 'Dropboxlike_Repository':get_file(Dropboxlike,dict:fetch(username, Dict), dict:fetch(token, Dict), FileName ),
                                {ok,OldPath} = file:get_cwd(),
                                file:set_cwd(dict:fetch(home, Dict)++"/"++dict:fetch(username, Dict)),
                                case file:write_file(FileName, DownloadedFile#'Dropboxlike_FileAtRepository'.'cont',[write]) of
                                     ok -> none;
                                    {error,_} -> io:fwrite("Error: Impossible to update your files\n")
                                end,
                                NewListFile = lists:append(remove_from_list(FileName, FilesList), [[DownloadedFile#'Dropboxlike_FileAtRepository'.'name', DownloadedFile#'Dropboxlike_FileAtRepository'.'md5']]),
                                {ok,File} = file:open(".data",[write]),
                                write_on_file(NewListFile, File, dict:fetch(username,Dict)),
                                file:close(File),
                                file:set_cwd(OldPath),
                                downloadNewFile(XS,NewListFile, Dict, Dropboxlike)
                        end;
                    false ->
                        DownloadedFile = 'Dropboxlike_Repository':get_file(Dropboxlike, dict:fetch(username, Dict), dict:fetch(token, Dict), FileName),
                        {ok,OldPath} = file:get_cwd(),
                        file:set_cwd(dict:fetch(home, Dict)++"/"++dict:fetch(username, Dict)),
                        case file:write_file(FileName, DownloadedFile#'Dropboxlike_FileAtRepository'.'cont',[write]) of
                            ok -> none;
                            {error,_} -> io:fwrite("Error: Impossible to update your files\n")
                        end,
                        case file:open(".data", [append]) of
                            {ok, Pointer} ->
                                io:format(Pointer, "~s:~s:~s\n",[DownloadedFile#'Dropboxlike_FileAtRepository'.'name',DownloadedFile#'Dropboxlike_FileAtRepository'.'md5', DownloadedFile#'Dropboxlike_FileAtRepository'.'ownerUserName']);
                            {error,_} -> 
                                case file:open(".data",[write] ) of
                                    {ok, Pointer2} ->
                                        io:format(Pointer2, "~s:~s:~s\n",[DownloadedFile#'Dropboxlike_FileAtRepository'.'name',DownloadedFile#'Dropboxlike_FileAtRepository'.'md5', DownloadedFile#'Dropboxlike_FileAtRepository'.'ownerUserName']);
                                    {error,_} ->
                                        io:fwrite("Error: I cannot update the metadata during the update\n")
                                end
                        end,
                        NewListFile = lists:append(FilesList, [[DownloadedFile#'Dropboxlike_FileAtRepository'.'name', DownloadedFile#'Dropboxlike_FileAtRepository'.'md5']]),
                        file:set_cwd(OldPath),
                        downloadNewFile(XS,NewListFile, Dict, Dropboxlike)
                end
        end
    catch
        throw:_ -> erlang:display(erlang:get_stacktrace());
        exit:_ -> erlang:display(erlang:get_stacktrace());
        error:_ -> erlang:display(erlang:get_stacktrace())
    end;
downloadNewFile([], FilesList,  _, _) -> FilesList.




read_command_loop() ->
    receive
        {main_loop, Data, PidMain } ->
            case  string:equal(dict:fetch(username, Data),"") of
                true ->
                    {ok,[Choice]} = io:fread("dropboxlike $ ", "~s");
                false ->
                    {ok, [Choice]} = io:fread(dict:fetch(username,Data)++"@dropboxlike $ ", "~s")
            end,
            PidMain!{read_loop, Choice},
            if
                Choice =:= "exit" ->
                    halt();
                true ->
                    read_command_loop()
            end
    end.


execute_command_loop() ->
    receive 
        {main_loop, Dropboximpl, Data, Exp, PidMain } ->
            New_Data =
                case Exp of
                    "subscribe"->
                        subscribe(Dropboximpl,Data);
                    "remove_account" ->
                        remove_account(Dropboximpl, Data);
                    "login" ->
                        login(Dropboximpl,Data);
                    "logout" ->
                        logout(Dropboximpl,Data);
                    "send_file" ->
                        send_file(Dropboximpl, Data);
                    "remove_file" ->
                        remove_file(Dropboximpl, Data);
                    "clear" ->
                        clear(Data);
                    "dir" ->
                        ls(Dropboximpl, Data);
                    "ls" ->
                        ls(Dropboximpl, Data);
                    "help" ->
                        help(Data);
                    "exit" ->
                        go_out(Dropboximpl, Data);
                    NoCommand ->
                        io:fwrite("dropboxlike: "++NoCommand++": command not found\n"),
                        Data
                end,
            PidMain!{execute_loop, New_Data},
            execute_command_loop()
    end.

subscribe(Dropboximpl,Data) ->
    Name = string:strip(io:get_line("Insert your name: "),both, $\n),
    Surname = string:strip(io:get_line("Insert your surname: "), both, $\n),
    Username = string:strip(insert_username(Dropboximpl, "Insert your username: "),both, $\n),
    Password = string:strip(insert_password(),both, $\n),
    case 'Dropboxlike_Repository':subscribe(Dropboximpl, Name, Surname, Username, Password) of 
        true ->
            io:fwrite("Registered user \n"),
            file:make_dir(dict:fetch(home,Data)++"/"++Username),
            io:fwrite("Home directory for user "++ Username ++ " created at "++dict:fetch(home,Data)++"/"++Username++"\n");
        false ->
            io:fwrite("Error during the registration \n")
    end,
    Data.


remove_account(Dropboximpl,Data) ->
    Username = string:strip(io:get_line("Insert your username: "), both, $\n),
    Password = string:strip(io:get_line("Insert the password for "++ Username ++": "), both, $\n),
    case 'Dropboxlike_Repository':remove(Dropboximpl, Username,Password) of
        true ->
            io:fwrite("Removed user \n"),
            file:del_dir(dict:fetch(home,Data)++"/"++Username),
            io:fwrite("Home directory for user "++ Username ++" removed\n");
        false ->
            io:fwrite("Error: maybe the user or/and the password are wrong\n")
    end,
    Data.


login(Dropboximpl, L) ->
    case 'Dropboxlike_Repository':isLogged(Dropboximpl, dict:fetch(username,L), dict:fetch(token,L)) of
    true ->
        io:fwrite("You are already logged!!!\n"), 
        io:fwrite("If you want to log in with a different user, please log out first\n"),
        L;
    false ->
        Dev_id = string:strip(io:get_line("Insert your hostname: "),both, $\n),
        Username = string:strip(io:get_line("Insert your username: "),both, $\n),
        Password = string:strip(io:get_line("Insert the password for "++ Username++ ": "), both, $\n),
        case 'Dropboxlike_Repository':login(Dropboximpl, Username, Password, Dev_id) of
            "INVALID_USER" ->
                io:fwrite("Error: maybe the user or/and the password are wrong\n"),
                L;
            Token ->
                L1 = dict:store(username,Username,L),
                L2 = dict:store(dev_id, Dev_id, L1),
                L3 = dict:store(token, Token, L2),
                L4 = load_info(L3),
                L4
        end
    end.

load_info(Dict) ->
    {ok, OldPath} = file:get_cwd(),
    case file:make_dir(dict:fetch(home,Dict)++"/"++dict:fetch(username, Dict)) of
        ok -> ok;
        {error, _} -> error
    end,
    case file:set_cwd(dict:fetch(home, Dict)++"/"++dict:fetch(username, Dict)) of
        ok -> ok;
        {error, _} -> error
    end,
    case file:open(".data",[read]) of
        {ok, File} ->
            List = read_line_from_file(File),
            file:set_cwd(OldPath),
            dict:append_list(files,List,Dict);
        {error,_} ->
            file:set_cwd(OldPath),
            dict:store(files,[],Dict)
    end.


read_line_from_file(File) ->
    case file:read_line(File) of
        {ok,Data} ->
            [FileName, Sha512, _] = string:tokens(Data, ":"),
            lists:append([[FileName, Sha512]], read_line_from_file(File));
        eof -> [];
        {error, _} -> io:fwrite("Impossible to read local metadata\n")
    end.

logout(Dropboximpl, L) ->
    case 'Dropboxlike_Repository':isLogged(Dropboximpl, dict:fetch(username, L), dict:fetch(token,L)) of
        false ->
            io:fwrite("Error: you are not logged\n"),
            L;
        true ->
            'Dropboxlike_Repository':logout(Dropboximpl, dict:fetch(username, L), dict:fetch(dev_id, L), dict:fetch(token, L)),
            L1 = dict:store(username, "", L),
            L2 = dict:store(dev_id, "", L1),
            L3 = dict:store(token, "", L2),
            L4 = dict:store(files, [],L3),
            L4
    end.

send_file(Dropboximpl, L) ->
    case 'Dropboxlike_Repository':isLogged(Dropboximpl, dict:fetch(username, L), dict:fetch(token, L)) of
        false ->
            io:fwrite("Error: you are not logged\n"),
            L;
        true ->
            Path = string:strip(io:get_line("Insert the _complete_ path to the file to upload: "),both, $\n),
            try
                {ok,OldPath} = file:get_cwd(),
                file:set_cwd(filename:dirname(Path)),
                {ok, Binary} = file:read_file(filename:basename(Path)),
                ContextSha512 = crypto:hash_init(sha512),
                PartialSha512 = crypto:hash_update(ContextSha512, Binary),
                Sha512 = hexstring(crypto:hash_final(PartialSha512)),
                case already_present(filename:basename(Path),Sha512,dict:fetch(files,L)) of
                    false ->
                        file:set_cwd(dict:fetch(home,L)++"/"++dict:fetch(username,L)),
                        file:write_file(filename:basename(Path), Binary),
                        Basename = filename:basename(Path),
                        Owner = dict:fetch(username,L),
                        Entity = #'Dropboxlike_FileAtRepository' {'ownerUserName'=Owner, 'md5'=Sha512, 'cont'=Binary, 'name'=Basename},
                        'Dropboxlike_Repository':send(Dropboximpl,Entity, dict:fetch(username, L), dict:fetch(token, L)),
                        case file:read_file_info(".data") of
                            {ok, _} ->
                                file:write_file(".data", Basename++":"++Sha512++":"++Owner++"\n", [append]);
                            {error, enoent} ->
                                file:write_file(".data", Basename++":"++Sha512++":"++Owner++"\n", [write])
                        end,
                        L2 = dict:append(files,[Basename, Sha512], L),
                        file:set_cwd(OldPath),
                        L2;
                    true ->
                        io:fwrite("Error: The file is already in your repository\n"),
                        L
                end
            catch
                {error, Reason} -> 
                    io:fwrite("Error: " ++ Reason),
                    io:fwrite("Try again...\n"),
                    send_file(Dropboximpl, L)
            end
    end.

already_present_name(FileName,[X|XS]) ->
    [Name, _] = X,
    case string:equal(FileName,Name) of
        true ->
            true;
        false ->
            already_present_name(FileName,XS)
    end;
already_present_name(_,[]) -> false.

already_present(FileName,Sha512,[X|XS]) ->
    [Name,Sha] = X,
    case string:equal(FileName, Name) and string:equal(Sha512,Sha) of
        true ->
            true;
        false ->
            already_present(FileName,Sha512, XS)
    end;
already_present(_,_,[]) -> false.

is_present(FileName, [X|XS]) ->
    [Name, _] = X, 
    case string:equal(FileName, Name) of
        true ->
            true;
        false ->
            is_present(FileName, XS)
    end;
is_present(_, []) -> false.


hexstring(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end,
        binary_to_list(Binary))).

remove_file(Dropboximpl, L) ->
    case 'Dropboxlike_Repository':isLogged(Dropboximpl, dict:fetch(username,L), dict:fetch(token, L)) of
        false ->
            io:fwrite("Error: you are not logged\n"),
            L;
        true ->
            ls(Dropboximpl, L),
            FileName = string:strip(io:get_line("Insert the file to delete: "),both,$\n),
            case dict:fetch(files, L) of
                [] ->
                    io:fwrite("Error: the filename provided is not valid. Try again...\n"),
                    L;
                Files ->
                    case is_present(FileName, Files) of
                        true ->
                            'Dropboxlike_Repository':delete(Dropboximpl,FileName, dict:fetch(username, L), dict:fetch(token, L)),
                            NewFiles = remove_from_list(FileName, Files),
                            NewDict = dict:erase(files,L),
                            ToReturn = dict:append_list(files, NewFiles, NewDict),
                            {ok,OldPath} = file:get_cwd(),
                            case file:set_cwd(dict:fetch(home, ToReturn)++"/"++dict:fetch(username,ToReturn)) of
                                ok -> ok;
                                {error, _} -> erlang:display("Problem to set_cwd ")
                            end,
                            file:delete(FileName),
                            {ok,File} = file:open(".data",[write]),
                            write_on_file(NewFiles, File, dict:fetch(username,L)),
                            file:close(File),
                            file:set_cwd(OldPath),
                            ToReturn;
                        false ->
                            io:fwrite("Error: the filename provided is not valid. Try again...\n"),
                            L
                    end
            end
    end.

write_on_file([X|XS], File,Username) ->
    [Name,Sha512] = X,
    io:format(File, "~s:~s:~s\n",[Name, Sha512, Username]),
    write_on_file(XS, File, Username);
write_on_file([],_,_) -> done.


remove_from_list(FileName, [X|XS]) ->
    [Name, Sha512] = X,
    case string:equal(FileName, Name) of
        true ->
            remove_from_list(FileName, XS);
        false ->
            lists:append([[Name,Sha512]], remove_from_list(FileName,XS))
    end;
remove_from_list(_,[]) -> [].


clear(L) ->
    io:fwrite("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"),
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
                Files -> print_file_list(Files, 0)
            end,
            L
    end.

print_file_list([X|XS], Num) ->
    [Name, _] = X,
    io:fwrite(integer_to_list(Num) ++ ") " ++ Name ++"\n"),
    print_file_list(XS, Num+1);
print_file_list([], _)-> finish.


help(L) -> menu(),
    L.

go_out(Dropboximpl, L) ->
    case 'Dropboxlike_Repository':isLogged(Dropboximpl, dict:fetch(username, L), dict:fetch(token,L)) of
        false ->
            halt();
        true ->
            'Dropboxlike_Repository':logout(Dropboximpl, dict:fetch(username, L), dict:fetch(dev_id, L), dict:fetch(token, L)),
            halt()
    end.



insert_username(Dropboximpl, ToBeShown) ->
    El = io:get_line(ToBeShown),
    Res = 'Dropboxlike_Repository':check_username(Dropboximpl, El),
    case Res of
        false ->
            io:fwrite("Username not available. Try again...\n"),
            insert_username(Dropboximpl, ToBeShown);
        true ->
            El
    end.

insert_password() ->
    Password1 = string:strip(io:get_line("Enter your new password: "),both, $\n),
    Password2 = string:strip(io:get_line("Enter your new password again: "),both, $\n),
    case string:equal(Password1, Password2) of
        true ->
            Password1;
        false ->
            io:fwrite("Passwords don't match. Try again... \n"),
            insert_password()
    end.



