-module(main).
-export([init/0]).

dropboxlike_client(X) -> Ciao=1.

init() -> 
    mnesia:start(),
    corba:orb_init([{domain, "ramnode"}]),
    orber:install([node()], [{ifr_storage_type, ram_copies}]),
    orber:start(),
    'oe_Account':'oe_register'(),
    dropboxlike_client("A").

run() ->
    NS = corba:string_to_object("corbaloc:iiop:localhost:1050/NameService"),
    Dropboximpl = 'CosNaming_NamingContextExt':'resolve_str' (NS, "DBServer").
